#!/software/bin/perl

# populate the interpro and gene_ontology tables in the Pfam database,
# given a list of Pfam accessions from the database and a set of data
# files downloaded from interpro
# JT 20051124 WTSI

# uses two tables, "interpro" and "gene_ontology", previously created
# as follows:
#
# CREATE TABLE interpro (
#   auto_pfamA     int(10)  UNSIGNED NOT NULL,
#   interpro_id    tinytext NOT NULL,
#   abstract       longtext,
#   KEY          ( auto_pfamA ),
#   FULLTEXT KEY   text_index ( interpro_id, abstract )
# )
#
# CREATE TABLE gene_ontology (
#   auto_pfamA     int(10)  UNSIGNED NOT NULL,
#   go_id          tinytext NOT NULL,
#   term           longtext,
#   category       tinytext,
#   FULLTEXT KEY   text_index ( go_id, term )
# )

use strict;
use warnings;

use XML::LibXML;
use CGI;
use File::Basename;
use File::Copy;
use Getopt::Std;
use LWP::Simple;
use Log::Log4perl qw(:easy);
Log::Log4perl->easy_init($DEBUG);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

my $logger = Log::Log4perl->get_logger();
$logger->debug( "Using database:" . $config->pfamlive->{database} );

use vars qw( %opts );

$| = 1;

#-------------------------------------------------------------------------------
# function prototypes

sub usage( ;$ );
sub parseInterproXML;
sub parseOntologies;
sub parseInterpro2Go;
sub getPfamAcc( $ );

#-------------------------------------------------------------------------------
# parse the command line options

getopts( "hd:f:", \%opts ) or usage;
usage(1) if $opts{h};

#-------------------------------------------------------------------------------
# configuration

# point to the data files

my $ROOT = $config->localDbsLoc . '/interpro';

unless ( -d $ROOT ) {
  mkdir($ROOT) or $logger->logdie("Could not create directory $ROOT:[$!]");
}

my $INTERPRO_FILE = "$ROOT/interpro.xml";
my @ONTOLOGIES    = (
  "$ROOT/function.ontology", "$ROOT/component.ontology",
  "$ROOT/process.ontology"
);
my $INTERPRO2GO_FILE = "$ROOT/interpro2go";

#before we go an retrieve the files, move any existing ones sideways
foreach ( $INTERPRO_FILE, @ONTOLOGIES, $INTERPRO2GO_FILE, 'interpro.xml'  ) {
  if(-e $ROOT . '/' . $_){
    $logger->debug("Movig \"$_\" sidways");
    move( $ROOT . '/' . $_, $ROOT . '/' . $_ . '.old' )
      or $logger->logdie("Failed to move $_ to $_.old in $ROOT:[$!]");
  }
}

$logger->debug('Downloading files');
#Now go and get the files from InterPro!
foreach my $file (qw(interpro.xml.gz interpro2go)) {
  my $rc = getstore( "ftp://ftp.ebi.ac.uk/pub/databases/interpro/$file", $ROOT . "/" . $file );
  $logger->logdie("Failed to get the file $file:[ got response code $rc ]")
    unless ( is_success($rc) );
}
system("gunzip $ROOT/interpro.xml.gz")
  and $logger->logdie("Could not gunzip interpro.xml.gz [$!]");

foreach my $file (qw(component.ontology function.ontology process.ontology)) {
  my $rc = getstore( "ftp://ftp.geneontology.org/pub/go/ontology/$file", $ROOT . "/" . $file );
  $logger->logdie( "Failed to get the file $file:[ got response code $rc ]")
    unless ( is_success($rc) );
}

# make sure all of the data files are found
foreach ( $INTERPRO_FILE, @ONTOLOGIES, $INTERPRO2GO_FILE ) {
  $logger->logdie( "(EE) ERROR: couldn't find \"$_\": $!") unless -f $_;
}

# set up the database connection
my $dbh      = $pfamDB->getSchema->storage->dbh;
print $logger->info( "(ii) successfully connected to database" );

# the various SQL queries
my $ipDelete = "DELETE FROM interpro";
my $goDelete = "DELETE FROM gene_ontology";
my $apQuery  = "SELECT pfamA_Acc, auto_pfamA FROM pfamA";
my $ipQuery =
"INSERT INTO interpro ( auto_pfamA, interpro_id, abstract ) VALUES ( ?, ?, ? )";
my $goQuery =
"INSERT INTO gene_ontology ( auto_pfamA, go_id, term, category ) VALUES ( ?, ?, ?, ? )";

my $deadQuery = "SELECT COUNT(*) FROM dead_families WHERE pfamA_acc = ?";

# prepare the insert statement handles
my $apSth = $dbh->prepare($apQuery)
  or $logger->logdie("(EE) ERROR: couldn't prepare auto_pfamA select query: ",
  $dbh->errstr );
my $ipSth = $dbh->prepare($ipQuery)
  or $logger->logdie( "(EE) ERROR: couldn't prepare interpro insert query: ",
  $dbh->errstr);
my $goSth = $dbh->prepare($goQuery)
  or $logger->logdie( "(EE) ERROR: couldn't prepare gene_ontology insert query: ",
  $dbh->errstr);

$logger->info( "(iia) prepared statement handles" );

$logger->debug("executing $ipDelete");
$dbh->do($ipDelete)
  or die "(EE) ERROR: could not delete from interpro:", $dbh->errstr;

$logger->debug("executing $goDelete");
$dbh->do($goDelete)
  or die "(EE) ERROR: could not delete from gene_ontology:", $dbh->errstr;

$logger->info( "(iib) delete contents from interpro and gene_ontology tables");

#-------------------------------------------------------------------------------
# main

# get the list of pfamA_acc ---> auto_pfamA mappings
my $pfamAccMapping = getPfamAcc($apSth);
$logger->info("(ii) got PfamAcc-to-auto_pfamA mapping from DB");

# parse the interpro XML file
my $ip = parseInterproXML;
$logger->info("(ii) parsed interpro XML");

# parse the interpro2go file
my $ip2go = parseInterpro2Go;
$logger->info("(ii) parsed interpro2go file");

# parse the ontology files
my $ont = parseOntologies;
$logger->info("(ii) parsed ontology data files");

my ( $auto_pfamA, $ipId, $ipAbstract, $goId, $goCat, $goInfo );

PFAMID: foreach my $pfamAcc ( keys %{$ip} ) {

  $auto_pfamA = $pfamAccMapping->{$pfamAcc};

  # if there's no auto_pfamA number for this family, check if it's a
  # dead entry
  unless ( defined $auto_pfamA ) {

    # should return undef if this pfamA family exists
    my $found = $dbh->do( $deadQuery, undef, $pfamAcc );
    unless ( defined $found ) {
      $logger->warn("(WW) WARNING: no auto_pfamA value for existing family \"$pfamAcc\"");
    }

    # skip to the next family, whatever the result of the dead check
    next PFAMID;
  }

  $ipId = $ip->{$pfamAcc}->{interproId};

  $ipAbstract =
    defined $ip->{$pfamAcc}->{abstract}
    ? $ip->{$pfamAcc}->{abstract}
    : "null";

  $ipSth->execute( $auto_pfamA, $ipId, $ipAbstract )
    or $logger->warn(
"(WW) WARNING: couldn't complete insert for interpro values |$auto_pfamA|$ipId|<abstract omitted>| (pfamA_acc: |$pfamAcc|) ",
    $dbh->errstr)
    and next PFAMID;

  foreach $goId ( keys %{ $ip2go->{$ipId} } ) {
  GOCAT: foreach $goCat (qw( component function process )) {

      next GOCAT unless exists $ont->{$goCat}->{$goId};

      $goInfo = $ip2go->{$ipId}->{$goId};

      $goSth->execute( $auto_pfamA, $goId, $goInfo, $goCat )
        or $logger->warn(
"(WW) WARNING: couldn't complete insert for values |$auto_pfamA|$goId|$goInfo|$goCat|: ",
        $dbh->errstr)
        and next GOCAT;
    }
  }
}

$logger->info( "(ii) done." );

exit;

#===============================================================================
#= subroutines =================================================================
#===============================================================================

sub usage( ;$ ) {

  my $long = shift;

  my $me = basename $0;

  print STDERR "usage: $me [-h] -d database_name -f path_to_files\n";

  print STDERR << "EOFmsg" if defined $long;

 -h      : prints this message

example: $me 

This script populates the "interpro" and "gene_ontology" tables in the
Pfam database. It needs to connect to the database first to extract a
list of Pfam accessions and then to insert data. The data are extracted
from a set of flat files and an XML file that are downloaded from
interpro. These files need to exist before this script is run:

    interpro.xml
    interpro2go
    component.ontology
    function.ontology
    process.ontology

EOFmsg

  exit;
}

#-------------------------------------------------------------------------------

sub parseInterproXML {

  my $parser = XML::LibXML->new;
  my $cgi    = CGI->new;

  # original file: /pfam/data1/localdbs/interpro/interpro.xml
  my $DOM = $parser->parse_file($INTERPRO_FILE)
    or die "(EE) ERROR: couldn't parse file: $!";

  my $root = $DOM->documentElement;

  my %ip = ();
  my $entry;
  foreach my $ipNode ( $root->findnodes("/interprodb/interpro") ) {

    # check for PFAM cross-references in this interpro entry
    my @pfamAccs =
      $ipNode->findnodes("member_list/db_xref[\@db='PFAM']/\@dbkey");

    next unless scalar @pfamAccs;

    # the interpro ID
    $entry = $ipNode->getAttribute("id");

    #print "\n-------------------- $entry --------------------\n";

    my %ipEntry = ();
    $ipEntry{interproId} = $entry;

    # get the abstract
    $ipEntry{abstract} = $ipNode->findvalue("abstract/text()");

    # parse the abstract a little further, exchanging non-text child
    # nodes for HTML links
    my $abstractNode = ( $ipNode->findnodes("abstract") )[0];

    my ( $abstractText, $url, $pmRef, $pmId );
    $url = "";
    foreach my $child ( $abstractNode->childNodes ) {

      # text nodes and CDATA sections can be handled the same way, as
      # plain text
      if ( $child->nodeType == XML::LibXML::XML_TEXT_NODE
        or $child->nodeType == XML::LibXML::XML_CDATA_SECTION_NODE )
      {

        # add straight to the abstract string
        $abstractText .= $child->data;

      }
      else {

        # anything else needs to be treated as an XML node and messed
        # with appropriately...

        if ( $child->nodeName eq "cite" ) {

          # get the pubmed ID for this reference
          $pmRef = $child->getAttribute("idref");
          $pmId  = $ipNode->findvalue(
            "pub_list/publication[\@id='$pmRef']/db_xref/\@dbkey");

          # map it into the URL for pubmed
          $url = 'http://www.ncbi.nlm.nih.gov:80/entrez/query.fcgi?cmd=Retrieve&amp;db=PubMed&amp;dopt=Abstract&amp;list_uids=';
            
          # drop it into the abstract text
          $abstractText .=
            $cgi->a( { -href => $url . $pmId }, "PUBMED:" . $pmId );

        }
        elsif ( $child->nodeName eq "taxon" ) {

          # map to a URL
          $url = 'http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=$id';

          # add the tax ID
          $url =~ s/\$id/$child->getAttribute("tax_id")/e;

          # add to the abstract
          $abstractText .= $cgi->a( { -href => $url }, $child->textContent );
        }
      }
    }

    # store the abstract that we've painstakingly built
    $ipEntry{abstract} = $abstractText;

    foreach my $pubNode ( $ipNode->findnodes("pub_list/publication") ) {
      push @{ $ipEntry{publications} }, $pubNode->getAttribute("id");
    }

    # and stuff the hash containing the data for this interpro entry
    # into a hash containing data for all interpro entries
    foreach my $pfamAcc (@pfamAccs) {
      $ip{ $pfamAcc->getValue } = \%ipEntry;

     # NB need to use getValue because $pfamAcc contains XML::LibXML::Attr nodes
    }
  }

  #foreach my $entry ( sort keys %ip ) {
  #  print "-------------------- $entry----------------\n";
  #  print "abstract:\n$ip{$entry}->{abstract}\n";
  #}

  return \%ip;
}

#-------------------------------------------------------------------------------

sub parseOntologies {

  my %terms;

  foreach my $file (@ONTOLOGIES) {

    open( FILE, $file )
      or $logger->logdie( "(EE) ERROR: couldn't open file \"$file\": $!");

    ( $file = basename $file ) =~ s/^(\w+)\..*/$1/;

    my $count = 0;
    while ( my $line = <FILE> ) {

      next if $line =~ /^\s*\!/;

      foreach my $goId ( grep /(GO:\d+)/, split( /s+/, $line )) {
        chomp $goId;
        $goId =~ s/.*?(GO:\d+).*/$1/;
        $terms{$file}{$goId} = "";
      }
    }

    close FILE;
  }

  return \%terms;
}

#-------------------------------------------------------------------------------

sub parseInterpro2Go {

  my %goData;

  open( FILE, $INTERPRO2GO_FILE )
    or $logger->logdie( "(EE) ERROR: couldn't open \"$INTERPRO2GO_FILE\": $!" );

  while (<FILE>) {

    next if /^\s*\!/;

    /InterPro:(IPR\d{6}) .*? \> GO:(.*?) \; (GO:\d{7})\s*$/;

    # $1 = interpro ID
    # $2 = GO description text
    # $3 = GO ID
    #print "|$1| -> |$3| = |$2|\n";

    $goData{$1}->{$3} = $2;

  }

  return \%goData;
}

#-------------------------------------------------------------------------------

sub getPfamAcc( $ ) {

  my $sth = shift;
  $sth->execute
    or $logger->logdie( "(EE) ERROR: couldn't execute \"select\" for auto_pfamA numbers: ",
    $sth->errstr);

  my $rs = $sth->fetchall_arrayref
    or $logger->logdie("(EE) ERROR: couldn't retrieve auto_pfamA numbers: ",
    $sth->errstr);

  my %mapping = ();
  foreach my $row (@$rs) {
    $mapping{ $row->[0] } = $row->[1];
  }

  return \%mapping;
}

#-------------------------------------------------------------------------------
