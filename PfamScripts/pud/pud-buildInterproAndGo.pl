#!/usr/bin/env perl

# build the interpo and gene_ontology tables in pfam_live
# jt6 20111209 WTSI
#
# $Id$

use strict;
use warnings;

use File::Copy;
use LWP::Simple;
use XML::LibXML 1.70;
use Getopt::Long;
use Pod::Usage;
use CGI;
use Log::Log4perl qw(get_logger :levels);
use Data::Dump qw( dump );

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#-------------------------------------------------------------------------------
# set up logging

BEGIN {
    Log::Log4perl->init( \<<EOF
log4perl.rootLogger=INFO, SCREEN
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p: %F{1} line %L (%M): %m%n
EOF
    );
}
my $log = get_logger();

#-------------------------------------------------------------------------------
# config

my $config = Bio::Pfam::Config->new;

my $IP_URL_ROOT = 'https://ftp.ebi.ac.uk/pub/databases/interpro/current_release/';
my $GO_URL_ROOT = 'http://current.geneontology.org/ontology';
my $FILE_ROOT = $config->localDbsLoc . '/interpro';

# URLs that will be substituted into the abstract
my $pm_url  = 'http://www.ncbi.nlm.nih.gov:80/entrez/query.fcgi?cmd=Retrieve&amp;db=PubMed&amp;dopt=Abstract&amp;list_uids=';
my $tax_url = 'http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=';
my %db_xref_urls = (
  CAZY       => 'http://www.cazy.org/%s.html',
  EC         => 'http://expasy.org/enzyme/%s',
  INTERPRO   => 'http://www.ebi.ac.uk/interpro/entry/%s',
  PROSITEDOC => 'http://expasy.org/prosite/%s',
  SWISSPROT  => 'http://www.uniprot.org/uniprot/%s',
);

# other possible databases for db_xrefs
# BLOCKS CATH COG COMe GENPROP INTACT IUPHAR MEROPS MSDsite #
# PANDIT PDB PFAM PIRSF PRIAM PROSITE PUBMED SCOP TC TIGRFAMs
# TIGRFAMS TREMBL

#-------------------------------------------------------------------------------
# set up

# process the command-line options
my $SKIP_RETRIEVALS = 0;
my $DEBUGGING       = 0;
my $HELP            = 0;
GetOptions( 'skipdl' => \$SKIP_RETRIEVALS,
            'root=s' => \$FILE_ROOT,
            'debug'  => \$DEBUGGING,
            'help|?' => \$HELP )
  or $log->logdie( 'ERROR: failed to process command-line arguments' );

pod2usage( -exitstatus => 0,
           -verbose    => 2) if $HELP;

# check that the root directory exists
unless ( -d $FILE_ROOT ) {
  mkdir( $FILE_ROOT )
    or $log->logdie( "ERROR: could not create directory '$FILE_ROOT': $!" );
  $log->info( "created directory '$FILE_ROOT'" );
}

# turn on debugging ?
if ( $DEBUGGING or $ENV{DEBUG} ) {
  $log->level( $DEBUG );
  $log->debug( 'debug messages enabled' );
  $ENV{DBIC_TRACE} = 1;
}

$log->debug( "saving downloaded files to root directory '$FILE_ROOT'" );

#-------------------------------------------------------------------------------
# retrieve files

my $ip_file    = 'interpro.xml';
my $ip2go_file = 'interpro2go';
my $ontologies = [ 'go.obo' ];

if ( $SKIP_RETRIEVALS ) {
  $log->debug( 'skipping file retrieval' );
}
else {
  retrieve_file( $IP_URL_ROOT, $FILE_ROOT, "$ip_file.gz" );
  $log->logdie( "ERROR: failed to retrieve and uncompress '$ip_file.gz': $!" )
    unless -f "$FILE_ROOT/$ip_file";

  retrieve_file( $IP_URL_ROOT, $FILE_ROOT, $ip2go_file );
  $log->logdie( "ERROR: failed to retrieve '$ip2go_file': $!" )
    unless -f "$FILE_ROOT/$ip2go_file";

  foreach my $go_file ( @$ontologies ) {
    retrieve_file( $GO_URL_ROOT, $FILE_ROOT, $go_file );
    $log->logdie( "ERROR: failed to retrieve '$go_file': $!" )
      unless -f "$FILE_ROOT/$go_file";
  }
}

#-------------------------------------------------------------------------------
# get a database connection

my $dbm = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $schema = $dbm->getSchema;

$log->info( 'got a schema object' );

#-------------------------------------------------------------------------------
# main

my $CGI = CGI->new;

# clear out the database tables that we're about to populate
empty_tables( $schema );

#---------------------------------------

# read in the XML string
my $interpro_xml_string = read_interpro_xml( "$FILE_ROOT/$ip_file" );

# parse it and walk over the DOM to get the abstracts
my $dom  = XML::LibXML->load_xml( string => $interpro_xml_string );
my $root = $dom->documentElement;

#---------------------------------------

# read in the interpro-to-go mapping and GO itself

my $ip2go = parse_interpro2go( "$FILE_ROOT/$ip2go_file" );

my $terms = {};
foreach my $go_file ( @$ontologies ) {
  parse_ontology( $terms, "$FILE_ROOT/$go_file" );
}

#---------------------------------------

# for each interpro abstract...
# IP_ID: foreach my $node ( ( $root->findnodes('/interprodb/interpro') )[0..50] ) { # TODO testing only
IP_ID: foreach my $node ( $root->findnodes('/interprodb/interpro') ) {

  # TODO there's unnecessary overhead here, because we're retrieving and editing 
  # every interpro abstract before checking whether it actually maps to a Pfam-A.
  # Really, we should be checking first and only editing the abstract if there's
  # a mapping, but hey...
  # jt6 20111208 WTSI
  
  # fix the <cite>, <taxon> and <db_xref> tags in the abstract and return
  # the mapping between Pfam-A and interpro
  my ( $pfam_accessions, $interpro_id, $abstract ) = edit_abstract( $node );

  # if this abstract maps to a Pfam-A family, insert it
  PFAM_ACC: foreach my $pfam_acc ( @$pfam_accessions ) {
    my $row = $schema->resultset('PfamA')
                     ->search( { 'me.pfama_acc' => $pfam_acc },
                               { join     => [ qw( interpros gene_ontologies ) ] } )
                     ->first;

    next PFAM_ACC unless $row;

    $log->debug( "got row for $pfam_acc" ) if $row;

    my $rv = $row->interpros->update_or_create( { 
      interpro_id => $interpro_id,
      abstract    => $abstract 
    },
    { key => 'pfama_ip_unq' } ); #possible changes needed for mySQL / schema update
    if ( $rv ) {
      $log->info( "inserted '$interpro_id' abstract for '$pfam_acc'" );
    }
    else {
      $log->logwarn( "WARNING: failed to update '$pfam_acc' with abstract for '$interpro_id'" );
      next;
    }

    # if this interpro has GO annotations, add them
    GO_ID: foreach my $go_id ( keys %{ $ip2go->{$interpro_id} } ) {
      GO_CAT: foreach my $category ( qw( component function process ) ) {

        next GO_CAT unless exists $terms->{$category}->{$go_id};

        my $term = $ip2go->{$interpro_id}->{$go_id};

        my $rv = $row->gene_ontologies->update_or_create( { 
          go_id => $go_id,
          term  => $term,
          category => $category
        },
        { key => 'pfama_go_unq' } ); 

        if ( $rv ) {
          $log->info( "inserted GO term '$term', category '$category', ID '$go_id' for '$pfam_acc'" );
        }
        else {
          $log->logwarn( "WARNING: failed to update '$pfam_acc' with with GO term" );
          next;
        }

      } # GO_CAT
    } # GO_ID

  } # PFAM_ACC
} # IP_ID

$log->info( 'done' );

exit;

#-------------------------------------------------------------------------------
#- subroutines -----------------------------------------------------------------
#-------------------------------------------------------------------------------

# clear out the contents of the various tables that this script will populate

sub empty_tables {
  my $schema = shift;

  my $rv = $schema->resultset( 'Interpro' )
                  ->search( {}, {} )
                  ->delete;

  if ( $rv ) {
    $log->info( "deleted $rv rows from 'interpro' table" );
  }
  else {
    $log->logdie( "ERROR: failed to empty 'interpro' table" );
  }

  $rv = $schema->resultset( 'GeneOntology' )
               ->search( {}, {} )
               ->delete;

  if ( $rv ) {
    $log->info( "deleted $rv rows from 'gene_ontology' table" );
  }
  else {
    $log->logdie( "ERROR: failed to empty 'gene_ontology' table" );
  }
}

#-------------------------------------------------------------------------------
# retrieve the raw interpro data files via FTP

sub retrieve_file {
  my ( $url_root, $file_root, $file ) = @_;

  my $file_url  = "$url_root/$file";
  my $file_path = "$file_root/$file";

  $log->info( "Downloading '$file' from '$file_url' and storing at '$file_path'" );

  # move an existing file out of the way. This is going to break after
  # we've uncompressed the gzipped XML file... Just make sure there's
  # nothing in the way, it'll be easier.
  if ( -e $file_path ) {
    $log->debug( "Moving '$file_path' sideways" );
    move( $file_path, "$file_path.old" )
      or $log->logdie( "Failed to move '$file_path' to '$file_path.old': !" );
  }

  # retrieve the new file
  my $rv = getstore( $file_url, $file_path );
  $log->logdie( "Failed to download '$file_url': $rv" )
    unless is_success( $rv );

  # gunzip it in situ
  if ( $file_path =~ m/\.gz$/ ) {
    $log->debug( "Gunzipping '$file_path'" );
    system( "gunzip -f $file_path" ) == 0 
      or $log->logdie( "Failed to uncompress '$file_path': $!" );
  }
}

#-------------------------------------------------------------------------------
# read in the XML file and force every abstract to be a CDATA string

sub read_interpro_xml {
  my $ip_file = shift;
  open ( IPXML, $ip_file )
    or $log->logdie( "ERROR: couldn't open interpro XML file '$ip_file': $!" );

  my $interpro_xml_string;
  while ( <IPXML> ) {
    s/\<abstract\>/<abstract><![CDATA[/g;
    s/\<\/abstract\>/]]><\/abstract>/g;
    $interpro_xml_string .= $_;
  }

  close IPXML;
  $log->info( 'read in interpro XML file' );

  return $interpro_xml_string;
}

#-------------------------------------------------------------------------------

sub edit_abstract {
  my $node = shift;

  my $interpro_id = $node->getAttribute("id");

  # find out which Pfam accessions map to this interpro
  my @dbkey_attrs = $node->findnodes('member_list/db_xref[@db="PFAM"]/@dbkey');

  # we don't care about this interpro unless it does map to a family...
  unless ( scalar @dbkey_attrs ) {
    $log->debug( "no Pfam accessions for $interpro_id" );
    return;
  }

  # get the actual Pfam-A accessions from the XML::LibXML::Attr objects
  my @pfam_accessions;
  push @pfam_accessions, $_->value for @dbkey_attrs;

  $log->info( "fixing abstract for $interpro_id" );

  my $abstract_node = ( $node->findnodes('abstract') )[0];

  my $abstract = '';
  my ( $pm_id, $xref_url );
  foreach my $child ( $abstract_node->childNodes ) {

    my $abstract_chunk = $child->data;

    $abstract_chunk =~ 
      s {<cite\ idref="(.*?)"\s*/>} {
        $log->debug( "adding URL for citation: $1" );
        $pm_id = $node->findvalue("pub_list/publication[\@id='$1']/db_xref/\@dbkey");
        $CGI->a( { -href => $pm_url . $pm_id, -class => 'ext' }, "PUBMED:$pm_id" );
      }sgex;
   
    $abstract_chunk =~ 
      s {<taxon\ tax_id="(.*?)"\s*>(.*?)</taxon>} {
        $log->debug( "adding URL for taxon: $1" );
        $CGI->a( { -href => $tax_url . $1 }, $2 );
      }sgex;
   
    $abstract_chunk =~ 
      s {<db_xref\ db="(.*?)"\ dbkey="(.*?)"\s*/>} {
        if ( exists $db_xref_urls{$1} ) {
          my $xref_db = $1;
          my $xref_id = $2;
          ( $xref_url = $db_xref_urls{$1} ) =~ s/\%s/$xref_id/;
          $log->debug( "adding URL for '$xref_db': $xref_url" );
          $CGI->a( { -href => $xref_url, -class => 'ext' }, $xref_db );
        }
        else {
          '';
        }
      }sgex;

    $abstract .= $abstract_chunk;
  }

  return ( \@pfam_accessions, $interpro_id, $abstract );
}

#-------------------------------------------------------------------------------

sub parse_interpro2go {
  my $file = shift;

  open( FILE, $file )
    or $log->logdie( "ERROR: couldn't open interpro2go file '$file': $!" );

  $log->info( "parsing interpro2go file '$file'" );

  my $go_data = {};
  while ( <FILE> ) {
    next if m/^\s*\!/; # skip comments

    m/^InterPro:(IPR\d{6}) .*? \> GO:(.*?) \; (GO:\d{7})\s*$/;
    # $1 = interpro ID
    # $2 = GO description text
    # $3 = GO ID

    $go_data->{$1}->{$3} = $2;
  }

  close FILE;

  return $go_data;
}

#-------------------------------------------------------------------------------

sub parse_ontology {
  my ( $terms, $file ) = @_;

  open( FILE, $file )
    or $log->logdie( "ERROR: couldn't open ontology file '$file': $!" );

  $log->info( "parsing ontology file '$file'" );
  
  my ( $id, $name, $ns );
  while ( <FILE> ) {
    if ( m/^id:\s+(GO:\d+)/ ) {
      $id = $1;    
    }
    elsif ( m/^name:\s+(.*)/ ) {
      $name = $1;
    }
    elsif ( m/^namespace:\s+\w+_(\w+)/ ) {
      $ns = $1;
      if ( defined( $ns )   and 
           defined( $name ) and 
           defined( $id ) ) {
        $terms->{$ns}->{$id} = $name;
      }
      $ns = $name = $id = undef;
    }
  }

  close FILE;
}

#-------------------------------------------------------------------------------

__END__

=head1 NAME

pud-buildInterproAndGo - populate interpro and gene_ontology tables

=head1 SYNOPSIS

pud-buildInterproAndGo [options]

  Options:
    --help        show this help message
    --skipdl      skip the downloading of interpro data files
    --root <dir>  specific root directory, where downloaded files will be stored
    --debug       turn on debugging messages, including DBIC tracing

=head1 OPTIONS

=over

=item B<--help> | B<-?>

Prints help message and exits.

=item B<--skipdl>

By default the script will try to download interpro data files from the EBI FTP 
area. 

=item B<--root>

Specifies the root directory where downloaded files should be stored. 

=item B<--debug>

Enables debugging messages, including generated SQL command tracing in 
L<DBIx::Class>.

=back

=head1 DESCRIPTION

This script tries to populate the C<interpro> and C<gene_ontology> tables of
the live Pfam database, using data derived from three interpro data files:

=over

=item interpro.xml (gzip compressed on FTP site)

=item interpro2go

=item gene_ontology.1_2.obo

=back

By default the script will try to download the files from the EBI FTP area but
this behaviour can be turned off with the B<--skipdl> flag. Before trying to
download files, the script checks to see if there are existing files in the
root directory, and will move them to B<file.old> before attempting the
download. There is no check to see if the ".old" file already exists.

If B<--skipdl> is specified, so that no files are downloaded, the script will
still look in the directory specified with B<--root> when looking for the
interpro data files.

The default root directory is specified in the configuration file, loaded
via the module L<Bio::Pfam::Config>.

If debugging is enabled, more verbose messages are printed as the script runs.
The environment variable B<DBIC_TRACE> is also set to true, enabling the listing
of SQL commands generated by L<DBIx::Class>. Debugging can also be enabled by 
setting the environment variable B<DEBUG> to true.

=cut

