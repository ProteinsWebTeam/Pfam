#!/usr/local/bin/perl

use strict;
use warnings;

use Log::Log4perl;
use DBI;
use Data::Dump qw(dump);
use JSON;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#-------------------------------------------------------------------------------
# configure logging

my $log_conf = q(
  log4perl.logger                   = INFO, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$log_conf );
my $log = Log::Log4perl->get_logger;

#-------------------------------------------------------------------------------
# connect to the database

my $config = Bio::Pfam::Config->new;

# database connections and statement handles

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  $log->logdie(
    "View process failed as we could not connect to pfamlive");
}

my $dbh = $pfamDB->getSchema->storage->dbh;
$log->debug("Got pfamlive database connection");

#-------------------------------------------------------------------------------
#Now process the options

my ( $chunk, $chunkSize, $acc, $help, $file );
GetOptions(
  "chunk=i"     => \$chunk,
  "chunkSize=i" => \$chunkSize,
  "acc=s"       => \$acc,
  "file=s"      => \$file,
  "help"        => \$help
  )
  or ( help() and die "Invalid option\n" );

if($help){
  help();
  exit;  
}

unless($file or $acc or ($chunkSize and $chunk)){
  help();
  $log->warn(" No accession, accession file or range passed in\n");
  exit;
}

if($file and $acc){
  help();
  $log->warn("Can not specify both accession AND file containing accessions.\n");
  exit;
}

my @auto_pfamAs;

if($acc){
  chomp($acc);
  my $f = $pfamDB->getPfamData($acc);
  if($f and $f->auto_pfama){
    push @auto_pfamAs, $f->auto_pfama;
  }else{
    $log->warn("No data found for $_");
  }
}

if($file){ 
  # get the list of auto_pfamAs
  
  $log->die("$file has no size") unless -s $file;
 
  open LIST, $file
    or $log->logdie("ERROR: couldn't open list file ($file): $!");

  while ( <LIST> ) {
    chomp;
    my $f = $pfamDB->getPfamData($_);
    if($f and $f->auto_pfama){
      push @auto_pfamAs, $f->auto_pfama;
    }else{
      $log->warn("No data found for $_");  
    }
  }

  close LIST;
}

if(($chunkSize and $chunk) and (!$file and !$acc)){
  
  $log->debug("Calculating Pfam-A paging");

  my $famAll = $pfamDB->getSchema->resultset('Pfama')->search(
    {},
    {
      page => 1,
      rows => $chunkSize
    }
  );

  my $pager = $famAll->pager;
  $log->info( "Working on page $chunk out of " . $pager->last_page );

  my @allRS = $famAll->page($chunk)->all;
  foreach my $f (@allRS){
     push @auto_pfamAs, $f->auto_pfama;
  }
  
}


$log->info( "building " . scalar @auto_pfamAs . " sunbursts" );

#-------------------------------------------------------------------------------
# set up the queries

# list of seed sequences
my $seedSth = $dbh->prepare( q[SELECT pfamseq_acc FROM pfamA_reg_seed r, pfamseq s WHERE s.auto_pfamseq = r.auto_pfamseq AND auto_pfamA= ?] );

my $fullSth = $dbh->prepare( q[ SELECT t.species, t.parent, t.minimal, t.rank, pfamseq_acc, t.ncbi_taxid, COUNT(s.auto_pfamseq) FROM pfamA_reg_full_significant r, pfamseq s LEFT JOIN taxonomy t ON t.ncbi_taxid = s.ncbi_taxid join taxonomy p on p.ncbi_taxid = t.parent WHERE s.auto_pfamseq = r.auto_pfamseq AND auto_pfamA = ? AND in_full = 1 and ( t.rank='species' or p.rank='species') GROUP BY s.auto_pfamseq ] );

my $taxFSth = $dbh->prepare( q[SELECT parent, minimal, level, rank FROM taxonomy WHERE ncbi_taxid = ?] );

#Unclassified sequences
my $unclassSth = $dbh->prepare(q[ SELECT s.species, s.taxonomy, pfamseq_acc, COUNT(s.auto_pfamseq), s.ncbi_taxid FROM pfamA_reg_full_significant r, pfamseq s LEFT JOIN taxonomy t ON t.ncbi_taxid = s.ncbi_taxid WHERE s.auto_pfamseq = r.auto_pfamseq AND auto_pfamA = ? AND in_full=1 AND t.ncbi_taxid IS null GROUP BY s.auto_pfamseq ]);

#-------------------------------------------------------------------------------

my @expectedLevels = qw(superkingdom kingdom phylum class order family genus species);
my %superkingdom_members = (
  'Bacteria'              => 'bacterium',
  'Eukaryota'             => 'eukaryote',
  'Archea'                => 'archea',
  'Viruses'               => 'virus',
  'Viroids'               => 'viroid',
  'Other sequences'       => 'sequence',
  'Unclassified'          => 'sequence',
  'Unclassified sequence' => 'sequence',
);

my $json = JSON->new;
# $json->pretty(1);

my $schema = $pfamDB->getSchema;

foreach my $auto_pfamA ( @auto_pfamAs ) {
  build( $auto_pfamA );
}

exit;

#-------------------------------------------------------------------------------

sub build {
  my $auto_pfamA = shift;

  $log->debug( "building $auto_pfamA..." );

  # get the list of seed sequences
  $seedSth->execute($auto_pfamA);
  my %seedSeqs;
  foreach my $rRef ( @{ $seedSth->fetchall_arrayref } ) {

    #print $rRef->[0]."\n";
    $seedSeqs{ $rRef->[0] }++;
  }

  my @tree;
  my %seenTaxIds;
  my $unique  = 1;
  my $counter = 1;

  #----------------------------------------
 
  # build the list of unclassified levels

  $unclassSth->execute($auto_pfamA);
  foreach my $rRef ( @{ $unclassSth->fetchall_arrayref }){
    $log->debug( dump($rRef) );
    my $thisBranch;
    $thisBranch->{sequence} = {
      seqAcc     => $rRef->[2],
      seedSeq    => ( defined( $seedSeqs{ $rRef->[2] } ) ? 1 : 0 ),
      numDomains => $rRef->[3]
    }; 

    $thisBranch->{species} = {
      node  => $rRef->[0],
      taxid => 0
    };

    my $speciesCounter = 0;
    #We do not have a useful taxid, use species name
    unless($seenTaxIds{ $rRef->[0]}){
      $speciesCounter++;
    }
    $seenTaxIds{ $rRef->[0] }++;


    my ($skName) = $rRef->[1] =~ /^(.*?)\; /;
    my $sk_member = $superkingdom_members{$skName} || 'entity';
    for ( my $i = 0 ; $i < $#expectedLevels ; $i++ ) {
      # $thisBranch->{$expectedLevels[$i]}->{node} = $i > 0 ? 'Unclassified '.$expectedLevels[$i] : $skName;
      $thisBranch->{$expectedLevels[$i]}->{node} = $i > 0 ? "Uncategorised $sk_member" : $skName;
    }
    
    my $previousNode; 
    for ( my $i = 0 ; $i <= $#expectedLevels ; $i++ ) {
      my $node;
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if ( $c->{node} and
               $c->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} ) {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $expectedLevels[$i] }->{node} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {
   
        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if ( $skNode->{node} and
               $skNode->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} ) {
            $node = $skNode;
          }
        }
   
        unless ($node) {
          $node = {
            node   => $thisBranch->{ $expectedLevels[$i] }->{node},
            parent => 'root'
          };
          push( @tree, $node );
        }
      }
      $node->{id} = $unique++;
      $node->{numSequences}++;
      $node->{numSpecies} += $speciesCounter;
      $node->{numDomains} += $rRef->[3];
   
      $previousNode = $node;
    }
    push( @{ $previousNode->{sequences} }, $thisBranch->{sequence} );
  }
  $log->debug(dump(\@tree));

  #----------------------------------------

  # build the full tree

  $fullSth->execute($auto_pfamA);
  foreach my $rRef ( @{ $fullSth->fetchall_arrayref } ) {
    $log->debug( "rRef: ", dump($rRef) );

    my $thisBranch;
    $thisBranch->{sequence} = {
      seqAcc     => $rRef->[4],
      seedSeq    => ( defined( $seedSeqs{ $rRef->[4] } ) ? 1 : 0 ),
      numDomains => $rRef->[6]
    };

    $thisBranch->{ $rRef->[3] } = {
      node  => $rRef->[0],
      taxid => $rRef->[5]
    };

    my $speciesCounter = 0;
    unless ( $seenTaxIds{ $rRef->[5] } ) {
      $speciesCounter = 1;
    }
    $seenTaxIds{ $rRef->[5] }++;

    my $atRoot = 0;

    #print "Looking up ".$rRef->[1]."\n";
    $log->debug( "looking up $rRef->[1]" );
    $taxFSth->execute( $rRef->[1] );
    my $rHashRef = $taxFSth->fetchrow_hashref;
    $log->debug( "rHashRef: ", dump($rHashRef) );

    until ($atRoot) {
      $atRoot = 1 if ( $rHashRef->{parent} and $rHashRef->{parent} == 1 );
      if ( $rHashRef->{minimal} and $rHashRef->{minimal} == 1 ) {

        #Harvest the information that we want!
        $thisBranch->{ $rHashRef->{rank} } = { node => $rHashRef->{level} };
      }
      $taxFSth->execute( $rHashRef->{parent} );
      $rHashRef = $taxFSth->fetchrow_hashref;
      $log->debug( dump($rHashRef) );
    }

    my $previousNode;
    for ( my $i = 0 ; $i <= $#expectedLevels ; $i++ ) {
      my $node;

      #Become the same name as the parent if this taxonomic level is unknown. Everything should have a superkingdom.
      unless ( $thisBranch->{ $expectedLevels[$i] } ) {
        die "Trying to assign a name to a superkingdom" unless ( $i > 0 );
        die "Trying to assign "
          . $expectedLevels[$i]
          . " name based on "
          . $expectedLevels[ $i - 1 ]
          . " level\n"
          unless ( $thisBranch->{ $expectedLevels[ $i - 1 ] } );
        $thisBranch->{ $expectedLevels[$i] }->{node} =
          '(No ' . $expectedLevels[$i] . ')';
          # $thisBranch->{ $expectedLevels[ $i - 1 ] }->{node};
      }
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if ( $c->{node} and
               $c->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} ) {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $expectedLevels[$i] }->{node} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {

        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if ( $skNode->{node} and
               $skNode->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $skNode;
          }
        }

        unless ($node) {
          $node = {
            node   => $thisBranch->{ $expectedLevels[$i] }->{node},
            parent => 'root'
          };
          push( @tree, $node );
        }
      }
      $node->{id} = $unique++;
      $node->{numSequences}++;
      $node->{numSpecies} += $speciesCounter;
      $node->{numDomains} += $rRef->[6];

      $previousNode = $node;
    }
    push( @{ $previousNode->{sequences} }, $thisBranch->{sequence} );
  }

  # stats...
  my ( $totalSequence, $totalSpecies, $totalDomains );
  foreach my $skNode (@tree) {
    $totalSequence += $skNode->{numSequences};
    $totalSpecies  += $skNode->{numSpecies};
    $totalDomains  += $skNode->{numDomains};
  }

  # make a root for the tree
  my $rootedTree = {
    id           => 0,
    node         => 'root',
    numSequences => $totalSequence,
    numSpecies   => $totalSpecies,
    numDomains   => $totalDomains,
    children     => \@tree
  };

  $log->debug( dump( $rootedTree ) );

  my $json_string = $json->encode( $rootedTree );
  $log->debug('built JSON string');

  $schema->resultset('PfamaSpeciesTree')
         ->update_or_create( { auto_pfama  => $auto_pfamA,
                               json_string => $json_string } );

}

# end of "sub build"

sub help{
  print<<EOF;
  
  usage: $0 <options>
  
This script is used for making the JSON data string that are renders by the
website as sunbursts style reprensentations.  This code only works for Pfam-A families
at the moment. Also, it is particularly sensitive to the contents of the taxonomy table.


chunk     : Used in combination with chunksize, this says which page, or multiple
          : of the page desired.
chunksize : The number of families to be processed by this script

OR

acc       : A Pfam accession (or id). This code will also make JSON string for sunburst for the given family.

OR

file      : A file containing a list of accessions or identifiers that you wish to  make JSON string for


This was how this script was run on the Sanger farm.
bsub -q normal  -R"select[mem>4000] rusage[mem=4000]" -M 4000000 -J "sunburst[1-20]" 
-o "sunburst.\%J.\%I.log" 'makeSunburst.pl -chunkSize 684 -chunk \$\{LSB_JOBINDEX\}'
  
EOF
}

