#!/usr/bin/env perl

#
# Script to download the taxonomy data from NCBI and to build the taxonomic
# tree, strip out the levels that are not so meaningful to generate a minimal
# tree. This 
#

use strict;
use warnings;

use DBI;
use Data::Dumper;
use Getopt::Long;
use Log::Log4perl;

use Data::Dump qw(dump);
use Time::HiRes qw(gettimeofday tv_interval);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


#-------------------------------------------------------------------------------
# configure logging
my $logger_conf = q(
  log4perl.logger                   = DEBUG, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );
my $logger = Log::Log4perl->get_logger;

#-------------------------------------------------------------------------------
#Get the config for the site

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{  $config->pfamlive }
);


my ($help);
GetOptions( "help" => \$help );

usage() and exit if $help;

#-------------------------------------------------------------------------------
#- main ------------------------------------------------------------------------
#-------------------------------------------------------------------------------

              
#-------------------------------------------------------------------------------
# Build up all the ncbi nodes. Read into a hash array
$logger->info('Parsing ncbi data (names.dmp)');

my %names;
my %minNames;
open( _NAMES, $config->localDbsLoc . '/ncbi/names.dmp' )
  or die "Could not open " . $config->localDbsLoc . "/ncbi/names.dmp: [$!]\n";

#------------------------------------------------------------------------------
#This is a bit of a hack, but we want to keep these levels so we can hang
#all sequences off this

#Move to config
my %promote;
foreach
  my $l ( "Viroids", "Viruses", "unclassified sequences", "other sequences" )
{
  $promote{$l}++;
}

my %promoteTaxIds;

#------------------------------------------------------------------------------
# Extract just the scientific names (for the time being, it may be useful to
# have synonyms and lay terms ).
#
while (<_NAMES>) {
  next unless (/scientific name/);
  @_ = split( /\|/, $_ );
  my ($taxid) = $_[0] =~ /(\d+)/;
  my ($name)  = $_[1] =~ /\s+(.*)\s+/;
  $names{$taxid} = $name;
  $promoteTaxIds{$taxid}++ if ( $promote{$name} );
}




#------------------------------------------------------------------------------
#Now parse the nodes file
#
open( _NODES,  $config->localDbsLoc . '/ncbi/nodes.dmp'  )
  or $logger->logdie("Could not open nodes.dmp: [$!]");

my $nodes = [];    #This will be out store for all the nodes in the tree
while (<_NODES>) {
  @_ = split( /\|/, $_ );
  my ($taxid)  = $_[0] =~ /(\d+)/;
  my ($parent) = $_[1] =~ /(\d+)/;
  my ($rank)   = $_[2] =~ /\s+(.*)\s+/;
  #print "$rank\|"; 
  #Shunt those special ids to be superkingdoms.
  $rank = 'superkingdom' if ( $promoteTaxIds{$taxid} );
    
  unless(defined($names{$taxid})){
     #warn $taxid. " has no name\n"; 
     $names{$taxid} = '_unnamed';
  }
  $nodes->[$taxid] = {
                       taxid  => $taxid,
                       parent => $parent,
                       rank   => $rank,
                       name   => $names{$taxid} };
  
}
close(_NAMES);
close(_NODES);

#------------------------------------------------------------------------------
$pfamDB->getSchema->resultset('Taxonomy')->delete;

my $dbh = $pfamDB->getSchema->storage->dbh;
$logger->debug( 'successfully connected to database' );

my $insertTaxSth = $dbh->prepare( 
                "INSERT INTO taxonomy VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ? )" );




#Now build the fuill tree as we get it from NCBI
#
$logger->info('Building full taxonomic tree');
my $tree = {};
#buildTree( $tree, $nodes );

#------------------------------------------------------------------------------
# Cut out the levels that are excessive by reassigning parentage
#
$logger->info('Removing excessive nodes');
my %ranks = ( superkingdom => 1,
              kingdom      => 1,
              phylum       => 1,
              class        => 1,
              order        => 1,
              family       => 1,
              genus        => 1,
              species      => 1 );

buildTree( $tree, $nodes );
$logger->info('Traversing tree');
my $count = 0;
traverseTree( $tree, $nodes, $count );

#------------------------------------------------------------------------------
# Print tree out/store it in the database.
#

$dbh->do("DELETE FROM taxonomy");

my $taxString = '';
$logger->info("Updating taxonomy table");
traverseTreeAndStore( $tree, $nodes, $taxString, $insertTaxSth, \%ranks );

exit;

#------------------------------------------------------------------------------
# subroutines ----------------------------------------------------------------- 
#------------------------------------------------------------------------------

sub traverseTree {
  my ( $hash, $nodes, $count ) = @_;
  foreach my $k ( keys %{$hash} ) {
    $nodes->[$k]->{lft} = $count++;
    $count = traverseTree( $hash->{$k}, $nodes, $count );
    $nodes->[$k]->{rgt} = $count++;
  }
  return $count;
}


sub traverseTreeAndStore {
  my ( $hash, $nodes, $taxString, $insertTaxSth, $ranksRef ) = @_;
  
  foreach my $k ( keys %{$hash} ) {
    my $thisTaxString = $taxString;
    #if(defined $nodes->[$k]->{name} ){
    $thisTaxString .= $nodes->[$k]->{name} . ';';
    $insertTaxSth->execute(
        $k,
        $nodes->[$k]->{rank} eq 'species' ? $nodes->[$k]->{name} : undef,
        $thisTaxString,
        $nodes->[$k]->{lft},
        $nodes->[$k]->{rgt}, 
        $nodes->[$k]->{parent},
        $nodes->[$k]->{name},
        exists ($ranksRef->{$nodes->[$k]->{rank}}) ? 1 : 0, 
        $nodes->[$k]->{rank});
    #}
    traverseTreeAndStore( $hash->{$k}, $nodes, $thisTaxString, $insertTaxSth, $ranksRef );
  }
}

sub traverseTreeAndPrint {
  my ( $hash, $nodes, $taxString ) = @_;
  foreach my $k ( keys %{$hash} ) {
    my $thisTaxString = $taxString;
    $thisTaxString .= $nodes->[$k]->{name} . ';';
    print $k. "\t"
      . $nodes->[$k]->{parent} . "\t"
      . $nodes->[$k]->{rank} . "\t"
      . $nodes->[$k]->{lft} . "\t"
      . $nodes->[$k]->{rgt} . "\t"
      . $nodes->[$k]->{name} . "\t"
      . $thisTaxString . "\n";
    traverseTreeAndPrint( $hash->{$k}, $nodes, $thisTaxString );
  }
}

sub buildTree {
  my ( $tree, $nodes ) = @_;

  foreach my $node (@$nodes) {
    next unless ($node);
    next unless ( $node->{rank} eq 'species' or $node->{rank} eq 'no rank');

    my @speciesNodes;
    push( @speciesNodes, $node );
    my $pnode = $node;
    until ( $pnode->{parent} == $pnode->{taxid} ) {
      push( @speciesNodes, $nodes->[ $pnode->{parent} ] );
      $pnode = $nodes->[ $pnode->{parent} ];
    }

    my $parent = $tree;
    #Now walk down buiding up the tree.
    for ( my $i = $#speciesNodes ; $i >= 0 ; $i-- ) {
      $parent->{ $speciesNodes[$i]->{taxid} } = {}
        unless exists $parent->{ $speciesNodes[$i]->{taxid} };
      $parent = $parent->{ $speciesNodes[$i]->{taxid} };
    }
  }

}


#-------------------------------------------------------------------------------

sub usage {

  print <<'EOF_help';
Usage: $0 

Build a database table of the taxonomic tree using the ncbi taxonomy files names.dmp and nodes.dmp

EOF_help

}

#-------------------------------------------------------------------------------

