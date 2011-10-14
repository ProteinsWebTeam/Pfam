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
$logger->info('Parsing ncbi data (nodes.dmp)');

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
  my ($rank)   = $_[2] =~ /(\S+)/;

  #Shunt those special ids to be superkingdoms.
  $rank = 'superkingdom' if ( $promoteTaxIds{$taxid} );

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
buildTree( $tree, $nodes );

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

my $parent = 1;                         #The root id
walkAndCut( $tree, $nodes, $parent, \%ranks );

#------------------------------------------------------------------------------
# Now rebuild the minimal tree from the nodes.
#
my $t0 = [gettimeofday];
$logger->info('Building minimal taxonomic tree');
my $minTree = {};
buildTree( $minTree, $nodes );
my $elapsed = tv_interval( $t0 );
$logger->info( "Minimal tree building took $elapsed");

#------------------------------------------------------------------------------
# To mkae the storage/quereies in the database faster we need to traverse the
# tree to store the
#
$logger->info('Traversing tree');
my $count = 0;
traverseTree( $minTree, $nodes, $count );

#------------------------------------------------------------------------------
# Print tree out/store it in the database.
#

#$dbh->do("DELETE FROM taxonomy");

my $taxString = '';
$logger->info("Updating taxonomy table");
traverseTreeAndStore( $minTree, $nodes, $taxString, $insertTaxSth );


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
  my ( $hash, $nodes, $taxString, $insertTaxSth ) = @_;
  
  foreach my $k ( keys %{$hash} ) {
    my $thisTaxString = $taxString;
    if(defined $nodes->[$k]->{name} ){
    $thisTaxString .= $nodes->[$k]->{name} . ';';
    $insertTaxSth->execute(
        $k,
        $nodes->[$k]->{rank} eq 'species' ? $nodes->[$k]->{name} : undef,
        $thisTaxString,
        $nodes->[$k]->{lft},
        $nodes->[$k]->{rgt}, 
        $nodes->[$k]->{parent},
        $nodes->[$k]->{name},
        1, 
        $nodes->[$k]->{rank});
    }
    traverseTreeAndStore( $hash->{$k}, $nodes, $thisTaxString, $insertTaxSth );
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

sub walkAndCut {
  my ( $hash, $nodesRef, $parent, $ranks ) = @_;

  foreach my $k ( keys %{$hash} ) {
    my $thisParent = $parent;
    $nodesRef->[$k]->{parent} = $parent;
    if ( $ranks->{ $nodesRef->[$k]->{rank} } ) {
      $thisParent = $nodesRef->[$k]->{taxid};
    }
    walkAndCut( $hash->{$k}, $nodesRef, $thisParent, $ranks );
  }
}

sub buildTree {
  my ( $tree, $nodes ) = @_;

  foreach my $node (@$nodes) {
    next unless ($node);
    next unless ( $node->{rank} eq 'species' );

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



# prepare the database access and the queries that we'll use. We expect to
# write to a table like this:
#
# CREATE TABLE `taxonomy` (
#  `taxid` int(10) NOT NULL,
#  `parent_taxid` int(11) NOT NULL,
#  `rank` varchar(16) DEFAULT NULL,
#  `lft` int(10) NOT NULL,
#  `rgt` int(10) NOT NULL,
#  `name` varchar(100) NOT NULL,
#  `taxonomy` mediumtext,
#  PRIMARY KEY (`taxid`)
#) ENGINE=InnoDB
#
# query the table with something like:
# SELECT ncbi_taxid,
#        species,
#        lft,
#        rgt,
#        parent,
#        level
# FROM   taxonomy
# WHERE  lft <= 98440
# AND    rgt >= 98441
# ORDER BT lft ASC;
;

#-------------------------------------------------------------------------------

sub usage {

  print <<'EOF_help';
Usage: $0 

Build a database table of the taxonomic tree.

EOF_help

}

#-------------------------------------------------------------------------------

