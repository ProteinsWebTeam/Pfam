#!/usr/bin/env perl

use strict;
use warnings;

use Log::Log4perl qw( get_logger :levels );
use Getopt::Long;
use File::Temp;
use File::Basename;
use Cwd;
use LWP::Simple;
use Archive::Tar;

use Bio::Rfam::Config;

#-------------------------------------------------------------------------------
# configure logging

BEGIN {
  my $logger_conf = q(
    log4perl.logger                                   = INFO, Screen
    log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.layout                   = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.Screen.layout.ConversionPattern = %d %M:%L %p: %m%n
  );

  Log::Log4perl->init( \$logger_conf );
}

my $log = Log::Log4perl->get_logger;

#-------------------------------------------------------------------------------
# parse options and get configuration

my ( $dir, $help, $retrieve, $verbose );

my $rv = GetOptions( 'dir=s'      => \$dir,
                     'help'       => \$help,
                     'retrieve'   => \$retrieve,
                     'verbose'    => \$verbose );

usage() and exit if $help;

if ( $verbose ) {
  $log->level( $DEBUG );
  $log->debug( 'verbose logging enabled' );
}

my $config = Bio::Rfam::Config->new;

#-------------------------------------------------------------------------------
# get the data files

# if we were handed a directory, use that for storing the dumps, otherwise
# set up a temp dir
if ( defined $dir ) {
  $log->info( "working in user-supplied directory, '$dir'" );
}
else {
  if ( $retrieve ) {
    $dir = File::Temp->newdir();
    $log->warn( "WARNING: working in an temporary directory, '$dir'." );
    $log->warn( '         Contents will be deleted automatically when the script exits.' );
  }
  else {
    $log->logdie( q(ERROR: if you're not retrieving files, you must specify a directory containing the dumps,) );
  }
}

# download the dump
my $archive_url  = $config->config->{tax_table}->{archive_url};
my $archive_file = "${dir}/taxdump.tar.gz";

my $names_file   = "${dir}/names.dmp";
my $nodes_file   = "${dir}/nodes.dmp";

if ( $retrieve ) {
  $log->logdie( "ERROR: can't find directory '$dir' for file retrieval" )
    unless -d $dir;

  $log->debug( "downloading archive to '$dir'" );

  retrieve_files( $archive_url, $archive_file );
}

$log->debug( "looking for nodes archive '$archive_file'" );

$log->logdie( "ERROR: can't find archive '$archive_file'" )
  unless -f $archive_file;

#-------------------------------------------------------------------------------
# get a database connection and prepare the statement handle

my $schema = $config->rfamlive;
my $dbh    = $schema->storage->dbh;

$log->logdie( "ERROR: couldn't get a database connection" )
  unless $dbh;

my $sth = $dbh->prepare( 'INSERT INTO taxonomy_websearch VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ? )' );

#-------------------------------------------------------------------------------
# extract the names.dmp and nodes.dmp files

unless ( -s $names_file and -s $nodes_file ) {
  extract_files( $archive_file, $names_file, $nodes_file );
}

#-------------------------------------------------------------------------------
# parse the taxonomy files

$log->info( 'parsing names' );
my ( $names, $promoted_taxids ) = parse_names( $names_file );

$log->info( 'parsing nodes' );
my $nodes = parse_nodes( $nodes_file, $names, $promoted_taxids );

# build the tree. This is the node hierarchy, but the meta data for each
# node is stored in the array "nodes"
$log->info( 'building tree hierarchy' );
my $tree = build_tree( $nodes );

# traverse the tree and add left and right values to each node
$log->info( 'ordering tree nodes' );

my $count = 0;
order_nodes( $tree, $nodes, $count );

# these are the levels to enforce in the hierarchy
my $ranks = { superkingdom => 1,
              kingdom      => 1,
              phylum       => 1,
              class        => 1,
              order        => 1,
              family       => 1,
              genus        => 1,
              species      => 1 };

# traverse the tree and add the metadata for each node to the DB
$log->info( 'traversing tree and storing nodes in database' );
traverse_tree( $sth, $tree, $nodes, $ranks, '' );

exit;

#-------------------------------------------------------------------------------
#- functions -------------------------------------------------------------------
#-------------------------------------------------------------------------------

sub retrieve_files {
  my ( $archive_url, $archive_file ) = @_;

  if ( -f $archive_file ) {
    $log->logwarn( "WARNING: removing old archive '$archive_file'" );
    unlink $archive_file;
  }

  getstore( $archive_url, $archive_file ) == RC_OK
    or $log->logdie( "ERROR: failed to retrieve archive: $!" );

  $log->logdie( "error: failed to write archive as '$archive_file'" )
    unless -f $archive_file;

  $log->info( "wrote nodes archive as '$archive_url'" );
}

#-------------------------------------------------------------------------------

sub extract_files {
  my ( $archive_file, $names_file, $nodes_file ) = @_;

  $log->info( 'extracting names and nodes files from tar archive' );

  my $tar = Archive::Tar->new;
  $tar->read( $archive_file )
    or $log->logdie( "EROR: failed to read tar archive '$archive_file'" );

  $tar->extract_file( 'names.dmp', $names_file )
    or $log->logdie( "EROR: failed to extract 'names.dmp' from tar archive '$archive_file'" );
  $log->debug( "successfully extracted 'names.dmp' from archive" );
    
  $tar->extract_file( 'nodes.dmp', $nodes_file )
    or $log->logdie( "EROR: failed to extract 'nodes.dmp' from tar archive '$archive_file'" );
  $log->debug( "successfully extracted 'nodes.dmp' from archive" );
}

#-------------------------------------------------------------------------------
# parse the taxonomy file

sub parse_names {
  my $names_file = shift;

  my %levels_to_promote = (
    Viroids                  => 1,
    Viruses                  => 1,
    'unclassified sequences' => 1,
    'other sequences'        => 1,
  );

  open ( NAMES, $names_file )
    or $log->logdie( "ERROR: couldn't open names file '$names_file': $!" );

  my $names;
  my $promoted_taxids;
  while ( <NAMES> ) {
    next unless m/^(\d+)\t\|\t(.*?)\t.*?scientific name/;
    $names->{$1} = $2;
    $promoted_taxids->{$1}++ if $levels_to_promote{$2};
  }

  close NAMES;

  return ( $names, $promoted_taxids );
}

#-------------------------------------------------------------------------------

sub parse_nodes {
  my ( $nodes_file, $names, $promoted_taxids ) = @_;

  open ( NODES, $nodes_file )
    or $log->logdie( "ERROR: couldn't open nodes file '$nodes_file': $!" );

  my $nodes = [];
  while ( <NODES> ) {
    my ( $taxid, $parent, $rank ) = $_ =~ m/^(\d+)\t\|\t(\d+)\t\|\t([\w\s]+)\t\|/;

    # shunt those special ids to be superkingdoms
    $rank = 'superkingdom' if $promoted_taxids->{$taxid};
      
    $names->{ $taxid } = '_unnamed' unless $names->{$taxid};

    # treat tax ID as an array index
    $nodes->[$taxid] = { taxid  => $taxid,
                         parent => $parent,
                         rank   => $rank,
                         name   => $names->{$taxid} };
  }

  close NODES;

  return $nodes;
}

#-------------------------------------------------------------------------------

sub build_tree {
  my $nodes = shift;

  my $tree = {};

  foreach my $node ( @$nodes ) {
    next unless $node;
    next unless ( $node->{rank} eq 'species' or 
                  $node->{rank} eq 'subspecies' or 
                  $node->{rank} eq 'no rank' );

    # walk up the hierarchy from the current node, storing each node in this
    # path in an array
    my $parent_node = $node;
    my @branch = ( $node );
    until ( $parent_node->{parent} == $parent_node->{taxid} ) {
      push @branch, $nodes->[ $parent_node->{parent} ];
      $parent_node = $nodes->[ $parent_node->{parent} ];
    }

    # now walk down that array and add each node in the path array as a node
    # in the full tree
    my $parent = $tree;
    my $subspecies = 0;
    for ( my $i = $#branch; $i >= 0; $i-- ) {
      $parent->{ $branch[$i]->{taxid} } = {}
        unless exists $parent->{ $branch[$i]->{taxid} };
      $parent = $parent->{ $branch[$i]->{taxid} };

      $branch[$i]->{rank} = 'subspecies' if $subspecies;
      $subspecies = 1 if $branch[$i]->{rank} eq 'species';
    }
  }

  return $tree;
}

#-------------------------------------------------------------------------------

sub order_nodes {
  my ( $subtree, $nodes, $c ) = @_;

  foreach my $taxid ( keys %{ $subtree } ) {
    $nodes->[$taxid]->{lft} = $c++;
    $c = order_nodes( $subtree->{$taxid}, $nodes, $c );
    $nodes->[$taxid]->{rgt} = $c++;
  }

  return $c;
}

#-------------------------------------------------------------------------------

sub traverse_tree {
  my ( $sth, $subtree, $nodes, $ranks, $tax_string ) = @_;
  
  foreach my $taxid ( keys %{ $subtree } ) {
    my $node_tax_string = $tax_string;
    $node_tax_string .= $nodes->[$taxid]->{name} . ';';
    $sth->execute( 
      $taxid,
      ( $nodes->[$taxid]->{rank} eq 'species' ) ? $nodes->[$taxid]->{name} : undef,
      $node_tax_string,
      $nodes->[$taxid]->{lft},
      $nodes->[$taxid]->{rgt}, 
      $nodes->[$taxid]->{parent},
      $nodes->[$taxid]->{name},
      ( exists $ranks->{ $nodes->[$taxid]->{rank} } ) ? 1 : 0, 
      $nodes->[$taxid]->{rank}
    );

    traverse_tree( $sth, $subtree->{$taxid}, $nodes, $ranks, $node_tax_string );
  }
}

#-------------------------------------------------------------------------------

sub usage {
  print STDERR <<EOF_help;

usage:  $0 [--dir <download dir>] [--retrieve] [--verbose] [--help]

This script builds the taxonomy tree table for the Rfam database using the NCBI
taxonomy tree dump files. 

parameters:
  --dir | -d <download dir>  directory to use for downloaded dump files
  --retrieve | -r            retrieve dump file from NCBI
  --verbose | -v             show debug messages
  --help | -h                show this message

The script can either work with a pre-downloaded dump tarball or it can
retrieve the dump and work with that. The directory to use for downloading
the dumps can be specified with "--dir"; the default behaviour is to use a
temp directory.

If a download directory is specified, when the script exits, the downloaded
files will be left in place, meaning that the script can be re-run (without the
-r flag) and use the existing files. If no directory is specified, the tarball
will be downloaded and unpacked in a temporary directory and these files will
be deleted when the script exits.

If the script files the names.dmp and nodes.dmp files in the working directory,
it will not attempt to unpack them from the tar archive again. If you want to
force that behaviour, delete the ".dmp" files before running the script.

The taxonomy tree will be loaded into the "taxonomy_websearch" table in the
"rfam_live" database. The connection to the database is retrieved from the
L<Bio::Rfam::Config> object, which needs to read the location of the
configuration file from the C<RFAM_CONFIG> environment variable.

The script makes no effort to check the contents of the taxonomy table before
trying to populate it. If the table is already loaded you'll probably see
errors from the database about duplicate rows; truncate the table before
running the script in order to avoid them.

EOF_help
}

