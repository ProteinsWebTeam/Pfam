#!/software/bin/perl

use strict;
use warnings;

use DBI;
use Data::Dumper;
use Getopt::Long;

use Log::Log4perl;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


#-------------------------------------------------------------------------------
# configure logging

my $logger_conf = q(
  log4perl.logger                   = WARN, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );
my $log = Log::Log4perl->get_logger;

#-------------------------------------------------------------------------------
# check input

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{  $config->pfamlive }
);

my ( $help );

GetOptions(

  "help"     => \$help
);

usage() and exit if $help;



#-------------------------------------------------------------------------------
#- main ------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# prepare the database access and the queries that we'll use. We expect to 
# write to a table like this:
#
# CREATE TABLE taxonomy (
#   ncbi_taxid INT(10)      DEFAULT 0,
#   species   VARCHAR(100) DEFAULT NULL,
#   taxonomy  MEDIUMTEXT,
#   lft       INT(10),
#   rgt       INT(10),
#   parent    VARCHAR(100),
#   level     VARCHAR(100)
# );
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

# connect
$pfamDB->getSchema->resultset('Taxonomy')->delete;

my $dbh = $pfamDB->getSchema->storage->dbh;
$log->debug( 'successfully connected to database' );

# just prepare the insert statement
my $insertSth = $dbh->prepare( "INSERT INTO taxonomy VALUES ( ?, ?, ?, ?, ?, ?, ? )" );
$log->debug( 'prepared INSERT statement' );

# prepare and execute the select
my $extractSth = $dbh->prepare( "SELECT ncbi_taxid, species, taxonomy FROM ncbi_taxonomy WHERE species != ''" );
$extractSth->execute;
$log->debug( 'executed SELECT statement to retrieve tree' );

my $tree = {};
while ( my $row = $extractSth->fetchrow_arrayref ) {
  
  my $parent = $tree;
  foreach ( split( /;/, $row->[2] )) {
    $parent->{$_} = {} unless exists $parent->{$_};
    $parent = $parent->{$_};
  }
  $parent->{$row->[1]."-IsLeaf"} = {};
  $parent->{$row->[1]."-IsLeaf"}->{NCBI} = $row->[0];
  $parent->{$row->[1]."-IsLeaf"}->{TAXONOMY} = $row->[2];
  
}

# open( TREE, ">tree.log" )
#   or die "Couldn't open \"tree.log\" for write: $!";
# print TREE Dumper( $tree );
# close TREE;

#my $indent = "";

$log->info( 'starting to build the tree' );
my $count  = 1;
build( $tree, "root" );

# open( TREE, ">tree_numbered.log" )
#   or die "Couldn't open \"tree.log\" for write: $!";
# print TREE Dumper( $tree );
# close TREE;

# open( WALK, ">walk.log" )
#   or die "Couldn't open \"walk.log\" for write: $!";

# $indent = "";

$log->info( 'starting to walk the tree' );
walk( $tree );

exit;

#-------------------------------------------------------------------------------
#- subroutines -----------------------------------------------------------------
#-------------------------------------------------------------------------------

sub build {
  my ( $hash, $parent ) = @_;

  foreach my $nodeName ( keys %$hash ) {
    #   print STDERR $indent . "key/value: |$nodeName|" . $hash->{$nodeName} . "| ";
    #    if( ref $hash->{$nodeName} ) {
    #      print STDERR "found a hash with " .
    #     scalar( keys %{$hash->{$nodeName}} ) . " keys; walking\n";
    #    } else {
    #      print STDERR "not a hash\n";
    #    }
    next unless ref $hash->{$nodeName};

    my $node = $hash->{$nodeName};
    $node->{PARENT} = $parent;
    if ( exists $node->{NCBI} ) {
      $node->{LEFT}  = $count++;
      $node->{RIGHT} = $count++;
    }
    else {
      $node->{LEFT} = $count++;
      # $indent .= "  ";
      build( $node, $nodeName );
      # $indent = substr( $indent, 2 );
      $node->{RIGHT} = $count++;
    }

  } # end of foreach node

}

#-------------------------------------------------------------------------------

sub walk {
  my $hash = shift;

  my ( $left, $right, $parent );
  foreach my $nodeName ( keys %$hash ) {

    $left   = $hash->{$nodeName}->{LEFT};   delete $hash->{$nodeName}->{LEFT};
    $right  = $hash->{$nodeName}->{RIGHT};  delete $hash->{$nodeName}->{RIGHT};
    $parent = $hash->{$nodeName}->{PARENT}; delete $hash->{$nodeName}->{PARENT};

    #   print WALK $indent, "name:      |$nodeName|\n";
    #   print WALK $indent, "left:      |$left|\n";
    #   print WALK $indent, "right:     |$right|\n";
    #   print WALK $indent, "parent:    |$parent|\n";
    $insertSth->bind_param( 4, $left );
    $insertSth->bind_param( 5, $right );
    $insertSth->bind_param( 6, $parent );

    if ( exists $hash->{$nodeName}->{NCBI} ) {
      #    print WALK $indent, "NCBI code: |".$hash->{$nodeName}->{NCBI}."|\n";
      #    print WALK $indent, "tax:       |".$hash->{$nodeName}->{TAXONOMY}."|\n";
      $insertSth->bind_param( 1, $hash->{$nodeName}->{NCBI} );
      $nodeName =~ /^(.*?)\-IsLeaf$/;
      $insertSth->bind_param( 2, $1 );
      $insertSth->bind_param( 3, $hash->{$nodeName}->{TAXONOMY} );
      $insertSth->bind_param( 7, "NULL" );
      $insertSth->execute;
    }
    else {
      $insertSth->bind_param( 1, "NULL" );
      $insertSth->bind_param( 2, "NULL" );
      $insertSth->bind_param( 3, "NULL" );
      $insertSth->bind_param( 7, $nodeName );
      $insertSth->execute;
      #    $indent .= "  ";
      walk( $hash->{$nodeName} );
      #    $indent = substr( $indent, 2 );
    }

  }
}

#-------------------------------------------------------------------------------

sub usage {

print <<'EOF_help';
Usage: tree.pl [OPTION]

Build a database table of the taxonomic tree.

Example: tree.pl -h pfamdb2a --rdb pfam_35_0 -u pfam -p wibble --port 3301 

Database parameters:
  -r, --rdb      database name
  -h, --host     database host name
      --port     database port number
  -u, --user     database account user name
  -p, --pass     database account password
      --help     show this message

EOF_help
 
}

#-------------------------------------------------------------------------------

