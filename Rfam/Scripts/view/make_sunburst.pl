#!/usr/bin/env perl

use strict;
use warnings;

use Log::Log4perl;
use RfamLive;
use DBI;
use JSON;
use Getopt::Long;

use Data::Dump qw(dump);

use Bio::Rfam::Config;
use Bio::Rfam::SunburstFactory;

#-------------------------------------------------------------------------------
# configure logging

my $logger_conf = q(
  log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout                   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
  log4perl.rootLogger                               = INFO, Screen
  log4perl.logger.Bio.Rfam.SunburstFactory          = INFO
);

Log::Log4perl->init( \$logger_conf );
my $log = Log::Log4perl->get_logger;

#-------------------------------------------------------------------------------
# process the options

my ( $chunk, $chunk_size, $entry, $pretty, $help, $file );

GetOptions( 'chunk=i'     => \$chunk,
            'chunksize=i' => \$chunk_size,
            'entry=s'     => \$entry,
            'file=s'      => \$file,
            'pretty'      => \$pretty,
            'help'        => \$help )
  or ( help() and die "Invalid option\n" );

help() and exit if $help;

#-------------------------------------------------------------------------------
# connect to the database

my $config = Bio::Rfam::Config->new;
my $rfam_live = $config->rfamlive;

#-------------------------------------------------------------------------------
# validate parameters

unless ( $file or $entry or ( $chunk_size and $chunk ) ) {
  help();
  $log->logdie( 'ERROR: no accession/ID, filename or chunk range specified' );
}

if ( $file and $entry ) {
  help();
  $log->logdie( "ERROR: can't specify both accession/ID AND file containing auto numbers" );
}

my @rfam_accessions;

if ( $entry ) {
  chomp $entry;
  my $row = $rfam_live->resultset( 'Family' )
                      ->search( [ { rfam_acc  => $entry },
                                  { rfam_id   => $entry } ],
                                { columns => [ 'rfam_acc' ] } )
                      ->first;
  if ( $row and $row->rfam_acc ) {
    push @rfam_accessions, $row->rfam_acc;
  }
  else {
    $log->warn( "No data found for $_" );
  }
}

if ( $file ) { 
  # get the list of rfam_accessions
  
  $log->logdie( "'$file' does not exist" ) unless -e $file;
  $log->logdie( "$file has no size" )      unless -s $file;
 
  $log->debug( "reading list of accessions from '$file'" );

  open LIST, $file
    or $log->logdie( "ERROR: couldn't open list file '$file': $!" );

  while ( <LIST> ) {
    chomp;
    my $row = $rfam_live->resultset( 'Family' )
                        ->search( [ { rfam_acc => $_ },
                                    { rfam_id  => $_ } ],
                                  { columns => [ 'rfam_acc' ] } )
                        ->first;
    if ( $row and $row->rfam_acc ) {
      push @rfam_accessions, $row->rfam_acc;
    }
    else {
      $log->logwarn( "WARNING: no data found for entry '$_'" );  
    }
  }

  close LIST;
}

if ( ( $chunk_size and $chunk ) and 
     ( not $file and not $entry ) ) {
  
  $log->debug( 'calculating chunk sizes' );

  my $all_families = $rfam_live->resultset('Family')
                               ->search( {},
                                         { page => 1,
                                           rows => $chunk_size } );

  my $pager = $all_families->pager;
  $log->info( "working on page $chunk out of " . $pager->last_page );

  my @chunk_rows = $all_families->page($chunk)->all;

  push @rfam_accessions, $_->rfam_acc for ( @chunk_rows );
}

$log->info( 'writing "pretty" JSON' ) if $pretty;

$log->info( 'building ' . scalar @rfam_accessions . ' sunbursts' );

#-------------------------------------------------------------------------------

my $sunburst_factory = Bio::Rfam::SunburstFactory->new(
  schema => $rfam_live,
  pretty_json => $pretty || 0,
);

foreach my $rfam_acc ( @rfam_accessions ) {
  $sunburst_factory->build( $rfam_acc );
}

exit;

#-------------------------------------------------------------------------------

sub help{
  print STDERR <<EOF_help;
  
usage:  $0 [--entry <accession|ID|rfam_acc> | --file <filename> | --chunk <n> --chunksize <n>] [--pretty]

This script is used for making the JSON data string that are rendered by the
website as sunbursts-style representations. 

parameters:
  --entry | -e <acc|ID|auto num>  Rfam family accession, ID or Rfam accession
  --file | -f <file>              name of a file containing a list of accessions/IDs
  --chunksize <N>                 number of sunbursts to generate in each "chunk" or batch
  --chunk | -c <N>                which particular sunburst of a batch to generate
  --pretty | -p                   write "pretty" JSON (nicely formatted, readable)

The script will generate sunbursts for families specified in one of three ways:

  accession/ID : a single Rfam accession or ID, specifying the family for which
                 to generate the sunburst
  file         : a file containing a list of Rfam family accessions or IDs. A
                 sunburst will be generated for each family in turn
  chunks       : the script retrieves the list of all Rfam families from the
                 database and splits it into multiple chunks or batches. The "chunk"
                 parameter specifies which chunk should be built for this job

The ability to process families in chunks allows the script to be run on the
farm, using a submission command something like:

  bsub -q normal  -R"select[mem>4000] rusage[mem=4000]" -M 4000000 \
  -J "sunburst[1-20]" -o "sunburst.\%J.\%I.log" '$0 \
  -chunksize 684 -chunk \$\{LSB_JOBINDEX\}'
  
EOF_help
}

