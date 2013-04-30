#!/software/bin/perl

# this is a cron script to archive jobs from the job_history and job_stream 
# tables. We archive any jobs older than one week, copying them across to
# the archive database that's configured in the config file. This should 
# probably be the "job_archive" database in the staging instance.
#
# jt6 20130430 WTSI

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::WebUserDBManager;
use Log::Log4perl qw(get_logger :levels);

#-------------------------------------------------------------------------------
# set up logging

my $logger_conf = q(
  log4perl.logger                   = DEBUG, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );

my $log = get_logger();

#-------------------------------------------------------------------------------
# retrieve DB connection parameters and set up the connections

my $config = Bio::Pfam::Config->new;

my $live_db_params = $config->webuseradmin;
my $live_wudbm = Bio::Pfam::WebUserDBManager->new( %$live_db_params );
my $live_schema = $live_wudbm->getSchema;

my $archive_db_params = $config->webuserarchive;
my $archive_wudbm = Bio::Pfam::WebUserDBManager->new( %$archive_db_params );
my $archive_schema = $archive_wudbm->getSchema;

$log->info( 'set up DB connections' );

#-------------------------------------------------------------------------------
# get the ID of the job run one week ago. This ID will be the starting point for
# the copy

$live_schema->storage->debug(1) if $log->is_debug;

my $id_rs = $live_schema->resultset('JobHistory')
                        ->search( { opened => { '>', \'DATE_SUB( NOW(), INTERVAL 1 WEEK )' } },
                                  { order_by => { -asc => [ 'id' ] },
                                    rows     => 1 } )
                        ->single;

my $cut_off_id = $id_rs->id;
$log->info( "archiving jobs with ID <= $cut_off_id" );

$live_schema->storage->debug(0);

#-------------------------------------------------------------------------------
# get the ResultSets for the history and stream tables

my $live_jh_rs = $live_schema->resultset( 'JobHistory' )
                             ->search( { id => { '<=', $cut_off_id } },
                                       { select => [ qw( id
                                                         job_id
                                                         status
                                                         options
                                                         estimated_time
                                                         opened
                                                         closed
                                                         started
                                                         job_type
                                                         email ) ] } );

my $live_js_rs = $live_schema->resultset('JobStream')
                             ->search( { id => { '<=', $cut_off_id } }, {} );

$log->info( 'got history/stream rows' );

#-------------------------------------------------------------------------------
# copy the rows to the archive database

my $copied = 0;
ROW: while ( my $live_jh_row = $live_jh_rs->next ) {

  my %history_data = $live_jh_row->get_columns;
  my %stream_data  = ( id     => $live_jh_row->id,
                       stdin  => $live_jh_row->stdin  || '',
                       stdout => $live_jh_row->stdout || '',
                       stderr => $live_jh_row->stderr || '' );

  my $id = $live_jh_row->id;

  $log->debug( "Copying row $id\n" ) if not $copied % 250;

  my $archive_jh_row = $archive_schema->resultset('JobHistory')
                                      ->find_or_new( \%history_data );
 
  my $archive_js_row = $archive_schema->resultset('JobStream')
                                      ->find_or_new( \%stream_data );

  if ( $archive_jh_row->in_storage or $archive_js_row->in_storage ) {
    $log->logwarn( "WARNING: row exists for job $id" );
    next ROW;
  }

  eval {
    $archive_schema->txn_do( sub {
      $archive_jh_row->insert;
      $archive_js_row->insert;
    } );
  };
  if ( $@ ) {
    $log->logwarn( "WARNING: failed to insert row for job $id ($@)" );
    next ROW;
  }

  $copied++
}

$log->info( "Copied $copied rows" );

#-------------------------------------------------------------------------------
# clean up. Delete the copied rows from the source tables

$live_schema->storage->debug(1) if $log->is_debug;

$live_jh_rs->delete;
$live_js_rs->delete;

$log->info( 'deleted old rows from live table' );

