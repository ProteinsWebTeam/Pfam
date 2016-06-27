#!/usr/bin/env perl

# This script tries to do two things.
#
# Firstly, it updates the mapping between Pfam/Rfam entries and wikipedia
# articles. For each database, the script retrieves the article titles that are
# associated with each family. For accessions with new mappings, the old
# mapping is delete and the new one is added to the "wiki_approve.article_mapping"
# table.
#
# Secondly, the script updates the list of wikipedia articles in the
# "wiki_approve.wikipedia". That table is used by the WikiApprove web
# application and associated cron jobs, giving the list of titles that need to
# be checked for updates and presented for approval.
#
# This script checks the pfam_live and rfam_live databases and retrieves the
# set of wikipedia articles that are currently in use. If an article isn't
# found in "wiki_approve.wikipedia", the title is added. No titles are deleted
# from that table.
#
# jt6 20111114 WTSI
#
# $Id$

use strict;
use warnings;

use Getopt::Long;
use Config::General;
use Data::Dump qw(dump);
use Log::Log4perl qw(get_logger :levels);

use WikiApprove;
use PfamLive;
use RfamDB;  # there is no "RfamLive" DBIC wrapper

#-------------------------------------------------------------------------------

# set up logging
my $logger_conf = q(
  log4perl.logger                   = INFO, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );

my $log = get_logger();

#-------------------------------------------------------------------------------

# handle the command-line options
my $config_file = 'conf/wiki.conf';
my $verbosity = 0;
GetOptions ( 'config=s' => \$config_file,
             'v+'       => \$verbosity );

# increase the amount of logging by one level for each "v" switch added
$log->more_logging(1) while $verbosity-- > 0;

# find the config file
$log->logdie( "ERROR: couldn't read config from '$config_file': $!" )
  unless -e $config_file;

#-------------------------------------------------------------------------------

# parse the config and get the section relevant to the web_user database
my $cg        = Config::General->new($config_file);
my %config    = $cg->getall;
my $wa_conf   = $config{WikiApprove};
my $pfam_conf = $config{PfamLive};
my $rfam_conf = $config{RfamLive};

#-------------------------------------------------------------------------------

# get all of the database connections that we'll need
my $wa_schema =
  WikiApprove->connect(
    "dbi:mysql:$wa_conf->{db_name}:$wa_conf->{db_host}:$wa_conf->{db_port}",
    $wa_conf->{username},
    $wa_conf->{password}
  );

$log->debug( 'connected to wiki_approve' ) if $wa_schema;

my $pfam_schema =
  PfamLive->connect(
    "dbi:mysql:$pfam_conf->{db_name}:$pfam_conf->{db_host}:$pfam_conf->{db_port}",
    $pfam_conf->{username},
    $pfam_conf->{password}
  );

$log->debug( 'connected to pfam_live' ) if $pfam_schema;

my $rfam_schema =
  RfamDB->connect(
    "dbi:mysql:$rfam_conf->{db_name}:$rfam_conf->{db_host}:$rfam_conf->{db_port}",
    $rfam_conf->{username},
    $rfam_conf->{password}
  );

$log->debug( 'connected to rfam_live' ) if $rfam_schema;

# if we can't connect to all of these, we're done here
$log->logdie( "ERROR: couldn't connect to one or more databases" )
  unless ( $wa_schema and $pfam_schema and $rfam_schema );

#-------------------------------------------------------------------------------
# get the Pfam article titles from the live database
my @live_articles =
  $pfam_schema->resultset('PfamAWiki')
				->search( 	{ 'title' => {'!=' => undef}},
							{ 	prefetch => 'auto_wiki',
								select   => [ qw(pfama_acc auto_wiki.title ) ],
								as       => [ qw( acc 				title ) ] });

$log->info( 'got ' . scalar @live_articles . ' articles for live Pfam-As' );

# when a family is killed we store the title that it maps to at that point in
# time. We'll retrieve those titles from "dead_families" and use the in the
# mapping unless there's a "forward_to" for the family instead
my @dead_titles =
  $pfam_schema->resultset('DeadFamily')
              ->search( { title => { '!=' => undef } },
                        { select => [ qw( pfama_acc title ) ],
                          as     => [ qw( acc title ) ] } );

$log->info( 'got ' . scalar @dead_titles . ' articles with titles in dead_families' );

# when we make a release and then kill a family, the dead family is still
# available via the website. In that case we still want to show the wp article,
# but we need to get it from the family that the dead one forwards to. This
# gnarly query joins "pfamA" to "dead_families" to get the row from "pfamA"
# corresponding to the forwarded-to family, and then joins that to "pfamA_wiki"
# and then onto "wikipedia" to get the article title
my @dead_articles =
  $pfam_schema->resultset('DeadFamily')
              ->search( { 'auto_wiki.title' => { '!=' => undef } },
                        { join     =>  {'pfam_a_wikis', 'auto_wiki'},
                          select   => [ qw( pfam_a_wikis.pfama_acc auto_wiki.title ) ],
                          as       => [ qw( acc                 title ) ] } );

$log->info( 'got ' . scalar @dead_articles . ' articles by mapping from dead_families via pfam' );

# hash the new Pfam mapping. Remove each acc in the new mapping from the old one
my %pfam_map;
foreach ( @live_articles, @dead_titles, @dead_articles ) {
  my $acc   = $_->get_column('acc');
  my $title = $_->get_column('title');
  push @{ $pfam_map{$acc} }, $title;
}

# $DB::single = 1;

#---------------------------------------

# update the wikipedia and article_mapping tables with the new Pfam data

# clear the table and repopulate it. This ensures that families which have been
# have had wikipedia mapping removed are correctly updated
$log->info("Deleting all existing article mappings");
$wa_schema->resultset('ArticleMapping')->delete_all();

foreach my $acc ( keys %pfam_map ) {
  my $titles = $pfam_map{$acc};

  $log->debug( "deleting mapping(s) for Pfam entry '$acc'" );
  my $rv = $wa_schema->resultset('ArticleMapping')
                     ->search( { accession => $acc } );
  if($rv->count) {
	my $deleted = $rv->delete;
  	$log->logwarn( "warning: failed to delete old mapping for '$acc'" )
    	unless $deleted;
  }

  $log->debug( "checking Pfam entry '$acc'" );
  foreach my $title ( @$titles ) {
    $log->debug( "checking title '$title'" );
    eval {
      add_row( $acc, $title, 'pfam' );
    };
    if ( $@ ) {
      $log->logwarn( $@ );
    }
  }
}

$log->info( 'done with Pfam entries/articles' );

#-------------------------------------------------------------------------------

# get the Rfam article titles from the live database
@live_articles =
  $rfam_schema->resultset('Family')
              ->search( undef,
                        { prefetch => 'auto_wiki',
                          select   => [ qw( rfam_acc auto_wiki.title ) ],
                          as       => [ qw( acc      title ) ] } );
# Note: because we're using RfamDB as a wrapper for the rfam_live database, there's
# a mismatch between the table and the table definition. Specifically, the "cmsearch"
# column doesn't exist in the table in rfam_live, but it's listed in the wrapper. By
# using the "columns" attribute on the search, we can restrict the columns that are
# requested in the raw SQL query.

$log->info( 'got ' . scalar @live_articles . ' articles for live Rfams' );

# see if there are any dead families which have a title directly in the
# dead_families table
@dead_titles =
  $rfam_schema->resultset('DeadFamily')
              ->search( { title => { '!=' => undef } },
                        { select => [ qw( rfam_acc title ) ],
                          as     => [ qw( acc      title ) ] } );

$log->info( 'got ' . scalar @dead_titles . ' articles with titles in dead_families' );

# get the articles that map to the family that a dead family forwards to... if
# that makes any sense...
@dead_articles =
  $rfam_schema->resultset('Family')
              ->search( undef,
                        { prefetch => [ qw( from_dead article ) ],
                          select   => [ qw( from_dead.rfam_acc article.title ) ],
                          as       => [ qw( acc                title ) ] } );

$log->info( 'got ' . scalar @dead_articles . ' articles by mapping from dead_families via rfam' );

my %rfam_map;
foreach ( @live_articles, @dead_titles, @dead_articles ) {
  my $acc   = $_->get_column('acc');
  my $title = $_->get_column('title');
  push @{ $rfam_map{$acc} }, $title;
}

#-------------------------------------------------------------------------------

foreach my $acc ( keys %rfam_map ) {
  my $titles = $rfam_map{$acc};

  foreach my $title ( @$titles ) {
    $log->debug( "checking Rfam entry/title: |$acc|$title|" );
    eval {
      add_row( $acc, $title, 'rfam' );
    };
    if ( $@ ) {
      $log->logwarn( $@ );
    }
  }
}

$log->info( 'done with Rfam entries/articles' );

#-------------------------------------------------------------------------------

$log->info( 'done' );

exit;

#-------------------------------------------------------------------------------
#- functions -------------------------------------------------------------------
#-------------------------------------------------------------------------------

# adds a row to the "article_mapping" table with the specified accession, title
# and "db" values, and a corresponding row to "wikipedia". All rows in the
# "article_mapping" table with the given accession are deleted before an attempt
# to add the new ones. If the delete fails, a warning is issues. If inserting
# into either "article_mapping" or "wikipedia" fails, an exception is thrown.

sub add_row {
  my ( $acc, $title, $db ) = @_;

  # this should be in a transaction

  $wa_schema->resultset('ArticleMapping')
            ->update_or_create( { title     => $title,
                                  accession => $acc,
                                  db        => $db },
                                { key => 'primary' } )
    or die "error: failed to add mapping for '$acc' --> '$title'";

  $wa_schema->resultset('Wikipedia')
            ->update_or_create( { title       => $title,
                                  approved_by => 'new',
                                  pfam_status => $db eq 'pfam' ? 'active' : 'inactive',
                                  rfam_status => $db eq 'rfam' ? 'active' : 'inactive' },
                                { key => 'primary' } )
  or die "error: failed to add wikipedia row for '$acc', '$title'";
}

#-------------------------------------------------------------------------------
