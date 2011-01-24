#!/software/bin/perl

# This script tries to do two things.
#
# Firstly, it updates the mapping between Pfam/Rfam entries and wikipedia
# articles. For each database, the script retrieves the article titles that are
# associated with each family and updates the "wikipedia.article_mapping"
# table. No mappings are deleted from the table.
#
# Secondly, the script updates the list of wikipedia articles in the
# "wiki_approve.wikipedia". That table is used by the WikiApprove web
# application and associated cron jobs, giving the list of titles that need to
# be checked for updates and presented for approval.
#
# This script checks the pfam_live and rfam_live databases and retrieves the
# set of wikipedia articles that are currently in use. If an article isn't found
# in "wiki_approve.wikipedia", the title is added. No titles are deleted from 
# that table.
#
# jt6 20110111 WTSI
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

# find the config file
my $config_file = 'conf/wiki.conf';
GetOptions ( 'config=s' => \$config_file );
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
my @pfam_articles 
  = $pfam_schema->resultset('Pfama')
                ->search( { 'auto_wiki.title' => { '!=' => undef } },
                          { join     => { 'pfama_wikis' => 'auto_wiki' },
                            prefetch => { 'pfama_wikis' => 'auto_wiki' } } );

$log->info( 'got ' . scalar @pfam_articles . ' Pfam-A rows' ); 

# need to do a bit of work to handle the many-to-many relationship here
foreach my $pfam ( @pfam_articles ) { 
  $log->debug( 'checking Pfam entry ' . $pfam->pfama_acc );
  foreach my $article ( $pfam->articles ) {

    $log->debug( 'checking title: ' . $article->title );

    # The wikipedia table is where we store the list of articles that are
    # tracked and presented for approval in the webapp. We want to find any
    # articles that aren't currently listed in the wikipedia table. If an
    # article isn't found, we add it, with default parameters. If it is there,
    # we don't bother.
    my $wp_row = $wa_schema->resultset('Wikipedia')
                           ->find_or_create( { title       => $article->title,
                                               approved_by => 'jt6',
                                               pfam_status => 'active', 
                                               rfam_status => 'inactive' },
                                             { key => 'primary' } );

    # We also want to keep track of the mapping between articles and Pfam
    # entries. If the mapping is already there, we update it to this latest
    # one. If not, we add it.
    my $mapping = $wa_schema->resultset('ArticleMapping')
                            ->update_or_create( { accession => $pfam->pfama_acc,
                                                  title     => $article->title,
                                                  db        => 'pfam' } ,
                                                { key => 'primary' } );
  }
}

$log->info( 'done with Pfam entries/articles' );

#-------------------------------------------------------------------------------

# get the Rfam article titles from the live database
my @rfam_articles = 
  $rfam_schema->resultset('Rfam')
              ->search( { },
                        { join     => 'auto_wiki',
                          prefetch => 'auto_wiki',
                          columns  => [ qw( rfam_acc auto_wiki.title ) ] } );
# Note: because we're using RfamDB as a wrapper for the rfam_live database, there's 
# a mismatch between the table and the table definition. Specifically, the "cmsearch"
# column doesn't exist in the table in rfam_live, but it's listed in the wrapper. By
# using the "columns" attribute on the search, we can restrict the columns that are
# requested in the raw SQL query.

$log->info( 'got ' . scalar @rfam_articles . ' Rfam rows' ); 

foreach my $rfam ( @rfam_articles ) { 
  my $acc   = $rfam->rfam_acc;
  my $title = $rfam->auto_wiki->title;

  $log->debug( "checking Rfam entry/title: |$acc|$title|" );

  my $wp_row = $wa_schema->resultset('Wikipedia')
                         ->find_or_create( { title       => $title,
                                             approved_by => 'jt6',
                                             pfam_status => 'inactive', 
                                             rfam_status => 'active' } );

  my $mapping = $wa_schema->resultset('ArticleMapping')
                          ->update_or_create( { accession => $acc,
                                                title     => $title,
                                                db        => 'rfam' } );
}

$log->info( 'done with Rfam entries/articles' );

#-------------------------------------------------------------------------------

exit;

