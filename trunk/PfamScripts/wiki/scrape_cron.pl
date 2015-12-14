#!/usr/bin/env perl

# This script connects to the wiki_approve database the list of current approved revision IDs for 
# wikipedia articles and scrapes the content for the approved revisions,
# depositing the content into the "wikitext" table.
#
# $Id$

use strict;
use warnings;

use Getopt::Long;
use Config::General;
use Bio::Pfam::Wiki::Scraper;

use Log::Log4perl qw(get_logger :levels);
use WikiApprove;
use WebUser;
use Data::Dump qw(dump);

# the script needs to be able to find a table wrapper for the "wikitext"
# table in the "web_user" database. Add the appropriate DBIC schema 
# description to PERL5LIB

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

# get a WikiScraper...
my $scraper = Bio::Pfam::Wiki::Scraper->new;

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
my $cg = Config::General->new($config_file);
my %config  = $cg->getall;
my $wu_conf = $config{WebUser};
my $wa_conf   = $config{WikiApprove};
my $scrape_loop_delay = $config{scrape_loop_delay};

#-------------------------------------------------------------------------------

# get all of the database connections that we'll need
my $wa_schema = 
  WikiApprove->connect( 
    "dbi:mysql:$wa_conf->{db_name}:$wa_conf->{db_host}:$wa_conf->{db_port}", 
    $wa_conf->{username},
    $wa_conf->{password}
  );

$log->debug( 'connected to wiki_approve' ) if $wa_schema;

# get all of the database connections that we'll need
my $wu_schema = 
  WebUser->connect( 
    "dbi:mysql:$wu_conf->{db_name}:$wu_conf->{db_host}:$wu_conf->{db_port}", 
    $wu_conf->{username},
    $wu_conf->{password},
    { mysql_enable_utf8 => 1 }
  );

$log->debug( 'connected to web_user' ) if $wu_conf;

#---------------------------------------
# fetch titles and approved revisions for active entries in wikipedia tables

my @rows = $wa_schema->resultset("Wikipedia")
				->search(
					{	-or => [{"pfam_status" => "active"}, {"rfam_status" => "active"}],
					},
					{ 	select => ["title", "approved_revision"] }
				);
$log->debug("Found ".(@rows)." hits");

# update the database with the new revision IDs and scrape the text for any
# that have changed
my $numRows    = 0;
my $numUpdated = 0;
foreach my $row (@rows) {
	my $title = $row->title();
	my $rev = $row->approved_revision();
	
	$log->debug("Checking article $title:$rev");
	
	unless ( $rev =~ m/^\d+$/ ) {
    	$log->error("WARNING: Invalid revision number for '$title' ($rev)");
    	next;
  	}
  	if ( $rev == 0 ) {
  		$log->error("unapproved article: '$title'; content will not be scraped until the article has been approved");
   		next;
 	}
 	
	# get the DBIC Row for that article. If there is no row for the article, it
	# means it's new to the approval process, and will presumably get an approved
	# revision number in the next round of approvals. We add a new row with the
	# default revision number of 0 in that case.
	my $wikitext_row = $wu_schema->resultset('Wikitext')
	              ->find_or_create( { title => $title } );
	
	# make sure the row object in memory matches the row in the database. We need
	# to do this to make sure that a new row, which will be created by the DB
	# with an approved_revision of 0, is correctly populated before we try the
	# test below
	$wikitext_row->discard_changes;
	my $approved_revision = $wikitext_row->approved_revision;
	$log->debug("Current revision=$approved_revision Approved revision=$rev");
       		
	if ( defined $approved_revision and $approved_revision != $rev ) {
		$log->debug("Fetching $rev for $title");
       	my $content = $scraper->scrape( $title, $rev );
       	$wikitext_row->update( { approved_revision => $rev,
                   				 text              => $content } );
        $numUpdated++;
		sleep $scrape_loop_delay;
	}
       
    #add title to pfam accession mapping
    my @pfam_hits = $wa_schema->resultset("ArticleMapping")->search({title => $title}, {select => "accession"});
	$log->debug("Updating ".@pfam_hits." matching $title");
	foreach my $pfam_hit (@pfam_hits) {
		my $pfam_acc = $pfam_hit->accession;
		$log->debug("Matched Pfam = $pfam_acc to $title");
		$wu_schema->resultset("ArticleMapping")->find_or_create({accession => $pfam_acc, title => $title});
	}
	   
    $numRows++;
}
print STDERR "scraped new content for $numUpdated out of $numRows articles\n";

