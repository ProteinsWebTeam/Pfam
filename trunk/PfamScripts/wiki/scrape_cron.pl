#!/usr/bin/perl

# This script downloads the list of current approved revision IDs for 
# wikipedia articles and scrapes the content for the approved revisions,
# depositing the content into the "wikitext" table.
#
# The revisions are retrieved as a JSON string from:
#
#   http://pfamsvn.sanger.ac.uk/cgi-bin/revisions.cgi
#
# jt6 20100625 WTSI
#
# $Id$

use strict;
use warnings;

use Getopt::Long;
use Config::General;
use LWP::UserAgent;
use JSON;
use Bio::Pfam::Wiki::Scraper;
use WebUser;
use Data::Dump qw(dump);

# the script needs to be able to find a table wrapper for the "wikitext"
# table in the "web_user" database. Add the appropriate DBIC schema 
# description to PERL5LIB

my $DEBUG = 0;

# get a WikiScraper...
my $scraper = Bio::Pfam::Wiki::Scraper->new;

# find the config file
my $config_file = 'conf/wiki.conf';
GetOptions ( 'config=s' => \$config_file );
die "ERROR: couldn't read config from '$config_file': $!" unless -e $config_file;

# parse the config and get the section relevant to the web_user database
my $cg = Config::General->new($config_file);
my %config  = $cg->getall;
my $db_conf = $config{WebUser};

# build database connection string
my $dsn = "dbi:mysql:$db_conf->{db_name}:$db_conf->{db_host}:$db_conf->{db_port}";

# get a DBIC database connection object
my $schema = WebUser->connect( $dsn, $db_conf->{username}, $db_conf->{password} );
# $schema->storage()->debug( 1 );

# get the URL for the CGI script that will distribute the approved revisions
my $revisions_script = $config{revisions_url};

# get the delay that we should use between wikipedia hits
my $scrape_loop_delay = $config{scrape_loop_delay};

# get a user agent and actually retrieve the list of revisions
my $ua = LWP::UserAgent->new;
$ua->env_proxy;

my $response = $ua->get($revisions_script);

my $revisions;
if ( $response->is_success ) {
  $revisions = decode_json( $response->decoded_content );
}
else {
  die 'ERROR: failed to retrieve revisions list: '. $response->status_line;
}

print STDERR "retrieved " . scalar( keys %$revisions ) . " article titles\n"
  if $DEBUG;

# update the database with the new revision IDs and scrape the text for any
# that have changed
my $numRows    = 0;
my $numUpdated = 0;
while ( my ( $title, $revision ) = each %$revisions ) {

  print STDERR "checking article '$title'... " if $DEBUG;

  unless ( $revision =~ m/^(\d+)$/ and $1 > 0 ) {
    print STDERR "invalid revision number for '$title' ($revision)\n";
    next;
  }

  # get the DBIC Row for that article. If there is no row for the article, it
  # means it's new to the approval process, and will presumably get an approved
  # revision number in the next round of approvals. We add a new row with the
  # default revision number of 0 in that case.
  my $row = $schema->resultset('Wikitext')
                ->find_or_create( { title => $title } );

	print STDERR "revision: |$revision|, approved_revision: |" . $row->approved_revision . "|\n"
	  if $DEBUG;

  # do we need to update the revision number and text ?
  if ( defined $row->approved_revision and
       $row->approved_revision != $revision ) {

    # yes; scrape the content and deposit it
    my $content = $scraper->scrape( $title );

    # print STDERR "\ncontent: |$content|\n" if $DEBUG;

    $row->update( { approved_revision => $revision,
                    text              => $content } );

    print STDERR "retrieved new content for '$title', revision ($revision)\n";
		$numUpdated++;

    sleep $scrape_loop_delay;
  }

  $numRows++;

}

print STDERR "scraped new content for $numUpdated out of $numRows articles\n";

