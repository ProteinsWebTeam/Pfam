#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Config::General;
use LWP::UserAgent;
use JSON;
use Bio::Pfam::Wiki::Scraper;
use WebUser;
use Data::Dump qw(dump);
use utf8;

# the script needs to be able to find a table wrapper for the "wikitext"
# table in the "web_user" database. Add the appropriate DBIC schema
# description to PERL5LIB

my $DEBUG = 0;

#-------------------------------------------------------------------------------

# check the inputs
my ( $acc, $title, $force );
my $config_file = 'conf/wiki.conf';

my $rv = GetOptions( 'config=s' => \$config_file,
                     'acc=s'    => \$acc,
                     'title=s'  => \$title,
                     'force'    => \$force,
                     'debug'    => \$DEBUG );

die 'ERROR: must supply either an accession or a wikipedia title'
  unless ( $acc or $title );

die 'ERROR: must supply either only one of accession or wikipedia title'
  if ( $acc and $title );

die "ERROR: couldn't read config from '$config_file': $!"
  unless -e $config_file;

#-------------------------------------------------------------------------------

# get a WikiScraper...
my $scraper = Bio::Pfam::Wiki::Scraper->new;

# parse the config and get the section relevant to the web_user database
my $cg = Config::General->new($config_file);
my %config  = $cg->getall;
my $db_conf = $config{WebUser};

# build database connection string
my $dsn = "dbi:mysql:$db_conf->{db_name}:$db_conf->{db_host}:$db_conf->{db_port}";

# get a DBIC database connection object
my $schema = WebUser->connect( $dsn, $db_conf->{username}, $db_conf->{password}, { mysql_enable_utf8mb4 => 1 } );
# $schema->storage()->debug( 1 ) if $DEBUG;

#-------------------------------------------------------------------------------

my @titles;
if ( $acc ) {
  die 'ERROR: not a valid accession' unless $acc =~ m/^[PR]F\d{5}$/;
  print STDERR "looking up article for accession '$acc'\n"
    if $DEBUG;

  # look up the article title(s) for the given accession
  my @rs = $schema->resultset('ArticleMapping')
                  ->search( { accession => $acc } );

  push @titles, $_->title for @rs;
}
else {
  print STDERR "looking up article '$title'\n"
    if $DEBUG;
  push @titles, $title;
}

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

foreach my $title ( @titles ) {

  print STDERR "checking article '$title'... " if $DEBUG;

  my $revision = $revisions->{$title};
  unless ( $revision ) {
    print STDERR "WARNING: Invalid title ('$title'); no revision ID in DB\n";
    next;
  }

  unless ( $revision =~ m/^\d+$/ ) {
    print STDERR "WARNING: Invalid revision number for '$title' ($revision)\n";
    next;
  }

  if ( $revision == 0 and not $force ) {
    print STDERR "unapproved article: '$title'; content will not be scraped until the article has been approved\n";
    next;
  }

  my $row;
  if ( $force ) {
    $row = $schema->resultset('Wikitext')
                  ->find_or_create( { title => $title } );
  }
  else {
    $row = $schema->resultset('Wikitext')
                  ->find( { title => $title } );
    unless ( $row ) {
      print STDERR "'$title' has not previously been scraped; use 'force' option to load anyway\n";
      next;
    }
  }

	print STDERR "revision: |$revision|, approved_revision: |" . $row->approved_revision . "|\n"
	  if $DEBUG;

  # do we need to update the revision number and text ?
  if ( ( defined $row->approved_revision and
         $row->approved_revision != $revision ) or $force ) {

    print STDERR "forcing loading of content\n" if $force;

    # yes; scrape the content and deposit it
    my $content = $scraper->scrape( $title, $revision );

    $row->update( { approved_revision => $revision,
                    text              => $content } );

    print STDERR "retrieved new content for '$title', revision $revision\n";

    sleep $scrape_loop_delay;
  }

}

print STDERR "done\n";
