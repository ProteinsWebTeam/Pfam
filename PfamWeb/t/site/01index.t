
# 001index.t
# jt6 20071027 WTSI
#
# tests the main index page

# controlled by a couple of environment variables: 
# export PFAMWEB_CONFIG=/nfs/team71/pfam/jt6/server/PfamConfigWTSI/pfamweb.conf 
# export SKIP_LINK_CHECK=1

use strict;
use warnings;
    
use PfamDB;          # database connections
use Config::General; # configuration

use Test::More tests => 7;
use Test::WWW::Mechanize::Catalyst 'PfamWeb';

#-------------------------------------------------------------------------------
# read the configuration for the application from file specified by the 
# "PFAMWEB_CONFIG" environment variable

# first check that we can find the file
die 'error: $ENV{PFAMWEB_CONFIG} not set correctly'
  unless( defined $ENV{PFAMWEB_CONFIG} and -s $ENV{PFAMWEB_CONFIG} );

my $conf;
eval { $conf = new Config::General( $ENV{PFAMWEB_CONFIG} ) };
die "error: problem parsing configuration file:\n$@" if $@;

my %config;
eval { %config = $conf->getall };
die "error: problem retrieving configuration from file:\n$@" if $@;

#-------------------------------------------------------------------------------
# get the database connection info from the configuration and retrieve the 
# contents of the VERSION table, so that we can check the front page details
# against it

my $db_info = $config{Model}->{PfamDB}->{connect_info};
my $schema = PfamDB->connect( @$db_info );
my $pfam_release_data = $schema->resultset( 'Version' )->find( {} );

die "error: couldn't retrieve database version data"
  unless defined $pfam_release_data;

#-------------------------------------------------------------------------------
# and onto the testing...

my $mech = Test::WWW::Mechanize::Catalyst->new();

$mech->get_ok( '/',
               'retrieve home page' );
  
my( $server, $pid ) = $mech->content =~ m{<!-- (.*?) / (\d+) -->};
diag( "served by process $pid on $server" ) if( $server and $pid );

$mech->title_is( 'Pfam: Home page',
                 'title as expected' );

$mech->content_contains( 'Pfam ' . $pfam_release_data->pfam_release,
                         'latest Pfam release number' );

$mech->content_contains( $pfam_release_data->number_families,
                         'number of families' );

$mech->content_contains( 'static/javascripts/prototype.js',
                         'correct prototype URL' );

SKIP:
{
  skip( 'link check; $ENV{SKIP_LINK_CHECK} == 1', 1 )
    if $ENV{SKIP_LINK_CHECK};

  $mech->page_links_ok( 'index page links' );
}

SKIP:
{
  eval 'use Test::HTML::Tidy';

  skip( 'HTML validation; Test::HTML::Tidy unavailable', 1 ) if $@;

  html_tidy_ok( 'HTML is valid' );
}
