
# 01index_cached_many_times.t
# jt6 20071027 WTSI
#
# this test clears the server cache, loads the index page and checks it, loads 
# it again, this time from cache, and checks that the second load exactly 
# matches the first. We repeat this loop one hundred times.

# needs to know the location of the configuration file for the server, so that
# it can retrieve cache server IPs
# export PFAMWEB_CONFIG=/nfs/team71/pfam/jt6/server/PfamConfigWTSI/pfamweb.conf 

use strict;
use warnings;
    
use Config::General;  # configuration
use Cache::Memcached; # cache control

use Test::More tests => 500;
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
# configure the wrapper for the cache servers

my $cache_server_ips = $config{cache}->{backends}->{default}->{servers};
my $cache = new Cache::Memcached( { servers => $cache_server_ips } );

#-------------------------------------------------------------------------------
# and onto the testing...

my $mech = Test::WWW::Mechanize::Catalyst->new();

for( 1 .. 100 ) {

  $cache->flush_all;

  $mech->get_ok( '/',
                 'retrieve home page' );

  my( $server, $pid ) = $mech->content =~ m{<!-- (.*?) / (\d+) -->};
  diag( "served by process $pid on $server" ) if( $server and $pid );

  # store the content so we can check it later
  my $first_hit = $mech->content;
  
  $mech->content_contains( 'static/javascripts/prototype.js',
                           'correct prototype URL' );

  $mech->content_contains( 'The Pfam database is a large collection',
                           'sensible content' );

  # repeat the request and see what we get
  $mech->get_ok( '/',
                 'retrieve cached home page' );

  is( $first_hit, $mech->content,
      'cached content matches first hit' );

}

