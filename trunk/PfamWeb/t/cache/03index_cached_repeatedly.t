
# 03index_cached_repeatedly.t
# jt6 20071027 WTSI
#
# tests the caching of the index page by repeatedly clearing the cache, 
# generating the index page and checking that subsequent reloads return
# exactly the same content.

# needs to know the location of the configuration file for the server, so that
# it can retrieve cache server IPs:
# export PFAMWEB_CONFIG=/nfs/team71/pfam/jt6/server/PfamConfigWTSI/pfamweb.conf

use strict;
use warnings;
    
use Test::More tests => 2150;
use Test::WWW::Mechanize::Catalyst 'PfamWeb';

use Config::General;     # configuration
use Cache::Memcached;    # cache control
 
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

# try a local configuration file
if( $ENV{PFAMWEB_CONFIG_LOCAL} ) {

  eval { $conf = new Config::General( $ENV{PFAMWEB_CONFIG_LOCAL} ) };
  die "error: problem parsing local configuration file:\n$@" if $@;

  my %local_config;
  eval { %local_config = $conf->getall };
  die "error: problem retrieving local configuration from file:\n$@" if $@;

  # merge the local config into the global one
  foreach my $key ( keys %local_config ) { 
    $config{$key} = $local_config{$key};
  }
}

#-------------------------------------------------------------------------------
# configure the wrapper for the cache servers

my $cache_server_ips = $config{cache}->{backends}->{default}->{servers};
my $cache = new Cache::Memcached( { servers => $cache_server_ips } );

#-------------------------------------------------------------------------------
# and onto the testing...

my $mech = Test::WWW::Mechanize::Catalyst->new();

for( 1 .. 50 ) {

  $cache->flush_all;
  
  # wait for the cache daemons to catch up...
  diag( 'cleared cache; waiting 2 seconds for daemons...' );
  sleep 2;
  
  # retrieve the page that will be cached on first load
  $mech->get_ok( '/',
                 'retrieve home page' );
  
  # store the content so that we can check the response from subsequent hits
  my $first_content = $mech->content;
  
  my( $server, $pid ) = $first_content =~ m{<!-- (.*?) / (\d+) -->};
  diag( "served by process $pid on $server" ) if( $server and $pid );
  
  # check the page content
  $mech->content_contains( 'static/javascripts/prototype.js',
                           'correct prototype URL' );

  $mech->content_contains( 'The Pfam database is a large collection',
                           'sensible content' );
  
  for( 1 .. 20 ) {
    
    $mech->get_ok( '/',
                   'retrieve (cached) home page' );
  
    $mech->content_is( $first_content,
                       'content matches' );

    sleep 1;

  }

}

