#!/usr/bin/env perl

# This script downloads the mapping between families and articles and updates
# the "web_user.article_mapping" table.
#
# The mapping is retrieved as a JSON string from:
#
#   http://pfamsrv.sanger.ac.uk/cgi-bin/mapping.cgi
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
use Data::Dump qw(dump);
use Log::Log4perl qw(get_logger :levels);

use WebUser;

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

# setup 

# find the config file
my $config_file = 'conf/wiki.conf';
GetOptions ( 'config=s' => \$config_file );
$log->logdie( "ERROR: couldn't read config from '$config_file': $!" )
  unless -e $config_file;

# parse the config and get the section relevant to the web_user database
my $cg = Config::General->new($config_file);
my %config  = $cg->getall;
my $db_conf = $config{WebUser};

# get a DBIC database connection object
my $schema = WebUser->connect( 
  "dbi:mysql:$db_conf->{db_name}:$db_conf->{db_host}:$db_conf->{db_port}",
  $db_conf->{username}, $db_conf->{password}
);

# $schema->storage()->debug( 1 );

# get the URL for the CGI script that distributes the mapping
my $mapping_url = $config{mapping_url};

#-------------------------------------------------------------------------------

# get a user agent and retrieve the mapping(s)
my $ua = LWP::UserAgent->new;
$ua->env_proxy;

# the configuration will specify whether to retrieve just Pfam, just Rfam or
# both sets of mappings. This is for the benefit of mirrors, which may be 
# running just one of the sites, rather than both.
my $new_mapping = {};
foreach my $db ( keys( %{$config{mappings}} ) ) {

  my $response = $ua->post( $mapping_url, { db => $db } );

  my $db_mapping = {};
  if ( $response->is_success ) {
    $db_mapping = decode_json( $response->decoded_content );
  }
  else {
    $log->logdie( 'ERROR: failed to retrieve mapping: '. $response->status_line );
  }

  $log->info( 'retrieved ' . scalar( keys %$db_mapping ) 
              . " article-to-entry rows for db $db" );

  foreach my $acc ( keys %$db_mapping ) {
    
    # clear out the old mapping
    my $rv = $schema->resultset('ArticleMapping')
                    ->search( { accession => $acc }, { } )
                    ->delete;

    $log->logwarn( "warning: failed to delete old mapping for '$acc'" )
      unless $rv > 0;

    # and add the new one
    foreach my $title ( @{ $db_mapping->{$acc} } ) {
      $schema->resultset('ArticleMapping')
             ->create( { accession => $acc,
                         title     => $title },
                       { key => 'primary' } )
        or $log->logwarn( "warning: failed to insert new mapping row for '$acc'" );
    }
  }
}

#-------------------------------------------------------------------------------

$log->info( 'done' );

