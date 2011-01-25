#!/usr/bin/perl

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

#-------------------------------------------------------------------------------

# retrieve the mapping(s)

# get the URL for the CGI script that distributes the mapping
my $mapping_url = $config{mapping_url};

# get a user agent and actually retrieve the mapping(s)
my $ua = LWP::UserAgent->new;
$ua->env_proxy;

# the configuration will specify whether to retrieve just Pfam, just Rfam or
# both sets of mappings. This is for the benefit of mirrors, which may be 
# running just one of the sites, rather than both.
my $mapping = {};
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

  # merge the mapping from this database into the global one
  $mapping->{$_} = $db_mapping->{$_} for keys %$db_mapping;
}

$log->info( 'got ' . scalar( keys %$mapping ) . ' accessions in final mapping' );

#-------------------------------------------------------------------------------

foreach my $acc ( sort keys %$mapping ) {

  $log->debug( "looking for |$acc|..." );

  my $rows = $schema->resultset('ArticleMapping')
                    ->search( { accession => $acc }, {} );

  $log->debug( '  found ' . $rows->count . ' rows in old mapping' );
  $log->debug( '  found ' . scalar @{ $mapping->{$acc} } . ' rows in new mapping' );

  # if new mapping has multiple titles for a single accession, we need to
  # delete the old mapping and create it afresh, otherwise we could end up with
  # a stale article mapped to a family
  if ( scalar @{ $mapping->{$acc} } > 1 ) {
    $rows->delete;
    $rows->create( { accession => $acc,
                     title     => $_ },
                   { key => 'primary' } ) for @{ $mapping->{$acc} };
  }
  # if there's just a single old and new mapping, we can just update
  else {
    $rows->update_or_create( { accession => $acc,
                               title     => $mapping->{$acc}->[0] },
                             { key => 'primary' } );
  }
}

