
# 01main_page.t
# jt6 20071027 WTSI
#
# tests the main body of the family page

# needs to know the location of the configuration file for the server, so that
# it can retrieve database connection parameters
# export PFAMWEB_CONFIG=/nfs/team71/pfam/jt6/server/PfamConfigWTSI/pfamweb.conf 

use strict;
use warnings;
    
use PfamDB;          # database connections
use Config::General; # configuration

use Test::More; # plan later
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
# data for the Piwi family

my $db_info = $config{Model}->{PfamDB}->{connect_info};
my $schema = PfamDB->connect( @$db_info );
my $rs = $schema->resultset( 'Pfam' )
                ->search( { pfamA_acc => 'PF02171' } );

my $piwi_data = $rs->first;

# if we got the data, write the plan, otherwise we're done here
if( defined $piwi_data ) {
  plan( tests => 6 );
} else {
  plan( skip_all => "error: couldn't retrieve data for Piwi (PF02171)" );
}

#-------------------------------------------------------------------------------
# and onto the testing...

my $mech = Test::WWW::Mechanize::Catalyst->new();

$mech->get_ok( '/family?acc=PF02171',
               'retrieve page for Piwi (PF02171)' );
  
my( $server, $pid ) = $mech->content =~ m{<!-- (.*?) / (\d+) -->};
diag( "served by process $pid on $server" ) if( $server and $pid );

$mech->title_is( 'Pfam: Family: Piwi (PF02171)',
                 'title as expected' );

$mech->content_contains( 'static/javascripts/prototype.js',
                         'correct prototype URL' );

$mech->content_contains( '<em>' . $piwi_data->num_full . '</em>&nbsp;sequences',
                         'number of sequences in summary' );

$mech->content_lacks( 'BlockSelector" class="inactive"',
                      'all tabs active' );

SKIP:
{
  eval 'use Test::HTML::Tidy';

  skip( 'HTML validation; Test::HTML::Tidy unavailable', 1 ) if $@;

  html_tidy_ok( 'HTML is valid' );
}
