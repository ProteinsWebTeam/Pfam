
# 01main_page.t
# jt6 20071027 WTSI
#
# tests the structure image page fragment

# needs to know the location of the configuration file for the server, so that
# it can retrieve database connection parameters
# export PFAMWEB_CONFIG=/nfs/team71/pfam/jt6/server/PfamConfigWTSI/pfamweb.conf 

use strict;
use warnings;
    
use PfamDB;          # database connections
use Config::General; # configuration

use Test::More tests => 3;
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

# get the row for Piwi
my $rs = $schema->resultset( 'Pfam' )
                ->search( { pfamA_acc => 'PF02171' } );
my $piwi_data = $rs->first;

# retrieve the PDB entries for Piwi
my @rs = $schema->resultset( 'Pdb_pfamA_reg' )
                ->search( { auto_pfamA => $piwi_data->auto_pfamA },
                          { join       => [ qw( pdb ) ],
                            prefetch   => [ qw( pdb ) ] } );
my %pdbUnique = map{ $_->pdb_id => $_ } @rs;

#-------------------------------------------------------------------------------

my $mech = Test::WWW::Mechanize::Catalyst->new();

$mech->get_ok( '/family/structures?acc=PF02171 ',
               'retrieve structure image page fragment' );

$mech->content_contains( 'pdb image block',
                         'sensible content' );

SKIP:
{
  skip( 'because we failed to get the number of structures from the DB', 1 )
    unless scalar( keys %pdbUnique );

  # how many "<option>" tags in the page fragment ?
  my $options = grep { /<option/ } split /\n/, $mech->content;

  ok( scalar keys %pdbUnique == $options,
      'number of <option> tags matches number of structures' ); 
}

