#!/software/bin/perl

use strict;
use warnings;

use Storable qw( thaw );
use Data::Dump qw( dump );

use WebUser;

my $id = $ARGV[0] || 1403043;

$ENV{DBIC_TRACE} = 1;

my $schema = WebUser->connect( "dbi:mysql:web_user:pfamdb2a:3301",
                               "pfam",
                               "mafp1" );
$schema->storage()->debug( 1 );


my $rs = $schema->resultset( "JobStream" )
                ->search( { id => $id },
                          {} )
                ->single;

my $storable = $rs->get_column( 'stdout' );
my $data     = thaw( $storable );

print dump( $data ) . "\n";

