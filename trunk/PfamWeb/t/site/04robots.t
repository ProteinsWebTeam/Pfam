
# 004robots.t
# jt6 20071030 WTSI
#
# make sure we get a sensible robots.txt

use strict;
use warnings;

use Test::More tests => 3;
use Test::WWW::Mechanize::Catalyst 'PfamWeb';

my $mech = Test::WWW::Mechanize::Catalyst->new();

$mech->get_ok( '/robots.txt',
               'can retrieve robots.txt' );

$mech->content_contains( 'User-agent:',
                         'sensible content' );
$mech->content_contains( 'Disallow:',
                         'at least one "disallow" rule' );
                         