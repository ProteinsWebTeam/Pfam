
# 01main_page.t
# jt6 20071027 WTSI
#
# tests the domain graphics page fragment

use strict;
use warnings;

use Data::Dump qw( dump );
    
use Test::More tests => 3;
use Test::WWW::Mechanize::Catalyst 'PfamWeb';
use WWW::Mechanize::Link;

my $mech = Test::WWW::Mechanize::Catalyst->new();

$mech->get_ok( '/domaingraphics?acc=PF02171 ',
               'retrieve domain graphics page fragment' );

my @content = split /\n/, $mech->content;

my $rowCount = grep { /class="graphicRow/ } @content;
ok( $rowCount >= 1,
    'at least one domain graphic row' );

my @rows = grep { /with the following architecture:/ } @content;
my $numPiwis = grep { /Piwi/ } @rows;

ok( $numPiwis == scalar @rows, 
    'all architectures contain "Piwi"' );
