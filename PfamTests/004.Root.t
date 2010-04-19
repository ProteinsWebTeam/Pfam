use strict;
use warnings;

use Test::More tests => 9;
use Test::Warn;
use Test::Exception;

my $m = 'Bio::Pfam::Root';

use_ok( $m );
can_ok( $m, 'new');

my $root = Bio::Pfam::Root->new;
isa_ok($root, $m);

$root->verbose("-1");
warnings_like(sub { $root->warn('Test warn') }, undef, "Passed test warnings, level -1");

$root->verbose(0);
warnings_like(sub { $root->warn('Test warn') }, qr/Test warn|WARNING/, "Passed test warnings, level 0");

$root->verbose(1);
warnings_like(sub { $root->warn('Test warn') }, qr/Test warn|WARNING/, "Passed test warnings, level 1");

$root->verbose(2);
dies_ok(sub{ $root->warn('Test warn') }, "Passed test warnings, level 2");

$root->verbose(0);
dies_ok(sub{ $root->throw('Test warn') }, "Throws exception okay");

my $before = ["-colour1", "red",  "-colour2" , "white", "-colour3", "blue" ];
my $after  = [ qw(white blue red) ];
my @reArranged = $root->_rearrange([qw(colour2 colour3 colour1)], @{$before} );
is_deeply(\@reArranged, $after, 'Passed _rearrange' );