use strict;
use warnings;

use Test::More tests => 2;
use Test::Exception;
use Test::Cmd;


my $test = Test::Cmd->new(prog => $ENV{HOME}."/Work/PfamCodeBase/PfamH3Scripts/make/pfmake.pl", workdir => '');
ok($test, "creating Test::Cmd object");

my $res = $test->run(args => '-h');

print $res."\n";
print $?."\n";
$test->fail($? != 0);

ok($? == 256, "executing test_program -h");


#require_ok( $ENV{HOME}."/Work/PfamCodeBase/PfamH3Scripts/make/pfmake.pl" );

#push(@ARGV, "-h");
#exits_ok(sub{ main(@ARGV) }, 'help okay');

