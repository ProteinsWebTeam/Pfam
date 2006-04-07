use strict;
use warnings;
use Test::More "no_plan"; #tests => 2;

BEGIN { use_ok 'Catalyst::Test', 'PfamWeb' }

ok( request('/')->is_success, 'Request should succeed' );
ok( request('/summary/PF00517')->is_success, 'Family page should load' );
