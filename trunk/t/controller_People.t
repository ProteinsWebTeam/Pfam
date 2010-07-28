use strict;
use warnings;
use Test::More;

BEGIN { use_ok 'Catalyst::Test', 'WikiApp' }
BEGIN { use_ok 'WikiApp::Controller::People' }

ok( request('/people')->is_success, 'Request should succeed' );
done_testing();
