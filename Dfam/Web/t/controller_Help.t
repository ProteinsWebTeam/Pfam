use strict;
use warnings;
use Test::More;

BEGIN { use_ok 'Catalyst::Test', 'DfamWeb' }
BEGIN { use_ok 'DfamWeb::Controller::Help' }

ok( request('/help')->is_success, 'Request should succeed' );
done_testing();
