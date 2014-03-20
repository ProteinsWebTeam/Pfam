use strict;
use warnings;
use Test::More;


use Catalyst::Test 'XfamWeb';
use XfamWeb::Controller::About;

ok( request('/about')->is_success, 'Request should succeed' );
done_testing();
