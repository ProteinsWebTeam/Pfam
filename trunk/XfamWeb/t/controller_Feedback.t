use strict;
use warnings;
use Test::More;


use Catalyst::Test 'XfamWeb';
use XfamWeb::Controller::Feedback;

ok( request('/feedback')->is_success, 'Request should succeed' );
done_testing();
