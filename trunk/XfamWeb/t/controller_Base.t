use strict;
use warnings;
use Test::More;


use Catalyst::Test 'XfamWeb';
use XfamWeb::Controller::Base;

ok( request('/base')->is_success, 'Request should succeed' );
done_testing();
