use strict;
use warnings;
use Test::More tests => 3;

BEGIN { use_ok 'Catalyst::Test', 'DasViewer' }
BEGIN { use_ok 'DasViewer::Controller::Alignment' }

ok( request('/alignment')->is_success, 'Request should succeed' );


