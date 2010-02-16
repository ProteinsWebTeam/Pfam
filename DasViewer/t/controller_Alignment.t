use strict;
use warnings;
use Test::More tests => 3;

BEGIN { use_ok 'Catalyst::Test', 'PfamViewer' }
BEGIN { use_ok 'PfamViewer::Controller::Alignment' }

ok( request('/alignment')->is_success, 'Request should succeed' );


