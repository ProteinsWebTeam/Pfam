use strict;
use warnings;

use XfamWeb;

my $app = XfamWeb->apply_default_middlewares(XfamWeb->psgi_app);
$app;

