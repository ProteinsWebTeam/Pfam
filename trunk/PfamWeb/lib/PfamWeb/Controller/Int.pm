
# Int.pm
# jt6 20060411 WTSI
#
# Controller to build the main iPfam page.
#
# $Id: Int.pm,v 1.3 2006-09-28 14:41:28 jt6 Exp $

package PfamWeb::Controller::Int;

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Section";

__PACKAGE__->config( SECTION => "ipfam" );

1;
