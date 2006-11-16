
# PfamLive.pm
# jt 20061109 WTSI

# The base class for the whole database model. Config comes from the
# catalyst application class.

# $Id: PfamLive.pm,v 1.1 2006-11-16 11:07:27 jt6 Exp $

package PfamLive;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes();

1;
