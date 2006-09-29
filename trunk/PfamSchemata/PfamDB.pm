
# BaseModel.pm
# jt 20060316 WTSI

# The base class for the whole database model. Config comes from the
# catalyst application class.

# $Id: PfamDB.pm,v 1.2 2006-09-29 15:23:22 jt6 Exp $

package PfamDB;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes();

1;
