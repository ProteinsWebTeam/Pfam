
# WebUser.pm
# jt 20060912 WTSI

# The base class for the web_user tables

# $Id: WebUser.pm,v 1.1 2006-09-13 08:36:21 jt6 Exp $

package PfamWeb::Schema::WebUser;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes();

1;

