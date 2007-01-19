
# WebUser.pm
# jt 20060912 WTSI

# The base class for the web_user tables

# $Id: WebUser.pm,v 1.3 2007-01-19 16:41:31 jt6 Exp $

package WebUser;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes( qw/ Das_sources  ErrorLog  Family_count  News  Users / );

1;

