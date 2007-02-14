
# Users.pm
# jt6 20061207 WTSI
#
# Model for the users table.
#
# $Id: Users.pm,v 1.1 2007-02-14 11:00:17 jt6 Exp $

package WebUser::Users;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components(qw/Core/);

# set up the table
__PACKAGE__->table("users");

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ user password name / );

# set up the primary keys/contraints
__PACKAGE__->set_primary_key("user");

# no relationships

1;

