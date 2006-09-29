
# ErrorLog.pm
# jt 20060912 WTSI

# DBIx::Class::ResultSource for the error_log table

# $Id: ErrorLog.pm,v 1.1.1.1 2006-09-29 09:56:35 jt6 Exp $

package PfamSchemata::WebUser::ErrorLog;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "error_log" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ message num first last / );

# set the the keys
__PACKAGE__->set_primary_key( "message" );

1;
