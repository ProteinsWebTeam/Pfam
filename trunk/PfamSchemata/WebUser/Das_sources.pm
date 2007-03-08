
# Das_sources.pm
# jt6 20060428 WTSI
#
# Model for the das_sources table.
#
# $Id: Das_sources.pm,v 1.3 2007-03-08 14:16:31 jt6 Exp $
#
# $Author: jt6 $

package WebUser::Das_sources;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "das_sources" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ server_id name url system helper_url default_server /);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( "server_id" );

# no relationships


1;
