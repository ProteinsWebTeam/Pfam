
# $Id: Dead_families.pm,v 1.3 2007-03-08 14:16:30 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Dead_families;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "dead_families" );

#Add the columns
__PACKAGE__->add_columns(qw/pfamA_acc pfamA_id comment forward_to/);

#Set up the primary key
__PACKAGE__->set_primary_key("pfamA_acc");

#In theory the forward_to could join onto self or pfamA...... -todo

1;
