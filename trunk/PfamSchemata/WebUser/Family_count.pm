
# $Id: Family_count.pm,v 1.2 2007-03-08 14:16:31 jt6 Exp $
#
# $Author: jt6 $

package WebUser::Family_count;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "family_count" );

__PACKAGE__->add_columns( qw/ auto_pfamA
															pfamA_id
															pfamA_acc
															view_count / );

__PACKAGE__->set_primary_key( qw/auto_pfamA/);

1;
