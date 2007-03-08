
# $Id: Interpro.pm,v 1.3 2007-03-08 14:16:24 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::Interpro;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "interpro" );
__PACKAGE__->add_columns( qw/auto_pfamA interpro_id abstract/ );
__PACKAGE__->set_primary_key( "auto_pfamA" );

__PACKAGE__->has_one( "pfamA" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA" },
		      { proxy => [ qw/ pfamA_acc pfamA_id description / ] } );
1;

