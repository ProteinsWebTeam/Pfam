
# $Id: Interpro.pm,v 1.2 2007-03-08 14:16:22 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::Interpro;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "interpro" );
__PACKAGE__->add_columns( qw/auto_pfamA interpro_id abstract/ );
__PACKAGE__->set_primary_key( "auto_pfamA" );

__PACKAGE__->has_one( "pfamA" => "PfamLive::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA" },
		      { proxy => [ qw/ pfamA_acc pfamA_id description / ] } );
1;

