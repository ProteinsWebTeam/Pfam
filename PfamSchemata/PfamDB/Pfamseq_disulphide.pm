
# $Id: Pfamseq_disulphide.pm,v 1.3 2007-03-08 14:16:27 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Pfamseq_disulphide;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamseq_disulphide" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq bond_start bond_end/);

__PACKAGE__->set_primary_key("auto_pfamseq");

__PACKAGE__->has_one("auto_pfamseq" => "PfamDB::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/pfamseq_id pfamseq_acc/]});



1;
