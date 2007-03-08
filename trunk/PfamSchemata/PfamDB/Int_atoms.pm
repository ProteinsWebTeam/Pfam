
# $Id: Int_atoms.pm,v 1.3 2007-03-08 14:16:24 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Int_atoms;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("int_atoms"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_int_atoms pdb_atom partner_pdb_atom auto_int_bonds/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_int_atoms", "auto_int_bonds");

#Set up relationships
#1 to many relationship

__PACKAGE__->has_many( "interactions" => "PfamDB::Interactions",
		      {"foreign.auto_int_atoms"  => "self.auto_int_atoms"});

__PACKAGE__->has_one( "bond" => "PfamDB::Int_bonds",
		      {"foreign.auto_int_bonds"  => "self.auto_int_bonds"},
		      {proxy  => [qw/bond_name/]});

1;
