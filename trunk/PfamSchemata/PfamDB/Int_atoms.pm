
# $Id: Int_atoms.pm,v 1.4 2007-03-16 11:25:16 jt6 Exp $
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

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;

