
# $Id: Ligands.pm,v 1.4 2007-03-16 11:25:18 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Ligands;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "ligands" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/ligand_id code three_letter_code one_letter_code name systematic_name num_atoms_all num_atoms_no_h stereo_smiles non_stereo_smiles charge category formula molecular_weight/);

__PACKAGE__->set_primary_key( "ligand_id", "code" );
#__PACKAGE__->might_have ( "synonym" 

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

