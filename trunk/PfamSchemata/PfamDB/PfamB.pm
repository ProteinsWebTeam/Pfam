
# $Id: PfamB.pm,v 1.5 2007-03-16 11:25:16 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamB;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamB" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamB pfamB_acc pfamB_id/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamB", "pfamB_acc");

#Now setup the relationship 

#PfamB joins are to pfamB_reg, pdbmap & pfamB_stockholm 

__PACKAGE__->has_many( pfamb_reg => "PfamDB::PfamB_reg",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" } );

__PACKAGE__->has_many( pdbMap => "PfamDB::PdbMap",
			  { "foreign.auto_pfam"  => "self.auto_pfamB" },
			  { proxy => [ qw/pdb_id/ ] } );

__PACKAGE__->has_one( pfamB_stockholm => "PfamDB::PfamB_stockholm",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" },
		      { proxy => qw[/stockholm_data/] } );

__PACKAGE__->has_many( pfamB_database_links => "PfamDB::PfamB_database_links",
					   { "foreign.auto_pfamB" => "self.auto_pfamB" } );


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

