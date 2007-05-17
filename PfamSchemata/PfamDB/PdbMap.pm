
# $Id: PdbMap.pm,v 1.5 2007-05-17 08:41:02 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::PdbMap;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdbmap" );
__PACKAGE__->add_columns( qw/auto_pdb auto_pfam auto_pfamseq chain pdb_start_res pdb_end_res
							 pfam_start_res pfam_end_res hex_colour pfam_region/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one( "pfamA" => "PfamDB::Pfam",
		      { "foreign.auto_pfamA" => "self.auto_pfam" },
		       { proxy => [ qw/pfamA_id pfamA_acc description/ ] } );

__PACKAGE__->has_one( "pfamB" => "PfamDB::PfamB",
		      { "foreign.auto_pfamB" => "self.auto_pfam" } );

__PACKAGE__->has_one( "pdb"  => "PfamDB::Pdb",
		      { "foreign.auto_pdb"   => "self.auto_pdb" },
		      { proxy => [ qw/pdb_id header title pdb_image/ ] } );

__PACKAGE__->has_one( "pfamseq" => "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq" => "self.auto_pfamseq"},
		      { proxy => [ qw/pfamseq_id pfamseq_acc ncbi_code species/]});

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


