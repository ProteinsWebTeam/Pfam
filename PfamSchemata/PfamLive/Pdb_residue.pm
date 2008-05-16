
# $Id: Pdb_residue.pm,v 1.3 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::Pdb_residue;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "pdb_residue_data" );
__PACKAGE__->add_columns( qw/ auto_pdb
							  msd_chain
							  chain
							  serial
							  pdb_res
							  pdb_seq_number
							  dssp_code
							  auto_pfamseq
							  pfamseq_res
							  pfamseq_seq_number / );

__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one( "pdb" => "PfamLive::Pdb",
					  { "foreign.auto_pdb" => "self.auto_pdb" },
					  { proxy => [ qw( pdb ) ] }
					);

__PACKAGE__->has_one( "pfamseq" => "PfamLive::Pfamseq",
					  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
					  { proxy => [ qw( pfamseq_id pfamseq_acc length) ] }
					);
					
__PACKAGE__->has_one( "pfam_anseq" => "PfamLive::Pfam_annseq",
					  { "foreign.auto_pfam" => "self.auto_pfam" },
					  { proxy => [ qw( pfamseq_storable ) ] }
					);

__PACKAGE__->might_have( pfamseqMarkup => "PfamLive::Pfamseq_markup",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq",
						   "foreign.residue"      => "self.pfamseq_seq_number" },
						 { proxy => [ qw( annotation auto_markup ) ] } );
						 
__PACKAGE__->might_have( pfamA_reg_full_significant => "PfamLive::PfamA_reg_full_significant",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq"});

__PACKAGE__->might_have( pfamA_reg_seed => "PfamLive::PfamA_reg_seed",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq"});

__PACKAGE__->might_have( pfamB_reg => "PfamLive::PfamB_reg",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq"});
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;


