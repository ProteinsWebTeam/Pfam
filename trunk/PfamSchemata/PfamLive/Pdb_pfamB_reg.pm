
# $Id: Pdb_pfamB_reg.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::Pdb_pfamB_reg;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_pfamB_reg" );
__PACKAGE__->add_columns( qw/auto_pdb_reg auto_pfamB_reg auto_pdb auto_pfamB auto_pfamseq chain pdb_res_start pdb_res_end seq_start seq_end/ );
__PACKAGE__->set_primary_key( "auto_pdb_reg" );

__PACKAGE__->has_one( "pfamB" => "PfamLive::PfamB",
		      { "foreign.auto_pfamB" => "self.auto_pfamB" },
		       { proxy => [ qw/pfamB_id pfamB_acc/ ] } );

__PACKAGE__->has_one( "pdb"  => "PfamLive::Pdb",
		      { "foreign.auto_pdb"   => "self.auto_pdb" },
		      { proxy => [ qw/pdb_id header title pdb_image/ ] } );

__PACKAGE__->has_one( "pfamseq" => "PfamLive::Pfamseq",
		      { "foreign.auto_pfamseq" => "self.auto_pfamseq"},
		      { proxy => [ qw/pfamseq_id pfamseq_acc genome_seq ncbi_code/]});

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


