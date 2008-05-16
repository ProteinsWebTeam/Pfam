
# $Id: Pdb_pfamA_reg.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::Pdb_pfamA_reg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );
__PACKAGE__->table( 'pdb_pfamA_reg' );

__PACKAGE__->add_columns( qw( auto_pdb_reg 
                              auto_pfamA_reg_full 
                              auto_pdb 
                              auto_pfamA 
                              auto_pfamseq 
                              chain 
                              pdb_res_start 
                              pdb_res_end 
                              seq_start 
                              seq_end
                              hex_colour ) );

__PACKAGE__->set_primary_key( 'auto_pdb_reg' );

__PACKAGE__->has_one( pfamA => 'PfamLive::Pfam',
                      { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                      { proxy => [ qw( pfamA_id pfamA_acc description ) ] } );

__PACKAGE__->has_one( pdb  => 'PfamLive::Pdb',
                      { 'foreign.auto_pdb' => 'self.auto_pdb' },
                      { proxy => [ qw( pdb_id header title pdb_image ) ] } );

__PACKAGE__->has_one( pfamseq => 'PfamLive::Pfamseq',
                      { 'foreign.auto_pfamseq' => 'self.auto_pfamseq' },
                      { proxy => [ qw( pfamseq_id pfamseq_acc genome_seq ncbi_code ) ] } ) ;

__PACKAGE__->has_one( clanMembers => 'PfamLive::Clan_membership',
                      { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

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


