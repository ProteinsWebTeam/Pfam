
# $Id: PfamA_reg_full_significant.pm,v 1.5 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::PfamA_reg_full_significant;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( 'Core' );

#Set up the table
__PACKAGE__->table( 'pfamA_reg_full_significant' );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw( auto_pfamA_reg_full 
                              auto_pfamseq 
                              auto_pfamA 
                              seq_start 
                              seq_end 
                              model_start 
                              model_end 
                              domain_bits_score 
                              domain_evalue_score 
                              sequence_bits_score 
                              sequence_evalue_score 
                              mode 
                              cigar 
                              in_full 
                              tree_order ) );

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key( qw( auto_pfamA_reg_full
                                  auto_pfamA
                                  auto_pfamseq ) );

#Now setup the relationship
__PACKAGE__->has_one( pfamA =>  'PfamLive::Pfam',
                      { 'foreign.auto_pfamA'  => 'self.auto_pfamA' },
                      { proxy => [ qw( pfamA_id 
                                       pfamA_acc 
                                       description 
                                       model_length 
                                       type 
                                       version ) ] } );

__PACKAGE__->has_one( pfamseq =>  'PfamLive::Pfamseq',
                      { 'foreign.auto_pfamseq'  => 'self.auto_pfamseq' },
                      { proxy => [ qw( pfamseq_acc 
                                       pfamseq_id
                                       seq_version
                                       crc64 
                                       md5 
                                       description
                                       length
                                       species 
                                       taxonomy
                                       ncbi_code 
                                       sequence
                                       updated
                                       created ) ] } );
          
__PACKAGE__->has_one( pfam_annseq =>  'PfamLive::Pfam_annseq',
                      { 'foreign.auto_pfamseq'  => 'self.auto_pfamseq' },
                      { proxy => [ qw( annseq_storable ) ] } );


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

