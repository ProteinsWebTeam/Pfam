
# $Id: Meta_pfama_reg.pm,v 1.4 2008-06-03 13:56:48 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::Meta_pfama_reg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# set up the table
__PACKAGE__->table( 'meta_pfamA_reg' );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw( auto_meta_pfamA_reg
                              auto_pfamA
                              auto_metaseq
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

# set the the keys
__PACKAGE__->set_primary_key( qw( auto_pfamA
                                  auto_metaseq ) );

# set up the relationships
__PACKAGE__->has_one( metaseq => 'PfamDB::Metaseq',
                      { 'foreign.auto_metaseq' => 'self.auto_metaseq' } );

__PACKAGE__->has_one( pfamA => 'PfamDB::Pfam',
                      { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                      { proxy => [ qw( pfamA_acc 
                                       pfamA_id 
                                       description
                                       model_length
                                       type ) ] } );

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
