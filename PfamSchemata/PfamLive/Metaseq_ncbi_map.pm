
# $Id: Metaseq_ncbi_map.pm,v 1.2 2008-06-02 10:48:17 rdf Exp $
#
# $Author: rdf $

package PfamLive::Metaseq_ncbi_map;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# set up the table
__PACKAGE__->table( 'metaseq_ncbi_map' );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw( auto_metaseq 
                              gi ) );

# set the the keys
__PACKAGE__->set_primary_key( qw( auto_metaseq gi ) );

# set up the relationships
__PACKAGE__->has_one( ncbi_seq => 'PfamLive::Ncbi_seq',
                       { 'foreign.auto_metaseq' => 'self.auto_metaseq' } );

__PACKAGE__->has_one( metaseq => 'PfamLive::Metaseq',
                       { 'foreign.auto_metaseq' => 'self.auto_metaseq' },
                       { 'proxy' => [ qw(metaseq_id metaseq_acc description)]} );

__PACKAGE__->might_have( ncbi_pfamA => 'PfamLive::Ncbi_pfama_reg',
                        { 'foreign.gi' => 'self.gi' },
                        { 'proxy' => [qw(seq_start 
                              seq_end 
                              model_start 
                              model_end
                              domain_bits_score 
                              domain_evalue_score 
                              sequence_bits_score 
                              sequence_evalue_score
                              mode )]} );


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
