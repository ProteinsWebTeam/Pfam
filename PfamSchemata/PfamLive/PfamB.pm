
# $Id: PfamB.pm,v 1.3 2008-07-12 20:14:11 rdf Exp $
#
# $Author: rdf $
package PfamLive::PfamB;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# the table
__PACKAGE__->table( 'pfamB' );

# columns that we want to keep
__PACKAGE__->add_columns( qw( auto_pfamB 
                              pfamB_acc
                              pfamB_id
                              number_archs
                              number_regions
                              number_species
                              number_structures ) );

# primary keys
__PACKAGE__->set_primary_key( qw( auto_pfamB pfamB_acc ) );

# relationships 

# pfamB joins are to pfamB_reg, pdbmap & pfamB_stockholm 

__PACKAGE__->has_many( pfamb_reg => 'PfamLive::PfamB_reg',
                       { 'foreign.auto_pfamB' => 'self.auto_pfamB' } );

__PACKAGE__->has_one( pfamB_stockholm => 'PfamLive::PfamB_stockholm',
                      { 'foreign.auto_pfamB'  => 'self.auto_pfamB' },
                      { proxy => [ qw( stockholm_data jtml ) ] } );

__PACKAGE__->has_many( pfamB_database_links => 'PfamLive::PfamB_database_links',
                       { 'foreign.auto_pfamB' => 'self.auto_pfamB' } );

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

