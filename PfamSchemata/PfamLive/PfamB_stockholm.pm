
# $Id: PfamB_stockholm.pm,v 1.1 2008-05-02 09:43:01 rdf Exp $
#
# $Author: rdf $
package PfamLive::PfamB_stockholm;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# the table
__PACKAGE__->table( 'pfamB_stockholm' );

# columns that we want to keep
__PACKAGE__->add_columns( qw( auto_pfamB 
                              stockholm_data
                              jtml ) );

# primary keys
__PACKAGE__->set_primary_key( 'auto_pfamB' );

# relationships 

# pfamB_stockholm joins are to pfamB

__PACKAGE__->has_one( pfamB => 'PfamLive::PfamB',
            		      { 'foreign.auto_pfamB' => 'self.auto_pfamB' },
            		      { proxy => [ qw( pfamB_id pfamB_acc ) ] } );

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

