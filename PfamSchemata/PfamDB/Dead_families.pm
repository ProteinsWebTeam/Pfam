
# $Id: Dead_families.pm,v 1.6 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Dead_families;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( 'Core' );

#Set up the table
__PACKAGE__->table( 'dead_families' );

#Add the columns
__PACKAGE__->add_columns(qw( pfamA_acc 
                             pfamA_id 
                             comment 
                             forward_to ) );

#Set up the primary key
__PACKAGE__->set_primary_key('pfamA_acc');

#In theory the forward_to could join onto self or pfamA...... -todo
# something like this...
#__PACKAGE__->has_one( pfamA => 'PfamDB::Pfam',
#                      { 'foreign.pfamA_acc' => 'self.forward_to' },
#                      {  proxy => [ qw( pfamA_id ) ] } );

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

