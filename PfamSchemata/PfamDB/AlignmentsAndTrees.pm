
# $Id: AlignmentsAndTrees.pm,v 1.2 2007-08-02 16:05:44 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::AlignmentsAndTrees;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# the table
__PACKAGE__->table( qq( alignments_and_trees ) );

# the columns
__PACKAGE__->add_columns( qw( auto_pfamA
                              alignment
                              tree
                              jtml
                              type ) );

# keys
__PACKAGE__->set_primary_key( qw( auto_pfamA 
                                  type ) );

# relationships

__PACKAGE__->has_one( pfam => 'PfamDB::Pfam',
                      { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

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

