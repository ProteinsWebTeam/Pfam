
# $Id: Ncbi_seq.pm,v 1.1 2007-10-10 14:50:32 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::Ncbi_seq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# set up the table
__PACKAGE__->table( 'ncbi_seq' );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw( gi 
                              secondary_acc 
                              tertiary_acc 
                              md5 
                              description 
                              length
                              sequence ) );

# set the the keys
__PACKAGE__->set_primary_key( 'gi' );

# set up the relationships
__PACKAGE__->has_one ( ncbi_map => 'PfamDB::NCBI_map',
                       { 'foreign.gi' => 'self.gi' } );

__PACKAGE__->has_many( ncbi_pfama_reg => 'PfamDB::NCBI_pfama_reg',
                       { 'foreign.gi' => 'self.gi' } );

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
