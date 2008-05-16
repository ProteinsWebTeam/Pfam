
# $Id: PfamB_fasta.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::PfamB_fasta;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# the table
__PACKAGE__->table( qq( pfamB_fasta ) );

# the columns
__PACKAGE__->add_columns( qw( auto_pfamB
                              fasta 
                              nr_threshold 
                               ) );

# keys
__PACKAGE__->set_primary_key( qw( auto_pfamB
                                  ) );

# relationships

__PACKAGE__->has_one( pfam => 'PfamLive::Pfam',
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

