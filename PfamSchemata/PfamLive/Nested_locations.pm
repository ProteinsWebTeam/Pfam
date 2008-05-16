
# $Id: Nested_locations.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::Nested_locations;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# the table
__PACKAGE__->table( qq( nested_locations ) );

# the columns
__PACKAGE__->add_columns( qw( auto_pfamA
                              nested_auto_pfamA
                              nested_pfamA_acc
                              pfamseq_acc
                              seq_version
                              seq_start
                              seq_end
                               ) );

# keys
__PACKAGE__->set_primary_key( qw( auto_pfamA 
                                  ) );

# relationships

__PACKAGE__->has_one( pfam => 'PfamLive::Pfam',
                      { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                      { proxy => [qw(pfamA_id pfamA_acc)]} );

__PACKAGE__->has_one( nestedPfam => 'PfamLive::Pfam',
                      { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                      { proxy => [qw(pfamA_id pfamA_acc)]} );

__PACKAGE__->has_one( pfamseq => 'PfamLive::Pfamseq',
                      { 'foreign.pfamseq_acc' => 'self.pfamseq_acc' },
                      { proxy => [qw(auto_pfamseq pfamseq_id pfamseq_acc)]} );
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

