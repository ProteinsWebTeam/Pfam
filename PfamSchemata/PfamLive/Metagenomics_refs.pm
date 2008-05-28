
# $Id: Metagenomics_refs.pm,v 1.1 2008-05-28 10:21:29 rdf Exp $
#
# $Author: rdf $

package PfamLive::Metagenomics_refs;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( qw( Core ) );

# set up the table
__PACKAGE__->table( 'metagenomics_refs' );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw( auto_meta_ref 
                              source 
                              long_source 
                              pmid ) );

# set the the keys
__PACKAGE__->set_primary_key( qw( auto_meta_ref source ) );

# set up the relationships
__PACKAGE__->has_many( metaseq => 'PfamLive::Metaseq',
                       { 'foreign.auto_metaseq' => 'self.auto_metaseq' } );

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
