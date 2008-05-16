
# $Id: Literature_references.pm,v 1.4 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::Literature_references;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("literature_references"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_lit medline title author journal/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_lit" );

#Set up relationships

#1 to many relationship


__PACKAGE__->has_one( "clan_lit_refs" => "PfamLive::Clan_lit_refs",
		       {"foreign.auto_lit"  => "self.auto_lit"});

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

