
# $Id: PfamA_literature_references.pm,v 1.5 2008-07-22 15:28:37 jm14 Exp $
#
# $Author: jm14 $
package PfamLive::PfamA_literature_references;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("pfamA_literature_references"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_pfamA auto_lit order_added comment/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamA", "auto_lit" );

#Set up relationships
#1 to many relationship

__PACKAGE__->has_one( "pfamA" => "PfamLive::Pfam",
		       {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		       {proxy => [qw/pfamA_id pfamA_acc/]});


__PACKAGE__->has_one( "literature" => "PfamLive::Literature_references",
		       {"foreign.auto_lit"  => "self.auto_lit"},
		       {proxy => [qw/medline title author journal pmid/]});

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

