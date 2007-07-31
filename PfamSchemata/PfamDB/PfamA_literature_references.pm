
# $Id: PfamA_literature_references.pm,v 1.5 2007-07-31 13:07:00 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamA_literature_references;

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

__PACKAGE__->has_one( "pfamA" => "PfamDB::Pfam",
		       {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		       {proxy => [qw/pfamA_id pfamA_acc/]});


__PACKAGE__->has_one( "literature" => "PfamDB::Literature_references",
		       {"foreign.auto_lit"  => "self.auto_lit"},
		       {proxy => [qw/medline pmid title author journal/]});


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

