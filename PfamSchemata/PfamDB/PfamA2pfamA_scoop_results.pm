
# $Id: PfamA2pfamA_scoop_results.pm,v 1.3 2007-03-16 11:25:15 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamA2pfamA_scoop_results;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("pfamA2pfamA_scoop_results"); # This is how we define the table
__PACKAGE__->add_columns( qw/ auto_pfamA1 auto_pfamA2 score /); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamA1", "auto_pfamA2");

#Set up relationships
#1 to many relationship
__PACKAGE__->has_one( "pfamA1" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA1"},);
		      #{proxy => [qw/pfamA_id pfamA_acc/]});

__PACKAGE__->has_one( "pfamA2" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA2"},);
		      #{proxy => [qw/pfamA_id pfamA_acc/]});



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

