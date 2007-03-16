
# $Id: PfamB2pfamA_PRC_results.pm,v 1.4 2007-03-16 11:25:15 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamB2pfamA_PRC_results;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("pfamB2pfamA_PRC_results"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_pfamB model_start1 model_end1 length1 align1 auto_pfamA model_start2 model_end2 length2 align2 evalue/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamB", "auto_pfamA");

#Set up relationships
#1 to many relationship
__PACKAGE__->has_one( "pfamA" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		      {proxy => [qw/pfamA_id pfamA_acc/]});

__PACKAGE__->has_one( "pfamB" => "PfamDB::PfamB",
		      {"foreign.auto_pfamB"  => "self.auto_pfamB"},
		      {proxy => [qw/pfamB_id pfamB_acc/]});

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

