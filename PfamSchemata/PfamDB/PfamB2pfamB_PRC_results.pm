
# $Id: PfamB2pfamB_PRC_results.pm,v 1.4 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::PfamB2pfamB_PRC_results;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/);

__PACKAGE__->table("pfamB2pfamB_PRC_results");

__PACKAGE__->add_columns( qw/auto_pfamB1 model_start1 model_end1 length1 align1
							 auto_pfamB2 model_start2 model_end2 length2 align2 /);

# set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamB1", "auto_pfamB2");

# set up relationships
# 1 to many relationship
__PACKAGE__->has_one( "pfamB1" => "PfamDB::PfamB",
					  { "foreign.auto_pfamB" => "self.auto_pfamB1" } );

__PACKAGE__->has_one( "pfamB2" => "PfamDB::PfamB",
					  { "foreign.auto_pfamB" => "self.auto_pfamB2" } );

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

