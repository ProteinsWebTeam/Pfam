
# $Id: Int_ext_links.pm,v 1.4 2007-03-16 11:25:17 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Int_ext_links;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("int_ext_links"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_int_ext_links auto_pfamaseq_A pfamseq_acc_A auto_pfamA_A pfamA_id_A auto_pfamseq_B pfamseq_acc_B auto_pfamA_B pfamA_id_B reference experiment_ref auto_int_pfamAs dataset/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key("auto_int_ext_links", "auto_pfamaseq_A", "pfamseq_acc_A", "auto_pfamA_A", "auto_pfamseq_B", "pfamseq_acc_B",  "auto_pfamA_B", "auto_int_pfamAs" );

#Set up relationships
#1 to many relationship

__PACKAGE__->has_one( "pfamA_A" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_A"});

__PACKAGE__->has_one( "pfamA_B" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_B"});

__PACKAGE__->has_one( "pfamseq_A" => "PfamDB::Pfamseq",
		      {"foreign.auto_pfamseq"  => "self.auto_pfamseq_A"});

__PACKAGE__->has_one( "pfamseq_B" => "PfamDB::Pfam",
		      {"foreign.auto_pfameq"  => "self.auto_pfamseq_B"});

__PACKAGE__->has_one( "int_pfamAs" => "PfamDB::IntPfamAs",
		      {"foreign.auto_int_pfamAs" => "self.auto_int_pfamAs"});

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

