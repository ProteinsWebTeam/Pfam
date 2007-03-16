
# $Id: Clan_membership.pm,v 1.4 2007-03-16 11:25:19 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::Clan_membership;

use strict;
use warnings;

use base "DBIx::Class";

#This table links the clans table to pfamAs;  There are many pfamAs to a single clans 

__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clan_membership"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_pfamA auto_clan/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_pfamA" );

# For UNIQUE (auto_pfamA);
#__PACKAGE__->add_unique_constraint(constraint_name => [ qw/auto_pfamA/ ]);

#Set up relationships


#1 to 1 relationship
__PACKAGE__->has_one( "pfam" => "PfamLive::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		        {proxy => [qw/ pfamA_id pfamA_acc num_seed num_full model_length /],
		         cascade_delete => 0});

#Not sure about this one.... there will be many of the same auto_clan in this table,
#but only one in the clans table, another one for jt6
__PACKAGE__->has_one( "clans" => "PfamLive::Clans",
		      {"foreign.auto_clan" => "self.auto_clan"},
		      {proxy => [qw/clan_acc clan_id clan_description/],
		       cascade_delete => 0});


__PACKAGE__->has_many( "pfamARegFull" => "PfamLive::PfamA_reg_full",
		      {"foreign.auto_pfamA" => "self.auto_pfamA"}, 
		      {cascade_delete => 0});

#__PACKAGE__->might_have( "pfamAInts" => "PfamLive::Int_pfamAs",
#		      {"foreign.auto_pfamA_A" => "self.auto_pfamA"});

#__PACKAGE__->might_have( "pdbmap" => "PfamLive::PdbMap",
#		      {"foreign.auto_pfam" => "self.auto_pfamA"});

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

