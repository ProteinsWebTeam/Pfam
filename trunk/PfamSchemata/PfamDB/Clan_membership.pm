
# $Id: Clan_membership.pm,v 1.6 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Clan_membership;

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


#1 to 1 releationship
__PACKAGE__->has_one( "pfam" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		        {proxy => [qw/ pfamA_id pfamA_acc num_seed num_full model_length /]});

#Not sure about this one.... there will be many of the same auto_clan in this table,
#but only one in the clans table, another one for jt6
__PACKAGE__->has_one( "clans" => "PfamDB::Clans",
		      {"foreign.auto_clan" => "self.auto_clan"},
		      {proxy => [qw/clan_acc clan_id clan_description/]});


__PACKAGE__->has_many( "pfamARegFull" => "PfamDB::PfamA_reg_full",
		      {"foreign.auto_pfamA" => "self.auto_pfamA"});

__PACKAGE__->has_many( "pdb_pfamA_reg" => "PfamDB::Pdb_pfamA_reg",
		      {"foreign.auto_pfamA" => "self.auto_pfamA"});

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

