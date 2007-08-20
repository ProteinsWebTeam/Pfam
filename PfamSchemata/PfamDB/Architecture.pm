
# $Id: Architecture.pm,v 1.7 2007-08-20 08:58:47 rdf Exp $
#
# $Author: rdf $

package PfamDB::Architecture;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "architecture" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_architecture architecture type_example no_seqs architecture_acc/);


#Set the the keys
__PACKAGE__->set_primary_key( "auto_architecture", "architecture", "type_example" );


#Now on to the relationships

__PACKAGE__->has_one    ( "pfamA_architecture" => "PfamDB::PfamA_architecture",
			  {"foreign.auto_architecture" => "self.auto_architecture"},
			{proxy => [qw/auto_pfamA/]});

__PACKAGE__->has_one    ( "type_example" => "PfamDB::Pfamseq",
			  {"foreign.auto_pfamseq" => "self.type_example"},
			  {proxy => [ qw/pfamseq_id pfamseq_acc/ ]});
			  
__PACKAGE__->has_many ( "pfamseq" => "PfamDB::Pfamseq",
			  {"foreign.auto_architecture" => "self.auto_architecture"});
			  
__PACKAGE__->has_one    ( "storable" => "PfamDB::Pfam_annseq",
			  {"foreign.auto_pfamseq" => "self.type_example"},
			  {proxy => [ qw/annseq_storable/ ]});

__PACKAGE__->has_one    ( "clan_arch" => "PfamDB::ClanArchitecture",
			  {"foreign.auto_architecture" => "self.auto_architecture"},
			{proxy => [qw/auto_clan/]});


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

