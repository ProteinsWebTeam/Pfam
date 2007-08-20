
# $Id: ClanArchitecture.pm,v 1.5 2007-08-20 08:58:47 rdf Exp $
#
# $Author: rdf $
package PfamDB::ClanArchitecture;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "clan_architecture" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_clan auto_architecture/);


#Set the the keys
__PACKAGE__->set_primary_key( "auto_clan", "auto_architecture" );


#Now on to the relationships

__PACKAGE__->has_one    ( "clan" => "PfamDB::Pfam",
			  {"foreign.auto_clan" => "self.clan"},
			  {proxy => [ qw/clan_id clan_acc/]});

__PACKAGE__->has_one    ( "arch" => "PfamDB::Architecture",
			  {"foreign.auto_architecture" => "self.auto_architecture"},
			  {proxy => [ qw/architecture type_example no_seqs pfamseq_id pfamseq_acc annseq_storable/ ]});
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

