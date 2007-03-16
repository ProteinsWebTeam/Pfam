
# $Id: PfamA_architecture.pm,v 1.4 2007-03-16 11:25:18 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::PfamA_architecture;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamA_architecture" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamA auto_architecture/);


#Set the the keys
__PACKAGE__->set_primary_key( "auto_pfamA", "auto_architecture" );


#Now on to the relationships

#__PACKAGE__->has_one    ( "pfamA_web" => "PfamDB::PfamA_web",
#			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
	#		  {proxy => [ qw/average_length percentage_id average_coverage status/]});

__PACKAGE__->has_one    ( "pfam" => "PfamDB::Pfam",
			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
			  {proxy => [ qw/pfamA_id pfamA_acc/]});

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

