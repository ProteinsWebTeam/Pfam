
# $Id: EC_info.pm,v 1.3 2007-03-16 11:25:16 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::EC_info;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "ec_info" );

__PACKAGE__->add_columns( qw/ index_pfamA_ec
							  auto_pfamA
							  ec_number
							  reaction
							  number_seq
							  in_family / );

__PACKAGE__->set_primary_key( "index_pfamA_ec" );

__PACKAGE__->has_one( pfamA => "PfamDB::Pfam",
					  { "foreign.auto_pfamA" => "self.auto_pfamA" },
					  { proxy                => [ qw/ pfamA_acc pfamA_id description / ] } );

__PACKAGE__->has_many( EC_seq => "PfamDB::EC_seq",
					   { "foreign.index_pfamA_ec" => "self.index_pfamA_ec"});

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


