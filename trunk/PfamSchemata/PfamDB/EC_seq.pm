
# $Id: EC_seq.pm,v 1.3 2007-03-16 11:25:17 jt6 Exp $
#
# $Author: jt6 $

package PfamDB::EC_seq;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "ec_seq" );

__PACKAGE__->add_columns( qw/ index_pfamA_ec
							  auto_pfamseq / );

__PACKAGE__->set_primary_key( "index_pfamA_ec" );

__PACKAGE__->has_one( EC_info => "PfamDB::EC_info",
					  { "foreign.index_pfamA_ec" => "self.index_pfamA_ec" },
					  { proxy                    => [ qw/ index_pfamA_ec
														  auto_pfamA
														  ec_number
														  reaction
														  number_seq
														  in_family / ] } );

__PACKAGE__->might_have( pfamseq_markup => "PfamDB::Pfamseq_markup",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
						 {  proxy                 => [ qw/ auto_markup / ] });

__PACKAGE__->might_have( pfamseq => "PfamDB::Pfamseq_markup",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
						 {  proxy                 => [ qw/ pfamseq_acc pfamseq_id / ] });

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

