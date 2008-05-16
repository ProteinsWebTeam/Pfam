
# $Id: EC_seq.pm,v 1.4 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::EC_seq;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "ec_seq" );

__PACKAGE__->add_columns( qw/ index_pfamA_ec
							  auto_pfamseq / );

__PACKAGE__->set_primary_key( "index_pfamA_ec" );

__PACKAGE__->has_one( EC_info => "PfamLive::EC_info",
					  { "foreign.index_pfamA_ec" => "self.index_pfamA_ec" },
					  { proxy                    => [ qw/ index_pfamA_ec
														  auto_pfamA
														  ec_number
														  reaction
														  number_seq
														  in_family / ] } );

__PACKAGE__->might_have( pfamseq_markup => "PfamLive::Pfamseq_markup",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
						 {  proxy                 => [ qw/ auto_markup / ] });

__PACKAGE__->might_have( pfamseq => "PfamLive::Pfamseq_markup",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
						 {  proxy                 => [ qw/ pfamseq_acc pfamseq_id / ] });

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

