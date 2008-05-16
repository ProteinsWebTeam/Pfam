
# $Id: PfamA_reg_seed.pm,v 1.7 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::PfamA_reg_seed;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamA_reg_seed" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq auto_pfamA seq_start seq_end cigar tree_order /);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamA", "auto_pfamseq", "seq_start");

#Now setup the relationship
__PACKAGE__->has_one( "pfamA" =>  "PfamLive::Pfam",
		      { "foreign.auto_pfamA"  => "self.auto_pfamA" },
		      { proxy => [ qw (pfamA_id ) ] } );

__PACKAGE__->has_one( "pfamseq" =>  "PfamLive::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw ( pfamseq_acc pfamseq_id seq_version) ] } );


__PACKAGE__->add_unique_constraint(
	 region => [ qw/auto_pfamA auto_pfamseq seq_start seq_end/]
);
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

