
# $Id: PfamA_reg_seed.pm,v 1.4 2008-04-21 15:42:55 rdf Exp $
#
# $Author: rdf $
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
__PACKAGE__->set_primary_key("auto_pfamA", "auto_pfamseq");

#Now setup the relationship
__PACKAGE__->has_one( "pfamA" =>  "PfamLive::Pfam",
		      { "foreign.auto_pfamA"  => "self.auto_pfamA" },
		      { proxy => [ qw (pfamA_id ) ] } );

__PACKAGE__->has_one( "pfamseq" =>  "PfamLive::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw ( pfamseq_acc pfamseq_id seq_version) ] } );

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

