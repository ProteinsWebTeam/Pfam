
# $Id: Smart_reg.pm,v 1.4 2007-03-16 11:25:17 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Smart_reg;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "smart_regions" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq auto_smart seq_start seq_end domain_bits_score domain_evalue_score/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_smart", "auto_pfamseq");

#Now setup the relationship

__PACKAGE__->has_one( "smart" =>  "PfamDB::Smart",
		      { "foreign.auto_smart"  => "self.auto_smart" },
		      { proxy => [ qw/smart_id smart_acc/ ] } );


__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_acc pfamseq_id/ ] } );
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

