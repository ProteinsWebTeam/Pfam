
# $Id: PfamB_reg.pm,v 1.7 2007-05-23 09:18:27 rdf Exp $
#
# $Author: rdf $
package PfamDB::PfamB_reg;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamB_reg" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq auto_pfamB seq_start seq_end /);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamB", "auto_pfamseq");

#Now setup the relationship

__PACKAGE__->has_one( "pfamB" =>  "PfamDB::PfamB",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" },
		      { proxy => [ qw/pfamB_id pfamB_acc/ ] } );


__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_acc pfamseq_id md5 species taxonomy/ ] } );

__PACKAGE__->might_have( "pfamseq_architecture" =>  "PfamDB::Pfamseq_architecture",
			 { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
			 { proxy => [ qw/auto_architecture/ ] } );

__PACKAGE__->has_one("annseq" => "PfamDB::Pfam_annseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/annseq_storable/]});

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

