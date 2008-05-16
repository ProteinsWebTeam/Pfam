
# $Id: Pfamseq_markup.pm,v 1.5 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Pfamseq_markup;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamseq_markup" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq auto_markup residue annotation/);

__PACKAGE__->set_primary_key("auto_pfamseq");

__PACKAGE__->has_one("pfamseq" => "PfamDB::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/pfamseq_id pfamseq_acc/]});

# renamed this relationship from "auto_markup" because it was blocking
# access to the column of that name.
# jt6 20060801 WTSI
__PACKAGE__->has_one("autoMarkup" => "PfamDB::Markup_key",
		     {"foreign.auto_markup" => "self.auto_markup"},
		     {proxy => [qw/label/]});

__PACKAGE__->might_have("pdbResidue" => "PfamDB::Pdb_residue",
						{"foreign.auto_pfamseq" => "self.auto_pfamseq",
						 "foreign.pfamseq_seq_number" => "self.residue"},
						{proxy => [qw/chain pdb_seq_number pdb_res auto_pdb pfamseq_seq_number/]});

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

