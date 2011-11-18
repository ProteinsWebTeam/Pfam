package PfamDB::PdbResidueData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_residue_data");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "chain",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "serial",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "pdb_res",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "pdb_seq_number",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "pdb_insert_code",
  { data_type => "VARCHAR", default_value => 0, is_nullable => 0, size => 1 },
  "observed",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 1 },
  "pfamseq_acc",
  { data_type => "VARCHAR", default_value => 0, is_nullable => 0, size => 6 },
  "auto_pfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "pfamseq_res",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "pfamseq_seq_number",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "dssp_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
);
__PACKAGE__->belongs_to("pdb_id", "PfamDB::Pdb", { pdb_id => "pdb_id" });

__PACKAGE__->might_have(
  "pfamseqs",
  "PfamDB::Pfamseq",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
);

__PACKAGE__->might_have(
  'pfamseqMarkup',
  'PfamDB::PfamseqMarkup',
  { "foreign.auto_pfamseq" => "self.auto_pfamseq",
    "foreign.residue"      => "self.pfamseq_seq_number" },
    { proxy => [ qw/ annotation auto_markup / ] } );

#__PACKAGE__->might_have( pfamA_reg_full_significant => "PfamDB::PfamaRegFullSignificant",
#             { "foreign.auto_pfamseq" => "self.auto_pfamseq"});

#__PACKAGE__->might_have( pfamA_reg_seed => "PfamDB::PfamaRegSeed",
#             { "foreign.auto_pfamseq" => "self.auto_pfamseq"});


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
