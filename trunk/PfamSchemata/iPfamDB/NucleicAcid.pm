package iPfamDB::NucleicAcid;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nucleic_acid");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "seq_version",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 3 },
  "md5",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 32,
  },
  "type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "source_db",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "length",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ncbi_code",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->has_many(
  "nadis",
  "iPfamDB::Nadi",
  { "foreign.nucleic_acid_acc" => "self.accession" },
);
__PACKAGE__->has_many(
  "napis",
  "iPfamDB::Napi",
  { "foreign.nucleic_acid_acc" => "self.accession" },
);
__PACKAGE__->belongs_to(
  "accession",
  "iPfamDB::PdbChainData",
  { "internal_chain_accession" => "accession" },
);
__PACKAGE__->has_many(
  "nucleic_acid_int_atoms",
  "iPfamDB::NucleicAcidIntAtoms",
  { "foreign.nucleic_acid_acc" => "self.accession" },
);
__PACKAGE__->has_many(
  "nucleic_acid_regions",
  "iPfamDB::NucleicAcidRegion",
  { "foreign.accession" => "self.accession" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 15:16:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:KVCQnRw+/mH3k6WPq6+acQ
# These lines were loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/NucleicAcid.pm' found in @INC.# They are now part of the custom portion of this file# for you to hand-edit.  If you do not either delete# this section or remove that file from @INC, this section# will be repeated redundantly when you re-create this# file again via Loader!
package iPfamDB::NucleicAcid;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nucleic_acid");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "seq_version",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 3 },
  "md5",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 32,
  },
  "type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "source_db",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "length",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ncbi_code",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->has_many(
  "nadis",
  "iPfamDB::Nadi",
  { "foreign.nucleic_acid_acc" => "self.accession" },
);
__PACKAGE__->has_many(
  "napis",
  "iPfamDB::Napi",
  { "foreign.nucleic_acid_acc" => "self.accession" },
);
__PACKAGE__->belongs_to(
  "accession",
  "iPfamDB::PdbChainData",
  { "internal_chain_accession" => "accession" },
);
__PACKAGE__->has_many(
  "nucleic_acid_int_atoms",
  "iPfamDB::NucleicAcidIntAtoms",
  { "foreign.nucleic_acid_acc" => "self.accession" },
);
__PACKAGE__->has_many(
  "nucleic_acid_regions",
  "iPfamDB::NucleicAcidRegion",
  { "foreign.accession" => "self.accession" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:pIR1VNpGQzUR+/kXHyaS/w


# You can replace this text with custom content, and it will be preserved on regeneration

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

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
# End of lines loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/NucleicAcid.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
