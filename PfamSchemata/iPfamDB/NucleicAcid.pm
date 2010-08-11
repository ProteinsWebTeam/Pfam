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


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ny1UWwHNgGED70gTknWNiA

__PACKAGE__->add_unique_constraint( unique_accession => [ "accession" ] );
# You can replace this text with custom content, and it will be preserved on regeneration
1;
