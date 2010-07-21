package iPfamDB::PliRes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pli_res");
__PACKAGE__->add_columns(
  "pli",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "internal_ligand_id",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 20 },
  "residue_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "residue_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);
__PACKAGE__->belongs_to(
  "protein_acc",
  "iPfamDB::Protein",
  { accession => "protein_acc" },
);
__PACKAGE__->belongs_to(
  "internal_ligand_id",
  "iPfamDB::Ligands",
  { internal_ligand_id => "internal_ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:HyKe3CfCG2TaAQBzbAnmpg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
