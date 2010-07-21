package iPfamDB::LigandSummary;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_summary");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "three_letter_code",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "formula",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "domcount",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 5 },
  "image",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
);
__PACKAGE__->add_unique_constraint("ligand_summary_ligand_id_Idx", ["ligand_id"]);
__PACKAGE__->belongs_to(
  "ligand_id",
  "iPfamDB::LigandChemistry",
  { ligand_id => "ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:KWegLvaXxyKjDPJiWPJWDQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
