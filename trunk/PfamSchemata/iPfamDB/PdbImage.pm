package iPfamDB::PdbImage;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_image");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "pdb_image",
  {
    data_type => "MEDIUMBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "pdb_image_sml",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
);
__PACKAGE__->set_primary_key("pdb_id");
__PACKAGE__->add_unique_constraint("pdb_id", ["pdb_id"]);
__PACKAGE__->belongs_to("pdb_id", "iPfamDB::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:JSAOeaud8hc855+8XuGDsw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
