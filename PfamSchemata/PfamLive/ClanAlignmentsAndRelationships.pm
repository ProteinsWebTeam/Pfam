package PfamLive::ClanAlignmentsAndRelationships;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_alignments_and_relationships");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 4 },
  "alignment",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "relationship",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "image_map",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "stockholm",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-04 15:06:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rbOAcSl6VxrgDFbjSuSoSw

__PACKAGE__->set_primary_key("auto_clan");

# You can replace this text with custom content, and it will be preserved on regeneration
1;
