package RfamDB::GenomeEntry;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("genome_entry");
__PACKAGE__->add_columns(
  "auto_genome",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "genome_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "description",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "circular",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 3 },
);
__PACKAGE__->set_primary_key("auto_genome");
__PACKAGE__->has_many(
  "chromosome_builds",
  "RfamDB::ChromosomeBuild",
  { "foreign.auto_genome" => "self.auto_genome" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:yYzGyl+4XZBE8afnjieQ5A


# You can replace this text with custom content, and it will be preserved on regeneration
1;
