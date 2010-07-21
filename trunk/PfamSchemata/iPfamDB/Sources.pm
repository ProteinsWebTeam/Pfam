package iPfamDB::Sources;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("sources");
__PACKAGE__->add_columns(
  "source_db",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("source_db");
__PACKAGE__->add_unique_constraint("IX_source_1", ["source_db"]);
__PACKAGE__->has_many(
  "proteins",
  "iPfamDB::Protein",
  { "foreign.source_db" => "self.source_db" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:W1nMnD39iSBizpYZXAAGRg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
