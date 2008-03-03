package iPfamDB::LigandSynonyms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_synonyms");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "alt_name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rvpvBM7PVRtutUa6W0jZcQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
