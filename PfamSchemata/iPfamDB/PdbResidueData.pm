package iPfamDB::PdbResidueData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_residue_data");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "chain",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "uniprot_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "pdb_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "uniprot_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:fPbacMAFZeh8UsL3iFjiUw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
