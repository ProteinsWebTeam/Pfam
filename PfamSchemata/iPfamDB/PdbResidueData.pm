package iPfamDB::PdbResidueData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_residue_data");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "msd_chain",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "chain",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "serial",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "pdb_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "pdb_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "pdb_insert_code",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 1 },
  "uniprot_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "seq_version",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 4 },
  "uniprot_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "uniprot_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "dssp_code",
  { data_type => "VARCHAR", default_value => 0, is_nullable => 0, size => 4 },
);
__PACKAGE__->belongs_to("pdb_id", "iPfamDB::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:uLhF7hUuS9rrrLBo12T2UA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
