package iPfamDB::PdbResidueData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_residue_data");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "chain",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "uniprot_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "pdb_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "pdb_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "dssp_code",
  { data_type => "VARCHAR", default_value => 0, is_nullable => 0, size => 4 },
  "uniprot_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "uniprot_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Oy1rZ1jmEKrpaafzIZ5+QQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
