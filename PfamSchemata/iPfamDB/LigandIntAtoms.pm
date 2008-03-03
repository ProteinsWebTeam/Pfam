package iPfamDB::LigandIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_int_atoms");
__PACKAGE__->add_columns(
  "internal_ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "ligand",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "atom_acc",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "atom",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
);
__PACKAGE__->set_primary_key("atom_acc");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:uISk6L2K6NV4JIgVllrmXw


# You can replace this text with custom content, and it will be preserved on regeneration

__PACKAGE__->add_unique_constraint(
    intAtomsConst => [ qw(internal_ligand_id atom_number) ],
);
1;
