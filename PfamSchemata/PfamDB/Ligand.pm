use utf8;
package PfamDB::Ligand;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Ligand

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<ligand>

=cut

__PACKAGE__->table("ligand");

=head1 ACCESSORS

=head2 ligand_id

  data_type: 'varchar'
  is_nullable: 0
  size: 3

=head2 name

  data_type: 'text'
  is_nullable: 0

=head2 formula

  data_type: 'text'
  is_nullable: 0

=head2 molecular_weight

  data_type: 'float'
  is_nullable: 0

=head2 smiles

  data_type: 'text'
  is_nullable: 0

=head2 inchi

  data_type: 'mediumtext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "varchar", is_nullable => 0, size => 3 },
  "name",
  { data_type => "text", is_nullable => 0 },
  "formula",
  { data_type => "text", is_nullable => 0 },
  "molecular_weight",
  { data_type => "float", is_nullable => 0 },
  "smiles",
  { data_type => "text", is_nullable => 0 },
  "inchi",
  { data_type => "mediumtext", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</ligand_id>

=back

=cut

__PACKAGE__->set_primary_key("ligand_id");

=head1 RELATIONS

=head2 pfama_ligands

Type: has_many

Related object: L<PfamDB::PfamaLigand>

=cut

__PACKAGE__->has_many(
  "pfama_ligands",
  "PfamDB::PfamaLigand",
  { "foreign.ligand_id" => "self.ligand_id" },
  undef,
);

=head2 pfama_accs

Type: many_to_many

Composing rels: L</pfama_ligands> -> pfama_acc

=cut

__PACKAGE__->many_to_many("pfama_accs", "pfama_ligands", "pfama_acc");


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:lr3wzMWz+phH5DjMKme0XA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
