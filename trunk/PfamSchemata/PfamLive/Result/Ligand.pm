use utf8;
package PfamLive::Result::Ligand;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Ligand

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

=head2 pfam_a_ligands

Type: has_many

Related object: L<PfamLive::Result::PfamALigand>

=cut

__PACKAGE__->has_many(
  "pfam_a_ligands",
  "PfamLive::Result::PfamALigand",
  { "foreign.ligand_id" => "self.ligand_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfama_accs

Type: many_to_many

Composing rels: L</pfam_a_ligands> -> pfama_acc

=cut

__PACKAGE__->many_to_many("pfama_accs", "pfam_a_ligands", "pfama_acc");


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-09-22 17:06:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:e1IHpMS0kqV8ughxZVfCOw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
