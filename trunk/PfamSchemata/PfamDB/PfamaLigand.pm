use utf8;
package PfamDB::PfamaLigand;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamaLigand

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_ligand>

=cut

__PACKAGE__->table("pfamA_ligand");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 ligand_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 3

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "ligand_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 3 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfama_acc>

=item * L</ligand_id>

=back

=cut

__PACKAGE__->set_primary_key("pfama_acc", "ligand_id");

=head1 RELATIONS

=head2 ligand_id

Type: belongs_to

Related object: L<PfamDB::Ligand>

=cut

__PACKAGE__->belongs_to("ligand_id", "PfamDB::Ligand", { ligand_id => "ligand_id" });

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:woxz9AHHojvKPEbd1iIs3A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
