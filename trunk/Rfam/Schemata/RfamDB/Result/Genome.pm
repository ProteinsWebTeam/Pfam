use utf8;
package RfamDB::Result::Genome;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::Genome

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<genome>

=cut

__PACKAGE__->table("genome");

=head1 ACCESSORS

=head2 genome_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 ensembl_id

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 description

  data_type: 'mediumtext'
  is_nullable: 1

=head2 length

  data_type: 'bigint'
  is_nullable: 1

=head2 circular

  data_type: 'tinyint'
  is_nullable: 1

=head2 ncbi_id

  data_type: 'integer'
  is_nullable: 1

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 species

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 kingdom

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=head2 regions

  data_type: 'integer'
  is_nullable: 1

=head2 families

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "genome_acc",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "ensembl_id",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "description",
  { data_type => "mediumtext", is_nullable => 1 },
  "length",
  { data_type => "bigint", is_nullable => 1 },
  "circular",
  { data_type => "tinyint", is_nullable => 1 },
  "ncbi_id",
  { data_type => "integer", is_nullable => 1 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "species",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "kingdom",
  { data_type => "varchar", is_nullable => 1, size => 50 },
  "regions",
  { data_type => "integer", is_nullable => 1 },
  "families",
  { data_type => "integer", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</genome_acc>

=back

=cut

__PACKAGE__->set_primary_key("genome_acc");

=head1 RELATIONS

=head2 genome_full_regions

Type: has_many

Related object: L<RfamDB::Result::GenomeFullRegion>

=cut

__PACKAGE__->has_many(
  "genome_full_regions",
  "RfamDB::Result::GenomeFullRegion",
  { "foreign.genome_acc" => "self.genome_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 genome_gffs

Type: has_many

Related object: L<RfamDB::Result::GenomeGff>

=cut

__PACKAGE__->has_many(
  "genome_gffs",
  "RfamDB::Result::GenomeGff",
  { "foreign.genome_acc" => "self.genome_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 genome_seqs

Type: has_many

Related object: L<RfamDB::Result::GenomeSeq>

=cut

__PACKAGE__->has_many(
  "genome_seqs",
  "RfamDB::Result::GenomeSeq",
  { "foreign.genome_acc" => "self.genome_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hguVGqCiAREYBOhw8NeISw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
