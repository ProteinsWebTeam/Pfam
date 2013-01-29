use utf8;
package RfamLive::Result::Refseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Refseq

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<refseq>

=cut

__PACKAGE__->table("refseq");

=head1 ACCESSORS

=head2 refseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 14

=head2 description

  data_type: 'mediumtext'
  is_nullable: 1

=head2 species

  data_type: 'mediumtext'
  is_nullable: 1

=head2 ncbi_taxid

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "refseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 14 },
  "description",
  { data_type => "mediumtext", is_nullable => 1 },
  "species",
  { data_type => "mediumtext", is_nullable => 1 },
  "ncbi_taxid",
  { data_type => "integer", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</refseq_acc>

=back

=cut

__PACKAGE__->set_primary_key("refseq_acc");

=head1 RELATIONS

=head2 refseq_full_regions

Type: has_many

Related object: L<RfamLive::Result::RefseqFullRegion>

=cut

__PACKAGE__->has_many(
  "refseq_full_regions",
  "RfamLive::Result::RefseqFullRegion",
  { "foreign.refseq_acc" => "self.refseq_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:C5FRb2sRsGqb5E854BmcoQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
