use utf8;
package WebUser::Result::Af2;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

WebUser::Result::Af2

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<AF2>

=cut

__PACKAGE__->table("AF2");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 0

=head2 length

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<pfamseq_pfamA_start_end>

=over 4

=item * L</pfamseq_acc>

=item * L</pfama_acc>

=item * L</seq_start>

=item * L</seq_end>

=back

=cut

__PACKAGE__->add_unique_constraint(
  "pfamseq_pfamA_start_end",
  ["pfamseq_acc", "pfama_acc", "seq_start", "seq_end"],
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2021-07-14 21:37:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3VNXDAS5EgyrLm5ZEPDUUg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
