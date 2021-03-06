package DfamLive::Schema::Result::DfamRegFullSignificant;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::DfamRegFullSignificant

=cut

__PACKAGE__->table("dfam_reg_full_significant");

=head1 ACCESSORS

=head2 auto_dfam_reg_full

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 dfam_acc

  data_type: 'varchar'
  default_value: 0
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 auto_dfamseq

  data_type: 'integer'
  default_value: 0
  is_foreign_key: 1
  is_nullable: 0

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 ali_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 ali_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 model_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 model_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 domain_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 domain_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 sequence_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 sequence_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 cigar

  data_type: 'text'
  is_nullable: 1

=head2 in_full

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 tree_order

  data_type: 'mediumint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_dfam_reg_full",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "dfam_acc",
  {
    data_type => "varchar",
    default_value => 0,
    is_foreign_key => 1,
    is_nullable => 0,
    size => 7,
  },
  "auto_dfamseq",
  {
    data_type      => "integer",
    default_value  => 0,
    is_foreign_key => 1,
    is_nullable    => 0,
  },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "ali_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "ali_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "model_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "model_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "domain_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "domain_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "sequence_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "sequence_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "cigar",
  { data_type => "text", is_nullable => 1 },
  "in_full",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "tree_order",
  { data_type => "mediumint", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("auto_dfam_reg_full");

=head1 RELATIONS

=head2 auto_dfamseq

Type: belongs_to

Related object: L<DfamLive::Schema::Result::Dfamseq>

=cut

__PACKAGE__->belongs_to(
  "auto_dfamseq",
  "DfamLive::Schema::Result::Dfamseq",
  { auto_dfamseq => "auto_dfamseq" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 dfam_acc

Type: belongs_to

Related object: L<DfamLive::Schema::Result::Dfam>

=cut

__PACKAGE__->belongs_to(
  "dfam_acc",
  "DfamLive::Schema::Result::Dfam",
  { dfam_acc => "dfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:18:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:dRoeOV7TXBKK3BwifvSY6A
# These lines were loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/DfamRegFullSignificant.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package DfamLive::Schema::Result::DfamRegFullSignificant;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::DfamRegFullSignificant

=cut

__PACKAGE__->table("dfam_reg_full_significant");

=head1 ACCESSORS

=head2 auto_dfam_reg_full

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 dfam_acc

  data_type: 'varchar'
  default_value: 0
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 auto_dfamseq

  data_type: 'integer'
  default_value: 0
  is_foreign_key: 1
  is_nullable: 0

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 ali_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 ali_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 model_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 model_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 domain_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 domain_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 sequence_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 sequence_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 cigar

  data_type: 'text'
  is_nullable: 1

=head2 in_full

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 tree_order

  data_type: 'mediumint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_dfam_reg_full",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "dfam_acc",
  {
    data_type => "varchar",
    default_value => 0,
    is_foreign_key => 1,
    is_nullable => 0,
    size => 7,
  },
  "auto_dfamseq",
  {
    data_type      => "integer",
    default_value  => 0,
    is_foreign_key => 1,
    is_nullable    => 0,
  },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "ali_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "ali_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "model_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "model_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "domain_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "domain_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "sequence_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "sequence_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "cigar",
  { data_type => "text", is_nullable => 1 },
  "in_full",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "tree_order",
  { data_type => "mediumint", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("auto_dfam_reg_full");

=head1 RELATIONS

=head2 auto_dfamseq

Type: belongs_to

Related object: L<DfamLive::Schema::Result::Dfamseq>

=cut

__PACKAGE__->belongs_to(
  "auto_dfamseq",
  "DfamLive::Schema::Result::Dfamseq",
  { auto_dfamseq => "auto_dfamseq" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 dfam_acc

Type: belongs_to

Related object: L<DfamLive::Schema::Result::Dfam>

=cut

__PACKAGE__->belongs_to(
  "dfam_acc",
  "DfamLive::Schema::Result::Dfam",
  { dfam_acc => "dfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:12:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:JjJTujr0aDmEKptDlNVR9A


# You can replace this text with custom content, and it will be preserved on regeneration
1;
# End of lines loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/DfamRegFullSignificant.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
