package DfamDB::Result::DfamRegSeed;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::DfamRegSeed

=cut

__PACKAGE__->table("dfam_reg_seed");

=head1 ACCESSORS

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
  is_nullable: 0

=head2 cigar

  data_type: 'text'
  is_nullable: 1

=head2 tree_order

  data_type: 'mediumint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
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
  { data_type => "mediumint", is_nullable => 0 },
  "cigar",
  { data_type => "text", is_nullable => 1 },
  "tree_order",
  { data_type => "mediumint", is_nullable => 1 },
);
__PACKAGE__->add_unique_constraint(
  "dfam_reg_seed_reg_idx",
  ["dfam_acc", "auto_dfamseq", "seq_start", "seq_end"],
);

=head1 RELATIONS

=head2 dfam_acc

Type: belongs_to

Related object: L<DfamDB::Result::Dfam>

=cut

__PACKAGE__->belongs_to(
  "dfam_acc",
  "DfamDB::Result::Dfam",
  { dfam_acc => "dfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 auto_dfamseq

Type: belongs_to

Related object: L<DfamDB::Result::Dfamseq>

=cut

__PACKAGE__->belongs_to(
  "auto_dfamseq",
  "DfamDB::Result::Dfamseq",
  { auto_dfamseq => "auto_dfamseq" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:YkrEWQFRyD9Ik0Zzt6lfZQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
