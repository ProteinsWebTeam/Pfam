use utf8;
package RfamLive::Result::AlignmentAndTree;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::AlignmentAndTree

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<alignment_and_tree>

=cut

__PACKAGE__->table("alignment_and_tree");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 type

  data_type: 'enum'
  extra: {list => ["seed","seedTax","genome","genomeTax"]}
  is_nullable: 0

=head2 alignment

  data_type: 'longblob'
  is_nullable: 1

=head2 tree

  data_type: 'longblob'
  is_nullable: 1

=head2 treemethod

  data_type: 'tinytext'
  is_nullable: 1

=head2 average_length

  data_type: 'double precision'
  is_nullable: 1
  size: [7,2]

=head2 percent_id

  data_type: 'double precision'
  is_nullable: 1
  size: [5,2]

=head2 number_of_sequences

  data_type: 'bigint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "type",
  {
    data_type => "enum",
    extra => { list => ["seed", "seedTax", "genome", "genomeTax"] },
    is_nullable => 0,
  },
  "alignment",
  { data_type => "longblob", is_nullable => 1 },
  "tree",
  { data_type => "longblob", is_nullable => 1 },
  "treemethod",
  { data_type => "tinytext", is_nullable => 1 },
  "average_length",
  { data_type => "double precision", is_nullable => 1, size => [7, 2] },
  "percent_id",
  { data_type => "double precision", is_nullable => 1, size => [5, 2] },
  "number_of_sequences",
  { data_type => "bigint", is_nullable => 1 },
);

=head1 RELATIONS

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:E6XR82q4mKjHyYYuekKrkg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->set_primary_key('rfam_acc', 'type');

__PACKAGE__->add_unique_constraint(
    rfam_acc_and_type => [ qw(rfam_acc type) ],
  );

1;
