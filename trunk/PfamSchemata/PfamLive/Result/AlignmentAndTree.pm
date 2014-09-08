use utf8;
package PfamLive::Result::AlignmentAndTree;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::AlignmentAndTree

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<alignment_and_tree>

=cut

__PACKAGE__->table("alignment_and_tree");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 alignment

  data_type: 'longblob'
  is_nullable: 1

=head2 tree

  data_type: 'longblob'
  is_nullable: 1

=head2 jtml

  data_type: 'longblob'
  is_nullable: 1

=head2 post

  data_type: 'longblob'
  is_nullable: 1

=head2 type

  data_type: 'enum'
  extra: {list => ["full","rp15","rp35","rp55","rp75","seed","meta","ncbi"]}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "alignment",
  { data_type => "longblob", is_nullable => 1 },
  "tree",
  { data_type => "longblob", is_nullable => 1 },
  "jtml",
  { data_type => "longblob", is_nullable => 1 },
  "post",
  { data_type => "longblob", is_nullable => 1 },
  "type",
  {
    data_type => "enum",
    extra => {
      list => ["full", "rp15", "rp35", "rp55", "rp75", "seed", "meta", "ncbi"],
    },
    is_nullable => 1,
  },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:06ry8/yuHIi+Opai3lhIMA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
