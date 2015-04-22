use utf8;
package PfamDB::AlignmentAndTree;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::AlignmentAndTree

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
  extra: {list => ["full","rp15","rp35","rp55","rp75","seed","meta","ncbi","ref_proteome"]}
  is_nullable: 0

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
      list => [
        "full",
        "rp15",
        "rp35",
        "rp55",
        "rp75",
        "seed",
        "meta",
        "ncbi",
        "ref_proteome",
      ],
    },
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:cm5R3oIW9JaVf6WBmQY8hQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
