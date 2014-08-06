use utf8;
package RfamDB::Result::MatchAndFasta;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::MatchAndFasta

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<matches_and_fasta>

=cut

__PACKAGE__->table("matches_and_fasta");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 match_list

  data_type: 'longblob'
  is_nullable: 1

=head2 fasta

  data_type: 'longblob'
  is_nullable: 1

=head2 type

  data_type: 'enum'
  extra: {list => ["rfamseq","genome","refseq"]}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "match_list",
  { data_type => "longblob", is_nullable => 1 },
  "fasta",
  { data_type => "longblob", is_nullable => 1 },
  "type",
  {
    data_type => "enum",
    extra => { list => ["rfamseq", "genome", "refseq"] },
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamDB::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamDB::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "NO ACTION", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 10:52:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:AnzJzPH8/J6QgS2vJOsPjQ

__PACKAGE__->set_primary_key('rfam_acc', 'type');
# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
