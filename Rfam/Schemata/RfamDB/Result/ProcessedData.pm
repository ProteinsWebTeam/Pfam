use utf8;
package RfamDB::Result::ProcessedData;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::ProcessedData

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<processed_data>

=cut

__PACKAGE__->table("processed_data");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 cm

  data_type: 'longblob'
  is_nullable: 1

=head2 ss_stats_pbp

  data_type: 'longblob'
  is_nullable: 1

=head2 ss_stats_seq

  data_type: 'longblob'
  is_nullable: 1

=head2 ss_stats_fam

  data_type: 'longblob'
  is_nullable: 1

=head2 scores_graph

  data_type: 'longblob'
  is_nullable: 1

=head2 genome_full

  data_type: 'mediumtext'
  is_nullable: 1

=head2 genome_full_md5

  data_type: 'varchar'
  is_nullable: 1
  size: 32

=head2 refseq_full

  data_type: 'mediumtext'
  is_nullable: 1

=head2 refseq_full_md5

  data_type: 'varchar'
  is_nullable: 1
  size: 32

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "cm",
  { data_type => "longblob", is_nullable => 1 },
  "ss_stats_pbp",
  { data_type => "longblob", is_nullable => 1 },
  "ss_stats_seq",
  { data_type => "longblob", is_nullable => 1 },
  "ss_stats_fam",
  { data_type => "longblob", is_nullable => 1 },
  "scores_graph",
  { data_type => "longblob", is_nullable => 1 },
  "genome_full",
  { data_type => "mediumtext", is_nullable => 1 },
  "genome_full_md5",
  { data_type => "varchar", is_nullable => 1, size => 32 },
  "refseq_full",
  { data_type => "mediumtext", is_nullable => 1 },
  "refseq_full_md5",
  { data_type => "varchar", is_nullable => 1, size => 32 },
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
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 10:52:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:uP4OCz8GOPOyM8HWUEugEA



__PACKAGE__->set_primary_key('rfam_acc');

1;
