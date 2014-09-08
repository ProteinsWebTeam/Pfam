use utf8;
package PfamLive::Result::SeqInfo;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::SeqInfo

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<seq_info>

=cut

__PACKAGE__->table("seq_info");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 description

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 seq_description

  data_type: 'text'
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "seq_description",
  { data_type => "text", is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:dbo9IVJpxIMCRDHdtZoqyg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
