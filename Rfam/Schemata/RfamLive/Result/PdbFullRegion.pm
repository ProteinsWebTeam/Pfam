use utf8;
package RfamLive::Result::PdbFullRegion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::PdbFullRegion

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_full_region>

=cut

__PACKAGE__->table("pdb_full_region");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pdb_id

  data_type: 'varchar'
  is_nullable: 0
  size: 4

=head2 chain

  data_type: 'varchar'
  default_value: 'NULL'
  is_nullable: 1
  size: 4

=head2 pdb_start

  data_type: 'mediumint'
  is_nullable: 0

=head2 pdb_end

  data_type: 'mediumint'
  is_nullable: 0

=head2 bit_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [7,2]

=head2 evalue_score

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 15

=head2 cm_start

  data_type: 'mediumint'
  is_nullable: 0

=head2 cm_end

  data_type: 'mediumint'
  is_nullable: 0

=head2 hex_colour

  data_type: 'varchar'
  default_value: 'NULL'
  is_nullable: 1
  size: 6

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pdb_id",
  { data_type => "varchar", is_nullable => 0, size => 4 },
  "chain",
  {
    data_type => "varchar",
    default_value => "NULL",
    is_nullable => 1,
    size => 4,
  },
  "pdb_start",
  { data_type => "mediumint", is_nullable => 0 },
  "pdb_end",
  { data_type => "mediumint", is_nullable => 0 },
  "bit_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [7, 2],
  },
  "evalue_score",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 15 },
  "cm_start",
  { data_type => "mediumint", is_nullable => 0 },
  "cm_end",
  { data_type => "mediumint", is_nullable => 0 },
  "hex_colour",
  {
    data_type => "varchar",
    default_value => "NULL",
    is_nullable => 1,
    size => 6,
  },
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


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2014-06-06 14:27:29
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:xDRCWgEPyZ8hMvlEsjbk5A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->set_primary_key('rfam_acc');
1;
