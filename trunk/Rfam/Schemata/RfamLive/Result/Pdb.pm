use utf8;
package RfamLive::Result::Pdb;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Pdb

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb>

=cut

__PACKAGE__->table("pdb");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_nullable: 0
  size: 4

=head2 keywords

  data_type: 'tinytext'
  is_nullable: 1

=head2 title

  data_type: 'mediumtext'
  is_nullable: 1

=head2 date

  data_type: 'tinytext'
  is_nullable: 1

=head2 resolution

  data_type: 'decimal'
  default_value: 0.00
  is_nullable: 1
  size: [5,2]

=head2 method

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_nullable => 0, size => 4 },
  "keywords",
  { data_type => "tinytext", is_nullable => 1 },
  "title",
  { data_type => "mediumtext", is_nullable => 1 },
  "date",
  { data_type => "tinytext", is_nullable => 1 },
  "resolution",
  {
    data_type => "decimal",
    default_value => "0.00",
    is_nullable => 1,
    size => [5, 2],
  },
  "method",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pdb_id>

=back

=cut

__PACKAGE__->set_primary_key("pdb_id");

=head1 RELATIONS

=head2 pdb_rfam_regs

Type: has_many

Related object: L<RfamLive::Result::PdbRfamReg>

=cut

__PACKAGE__->has_many(
  "pdb_rfam_regs",
  "RfamLive::Result::PdbRfamReg",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_sequences

Type: has_many

Related object: L<RfamLive::Result::PdbSequence>

=cut

__PACKAGE__->has_many(
  "pdb_sequences",
  "RfamLive::Result::PdbSequence",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:e6/yv5svaclH7oC8CHj2zQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
