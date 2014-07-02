use utf8;
package RfamLive::Result::MotifPdb;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::MotifPdb

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<motif_pdb>

=cut

__PACKAGE__->table("motif_pdb");

=head1 ACCESSORS

=head2 motif_acc

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
  is_nullable: 0
  size: 4

=head2 pdb_seq

  data_type: 'varchar'
  is_nullable: 1
  size: 6

=head2 pdb_res_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_res_end

  data_type: 'mediumint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "motif_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pdb_id",
  { data_type => "varchar", is_nullable => 0, size => 4 },
  "chain",
  { data_type => "varchar", is_nullable => 0, size => 4 },
  "pdb_seq",
  { data_type => "varchar", is_nullable => 1, size => 6 },
  "pdb_res_start",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_res_end",
  { data_type => "mediumint", is_nullable => 1 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<Composite Unique>

=over 4

=item * L</motif_acc>

=item * L</pdb_id>

=back

=cut

__PACKAGE__->add_unique_constraint("Composite Unique", ["motif_acc", "pdb_id"]);

=head1 RELATIONS

=head2 motif_acc

Type: belongs_to

Related object: L<RfamLive::Result::Motif>

=cut

__PACKAGE__->belongs_to(
  "motif_acc",
  "RfamLive::Result::Motif",
  { motif_acc => "motif_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-07-02 08:33:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XxVd3GUZcYxbzC0sk4JgCw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
