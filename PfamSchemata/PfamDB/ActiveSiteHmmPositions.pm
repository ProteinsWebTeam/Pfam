use utf8;
package PfamDB::ActiveSiteHmmPositions;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::ActiveSiteHmmPositions

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_active_site_hmm_positions>

=cut

__PACKAGE__->table("_active_site_hmm_positions");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 hmm_position

  data_type: 'smallint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 residue

  data_type: 'tinytext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "hmm_position",
  { data_type => "smallint", extra => { unsigned => 1 }, is_nullable => 0 },
  "residue",
  { data_type => "tinytext", is_nullable => 0 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2017-02-02 14:37:42
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6Dc94dP10zsXBF4izRi7Bw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
