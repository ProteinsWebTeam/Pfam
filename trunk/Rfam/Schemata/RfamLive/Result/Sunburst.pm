use utf8;
package RfamLive::Result::Sunburst;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Sunburst

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<sunburst>

=cut

__PACKAGE__->table("sunburst");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 data

  data_type: 'longblob'
  is_nullable: 0

=head2 type

  data_type: 'enum'
  extra: {list => ["rfamseq","genome","refseq"]}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "data",
  { data_type => "longblob", is_nullable => 0 },
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

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "NO ACTION", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 10:52:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:VOAtZgxTMsK75yTPfjWqVA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->set_primary_key('rfam_acc', 'type');

__PACKAGE__->add_unique_constraint(
	rfam_acc_and_type => [qw(rfam_acc type)],
	);


1;
