use utf8;
package RfamLive::Result::HtmlAlignment;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::HtmlAlignment

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<html_alignment>

=cut

__PACKAGE__->table("html_alignment");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 type

  data_type: 'enum'
  extra: {list => ["seed","genome","seedColorstock","genomeColorstock"]}
  is_nullable: 0

=head2 html

  data_type: 'longblob'
  is_nullable: 1

=head2 block

  data_type: 'integer'
  is_nullable: 0

=head2 html_alignmentscol

  data_type: 'varchar'
  is_nullable: 1
  size: 45

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "type",
  {
    data_type => "enum",
    extra => {
      list => ["seed", "genome", "seedColorstock", "genomeColorstock"],
    },
    is_nullable => 0,
  },
  "html",
  { data_type => "longblob", is_nullable => 1 },
  "block",
  { data_type => "integer", is_nullable => 0 },
  "html_alignmentscol",
  { data_type => "varchar", is_nullable => 1, size => 45 },
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


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:lu01zFkZ8vxNcSzarL412A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
