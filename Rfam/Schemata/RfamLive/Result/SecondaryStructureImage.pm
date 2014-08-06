use utf8;
package RfamLive::Result::SecondaryStructureImage;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::SecondaryStructureImage

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<secondary_structure_image>

=cut

__PACKAGE__->table("secondary_structure_image");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 type

  data_type: 'enum'
  extra: {list => ["cons","cov","dist","disttrunc","ent","fcbp","maxcm","norm","rchie","species","ss"]}
  is_nullable: 0

Should be ENUM list

=head2 image

  data_type: 'longblob'
  is_nullable: 1

=cut


__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "type",
  {
    data_type => "enum",
    extra => {
      list => [
        "cons",
        "cov",
        "dist",
        "disttrunc",
        "ent",
        "fcbp",
        "maxcm",
        "norm",
        "rchie",
        "species",
        "ss",
      ],
    },
    is_nullable => 0,
  },
  "image",
  { data_type => "longblob", is_nullable => 1 },
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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i3UAdDjdi71gNQEFCifZtw


__PACKAGE__->add_unique_constraint(
	acc_and_type => ["rfam_acc", "type"]
);

#__PACKAGE__->set_primary_key(__PACKAGE__->columns);
__PACKAGE__->set_primary_key('rfam_acc','type');

1;
