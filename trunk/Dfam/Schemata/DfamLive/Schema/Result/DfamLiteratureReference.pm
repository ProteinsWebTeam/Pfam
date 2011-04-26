package DfamLive::Schema::Result::DfamLiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::DfamLiteratureReference

=cut

__PACKAGE__->table("dfam_literature_references");

=head1 ACCESSORS

=head2 dfam_acc

  data_type: 'varchar'
  default_value: 0
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pmid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 comment

  data_type: 'tinytext'
  is_nullable: 1

=head2 order_added

  data_type: 'tinyint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "dfam_acc",
  {
    data_type => "varchar",
    default_value => 0,
    is_foreign_key => 1,
    is_nullable => 0,
    size => 7,
  },
  "pmid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "comment",
  { data_type => "tinytext", is_nullable => 1 },
  "order_added",
  { data_type => "tinyint", is_nullable => 1 },
);

=head1 RELATIONS

=head2 dfam_acc

Type: belongs_to

Related object: L<DfamLive::Schema::Result::Dfam>

=cut

__PACKAGE__->belongs_to(
  "dfam_acc",
  "DfamLive::Schema::Result::Dfam",
  { dfam_acc => "dfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 pmid

Type: belongs_to

Related object: L<DfamLive::Schema::Result::LiteratureReference>

=cut

__PACKAGE__->belongs_to(
  "pmid",
  "DfamLive::Schema::Result::LiteratureReference",
  { pmid => "pmid" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:18:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bk6GmTY150b1cyc55YAtQg
# These lines were loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/DfamLiteratureReference.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package DfamLive::Schema::Result::DfamLiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::DfamLiteratureReference

=cut

__PACKAGE__->table("dfam_literature_references");

=head1 ACCESSORS

=head2 dfam_acc

  data_type: 'varchar'
  default_value: 0
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pmid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 comment

  data_type: 'tinytext'
  is_nullable: 1

=head2 order_added

  data_type: 'tinyint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "dfam_acc",
  {
    data_type => "varchar",
    default_value => 0,
    is_foreign_key => 1,
    is_nullable => 0,
    size => 7,
  },
  "pmid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "comment",
  { data_type => "tinytext", is_nullable => 1 },
  "order_added",
  { data_type => "tinyint", is_nullable => 1 },
);

=head1 RELATIONS

=head2 dfam_acc

Type: belongs_to

Related object: L<DfamLive::Schema::Result::Dfam>

=cut

__PACKAGE__->belongs_to(
  "dfam_acc",
  "DfamLive::Schema::Result::Dfam",
  { dfam_acc => "dfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 pmid

Type: belongs_to

Related object: L<DfamLive::Schema::Result::LiteratureReference>

=cut

__PACKAGE__->belongs_to(
  "pmid",
  "DfamLive::Schema::Result::LiteratureReference",
  { pmid => "pmid" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:12:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:KHMbCsYtYMcmP3Rstz48jA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
# End of lines loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/DfamLiteratureReference.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
