use utf8;
package PfamLive::Result::Edit;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Edit

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<edits>

=cut

__PACKAGE__->table("edits");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 1

=head2 original_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 original_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 new_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 new_end

  data_type: 'mediumint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 1 },
  "original_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "original_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "new_start",
  { data_type => "mediumint", is_nullable => 1 },
  "new_end",
  { data_type => "mediumint", is_nullable => 1 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamLive::Result::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamLive::Result::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hirHM4Ia1IuZB8lbc8llTw
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Edit.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::Edit;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Edit

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<edits>

=cut

__PACKAGE__->table("edits");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 1

=head2 original_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 original_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 new_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 new_end

  data_type: 'mediumint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 1 },
  "original_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "original_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "new_start",
  { data_type => "mediumint", is_nullable => 1 },
  "new_end",
  { data_type => "mediumint", is_nullable => 1 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamLive::Result::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamLive::Result::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hOJ+5WrHUphbPCP9h5Z47w


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Edit.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
