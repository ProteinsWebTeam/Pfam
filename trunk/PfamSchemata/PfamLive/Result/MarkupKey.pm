use utf8;
package PfamLive::Result::MarkupKey;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::MarkupKey

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<markup_key>

=cut

__PACKAGE__->table("markup_key");

=head1 ACCESSORS

=head2 auto_markup

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 label

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=cut

__PACKAGE__->add_columns(
  "auto_markup",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "label",
  { data_type => "varchar", is_nullable => 1, size => 50 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_markup>

=back

=cut

__PACKAGE__->set_primary_key("auto_markup");

=head1 RELATIONS

=head2 pfamseq_markups

Type: has_many

Related object: L<PfamLive::Result::PfamseqMarkup>

=cut

__PACKAGE__->has_many(
  "pfamseq_markups",
  "PfamLive::Result::PfamseqMarkup",
  { "foreign.auto_markup" => "self.auto_markup" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:5MACfHyQDxtM/r8Du85aVw
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/MarkupKey.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::MarkupKey;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::MarkupKey

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<markup_key>

=cut

__PACKAGE__->table("markup_key");

=head1 ACCESSORS

=head2 auto_markup

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 label

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=cut

__PACKAGE__->add_columns(
  "auto_markup",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "label",
  { data_type => "varchar", is_nullable => 1, size => 50 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_markup>

=back

=cut

__PACKAGE__->set_primary_key("auto_markup");

=head1 RELATIONS

=head2 pfamseq_markups

Type: has_many

Related object: L<PfamLive::Result::PfamseqMarkup>

=cut

__PACKAGE__->has_many(
  "pfamseq_markups",
  "PfamLive::Result::PfamseqMarkup",
  { "foreign.auto_markup" => "self.auto_markup" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:J3YSsFvabJVyd8nZPakeMA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/MarkupKey.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
