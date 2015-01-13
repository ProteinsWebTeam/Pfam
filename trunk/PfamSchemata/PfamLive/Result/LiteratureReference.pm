use utf8;
package PfamLive::Result::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::LiteratureReference

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<literature_reference>

=cut

__PACKAGE__->table("literature_reference");

=head1 ACCESSORS

=head2 auto_lit

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 pmid

  data_type: 'integer'
  is_nullable: 1

=head2 title

  data_type: 'mediumtext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=head2 journal

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_lit",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "pmid",
  { data_type => "integer", is_nullable => 1 },
  "title",
  { data_type => "mediumtext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_lit>

=back

=cut

__PACKAGE__->set_primary_key("auto_lit");

=head1 UNIQUE CONSTRAINTS

=head2 C<IX_literature_references_1>

=over 4

=item * L</pmid>

=back

=cut

__PACKAGE__->add_unique_constraint("IX_literature_references_1", ["pmid"]);

=head1 RELATIONS

=head2 clan_lit_refs

Type: has_many

Related object: L<PfamLive::Result::ClanLitRef>

=cut

__PACKAGE__->has_many(
  "clan_lit_refs",
  "PfamLive::Result::ClanLitRef",
  { "foreign.auto_lit" => "self.auto_lit" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_literature_references

Type: has_many

Related object: L<PfamLive::Result::PfamALiteratureReference>

=cut

__PACKAGE__->has_many(
  "pfam_a_literature_references",
  "PfamLive::Result::PfamALiteratureReference",
  { "foreign.auto_lit" => "self.auto_lit" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:zsg0gimYQMZzhnSiz6lzJA
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/LiteratureReference.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::LiteratureReference

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<literature_reference>

=cut

__PACKAGE__->table("literature_reference");

=head1 ACCESSORS

=head2 auto_lit

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 pmid

  data_type: 'integer'
  is_nullable: 1

=head2 title

  data_type: 'mediumtext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=head2 journal

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_lit",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "pmid",
  { data_type => "integer", is_nullable => 1 },
  "title",
  { data_type => "mediumtext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_lit>

=back

=cut

__PACKAGE__->set_primary_key("auto_lit");

=head1 UNIQUE CONSTRAINTS

=head2 C<IX_literature_references_1>

=over 4

=item * L</pmid>

=back

=cut

__PACKAGE__->add_unique_constraint("IX_literature_references_1", ["pmid"]);

=head1 RELATIONS

=head2 clan_lit_refs

Type: has_many

Related object: L<PfamLive::Result::ClanLitRef>

=cut

__PACKAGE__->has_many(
  "clan_lit_refs",
  "PfamLive::Result::ClanLitRef",
  { "foreign.auto_lit" => "self.auto_lit" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_literature_references

Type: has_many

Related object: L<PfamLive::Result::PfamALiteratureReference>

=cut

__PACKAGE__->has_many(
  "pfam_a_literature_references",
  "PfamLive::Result::PfamALiteratureReference",
  { "foreign.auto_lit" => "self.auto_lit" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:lqTXKrsWEci8A23lFnDwzA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/LiteratureReference.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
