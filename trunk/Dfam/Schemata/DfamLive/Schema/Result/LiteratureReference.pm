package DfamLive::Schema::Result::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::LiteratureReference

=cut

__PACKAGE__->table("literature_references");

=head1 ACCESSORS

=head2 pmid

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=head2 journal

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pmid",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("pmid");

=head1 RELATIONS

=head2 dfam_literature_references

Type: has_many

Related object: L<DfamLive::Schema::Result::DfamLiteratureReference>

=cut

__PACKAGE__->has_many(
  "dfam_literature_references",
  "DfamLive::Schema::Result::DfamLiteratureReference",
  { "foreign.pmid" => "self.pmid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:18:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bvgNvoZ+wBucBo5wBOGgfw
# These lines were loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/LiteratureReference.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package DfamLive::Schema::Result::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::LiteratureReference

=cut

__PACKAGE__->table("literature_references");

=head1 ACCESSORS

=head2 pmid

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=head2 journal

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pmid",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("pmid");

=head1 RELATIONS

=head2 dfam_literature_references

Type: has_many

Related object: L<DfamLive::Schema::Result::DfamLiteratureReference>

=cut

__PACKAGE__->has_many(
  "dfam_literature_references",
  "DfamLive::Schema::Result::DfamLiteratureReference",
  { "foreign.pmid" => "self.pmid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:12:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:FfG1I+pDxxOfYDag6+/uRg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
# End of lines loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/LiteratureReference.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
