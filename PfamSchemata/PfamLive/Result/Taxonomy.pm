use utf8;
package PfamLive::Result::Taxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Taxonomy

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<taxonomy>

=cut

__PACKAGE__->table("taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=head2 species

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 lft

  data_type: 'integer'
  is_nullable: 1

=head2 rgt

  data_type: 'integer'
  is_nullable: 1

=head2 parent

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 level

  data_type: 'varchar'
  is_nullable: 1
  size: 200

=head2 minimal

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 rank

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=cut

__PACKAGE__->add_columns(
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "species",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "lft",
  { data_type => "integer", is_nullable => 1 },
  "rgt",
  { data_type => "integer", is_nullable => 1 },
  "parent",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "level",
  { data_type => "varchar", is_nullable => 1, size => 200 },
  "minimal",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "rank",
  { data_type => "varchar", is_nullable => 1, size => 100 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:TkLuWTFfl8vwuovYfsQjxw
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Taxonomy.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::Taxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Taxonomy

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<taxonomy>

=cut

__PACKAGE__->table("taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=head2 species

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 lft

  data_type: 'integer'
  is_nullable: 1

=head2 rgt

  data_type: 'integer'
  is_nullable: 1

=head2 parent

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 level

  data_type: 'varchar'
  is_nullable: 1
  size: 200

=head2 minimal

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 rank

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=cut

__PACKAGE__->add_columns(
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "species",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "lft",
  { data_type => "integer", is_nullable => 1 },
  "rgt",
  { data_type => "integer", is_nullable => 1 },
  "parent",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "level",
  { data_type => "varchar", is_nullable => 1, size => 200 },
  "minimal",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "rank",
  { data_type => "varchar", is_nullable => 1, size => 100 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:2hL2hrhMnlVof1XRRNeb3Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Taxonomy.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
