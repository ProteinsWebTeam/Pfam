package DfamLive::Schema::Result::NcbiTaxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::NcbiTaxonomy

=cut

__PACKAGE__->table("ncbi_taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 species

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "species",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 0 },
);
__PACKAGE__->set_primary_key("ncbi_taxid");

=head1 RELATIONS

=head2 dfamseqs

Type: has_many

Related object: L<DfamLive::Schema::Result::Dfamseq>

=cut

__PACKAGE__->has_many(
  "dfamseqs",
  "DfamLive::Schema::Result::Dfamseq",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 taxonomies

Type: has_many

Related object: L<DfamLive::Schema::Result::Taxonomy>

=cut

__PACKAGE__->has_many(
  "taxonomies",
  "DfamLive::Schema::Result::Taxonomy",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:18:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:DNhoi623ISIXstpnWYX6Pw
# These lines were loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/NcbiTaxonomy.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package DfamLive::Schema::Result::NcbiTaxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::NcbiTaxonomy

=cut

__PACKAGE__->table("ncbi_taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 species

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "species",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 0 },
);
__PACKAGE__->set_primary_key("ncbi_taxid");

=head1 RELATIONS

=head2 dfamseqs

Type: has_many

Related object: L<DfamLive::Schema::Result::Dfamseq>

=cut

__PACKAGE__->has_many(
  "dfamseqs",
  "DfamLive::Schema::Result::Dfamseq",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 taxonomies

Type: has_many

Related object: L<DfamLive::Schema::Result::Taxonomy>

=cut

__PACKAGE__->has_many(
  "taxonomies",
  "DfamLive::Schema::Result::Taxonomy",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:12:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XsjqCOKpdeyc+RhMYzGL2A


# You can replace this text with custom content, and it will be preserved on regeneration
1;
# End of lines loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/NcbiTaxonomy.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
