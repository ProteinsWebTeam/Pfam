package DfamLive::Schema::Result::Dfamseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::Dfamseq

=cut

__PACKAGE__->table("dfamseq");

=head1 ACCESSORS

=head2 auto_dfamseq

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 dfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 dfamseq_acc

  data_type: 'varchar'
  is_nullable: 1
  size: 12

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 length

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 updated

  data_type: 'timestamp'
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  is_nullable: 1

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_dfamseq",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "dfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "dfamseq_acc",
  { data_type => "varchar", is_nullable => 1, size => 12 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "updated",
  {
    data_type     => "timestamp",
    default_value => \"current_timestamp",
    is_nullable   => 0,
  },
  "created",
  { data_type => "datetime", is_nullable => 1 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
);
__PACKAGE__->set_primary_key("auto_dfamseq");
__PACKAGE__->add_unique_constraint("dfamseq_acc", ["dfamseq_acc"]);

=head1 RELATIONS

=head2 dfam_reg_full_significants

Type: has_many

Related object: L<DfamLive::Schema::Result::DfamRegFullSignificant>

=cut

__PACKAGE__->has_many(
  "dfam_reg_full_significants",
  "DfamLive::Schema::Result::DfamRegFullSignificant",
  { "foreign.auto_dfamseq" => "self.auto_dfamseq" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dfam_reg_seeds

Type: has_many

Related object: L<DfamLive::Schema::Result::DfamRegSeed>

=cut

__PACKAGE__->has_many(
  "dfam_reg_seeds",
  "DfamLive::Schema::Result::DfamRegSeed",
  { "foreign.auto_dfamseq" => "self.auto_dfamseq" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 ncbi_taxid

Type: belongs_to

Related object: L<DfamLive::Schema::Result::NcbiTaxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "DfamLive::Schema::Result::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:18:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:JGWBQpTdJy5xJB/AJ3XmkA
# These lines were loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/Dfamseq.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package DfamLive::Schema::Result::Dfamseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::Dfamseq

=cut

__PACKAGE__->table("dfamseq");

=head1 ACCESSORS

=head2 auto_dfamseq

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 dfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 dfamseq_acc

  data_type: 'varchar'
  is_nullable: 1
  size: 12

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 length

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 updated

  data_type: 'timestamp'
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  is_nullable: 1

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_dfamseq",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "dfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "dfamseq_acc",
  { data_type => "varchar", is_nullable => 1, size => 12 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "updated",
  {
    data_type     => "timestamp",
    default_value => \"current_timestamp",
    is_nullable   => 0,
  },
  "created",
  { data_type => "datetime", is_nullable => 1 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
);
__PACKAGE__->set_primary_key("auto_dfamseq");
__PACKAGE__->add_unique_constraint("dfamseq_acc", ["dfamseq_acc"]);

=head1 RELATIONS

=head2 dfam_reg_full_significants

Type: has_many

Related object: L<DfamLive::Schema::Result::DfamRegFullSignificant>

=cut

__PACKAGE__->has_many(
  "dfam_reg_full_significants",
  "DfamLive::Schema::Result::DfamRegFullSignificant",
  { "foreign.auto_dfamseq" => "self.auto_dfamseq" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dfam_reg_seeds

Type: has_many

Related object: L<DfamLive::Schema::Result::DfamRegSeed>

=cut

__PACKAGE__->has_many(
  "dfam_reg_seeds",
  "DfamLive::Schema::Result::DfamRegSeed",
  { "foreign.auto_dfamseq" => "self.auto_dfamseq" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 ncbi_taxid

Type: belongs_to

Related object: L<DfamLive::Schema::Result::NcbiTaxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "DfamLive::Schema::Result::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:12:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:GaZJJsRHqC2a4J+3SXatFw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
# End of lines loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/Dfamseq.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
