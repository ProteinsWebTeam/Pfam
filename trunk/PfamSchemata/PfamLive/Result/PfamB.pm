use utf8;
package PfamLive::Result::PfamB;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamB

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamB>

=cut

__PACKAGE__->table("pfamB");

=head1 ACCESSORS

=head2 pfamb_acc

  data_type: 'char'
  default_value: (empty string)
  is_nullable: 0
  size: 8

=head2 pfamb_id

  data_type: 'char'
  default_value: (empty string)
  is_nullable: 1
  size: 15

=head2 number_archs

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_regions

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_species

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_structures

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfamb_acc",
  { data_type => "char", default_value => "", is_nullable => 0, size => 8 },
  "pfamb_id",
  { data_type => "char", default_value => "", is_nullable => 1, size => 15 },
  "number_archs",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_regions",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "number_species",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_structures",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfamb_acc>

=back

=cut

__PACKAGE__->set_primary_key("pfamb_acc");

=head1 UNIQUE CONSTRAINTS

=head2 C<pfamB_id>

=over 4

=item * L</pfamb_id>

=back

=cut

__PACKAGE__->add_unique_constraint("pfamB_id", ["pfamb_id"]);

=head1 RELATIONS

=head2 pdb_pfam_b_regs

Type: has_many

Related object: L<PfamLive::Result::PdbPfamBReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfam_b_regs",
  "PfamLive::Result::PdbPfamBReg",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_database_links

Type: has_many

Related object: L<PfamLive::Result::PfamBDatabaseLink>

=cut

__PACKAGE__->has_many(
  "pfam_b_database_links",
  "PfamLive::Result::PfamBDatabaseLink",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_fastas

Type: has_many

Related object: L<PfamLive::Result::PfamBFasta>

=cut

__PACKAGE__->has_many(
  "pfam_b_fastas",
  "PfamLive::Result::PfamBFasta",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_regs

Type: has_many

Related object: L<PfamLive::Result::PfamBReg>

=cut

__PACKAGE__->has_many(
  "pfam_b_regs",
  "PfamLive::Result::PfamBReg",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_species_trees

Type: has_many

Related object: L<PfamLive::Result::PfamBSpeciesTree>

=cut

__PACKAGE__->has_many(
  "pfam_b_species_trees",
  "PfamLive::Result::PfamBSpeciesTree",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_stockholms

Type: has_many

Related object: L<PfamLive::Result::PfamBStockholm>

=cut

__PACKAGE__->has_many(
  "pfam_b_stockholms",
  "PfamLive::Result::PfamBStockholm",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:5utNG7+uXA7MYVf79IcWlA
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamB.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PfamB;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamB

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamB>

=cut

__PACKAGE__->table("pfamB");

=head1 ACCESSORS

=head2 pfamb_acc

  data_type: 'char'
  default_value: (empty string)
  is_nullable: 0
  size: 8

=head2 pfamb_id

  data_type: 'char'
  default_value: (empty string)
  is_nullable: 1
  size: 15

=head2 number_archs

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_regions

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_species

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_structures

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfamb_acc",
  { data_type => "char", default_value => "", is_nullable => 0, size => 8 },
  "pfamb_id",
  { data_type => "char", default_value => "", is_nullable => 1, size => 15 },
  "number_archs",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_regions",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "number_species",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_structures",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfamb_acc>

=back

=cut

__PACKAGE__->set_primary_key("pfamb_acc");

=head1 UNIQUE CONSTRAINTS

=head2 C<pfamB_id>

=over 4

=item * L</pfamb_id>

=back

=cut

__PACKAGE__->add_unique_constraint("pfamB_id", ["pfamb_id"]);

=head1 RELATIONS

=head2 pdb_pfam_b_regs

Type: has_many

Related object: L<PfamLive::Result::PdbPfamBReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfam_b_regs",
  "PfamLive::Result::PdbPfamBReg",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_database_links

Type: has_many

Related object: L<PfamLive::Result::PfamBDatabaseLink>

=cut

__PACKAGE__->has_many(
  "pfam_b_database_links",
  "PfamLive::Result::PfamBDatabaseLink",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_fastas

Type: has_many

Related object: L<PfamLive::Result::PfamBFasta>

=cut

__PACKAGE__->has_many(
  "pfam_b_fastas",
  "PfamLive::Result::PfamBFasta",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_regs

Type: has_many

Related object: L<PfamLive::Result::PfamBReg>

=cut

__PACKAGE__->has_many(
  "pfam_b_regs",
  "PfamLive::Result::PfamBReg",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_species_trees

Type: has_many

Related object: L<PfamLive::Result::PfamBSpeciesTree>

=cut

__PACKAGE__->has_many(
  "pfam_b_species_trees",
  "PfamLive::Result::PfamBSpeciesTree",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_b_stockholms

Type: has_many

Related object: L<PfamLive::Result::PfamBStockholm>

=cut

__PACKAGE__->has_many(
  "pfam_b_stockholms",
  "PfamLive::Result::PfamBStockholm",
  { "foreign.pfamb_acc" => "self.pfamb_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:xfJ7PqZTTnATEndegFqbfA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamB.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
