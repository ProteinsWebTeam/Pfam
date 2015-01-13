use utf8;
package PfamLive::Result::Pdb;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Pdb

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb>

=cut

__PACKAGE__->table("pdb");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_nullable: 0
  size: 5

=head2 keywords

  data_type: 'tinytext'
  is_nullable: 1

=head2 title

  data_type: 'mediumtext'
  is_nullable: 1

=head2 date

  data_type: 'tinytext'
  is_nullable: 1

=head2 resolution

  data_type: 'decimal'
  default_value: 0.00
  is_nullable: 1
  size: [5,2]

=head2 method

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_nullable => 0, size => 5 },
  "keywords",
  { data_type => "tinytext", is_nullable => 1 },
  "title",
  { data_type => "mediumtext", is_nullable => 1 },
  "date",
  { data_type => "tinytext", is_nullable => 1 },
  "resolution",
  {
    data_type => "decimal",
    default_value => "0.00",
    is_nullable => 1,
    size => [5, 2],
  },
  "method",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pdb_id>

=back

=cut

__PACKAGE__->set_primary_key("pdb_id");

=head1 RELATIONS

=head2 pdb_images

Type: has_many

Related object: L<PfamLive::Result::PdbImage>

=cut

__PACKAGE__->has_many(
  "pdb_images",
  "PfamLive::Result::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_pfam_a_regs

Type: has_many

Related object: L<PfamLive::Result::PdbPfamAReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfam_a_regs",
  "PfamLive::Result::PdbPfamAReg",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_pfam_b_regs

Type: has_many

Related object: L<PfamLive::Result::PdbPfamBReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfam_b_regs",
  "PfamLive::Result::PdbPfamBReg",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_residue_datas

Type: has_many

Related object: L<PfamLive::Result::PdbResidueData>

=cut

__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamLive::Result::PdbResidueData",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:MnrMVuF7KGxmEwT9tR7iPw
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Pdb.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::Pdb;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Pdb

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb>

=cut

__PACKAGE__->table("pdb");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_nullable: 0
  size: 5

=head2 keywords

  data_type: 'tinytext'
  is_nullable: 1

=head2 title

  data_type: 'mediumtext'
  is_nullable: 1

=head2 date

  data_type: 'tinytext'
  is_nullable: 1

=head2 resolution

  data_type: 'decimal'
  default_value: 0.00
  is_nullable: 1
  size: [5,2]

=head2 method

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_nullable => 0, size => 5 },
  "keywords",
  { data_type => "tinytext", is_nullable => 1 },
  "title",
  { data_type => "mediumtext", is_nullable => 1 },
  "date",
  { data_type => "tinytext", is_nullable => 1 },
  "resolution",
  {
    data_type => "decimal",
    default_value => "0.00",
    is_nullable => 1,
    size => [5, 2],
  },
  "method",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pdb_id>

=back

=cut

__PACKAGE__->set_primary_key("pdb_id");

=head1 RELATIONS

=head2 pdb_images

Type: has_many

Related object: L<PfamLive::Result::PdbImage>

=cut

__PACKAGE__->has_many(
  "pdb_images",
  "PfamLive::Result::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_pfam_a_regs

Type: has_many

Related object: L<PfamLive::Result::PdbPfamAReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfam_a_regs",
  "PfamLive::Result::PdbPfamAReg",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_pfam_b_regs

Type: has_many

Related object: L<PfamLive::Result::PdbPfamBReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfam_b_regs",
  "PfamLive::Result::PdbPfamBReg",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_residue_datas

Type: has_many

Related object: L<PfamLive::Result::PdbResidueData>

=cut

__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamLive::Result::PdbResidueData",
  { "foreign.pdb_id" => "self.pdb_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:WmwK+MqDJlOSxFmwWtR1BQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Pdb.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
