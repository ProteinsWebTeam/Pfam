use utf8;
package PfamLive::Result::PdbPfamBReg;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PdbPfamBReg

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_pfamB_reg>

=cut

__PACKAGE__->table("pdb_pfamB_reg");

=head1 ACCESSORS

=head2 auto_pdb_reg

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 auto_pfamb_reg

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 pfamb_acc

  data_type: 'char'
  is_foreign_key: 1
  is_nullable: 0
  size: 8

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 chain

  data_type: 'varchar'
  is_nullable: 0
  size: 4

=head2 pdb_res_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_res_end

  data_type: 'mediumint'
  is_nullable: 1

=head2 seq_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 seq_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_pdb_reg",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "auto_pfamb_reg",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "pfamb_acc",
  { data_type => "char", is_foreign_key => 1, is_nullable => 0, size => 8 },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "chain",
  { data_type => "varchar", is_nullable => 0, size => 4 },
  "pdb_res_start",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_res_end",
  { data_type => "mediumint", is_nullable => 1 },
  "seq_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 1 },
  "seq_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_pdb_reg>

=back

=cut

__PACKAGE__->set_primary_key("auto_pdb_reg");

=head1 RELATIONS

=head2 auto_pfamb_reg

Type: belongs_to

Related object: L<PfamLive::Result::PfamBReg>

=cut

__PACKAGE__->belongs_to(
  "auto_pfamb_reg",
  "PfamLive::Result::PfamBReg",
  { auto_pfamb_reg => "auto_pfamb_reg" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "NO ACTION",
  },
);

=head2 pdb

Type: belongs_to

Related object: L<PfamLive::Result::Pdb>

=cut

__PACKAGE__->belongs_to(
  "pdb",
  "PfamLive::Result::Pdb",
  { pdb_id => "pdb_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pfamb_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamB>

=cut

__PACKAGE__->belongs_to(
  "pfamb_acc",
  "PfamLive::Result::PfamB",
  { pfamb_acc => "pfamb_acc" },
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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:++qAyu+py5G50wpWsXROag
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PdbPfamBReg.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PdbPfamBReg;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PdbPfamBReg

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_pfamB_reg>

=cut

__PACKAGE__->table("pdb_pfamB_reg");

=head1 ACCESSORS

=head2 auto_pdb_reg

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 auto_pfamb_reg

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 pfamb_acc

  data_type: 'char'
  is_foreign_key: 1
  is_nullable: 0
  size: 8

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 chain

  data_type: 'varchar'
  is_nullable: 0
  size: 4

=head2 pdb_res_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_res_end

  data_type: 'mediumint'
  is_nullable: 1

=head2 seq_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 seq_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_pdb_reg",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "auto_pfamb_reg",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "pfamb_acc",
  { data_type => "char", is_foreign_key => 1, is_nullable => 0, size => 8 },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "chain",
  { data_type => "varchar", is_nullable => 0, size => 4 },
  "pdb_res_start",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_res_end",
  { data_type => "mediumint", is_nullable => 1 },
  "seq_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 1 },
  "seq_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_pdb_reg>

=back

=cut

__PACKAGE__->set_primary_key("auto_pdb_reg");

=head1 RELATIONS

=head2 auto_pfamb_reg

Type: belongs_to

Related object: L<PfamLive::Result::PfamBReg>

=cut

__PACKAGE__->belongs_to(
  "auto_pfamb_reg",
  "PfamLive::Result::PfamBReg",
  { auto_pfamb_reg => "auto_pfamb_reg" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "NO ACTION",
  },
);

=head2 pdb

Type: belongs_to

Related object: L<PfamLive::Result::Pdb>

=cut

__PACKAGE__->belongs_to(
  "pdb",
  "PfamLive::Result::Pdb",
  { pdb_id => "pdb_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pfamb_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamB>

=cut

__PACKAGE__->belongs_to(
  "pfamb_acc",
  "PfamLive::Result::PfamB",
  { pfamb_acc => "pfamb_acc" },
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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZB40XFf80tD5hNrXhSKNIw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PdbPfamBReg.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
