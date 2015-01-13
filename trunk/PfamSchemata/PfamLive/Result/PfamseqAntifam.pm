use utf8;
package PfamLive::Result::PfamseqAntifam;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamseqAntifam

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamseq_antifam>

=cut

__PACKAGE__->table("pfamseq_antifam");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 0

=head2 crc64

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 md5

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 evidence

  data_type: 'tinyint'
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

=head2 is_fragment

  data_type: 'tinyint'
  is_nullable: 1

=head2 sequence

  accessor: undef
  data_type: 'blob'
  is_nullable: 0

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 antifam_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 8

=head2 antifam_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "crc64",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "evidence",
  { data_type => "tinyint", is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "is_fragment",
  { data_type => "tinyint", is_nullable => 1 },
  "sequence",
  { accessor => undef, data_type => "blob", is_nullable => 0 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "antifam_acc",
  { data_type => "varchar", is_nullable => 0, size => 8 },
  "antifam_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfamseq_acc>

=back

=cut

__PACKAGE__->set_primary_key("pfamseq_acc");

=head1 RELATIONS

=head2 ncbi_taxid

Type: belongs_to

Related object: L<PfamLive::Result::NcbiTaxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamLive::Result::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Ag/kWrSM06DXiyj7PqZuOg
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamseqAntifam.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PfamseqAntifam;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamseqAntifam

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamseq_antifam>

=cut

__PACKAGE__->table("pfamseq_antifam");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 0

=head2 crc64

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 md5

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 evidence

  data_type: 'tinyint'
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

=head2 is_fragment

  data_type: 'tinyint'
  is_nullable: 1

=head2 sequence

  accessor: undef
  data_type: 'blob'
  is_nullable: 0

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 antifam_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 8

=head2 antifam_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "crc64",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "evidence",
  { data_type => "tinyint", is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "is_fragment",
  { data_type => "tinyint", is_nullable => 1 },
  "sequence",
  { accessor => undef, data_type => "blob", is_nullable => 0 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "antifam_acc",
  { data_type => "varchar", is_nullable => 0, size => 8 },
  "antifam_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfamseq_acc>

=back

=cut

__PACKAGE__->set_primary_key("pfamseq_acc");

=head1 RELATIONS

=head2 ncbi_taxid

Type: belongs_to

Related object: L<PfamLive::Result::NcbiTaxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamLive::Result::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-09-22 17:06:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:F8vgYtual/0gbgo2ungQ3g


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamseqAntifam.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
