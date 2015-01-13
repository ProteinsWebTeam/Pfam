use utf8;
package PfamLive::Result::PfamseqNcbi;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamseqNcbi

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamseq_ncbi>

=cut

__PACKAGE__->table("pfamseq_ncbi");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
);

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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LXeDCfhEb6LxNi29LHRnWA
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamseqNcbi.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PfamseqNcbi;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamseqNcbi

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamseq_ncbi>

=cut

__PACKAGE__->table("pfamseq_ncbi");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
);

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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:MpAlrJ5niihivFaJ43u7cg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamseqNcbi.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
