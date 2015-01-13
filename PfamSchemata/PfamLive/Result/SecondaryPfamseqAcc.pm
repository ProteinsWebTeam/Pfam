use utf8;
package PfamLive::Result::SecondaryPfamseqAcc;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::SecondaryPfamseqAcc

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<secondary_pfamseq_acc>

=cut

__PACKAGE__->table("secondary_pfamseq_acc");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 secondary_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "secondary_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
);

=head1 RELATIONS

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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:BAUoAlo9DhQ3MaKQ1nvnvg
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/SecondaryPfamseqAcc.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::SecondaryPfamseqAcc;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::SecondaryPfamseqAcc

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<secondary_pfamseq_acc>

=cut

__PACKAGE__->table("secondary_pfamseq_acc");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 secondary_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "secondary_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
);

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-09-22 17:06:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:aaEO2RIvt4eKzcwLh4mN5Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/SecondaryPfamseqAcc.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
