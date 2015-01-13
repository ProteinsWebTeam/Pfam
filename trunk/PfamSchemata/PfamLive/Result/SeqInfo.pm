use utf8;
package PfamLive::Result::SeqInfo;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::SeqInfo

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<seq_info>

=cut

__PACKAGE__->table("seq_info");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 description

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_description

  data_type: 'text'
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_description",
  { data_type => "text", is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:HCfevZEYsooyy7pW8bGp0g
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/SeqInfo.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::SeqInfo;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::SeqInfo

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<seq_info>

=cut

__PACKAGE__->table("seq_info");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 description

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_description

  data_type: 'text'
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_description",
  { data_type => "text", is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-09-22 17:06:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:L8OILJgCosxZm66ruvRbEQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/SeqInfo.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
