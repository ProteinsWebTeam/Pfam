use utf8;
package PfamLive::Result::PfamBStockholm;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamBStockholm

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamB_stockholm>

=cut

__PACKAGE__->table("pfamB_stockholm");

=head1 ACCESSORS

=head2 pfamb_acc

  data_type: 'char'
  is_foreign_key: 1
  is_nullable: 0
  size: 8

=head2 stockholm_data

  data_type: 'longtext'
  is_nullable: 0

=head2 jtml

  data_type: 'longblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfamb_acc",
  { data_type => "char", is_foreign_key => 1, is_nullable => 0, size => 8 },
  "stockholm_data",
  { data_type => "longtext", is_nullable => 0 },
  "jtml",
  { data_type => "longblob", is_nullable => 1 },
);

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:HIYUqrRDHba9wHY/z/MCCQ
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamBStockholm.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PfamBStockholm;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamBStockholm

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamB_stockholm>

=cut

__PACKAGE__->table("pfamB_stockholm");

=head1 ACCESSORS

=head2 pfamb_acc

  data_type: 'char'
  is_foreign_key: 1
  is_nullable: 0
  size: 8

=head2 stockholm_data

  data_type: 'longtext'
  is_nullable: 0

=head2 jtml

  data_type: 'longblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfamb_acc",
  { data_type => "char", is_foreign_key => 1, is_nullable => 0, size => 8 },
  "stockholm_data",
  { data_type => "longtext", is_nullable => 0 },
  "jtml",
  { data_type => "longblob", is_nullable => 1 },
);

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ybR/mm51Twcf8xI3KuXPEg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamBStockholm.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
