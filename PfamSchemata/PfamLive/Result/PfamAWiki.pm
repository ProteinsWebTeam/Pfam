use utf8;
package PfamLive::Result::PfamAWiki;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamAWiki

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_wiki>

=cut

__PACKAGE__->table("pfamA_wiki");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 auto_wiki

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "auto_wiki",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 auto_wiki

Type: belongs_to

Related object: L<PfamLive::Result::Wikipedia>

=cut

__PACKAGE__->belongs_to(
  "auto_wiki",
  "PfamLive::Result::Wikipedia",
  { auto_wiki => "auto_wiki" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2014-12-05 15:05:07
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:OBNNug4P7m5hvnLRIl+wig


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
