use utf8;
package PfamLive::Result::PfamseqMarkup;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamseqMarkup

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamseq_markup>

=cut

__PACKAGE__->table("pfamseq_markup");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 auto_markup

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 residue

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 annotation

  data_type: 'text'
  is_nullable: 1

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 1
  size: 8

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "auto_markup",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "residue",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "annotation",
  { data_type => "text", is_nullable => 1 },
  "pfama_acc",
  { data_type => "varchar", is_nullable => 1, size => 8 },
);

=head1 RELATIONS

=head2 auto_markup

Type: belongs_to

Related object: L<PfamLive::Result::MarkupKey>

=cut

__PACKAGE__->belongs_to(
  "auto_markup",
  "PfamLive::Result::MarkupKey",
  { auto_markup => "auto_markup" },
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


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2016-07-13 13:29:16
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:WG72pRHzxhs/2iqQS/9mlA

__PACKAGE__->has_one( "markup" => 'PfamLive::Result::MarkupKey',
 	{ 'foreign.auto_markup' => 'self.auto_markup' },
 	{ cascade_delete => 0,
 	proxy => [qw(label)] } );

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
