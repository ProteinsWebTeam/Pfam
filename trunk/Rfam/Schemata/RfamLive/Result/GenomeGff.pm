use utf8;
package RfamLive::Result::GenomeGff;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::GenomeGff

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<genome_gff>

=cut

__PACKAGE__->table("genome_gff");

=head1 ACCESSORS

=head2 genome_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 20

=head2 gff3

  data_type: 'longblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "genome_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 20 },
  "gff3",
  { data_type => "longblob", is_nullable => 1 },
);

=head1 RELATIONS

=head2 genome_acc

Type: belongs_to

Related object: L<RfamLive::Result::Genome>

=cut

__PACKAGE__->belongs_to(
  "genome_acc",
  "RfamLive::Result::Genome",
  { genome_acc => "genome_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:MaI/YSlo4lXQWtfJvWpOXA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
