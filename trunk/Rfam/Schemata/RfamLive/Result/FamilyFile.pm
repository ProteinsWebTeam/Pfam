use utf8;
package RfamLive::Result::FamilyFile;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::FamilyFile

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_family_file>

=cut

__PACKAGE__->table("_family_file");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 seed

  data_type: 'longblob'
  is_nullable: 0

=head2 cm

  data_type: 'longblob'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "seed",
  { data_type => "longblob", is_nullable => 0 },
  "cm",
  { data_type => "longblob", is_nullable => 0 },
);

=head1 RELATIONS

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:N6uku5X8JBlXO35Jbn5Siw


use Compress::Zlib;

sub unzipped_cm {
  my $self = shift;
  my $compressed = $self->cm;
  my $cm = Compress::Zlib::memGunzip($compressed) or
    carp("Failed to uncompress cm: $gzerrno");
  return $cm;
}

sub unzipped_seed {
  my $self = shift;
  my $compressed = $self->seed;
  my $seed = Compress::Zlib::memGunzip($compressed) or 
    carp( "Failed to uncompress hmm: $gzerrno");
  return $seed;
}

1;
