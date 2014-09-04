use utf8;
package RfamDB::Result::MotifFile;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::MotifFile

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<motif_file>

=cut

__PACKAGE__->table("motif_file");

=head1 ACCESSORS

=head2 motif_acc

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
  "motif_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "seed",
  { data_type => "longblob", is_nullable => 0 },
  "cm",
  { data_type => "longblob", is_nullable => 0 },
);

=head1 RELATIONS

=head2 motif_acc

Type: belongs_to

Related object: L<RfamDB::Result::Motif>

=cut

__PACKAGE__->belongs_to(
  "motif_acc",
  "RfamDB::Result::Motif",
  { motif_acc => "motif_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 10:52:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bCRNX4sYvVEGgEFpOUB0CQ


__PACKAGE__->set_primary_key('motif_acc');

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
    carp( "Failed to uncompress seed: $gzerrno");
  return $seed;
}

1;
