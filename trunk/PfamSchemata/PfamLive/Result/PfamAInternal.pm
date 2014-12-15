use utf8;
package PfamLive::Result::PfamAInternal;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamAInternal

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_pfamA_internal>

=cut

__PACKAGE__->table("_pfamA_internal");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 created_by

  data_type: 'tinytext'
  is_nullable: 1

=head2 iterated

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 iteration_gain

  data_type: 'integer'
  default_value: 0
  is_nullable: 1

=head2 iterated_by

  data_type: 'tinytext'
  is_nullable: 1

=head2 iteration_date

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 seed

  data_type: 'longblob'
  is_nullable: 1

=head2 full

  data_type: 'longblob'
  is_nullable: 1

=head2 seed_is_ref_proteome

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "created_by",
  { data_type => "tinytext", is_nullable => 1 },
  "iterated",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "iteration_gain",
  { data_type => "integer", default_value => 0, is_nullable => 1 },
  "iterated_by",
  { data_type => "tinytext", is_nullable => 1 },
  "iteration_date",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "seed",
  { data_type => "longblob", is_nullable => 1 },
  "full",
  { data_type => "longblob", is_nullable => 1 },
  "seed_is_ref_proteome",
  { data_type => "tinyint", default_value => 0, is_nullable => 1 },
);

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:nWR7R3aEmzgLA3VEK4Kd8Q

sub msas_uncompressed {
  my $self = shift;

  #open(A, '>', 'ALIGN') or die "Failed to open ALIGN:[$!]\n";
  #print A Compress::Zlib::memGunzip($self->full) or warn "Failed to uncompress hits: $gzerrno";
  #close(A);
  
   
  open(A, '>', 'ALIGN.gz') or die "Failed to open ALIGN:[$!]\n";
  print A $self->full;
  close(A);
  system("gunzip ALIGN.gz") and die "Failed to uncompress ALIGN!\n";
 
  #Seed is always small, so do in memory.
  open(S, '>', 'SEED') or die "Failed to open SEED for writing:[$!]\n";
#  print S Compress::Zlib::memGunzip($self->seed) or warn "Failed to uncompress hits: $gzerrno";
  print S Compress::Zlib::memGunzip($self->seed) or warn "Failed to uncompress SEED:[$!]\n";
  close(S);

} 



# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
