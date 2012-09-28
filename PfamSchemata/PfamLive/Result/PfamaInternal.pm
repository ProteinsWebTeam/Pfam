package PfamLive::Result::PfamaInternal;

use strict;
use warnings;

use base 'DBIx::Class::Core';

__PACKAGE__->table("_pfamA_internal");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "created_by",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "iterated",
  { data_type => "TINYINT", default_value => 0, is_nullable => 0, size => 1 },
  "iteration_gain",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "iterated_by",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "iteration_date",
  {
    data_type => "DATETIME",
    default_value => undef,
    is_nullable => 1,
    size => 19,
  },
  "seed",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "full",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("auto_pfama");
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Result::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-22 14:27:13
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:QTQKQJ6T+XIw+0KDM1B/rQ


# You can replace this text with custom content, and it will be preserved on regeneration
sub seed_uncompressed {
  my ($self, $path) = @_;
  $path = "." unless($path);
  open( S, ">", "$path/SEED" ) or die "Could not open $path/SEED:[$!]\n" ;
  print S Compress::Zlib::memGunzip( $self->seed );
  close S;
  unless ( -e "$path/SEED" and -s "$path/SEED" ) {
    die "Failed to generate SEED alignment";
  }
}

sub full_uncompressed {
  my ($self, $path) = @_;
  $path = "." unless($path);
  open( A, ">", "$path/ALIGN") or die "Could not open $path/ALIGN:[$!]\n";
  print A Compress::Zlib::memGunzip( $self->full );
  close A;
  unless ( -e "$path/ALIGN" and -s "$path/ALIGN" ) {
    die "Failed to generate ALIGN alignment";
  }
}
  
sub msas_uncompressed {
  my($self, $path) = @_;
  $self->seed_uncompressed($path);
  $self->full_uncompressed($path); 
}

1;
