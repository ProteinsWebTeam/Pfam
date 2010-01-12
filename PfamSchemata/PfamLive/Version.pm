package PfamLive::Version;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("VERSION");
__PACKAGE__->add_columns(
  "pfam_release",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "pfam_release_date",
  { data_type => "DATE", default_value => undef, is_nullable => 1, size => 10 },
  "swiss_prot_version",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "trembl_version",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "hmmer_version",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "pfama_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "pfamb_additional_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "pfama_residue_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "pfamb_additional_residue_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "number_families",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:9q+Z/Gv/sJhTPVKC1L56tg

__PACKAGE__->set_primary_key( qw/pfam_release swiss_prot_version trembl_version hmmer_version/);

# You can replace this text with custom content, and it will be preserved on regeneration
1;
