package PfamLive::PfambFasta;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB_fasta");
__PACKAGE__->add_columns(
  "auto_pfamb",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 6 },
  "fasta",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "nr_threshold",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 3 },
);
__PACKAGE__->belongs_to(
  "auto_pfamb",
  "PfamLive::Pfamb",
  { auto_pfamb => "auto_pfamb" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:N2pMMZXADD16xjRW++1ojQ

__PACKAGE__->set_primary_key("auto_pfamb");

# You can replace this text with custom content, and it will be preserved on regeneration
1;
