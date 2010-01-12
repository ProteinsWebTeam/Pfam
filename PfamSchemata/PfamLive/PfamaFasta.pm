package PfamLive::PfamaFasta;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_fasta");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
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
__PACKAGE__->add_unique_constraint("UQ_pfamA_fasta_1", ["auto_pfama"]);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UwGxESgGxgWTI9QuZ62JBg

__PACKAGE__->set_primary_key('auto_pfama');

# You can replace this text with custom content, and it will be preserved on regeneration
1;
