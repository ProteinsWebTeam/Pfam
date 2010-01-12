package PfamLive::ReleasedPfamVersion;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("released_pfam_version");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
  "seed",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 32,
  },
  "align",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 32,
  },
  "desc_file",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 32,
  },
  "hmm",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 32,
  },
  "version",
  {
    data_type => "SMALLINT",
    default_value => undef,
    is_nullable => 1,
    size => 5,
  },
);
__PACKAGE__->set_primary_key("auto_pfama");
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i3kocVIdTC1G7crDl1ZVBw
__PACKAGE__->has_one( pfama =>  'PfamLive::Pfama',
                      { 'foreign.auto_pfama'  => 'self.auto_pfama' },
                                            { proxy => [ qw( auto_pfama 
                                                             pfama_acc
                                                             pfama_id ) ] } );




# You can replace this text with custom content, and it will be preserved on regeneration
1;
