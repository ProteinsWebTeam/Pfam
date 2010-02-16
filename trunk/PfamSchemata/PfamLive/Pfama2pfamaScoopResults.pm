package PfamLive::Pfama2pfamaScoopResults;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA2pfamA_scoop_results");
__PACKAGE__->add_columns(
  "auto_pfama1",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfama2",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "score",
  { data_type => "FLOAT", default_value => 0, is_nullable => 0, size => 32 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6dB1NrW1CIVL+INW5m1itw

__PACKAGE__->has_one( 
  'pfamA1',
  'PfamLive::Pfama',
  { 'foreign.auto_pfama' => 'self.auto_pfama1' } 
);

__PACKAGE__->has_one( 
  'pfamA2',
  'PfamLive::Pfama',
  { 'foreign.auto_pfama' => 'self.auto_pfama2' } 
);

# You can replace this text with custom content, and it will be preserved on regeneration
1;
