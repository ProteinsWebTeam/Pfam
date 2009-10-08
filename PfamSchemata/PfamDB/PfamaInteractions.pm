package PfamDB::PfamaInteractions;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_interactions");
__PACKAGE__->add_columns(
  "auto_pfama_a",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_pfama_b",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:do6T35QdwsMj5EIok8v9kw


__PACKAGE__->has_one( auto_pfama_b => 'PfamDB::Pfama',
                      { 'foreign.auto_pfama' => 'self.auto_pfama1' } );


__PACKAGE__->has_one( auto_pfama_a => 'PfamDB::Pfama',
                      { 'foreign.auto_pfama'  => 'self.auto_pfama_a' },
                      { proxy =>  [ qw( pfama_id pfama_acc ) ] } );

__PACKAGE__->has_one( auto_pfama_b => 'PfamDB::Pfama',
                      { 'foreign.auto_pfama'  => 'self.auto_pfamA_B' },
                      { proxy => [ qw( pfama_id pfama_acc ) ] } );

__PACKAGE__->might_have( clan_membership => 'PfamDB::ClanMembership',
                         { 'foreign.auto_pfama' => 'self.auto_pfama_a' } );


1;
