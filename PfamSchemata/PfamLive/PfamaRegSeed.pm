package PfamLive::PfamaRegSeed;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_reg_seed");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 0,
    size => 8,
  },
  "cigar",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "tree_order",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 9,
  },
);
__PACKAGE__->add_unique_constraint(
  "pfamA_reg_seed_reg_idx",
  ["auto_pfama", "auto_pfamseq", "seq_start", "seq_end"],
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:p7pgogJbzXN6B35w0Ra+nw

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfama", "auto_pfamseq", "seq_start");

__PACKAGE__->has_one( "pfamseq" =>  "PfamLive::Pfamseq",
          { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
                    { proxy => [ qw ( pfamseq_acc pfamseq_id seq_version auto_pfamseq) ] } );

__PACKAGE__->has_one( "pfama" => "PfamLive::Pfama",
                    { "foreign.auto_pfama" => "self.auto_pfama"},
                      { proxy => [qw(pfama_id pfama_acc)]});

__PACKAGE__->might_have(
  "clan_membership" => 'PfamLive::ClanMembership',
			{ 'foreign.auto_pfama' => 'self.auto_pfama' } );



# You can replace this text with custom content, and it will be preserved on regeneration
1;
