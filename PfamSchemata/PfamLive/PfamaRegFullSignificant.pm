package PfamLive::PfamaRegFullSignificant;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_reg_full_significant");
__PACKAGE__->add_columns(
  "auto_pfama_reg_full",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "ali_start",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 0,
    size => 8,
  },
  "ali_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 0,
    size => 8,
  },
  "model_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "domain_bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.00",
    is_nullable => 0,
    size => 64,
  },
  "domain_evalue_score",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 15,
  },
  "sequence_bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.00",
    is_nullable => 0,
    size => 64,
  },
  "sequence_evalue_score",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 15,
  },
  "cigar",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "in_full",
  { data_type => "TINYINT", default_value => 0, is_nullable => 0, size => 4 },
  "tree_order",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 9,
  },
  "domain_order",
    { data_type => "TINYINT", default_value => 0, is_nullable => 1, size => 4 },
);
__PACKAGE__->set_primary_key("auto_pfama_reg_full");
__PACKAGE__->has_many(
  "pdb_pfama_regs",
  "PfamLive::PdbPfamaReg",
  { "foreign.auto_pfama_reg_full" => "self.auto_pfama_reg_full" },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Msk8B3fZGezXYYHD4+XC3g

__PACKAGE__->has_one( pfamseq =>  'PfamLive::Pfamseq',
                      { 'foreign.auto_pfamseq'  => 'self.auto_pfamseq' },
                                            { proxy => [ qw( auto_pfamseq pfamseq_acc
							     pfamseq_id
							     seq_version
							     crc64 
							     md5 
							     description
							     length
							     species 
							     taxonomy
							     ncbi_taxid 
							     sequence
                   is_fragment
                   auto_architecture
							     updated
							     created ) ] } );

__PACKAGE__->has_one( pfama =>  'PfamLive::Pfama',
                      { 'foreign.auto_pfama'  => 'self.auto_pfama' },
                                            { proxy => [ qw( auto_pfama pfama_acc
							     pfama_id
							     model_length
							     version 
							     description
							     type
							     updated
							     created ) ] } );


__PACKAGE__->might_have(
  "clan_membership" => 'PfamLive::ClanMembership',
			{ 'foreign.auto_pfama' => 'self.auto_pfama' } );

__PACKAGE__->might_have(
  "interactions" => 'PfamLive::PfamaInteractions',
			{ 'foreign.auto_pfama_a' => 'self.auto_pfama' } );

__PACKAGE__->might_have(
  "markup" => 'PfamLive::PfamseqMarkup',
			{ 'foreign.auto_pfamseq' => 'self.auto_pfamseq'});

__PACKAGE__->has_one( "pfama" => "PfamLive::Pfama",
                     { "foreign.auto_pfama" => "self.auto_pfama"},
                      { proxy => [qw(pfama_id pfama_acc)]});

# You can replace this text with custom content, and it will be preserved on regeneration
1;
