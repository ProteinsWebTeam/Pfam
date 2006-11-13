
package PfamDB::Pdb_pfamA_reg;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_pfamA_reg" );
__PACKAGE__->add_columns( qw/auto_pdb_reg auto_pfamA_reg_full auto_pdb auto_pfamA auto_pfamseq chain pdb_res_start pdb_res_end seq_start seq_end/ );
__PACKAGE__->set_primary_key( "auto_pdb_reg" );

__PACKAGE__->has_one( "pfamA" => "PfamDB::Pfam",
		      { "foreign.auto_pfamA" => "self.auto_pfamA" },
		       { proxy => [ qw/pfamA_id pfamA_acc description/ ] } );

__PACKAGE__->has_one( "pdb"  => "PfamDB::Pdb",
		      { "foreign.auto_pdb"   => "self.auto_pdb" },
		      { proxy => [ qw/pdb_id header title pdb_image/ ] } );

__PACKAGE__->has_one( "pfamseq" => "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq" => "self.auto_pfamseq"},
		      { proxy => [ qw/pfamseq_id pfamseq_acc genome_seq ncbi_code/]});

__PACKAGE__->has_one( "clanMembers" => "PfamDB::Clan_membership",
		      { "foreign.auto_pfamA" => "self.auto_pfamA" });

1;

