package PfamSchemata::PfamDB::Interactions;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("interactions"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_interactions auto_int_pfamAs auto_int_atoms auto_pdb chain_A pdb_seq_number_A auto_pfamseq_A pfamseq_acc_A auto_pfamA_A pfamA_acc_A pfamA_id_A pfamseq_seq_number_A chain_B pdb_seq_number_B auto_pfamseq_B pfamseq_acc_B auto_pfamA_B pfamA_acc_B pfamA_id_B pfamseq_seq_number_B/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_interactions",  "auto_int_pfamAs", "auto_int_atoms", "auto_pdb", "auto_pfamseq_A", "auto_pfamA_A", "auto_pfamseq_B", "auto_pfamA_B");

#Set up relationships
#1 to many relationship

__PACKAGE__->has_one( "int_pfamA" => "PfamSchemata::PfamDB::Int_pfamAs",
		      {"foreign.auto_int_pfamA"  => "self.auto_int_pfamA"});

__PACKAGE__->has_one( "int_atoms" => "PfamSchemata::PfamDB::Int_atoms",
		      {"foreign.auto_int_atoms"  => "self.auto_int_atoms"},
		      {proxy => [qw/pdb_atom partner_pdb_atom bond_name/]});

__PACKAGE__->has_one( "pdb" => "PfamSchemata::PfamDB::Pdb",
		      {"foreign.auto_pdb"  => "self.auto_pdb"},
		      {proxy => [qw/pdb_id/]});

__PACKAGE__->has_one( "pfamseqA" => "PfamSchemata::PfamDB::Pfamseq",
		      {"foreign.auto_pfamseq"  => "self.auto_pfamseq_A"},
		      {proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one( "pfamseqB" => "PfamSchemata::PfamDB::Pfamseq",
		      {"foreign.auto_pfamseq"  => "self.auto_pfamseq_B"},
		      {proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one( "pfamA_A" => "PfamSchemata::PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_A"});

__PACKAGE__->has_one( "pfamA_B" => "PfamSchemata::PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_B"});



1;
