package PfamSchemata::PfamDB::Int_ext_links;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("int_ext_links"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_int_ext_links auto_pfamaseq_A pfamseq_acc_A auto_pfamA_A pfamA_id_A auto_pfamseq_B pfamseq_acc_B auto_pfamA_B pfamA_id_B reference experiment_ref auto_int_pfamAs dataset/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key("auto_int_ext_links", "auto_pfamaseq_A", "pfamseq_acc_A", "auto_pfamA_A", "auto_pfamseq_B", "pfamseq_acc_B",  "auto_pfamA_B", "auto_int_pfamAs" );

#Set up relationships
#1 to many relationship

__PACKAGE__->has_one( "pfamA_A" => "PfamSchemata::PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_A"});

__PACKAGE__->has_one( "pfamA_B" => "PfamSchemata::PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_B"});

__PACKAGE__->has_one( "pfamseq_A" => "PfamSchemata::PfamDB::Pfamseq",
		      {"foreign.auto_pfamseq"  => "self.auto_pfamseq_A"});

__PACKAGE__->has_one( "pfamseq_B" => "PfamSchemata::PfamDB::Pfam",
		      {"foreign.auto_pfameq"  => "self.auto_pfamseq_B"});

__PACKAGE__->has_one( "int_pfamAs" => "PfamSchemata::PfamDB::IntPfamAs",
		      {"foreign.auto_int_pfamAs" => "self.auto_int_pfamAs"});

1;
