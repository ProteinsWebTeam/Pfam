package PfamDB::PfamA_reg_full_significant;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamA_reg_full_significant" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamA_reg_full auto_pfamseq auto_pfamA seq_start seq_end model_start model_end domain_bits_score domain_evalue_score sequence_bits_score sequence_evalue_score mode cigar in_full tree_order/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamA_reg_full", "auto_pfamA", "auto_pfamseq");

#Now setup the relationship
__PACKAGE__->has_one( "pfamA" =>  "PfamDB::Pfam",
		      { "foreign.auto_pfamA"  => "self.auto_pfamA" },
		      { proxy => [ qw/pfamA_id pfamA_acc description model_length type/ ] } );

__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_acc pfamseq_id species taxonomy/ ] } );



1;
