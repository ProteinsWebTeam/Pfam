package PfamDB::PfamB2pfamA_PRC_results;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("pfamB2pfamA_PRC_results"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_pfamB model_start1 model_end1 length1 align1 auto_pfamA model_start2 model_end2 length2 align2 evalue/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamB", "auto_pfamA");

#Set up relationships
#1 to many relationship
__PACKAGE__->has_one( "pfamA" => "PfamDB::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		      {proxy => [qw/pfamA_id pfamA_acc/]});

__PACKAGE__->has_one( "pfamB" => "PfamDB::PfamB",
		      {"foreign.auto_pfamB"  => "self.auto_pfamB"},
		      {proxy => [qw/pfamB_id pfamB_acc/]});

1;
