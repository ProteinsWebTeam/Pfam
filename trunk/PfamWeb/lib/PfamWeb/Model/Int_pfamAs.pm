package PfamWeb::Model::Int_pfamAs;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("int_pfamAs"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_pfamA_A auto_pfamA_B auto_int_pfamAs/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamA_A", "auto_pfamA_B", "auto_int_pfamAs" );

#Set up relationships
#1 to many relationship

__PACKAGE__->has_one( "pfamA_A" => "PfamWeb::Model::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_A"});

__PACKAGE__->has_one( "pfamA_B" => "PfamWeb::Model::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA_B"});


1;
