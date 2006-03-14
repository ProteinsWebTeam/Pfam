package PfamWeb::Model::Context_pfam_regions;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "context_pfam_regions" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamA auto_pfamseq seq_start seq_end domain_score/);

#Set the the keys
__PACKAGE__->set_primary_key( "auto_pfamA", "auto_pfamseq");


#Now on to the relationships

__PACKAGE__->has_one    ( "pfamA" => "PfamWeb::Model::Pfam",
			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
			  {proxy => [qw/pfamA_id pfamA_acc/]});

__PACKAGE__->has_one    ( "pfamseq" => "PfamWeb::Model::Pfamseq",
			  {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
			  {proxy => [ qw/pfamseq_id pfamseq_acc/ ]});

1;
