package PfamWeb::Model::Pfam_annseq;

use strict;
use warnings;
use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfam_annseq" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq annseq_storable/);

__PACKAGE__->set_primary_key("auto_pfamseq");

#This doesnot need any proxies as all of the data should be in the storable object
__PACKAGE__->has_one("auto_pfamseq" => "PfamWeb::Model::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"});

1;
