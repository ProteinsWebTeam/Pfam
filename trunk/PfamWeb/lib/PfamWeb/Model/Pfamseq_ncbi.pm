package PfamWeb::Model::Pfamseq_ncbi;

use strict;
use warnings;
use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamseq_ncbi" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq ncbi_code/);

__PACKAGE__->set_primary_key("auto_pfamseq", "ncbi_code");

__PACKAGE__->has_one("auto_pfamseq" => "PfamWeb::Model::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
		     {proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one("ncbi" => "PfamWeb::Model::Ncbi_taxonomy",
		     {"foreign.ncbi_code" => "self.ncbi_code"},
		     {proxy => [qw/species taxonomy/]});

1;
