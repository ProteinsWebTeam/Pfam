package PfamWeb::Schema::PfamDB::Ncbi_taxonomy;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "ncbi_taxonomy" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/ncbi_code species taxonomy/);

__PACKAGE__->set_primary_key("ncbi_code");

__PACKAGE__->has_many("auto_pfamseq" => "PfamWeb::Schema::PfamDB::Pfamseq_ncbi",
		      {"foreign.ncbi_code" => "self.ncbi_code"});

1;
