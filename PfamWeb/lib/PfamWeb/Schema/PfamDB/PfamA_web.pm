package PfamWeb::Schema::PfamDB::PfamA_web;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamA_web" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamA average_length percentage_id average_coverage status/);

__PACKAGE__->set_primary_key( "auto_pfamA" );

__PACKAGE__->has_one( "pfam" => "PfamWeb::Schema::PfamDB::Pfam",
		       {"foreign.auto_pfamA" => "self.auto_pfamA"});

1;
