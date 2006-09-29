package PfamDB::Other_reg;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "other_reg" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/region_id auto_pfamseq seq_start seq_end type_id source_id score orientation/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("region_id", "auto_pfamseq", "type_id");

#Now setup the relationship
__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_acc pfamseq_id/ ] } );


1;
