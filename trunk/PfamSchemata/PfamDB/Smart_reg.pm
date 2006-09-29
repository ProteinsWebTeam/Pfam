package PfamDB::Smart_reg;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "smart_regions" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq auto_smart seq_start seq_end domain_bits_score domain_evalue_score/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_smart", "auto_pfamseq");

#Now setup the relationship

__PACKAGE__->has_one( "smart" =>  "PfamDB::Smart",
		      { "foreign.auto_smart"  => "self.auto_smart" },
		      { proxy => [ qw/smart_id smart_acc/ ] } );


__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_acc pfamseq_id/ ] } );
1;
