
package PfamSchemata::PfamDB::PfamA_architecture;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamA_architecture" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamA auto_architecture/);


#Set the the keys
__PACKAGE__->set_primary_key( "auto_pfamA", "auto_architecture" );


#Now on to the relationships

#__PACKAGE__->has_one    ( "pfamA_web" => "PfamSchemata::PfamDB::PfamA_web",
#			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
	#		  {proxy => [ qw/average_length percentage_id average_coverage status/]});

__PACKAGE__->has_one    ( "pfam" => "PfamSchemata::PfamDB::Pfam",
			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
			  {proxy => [ qw/pfamA_id pfamA_acc/]});

__PACKAGE__->has_one    ( "arch" => "PfamSchemata::PfamDB::Architecture",
			  {"foreign.auto_architecture" => "self.auto_architecture"},
			  {proxy => [ qw/architecture type_example no_seqs pfamseq_id pfamseq_acc annseq_storable/ ]});
1;
