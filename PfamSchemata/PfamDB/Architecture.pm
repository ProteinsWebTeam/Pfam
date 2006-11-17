
package PfamDB::Architecture;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "architecture" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_architecture architecture type_example no_seqs architecture_acc/);


#Set the the keys
__PACKAGE__->set_primary_key( "auto_architecture", "architecture", "type_example" );


#Now on to the relationships

__PACKAGE__->has_one    ( "pfamA_architecture" => "PfamDB::PfamA_architecture",
			  {"foreign.auto_architecture" => "self.auto_architecture"},
			{proxy => [qw/auto_pfamA/]});

__PACKAGE__->has_one    ( "type_example" => "PfamDB::Pfamseq",
			  {"foreign.auto_pfamseq" => "self.type_example"},
			  {proxy => [ qw/pfamseq_id pfamseq_acc/ ]});

__PACKAGE__->has_one    ( "storable" => "PfamDB::Pfam_annseq",
			  {"foreign.auto_pfamseq" => "self.type_example"},
			  {proxy => [ qw/annseq_storable/ ]});

__PACKAGE__->has_one    ( "clan_arch" => "PfamDB::ClanArchitecture",
			  {"foreign.auto_architecture" => "self.auto_architecture"},
			{proxy => [qw/auto_clan/]});
1;
