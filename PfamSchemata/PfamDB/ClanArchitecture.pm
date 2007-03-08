
# $Id: ClanArchitecture.pm,v 1.3 2007-03-08 14:16:24 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::ClanArchitecture;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "clan_architecture" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_clan auto_architecture/);


#Set the the keys
__PACKAGE__->set_primary_key( "auto_clan", "auto_architecture" );


#Now on to the relationships

#__PACKAGE__->has_one    ( "pfamA_web" => "PfamDB::PfamA_web",
#			  {"foreign.auto_pfamA" => "self.auto_pfamA"},
	#		  {proxy => [ qw/average_length percentage_id average_coverage status/]});

__PACKAGE__->has_one    ( "clan" => "PfamDB::Pfam",
			  {"foreign.auto_clan" => "self.clan"},
			  {proxy => [ qw/clan_id clan_acc/]});

__PACKAGE__->has_one    ( "arch" => "PfamDB::Architecture",
			  {"foreign.auto_architecture" => "self.auto_architecture"},
			  {proxy => [ qw/architecture type_example no_seqs pfamseq_id pfamseq_acc annseq_storable/ ]});
1;
