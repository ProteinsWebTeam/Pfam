package PfamSchemata::PfamDB::PfamA_literature_references;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("pfamA_literature_references"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_pfamA auto_lit order_added comment/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamA", "auto_lit" );

#Set up relationships
#1 to many relationship

__PACKAGE__->has_one( "pfamA" => "PfamSchemata::PfamDB::Pfam",
		       {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		       {proxy => [qw/pfamA_id pfamA_acc/]});


__PACKAGE__->has_one( "literature" => "PfamSchemata::PfamDB::Literature_references",
		       {"foreign.auto_lit"  => "self.auto_lit"},
		       {proxy => [qw/medline title author journal/]});


1;
