
package PfamDB::EC_info;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "ec_info" );

__PACKAGE__->add_columns( qw/ index_pfamA_ec
							  auto_pfamA
							  ec_number
							  reaction
							  number_seq
							  in_family / );

__PACKAGE__->set_primary_key( "index_pfamA_ec" );

__PACKAGE__->has_one( pfamA => "PfamDB::Pfam",
					  { "foreign.auto_pfamA" => "self.auto_pfamA" },
					  { proxy                => [ qw/ pfamA_acc pfamA_id description / ] } );

__PACKAGE__->has_many( EC_seq => "PfamDB::EC_seq",
					   { "foreign.index_pfamA_ec" => "self.index_pfamA_ec"});

1;

