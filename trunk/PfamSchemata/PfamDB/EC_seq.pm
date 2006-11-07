
package PfamDB::EC_seq;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

__PACKAGE__->table( "ec_seq" );

__PACKAGE__->add_columns( qw/ index_pfamA_ec
							  auto_pfamseq / );

__PACKAGE__->set_primary_key( "index_pfamA_ec" );

__PACKAGE__->has_one( EC_info => "PfamDB::EC_info",
					  { "foreign.index_pfamA_ec" => "self.index_pfamA_ec" },
					  { proxy                    => [ qw/ index_pfamA_ec
														  auto_pfamA
														  ec_number
														  reaction
														  number_seq
														  in_family / ] } );

__PACKAGE__->might_have( pfamseq_markup => "PfamDB::Pfamseq_markup",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
						 {  proxy                 => [ qw/ auto_markup / ] });

__PACKAGE__->might_have( pfamseq => "PfamDB::Pfamseq_markup",
						 { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
						 {  proxy                 => [ qw/ pfamseq_acc pfamseq_id / ] });

1;
