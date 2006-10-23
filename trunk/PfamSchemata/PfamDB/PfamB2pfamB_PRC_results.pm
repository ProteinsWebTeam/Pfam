package PfamDB::PfamB2pfamB_PRC_results;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/);

__PACKAGE__->table("pfamB2pfamB_PRC_results");

__PACKAGE__->add_columns( qw/auto_pfamB1 model_start1 model_end1 length1 align1
							 auto_pfamB2 model_start2 model_end2 length2 align2 /);

# set up the primary keys
__PACKAGE__->set_primary_key( "auto_pfamB1", "auto_pfamB2");

# set up relationships
# 1 to many relationship
__PACKAGE__->has_one( "pfamB1" => "PfamDB::PfamB",
					  { "foreign.auto_pfamB" => "self.auto_pfamB1" } );

__PACKAGE__->has_one( "pfamB2" => "PfamDB::PfamB",
					  { "foreign.auto_pfamB" => "self.auto_pfamB2" } );

1;
