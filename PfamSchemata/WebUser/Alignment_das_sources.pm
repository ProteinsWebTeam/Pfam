
# Alignment_das_sources.pm
# aj5
#
# Model for the das_sources table.
#

package WebUser::Alignment_das_sources;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "alignment_das_sources" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ server_id name url from_system from_type to_system to_type helper_url /);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( qw/ from_system from_type to_system to_type / );

# relationships

__PACKAGE__->might_have( feature_sources_to => "WebUser::Feature_das_sources",
						{	"foreign.system"		=> "self.to_system",
						 	"foreign.sequence_type"	=> "self.to_type"	} );
							
__PACKAGE__->might_have( feature_sources_from => "WebUser::Feature_das_sources",
						{	"foreign.system"		=> "self.from_system",
						 	"foreign.sequence_type"	=> "self.from_type"	} );

1;
