
# Feature_das_sources.pm
# aj5 20070307 WTSI.
#
# Model for the das_sources table.
#
# $Id: Feature_das_sources.pm,v 1.2 2007-03-08 14:16:31 jt6 Exp $
#
# $Author: jt6 $

package WebUser::Feature_das_sources;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "feature_das_sources" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ server_id name url system sequence_type helper_url default_server /);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( qw/ server_id system sequence_type / );

# relationships

__PACKAGE__->might_have( alignment_sources_from => "WebUser::Alignment_das_sources",
						{	"foreign.from_system"	=> "self.system",
						 	"foreign.from_type"		=> "self.sequence_type"	} );

__PACKAGE__->might_have( alignment_sources_to => "WebUser::Alignment_das_sources",
						{	"foreign.to_system"	=> "self.system",
						 	"foreign.to_type"	=> "self.sequence_type"	} );

1;
