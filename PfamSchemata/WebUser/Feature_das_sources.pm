
# Feature_das_sources.pm
# aj5 20070307 WTSI.
#
# Model for the das_sources table.
#
# $Id: Feature_das_sources.pm,v 1.4 2008-05-16 15:23:16 jt6 Exp $
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

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
