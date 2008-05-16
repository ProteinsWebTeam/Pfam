
# News.pm
# jt6 20061207 WTSI
#
# Model for the news table.
#
# $Id: News.pm,v 1.4 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package WebUser::News;

use strict;
use warnings;

use base "DBIx::Class";

# add the HTMLWidget component, which will allow us to populate 
# widgets from rows and vice versa
__PACKAGE__->load_components(qw/Core HTMLWidget/);

# set up the table
__PACKAGE__->table("news");

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ auto_news
							  author
							  pubDate
							  title
							  summary
							  description
							  live / );

# set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_news");

# no relationships

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
