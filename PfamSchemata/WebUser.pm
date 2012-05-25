
# WebUser.pm
# jt 20060912 WTSI
#
# $Id: WebUser.pm,v 1.17 2009-10-09 09:09:51 jt6 Exp $
#
# $Author: jt6 $

=head1 NAME

WebUser - DBIC schema definition class for the web_user database

=cut

package WebUser;

=head1 DESCRIPTION

The base class for the whole web_user database model. Config comes from the
catalyst application class.

$Id: WebUser.pm,v 1.17 2009-10-09 09:09:51 jt6 Exp $

=cut

use strict;
use warnings;

use base "DBIx::Class::Schema";

#-------------------------------------------------------------------------------

# we need to specify exactly which classes to load, otherwise DBIC tries to
# load WebUser::DateTime as a ResultSource.
__PACKAGE__->load_classes( qw/ Alignment_das_sources
                               ArticleMapping
                               Das_sources
                               ErrorLog
                               Family_count
                               Feature_das_sources
                               JobHistory
                               JobStream
                               Species_collection
                               Wikitext / );

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

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
