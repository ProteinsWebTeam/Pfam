
# iPfamWebUser.pm
# jt 20060912 WTSI
#
# $Id: iPfamWebUser.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

=head1 NAME

iPfamWebUser - iPfam-specific DBIC schema definition class for the 
               web_user database

=cut

package iPfamWebUser;

=head1 DESCRIPTION

This is a cut-down version of the schema wrapper for the web_user database. We
only need a subset of the tables for iPfam, which are declared here.

$Id: iPfamWebUser.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $

=cut

use strict;
use warnings;

use base 'DBIx::Class::Schema';

#-------------------------------------------------------------------------------

__PACKAGE__->load_classes( { WebUser => [ qw( ErrorLog
                                              JobHistory
                                              JobStream ) ]
                           } );

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
