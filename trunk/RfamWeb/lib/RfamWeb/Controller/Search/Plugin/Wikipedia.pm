
# Wikipedia.pm
# jt6 20060810 WTSI
#
# $Id: Wikipedia.pm,v 1.4 2009-01-06 11:54:19 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Searches::wikipedia - search plugin for
wikipedia annotation text

=cut

package RfamWeb::Controller::Search::Plugin::Wikipedia;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the rfam_keywords table, searching
against the following columns:

=over

=item o wiki

=back

$Id: Wikipedia.pm,v 1.4 2009-01-06 11:54:19 jt6 Exp $

=cut

# based loosely on this original query:
# SELECT * 
# FROM   rfam_keywords
# WHERE  MATCH( wiki ) AGAINST( ? IN BOOLEAN MODE )

use strict;
use warnings;

use base 'RfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::Wikipedia::process: text querying Rfam wikipedia annotations' )
    if $c->debug;

  my $results = $c->model('RfamDB::RfamKeywords')
                  ->search( {},
                            {} )
                  ->search_literal( 'MATCH( wiki ) ' .
                                    'AGAINST( ? IN BOOLEAN MODE )',
                                    $c->stash->{terms} );

  return $results;
}

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
