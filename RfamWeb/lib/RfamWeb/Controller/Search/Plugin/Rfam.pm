
# Rfam.pm
# jt6 20060810 WTSI
#
# $Id: Rfam.pm,v 1.1 2008-09-12 09:14:14 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Searches::Plugin::Rfam - search plugin for the pfamA table

=cut

package RfamWeb::Controller::Search::Plugin::Rfam;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the pfamA table, searching
against the following columns:

=over

=item o pfamA_acc

=item o pfamA_id

=item o description

=item o comment

=item o description

=item o previous_id

=back

$Id: Rfam.pm,v 1.1 2008-09-12 09:14:14 jt6 Exp $

=cut

# based loosely on this original query:
# SELECT pfamA_acc   AS acc,
#        pfamA_id    AS id,
#        description AS descr
# FROM   pfamA
# WHERE  MATCH( pfamA_acc, pfamA_id, description, comment, previous_id )
# AGAINST( ? IN BOOLEAN MODE )

use strict;
use warnings;

use base 'RfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

Executes the query. Performs a fulltext query on the pfamA table,
searching the accession, ID, description, comment and "previous_id"
columns.

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::Rfam::process: text querying table pfamA using: |' .
                  $c->stash->{terms} . '|' ) if $c->debug;

  my $m = $c->model('RfamDB::Rfam');

  # do a full blown query...
  my $results =
    $m->search( {},
                {} )
      ->search_literal( 'MATCH( pfamA_acc, pfamA_id, description, comment, previous_id ) ' .
                        'AGAINST( ? IN BOOLEAN MODE )',
                        $c->stash->{terms} );

#  return $results, $lookup;
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
