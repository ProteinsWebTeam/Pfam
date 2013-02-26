
# Pfam.pm
# jt6 20060810 WTSI
#
# $Id: Pfam.pm,v 1.13 2009-12-08 13:50:05 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Plugin::Pfam - search plugin for the pfamA table

=cut

package PfamWeb::Controller::Search::Plugin::Pfam;

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

Also does a simple look up in the pfam table, checking to see if the
raw search terms match a Pfam family accession or ID.

$Id: Pfam.pm,v 1.13 2009-12-08 13:50:05 jt6 Exp $

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

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

Executes the query. Performs a fulltext query on the pfamA table,
searching the accession, ID, description, comment and "previous_id"
columns.

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::Pfam::process: text querying table pfamA using: |' .
                  $c->stash->{terms} . '|' ) if $c->debug;

  my $results = $c->model('Keyword::Pfam')->search(
    query => $c->stash->{rawQueryTerms},
  );

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
