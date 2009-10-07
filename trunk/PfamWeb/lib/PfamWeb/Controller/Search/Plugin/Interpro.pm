
# Interpro.pm
# jt6 20060816 WTSI
#
# $Id: Interpro.pm,v 1.6 2009-10-07 11:58:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Plugin::Pfam - search plugin for the interpro table

=cut

package PfamWeb::Controller::Search::Plugin::Interpro;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the interpro gene_ontology table, on the
following columns:

=over

=item o interpro_id

=item o abstract

=back

There's an explicit join against the pfamA table, so that we can
retrieve pfamA accession, ID and description.

$Id: Interpro.pm,v 1.6 2009-10-07 11:58:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

Executes the query.

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::Interpro::process: text querying table interpro' )
    if $c->debug;

  my $results = $c->model('PfamDB::Interpro')
                  ->search( {},
                            { join     => [ qw( auto_pfama ) ],
                              prefetch => [ qw( auto_pfama ) ] } )
                  ->search_literal( 'MATCH( interpro_id, abstract ) ' .
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
