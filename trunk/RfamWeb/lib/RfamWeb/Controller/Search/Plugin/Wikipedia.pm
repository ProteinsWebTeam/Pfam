
# Wikipedia.pm
# jt6 20060810 WTSI
#
# $Id: Wikipedia.pm,v 1.2 2008-09-12 09:48:23 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Searches::wikipedia - search plugin for the
wikipedia table

=cut

package RfamWeb::Controller::Search::Plugin::Wikipedia;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the wikipedia table.

$Id: Wikipedia.pm,v 1.2 2008-09-12 09:48:23 jt6 Exp $

=cut

# based loosely on this original query:
# SELECT DISTINCT( pfamA_acc ) AS acc,
#        pfamA_id AS id,
#        description AS descr
# FROM   seq_info
# WHERE  MATCH( seq_description, species ) AGAINST( ? IN BOOLEAN MODE );

use strict;
use warnings;

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::Seq_info::process: text querying table seq_info' )
    if $c->debug;

  my $results = $c->model('PfamDB::Seq_info')
                  ->search( {},
                            {} )
                  ->search_literal( 'MATCH( seq_description, species, pfamseq_id, pfamseq_acc ) ' .
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
