
# Interpro.pm
# jt6 20060810 WTSI
#
# $Id: Interpro.pm,v 1.7 2009-11-24 16:39:48 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Searches::Plugin::Pfam - search plugin for the pfamA table

=cut

package iPfamWeb::Controller::Search::Plugin::Interpro;

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

$Id: Interpro.pm,v 1.7 2009-11-24 16:39:48 pg6 Exp $

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
use Data::Dump qw( dump );

use base 'iPfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

Executes the query. Performs a fulltext query on the pfamA table,
searching the accession, ID, description, comment and "previous_id"
columns.

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::Interpro::process: text querying table pfamA using: |' .
                  $c->stash->{terms} . '|' ) if $c->debug;

  my $rs = $c->model('iPfamDB::Pfama')
                  ->search( {},
                            {} )
                  ->search_literal( 'MATCH( pfama_acc, pfama_id, interpro_id, interpro_abstract ) ' .
                                    'AGAINST( ? IN BOOLEAN MODE )',
                                    $c->stash->{terms} );
  
  # rather than retruning an object, just build a hash of what we need,
  my $results = [];
  while(my $r = $rs->next ){
    push @$results,{
      'acc' =>  $r->get_column( 'pfama_acc' ),
      'id'  =>  $r->get_column( 'pfama_id'),
      'desc'=>  $r->get_column( 'description'),
      'interpro'  =>  $r->get_column( 'interpro_id'),
      'type'  =>  'family'
    };
  }
  #$c->log->debug( "Search::Plugin::process:dump of the results is ".dump( $results ) );
  # do a simple lookup for the ID or accession...
#  $c->log->debug( "Search::Plugin::Pfam::process: doing a lookup for ID or acc using: |" .
#                  $c->stash->{rawQueryTerms} . '|' ) if $c->debug;

#  my $lookup = $c->model('PfamDB::Pfama')->search( [ { pfamA_acc => $c->stash->{rawQueryTerms} },
#                                                   { pfamA_id  => $c->stash->{rawQueryTerms} } ] );

#  return $results, $lookup;
  $c->log->debug( "the size of the resutls is ".scalar( @$results ) );
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
