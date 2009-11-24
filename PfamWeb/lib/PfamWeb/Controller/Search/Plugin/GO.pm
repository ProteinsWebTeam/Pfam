
# GO.pm
# jt6 20060816 WTSI
# pg6 20091123 WTSI
#
# $Id: GO.pm,v 1.7 2009-11-24 16:39:48 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Searches::Plugin::GO - search plugin for the gene_ontology table

=cut

package iPfamWeb::Controller::Search::Plugin::GO;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the gene_ontology table, on the
following columns:

=over

=item o go_id

=item o term

=back

There's an explicit join against the pfamA table, so that we can
retrieve pfamA accession, ID and description.

$Id: GO.pm,v 1.7 2009-11-24 16:39:48 pg6 Exp $

=cut

# based loosely on this original query:
#SELECT category,
#       term,
#       go_id
#FROM   pfamA AS p,
#       gene_ontology AS go
#WHERE  p.pfamA_acc = ?
#AND    p.auto_pfamA = go.auto_pfamA

use strict;
use warnings;
use Data::Dump qw( dump );

use base 'iPfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::GO::process: text querying table gene_ontology' )
    if $c->debug;

  my $rs = $c->model('iPfamDB::GeneOntology')
                  ->search( {},
                            { prefetch => [ qw( pfama_acc ) ] } )
                  ->search_literal( 'MATCH( go_id, term ) ' .
                                    'AGAINST( ? IN BOOLEAN MODE )',
                                    $c->stash->{terms} );
  
  my $results = [];
  while( my $r = $rs->next ){
    push @$results, {
      'acc'   =>  $r->get_column( 'pfama_acc' ),
      'id'    =>  $r->pfama_acc->get_column( 'pfama_id' ),
      'go_id' =>  $r->get_column( 'go_id' ),
      'desc'  =>  $r->pfama_acc->get_column( 'description' ),
      'type'  =>  'family'
    };
  }
  
  #$c->log->debug( "Search::Plugin::process:dump of the results is ".dump( $results ) );
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
