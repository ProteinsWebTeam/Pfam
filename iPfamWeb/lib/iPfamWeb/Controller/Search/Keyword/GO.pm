
# GO.pm
# jt6 20060816 WTSI
# pg6 20091123 WTSI
#
# $Id: GO.pm,v 1.1 2009-11-27 11:40:34 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Searches::Plugin::GO - search plugin for the gene_ontology table

=cut

package iPfamWeb::Controller::Search::Keyword::GO;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the gene_ontology table, on the
following columns:

=over

=item o go_id

=item o term

=back

There's an explicit join against the pfamA table, so that we can
retrieve pfamA accession, ID and description.

$Id: GO.pm,v 1.1 2009-11-27 11:40:34 pg6 Exp $

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

use base 'iPfamWeb::Controller::Search::Keyword';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

=cut

sub process : Path {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Keyword::GO::process: text querying table gene_ontology' )
    if $c->debug;

  my $rs = $c->model('iPfamDB::GeneOntology')
                  ->search( {},
                            { prefetch => [ qw( pfama_acc ) ] } )
                  ->search_literal( 'MATCH( go_id, term ) ' .
                                    'AGAINST( ? IN BOOLEAN MODE )',
                                    $c->stash->{terms} );
  
 my $results = [];my $seen = {};
  my $total_hits = 0;
  while(my $r = $rs->next ){
    
    #increment the hits coutner when we see the results;
    $total_hits++;
    
    my $acc   = $r->get_column( 'pfama_acc' );
    next if( exists $seen->{ $acc } ); 
    
    my $desc = $r->pfama_acc->get_column( 'description' );
    push @$results, {
      'acc'   =>  $acc,
      'id'    =>  $r->pfama_acc->get_column( 'pfama_id' ),
      'go_id' =>  $r->get_column( 'go_id' ),
      'desc'  =>  $desc,
      'type'  =>  'family'
    };
    $seen->{ $acc }++;
  }
  $c->stash->{ hits }= $total_hits;
  $c->log->debug( "Search::Keyword::go::process: the total number of results are  ".scalar( @$results ) );
  $c->stash->{ results } = $results;
  $c->stash->{ template } = 'components/blocks/search/keyword/go.tt';
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
