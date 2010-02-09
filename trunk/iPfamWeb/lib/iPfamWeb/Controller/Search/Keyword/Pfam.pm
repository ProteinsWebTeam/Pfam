
# Pfam.pm
# jt6 20060810 WTSI
#
# $Id: Pfam.pm,v 1.1 2009-11-27 11:40:34 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Searches::Keyword::Pfam - search Keyword for the pfamA table

=cut

package iPfamWeb::Controller::Search::Keyword::Pfam;

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

$Id: Pfam.pm,v 1.1 2009-11-27 11:40:34 pg6 Exp $

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

use base 'iPfamWeb::Controller::Search::Keyword';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Path

Executes the query. Performs a fulltext query on the pfamA table,
searching the accession, ID, description, comment and "previous_id"
columns.

=cut

sub process : Path {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Keyword::Pfam::private: text querying table pfamA using: |' .
                  $c->stash->{terms} . '|' ) if $c->debug;

  my $rs = $c->model('iPfamDB::Pfama')
                  ->search( {},
                            {} )
                  ->search_literal( 'MATCH( pfama_acc, pfama_id, description, comment ) ' .
                                    'AGAINST( ? IN BOOLEAN MODE )',
                                    $c->stash->{terms} );
  
  # rather than retruning an object, just build a hash of what we need,
  my $results = [];my $seen = {};
  my $total_hits = 0;
  while(my $r = $rs->next ){
    
    #increment the hits coutner when we see the results;
    $total_hits++;
    
    my $acc   = $r->get_column( 'pfama_acc' );
    next if( exists $seen->{ $acc } ); 
    
    my $desc = $r->get_column( 'description' );
    
    push @$results,{
      'acc'   =>  $acc,
      'id'    =>  $r->get_column( 'pfama_id'),
      'desc'  =>  $desc,
      'type'  =>  'family'
    };
    $seen->{ $acc }++;
  }
  $c->stash->{ hits } = $total_hits;
  $c->log->debug( "Search::Keyword::Pfam::process: the total number of results are  ".scalar( @$results ) );
  $c->stash->{ results } = $results;
  $c->stash->{ template } = 'components/blocks/search/keyword/pfam.tt';
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
