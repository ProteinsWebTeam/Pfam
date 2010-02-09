
# iPfamWeb::Controller::Search::Keyword::Protein
# pg6 20092712 WTSI
#
# $Id: Protein.pm,v 1.1 2009-12-01 12:02:31 pg6 Exp $

package iPfamWeb::Controller::Search::Keyword::Protein;

=head1 Name

iPfamWeb::Controller::Search::Keywrod::Protein

=head1 Description

Controller to retrieve protein information for the keyword entered by the user.

=cut

use strict;
use warnings;

use base 'iPfamWeb::Controller::Search::Keyword';

#------------------------------------------------------------------------------------------------------
=head1 Methods

=head2 protein: Path

runs the search using the query term and return the results, 

=cut

sub protien: Path {
  my ( $self, $c ) = @_;
  
  $c->log->debug( 'Search::Keyword::Pfam::private: text querying table pfamA using: |' .
                  $c->stash->{terms} . '|' ) if $c->debug;
  
  # not generating the query because, we havent got enough data about proteins;
    
#  my $rs = $c->model( 'iPfamDB::Protein' )
#                ->search()
#                ->search_literal( );
  my $results = [];my $seen = {};
  my $total_hits = 0;
  
  $c->stash->{ hits } = $total_hits;
  $c->log->debug( "Search::Keyword::Protein::process: the total number of results are  ".scalar( @$results ) );
  $c->stash->{ results } = $results;
  $c->stash->{ template } = 'components/blocks/search/keyword/protein.tt';
                    
}


#-------------------------------------------------------------------------------

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

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
