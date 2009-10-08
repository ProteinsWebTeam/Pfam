
# Funshift.pm
# jt6 20061108 WTSI
#
# $Id: Funshift.pm,v 1.4 2009-10-08 10:12:27 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Funshift - search for families according to functional similarity

=cut

package PfamWeb::Controller::Search::Funshift;

=head1 DESCRIPTION

This controller has only a single action, which searches for families according to their
functional similarity score.

$Id: Funshift.pm,v 1.4 2009-10-08 10:12:27 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 funshiftSearch : Path

Executes a functional similarity search.

=cut

sub funshift_search : Path {
  my ( $this, $c ) = @_;

  return unless $c->req->param( 'entry' ) =~ m/^([\w_-]+)$/;
  $c->log->debug( "Search::Funshift::funshift_search: executing a functional similarity search for |$1|" )
    if $c->debug;

  # check for an accession or an ID
  $c->stash->{pfam} = $c->model("PfamDB::Pfama")
                        ->search( [ { pfama_acc => $1 },
                                    { pfama_id  => $1 } ] )
                        ->single;

  unless ( defined $c->stash->{pfam} ) {
    $c->log->debug( "Search::Funshift::funshift_search: can't find a matching Pfam-A" )
      if $c->debug;

    $c->stash->{fsSearchError} = q(Couldn't find a Pfam family with that ID or accession.);

    return;
  }
    
  # got a valid accession. Now do the funshift search
  my @rs = $c->model('PfamDB::FunctionalSimilarity')
             ->search( { auto_pfama_a => $c->stash->{pfam}->auto_pfama,
                         auto_pfama_b => { '!=' => $c->stash->{pfam}->auto_pfama },
                         rfunSim      => { '>'  => 0.75 } },
                       { join     => [ qw( auto_pfama ) ],
                         prefetch => [ qw( auto_pfama ) ], 
                         order_by => "rfunSim DESC" } );

  $c->log->debug( 'Search::Funshift::funshift_search: found |' . scalar @rs . '| rows' )
    if $c->debug;

  if ( scalar @rs ) {

    $c->stash->{template} = "pages/search/funshift/results.tt";
    $c->stash->{results}  = \@rs;

    # generate a gradient for this many rows
    my $cm = new Sanger::Graphics::ColourMap;
    my @grad = $cm->build_linear_gradient( scalar @rs, '008000', 'C00000' );
    $c->stash->{gradient} = \@grad;

    my @clan_map = $c->model('PfamDB::ClanMembership')
                     ->search( {},
                               { join     => [ 'auto_clan' ],
                                 prefetch => [ 'auto_clan' ] } );

    my %clan_map;
    foreach my $row ( @clan_map ) {  
      $clan_map{ $row->get_column('auto_pfama') } = $row->auto_clan->clan_id;
    }
    $c->stash->{clan_map} = \%clan_map;

    $c->log->debug( 'Search::Funshift::funshift_search: clan_map: '
                    . dump( $c->stash->{clan_map} ) )
      if $c->debug;

  }
  else {
    # see if there are any GO terms for this family
    $c->stash->{goTerms} = $c->model('PfamDB::GeneOntology')
                             ->search( { 'me.auto_pfama' => $c->stash->{pfam}->auto_pfama } );
    
    $c->stash->{template} = 'pages/search/funshift/error.tt';
  }
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
