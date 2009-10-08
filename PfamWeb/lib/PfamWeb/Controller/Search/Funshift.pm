
# Funshift.pm
# jt6 20061108 WTSI
#
# $Id: Funshift.pm,v 1.3 2009-10-08 09:23:51 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Funshift - search for families according to functional similarity

=cut

package PfamWeb::Controller::Search::Funshift;

=head1 DESCRIPTION

This controller has only a single action, which searches for families according to their
functional similarity score.

$Id: Funshift.pm,v 1.3 2009-10-08 09:23:51 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 funshiftSearch : Path

Executes a functional similarity search.

=cut

sub funshiftSearch : Path {
  my ( $this, $c ) = @_;

  return unless $c->req->param( 'entry' ) =~ m/^([\w_-]+)$/;
  $c->log->debug( "Search::Funshift::funshiftSearch: executing a functional similarity search for |$1|" )
    if $c->debug;

  # check for an accession or an ID
  $c->stash->{pfam} = $c->model("PfamDB::Pfama")
                       ->search( [ { pfama_acc => $1 },
                                   { pfama_id  => $1 } ] )
                       ->single;

  unless ( defined $c->stash->{pfam} ) {
    $c->log->debug( "Search::Funshift::funshiftSearch: can't find a matching Pfam-A" )
      if $c->debug;

    $c->stash->{fsSearchError} = q(Couldn't find a Pfam family with that ID or accession.);

    return;
  }
    
  # got a valid accession. Now do the funshift search
  my @fs = $c->model('PfamDB::FunctionalSimilarity')
             ->search( { auto_pfama_a => $c->stash->{pfam}->auto_pfama,
                         auto_pfama_b => { '!=' => $c->stash->{pfam}->auto_pfama },
                         rfunSim      => { '>'  => 0.75 } },
                       { join     => [ 'auto_pfama',
                                       { auto_clan => 'auto_clan' } ],
                         prefetch => [ 'auto_pfama',
                                       { auto_clan => 'auto_clan' } ], 
                         select   => [ qw( rfunsim mfscore bpscore clan_id ) ],
                         as       => [ qw( rfunsim mfscore bpscore clan_id ) ],
                         order_by => "rfunSim DESC" } );

  $c->log->debug( 'Search::Funshift::funshiftSearch: found |' . scalar @fs . '| rows' )
    if $c->debug;

  if ( scalar @fs ) {

    $c->stash->{template} = "pages/search/funshift/results.tt";
    $c->stash->{results} = \@fs;
  
    # generate a gradient for this many rows
    my $cm = new Sanger::Graphics::ColourMap;
    my @grad = $cm->build_linear_gradient( scalar @fs, '008000', 'C00000' );
    $c->stash->{gradient} = \@grad;

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
