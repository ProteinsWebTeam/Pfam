
# Funshift.pm
# jt6 20061108 WTSI
#
# $Id: Funshift.pm,v 1.1 2007-07-25 10:26:17 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Funshift - search for families according to functional similarity

=cut

package PfamWeb::Controller::Search::Funshift;

=head1 DESCRIPTION

This controller has only a single action, which searches for families according to their
functional similarity score.

$Id: Funshift.pm,v 1.1 2007-07-25 10:26:17 jt6 Exp $

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
  my( $this, $c ) = @_;

  return unless $c->req->param( 'entry' ) =~ m/^([\w_-]+)$/;
  $c->log->debug( "Search::Funshift::funshiftSearch: executing a functional similarity search for |$1|" );

  # check for an accession or an ID
  if( $c->req->param( 'entry' ) =~ /^(PF\d{5})$/i ) {

    $c->log->debug( 'Search::Funshift::funshiftSearch: might be a Pfam accession' );
    $c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );

  } elsif( $c->req->param( "entry" ) =~ /^([\w_-]+)$/ ) {

    $c->log->debug( 'Search::Funshift::funshiftSearch: might be a Pfam accession' );
    $c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_id => $1 } );

  } else {
    
    $c->log->debug( q(Search::Funshift::funshiftSearch: can't figure out whether it's an ID or acc) );
    $c->stash->{fsSearchError} = q(Couldn't find a Pfam family with that ID or accession.);
    return;
    
  }
  
  # make sure the query actually found an entry
  if( not defined $c->stash->{pfam} ) {
    $c->log->debug( "Search::Funshift::funshiftSearch: can't find a Pfam family |$1|" );
    $c->stash->{fsSearchError} = q(Couldn't find a family with that ID or accession.);
    return;
  }
  
  # yes; now do the funshift search
  my @fs = $c->model('PfamDB::Funshift')
             ->search( { auto_pfamA_A => $c->stash->{pfam}->auto_pfamA,
                         auto_pfamA_B => { '!=' => $c->stash->{pfam}->auto_pfamA },
                         rfunSim      => { '>'  => 0.75 } },
                       { join     => [ qw( pfam clan ) ],
                         prefetch => [ qw( pfam clan ) ], 
                         order_by => "rfunSim DESC" } );

  $c->log->debug( 'Search::Funshift::funshiftSearch: found |' . scalar @fs . '| rows' );

  if( scalar @fs ) {

    $c->stash->{template} = "pages/search/funshift/results.tt";
    $c->stash->{results} = \@fs;
  
    # generate a gradient for this many rows
    my $cm = new Sanger::Graphics::ColourMap;
    my @grad = $cm->build_linear_gradient( scalar @fs, '008000', 'C00000' );
    $c->stash->{gradient} = \@grad;

  } else {
    # see if there are any GO terms for this family
    $c->stash->{goTerms} = $c->model('PfamDB::GO')
                             ->search( { 'me.auto_pfamA' => $c->stash->{pfam}->auto_pfamA } );
    
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

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
