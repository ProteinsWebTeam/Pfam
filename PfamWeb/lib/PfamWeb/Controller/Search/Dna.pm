
# Dna.pm
# jt6 20070731 WTSI
#
# $Id: Dna.pm,v 1.1 2007-07-31 21:59:36 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Dna - perform batch DNA sequence searches

=cut

package PfamWeb::Controller::Search::Dna;

=head1 DESCRIPTION

This controller is responsible for running batch DNA sequence searches.

$Id: Dna.pm,v 1.1 2007-07-31 21:59:36 jt6 Exp $

=cut

use strict;
use warnings;

use Email::Valid;

use base 'PfamWeb::Controller::Search::Batch';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Executes a batch search. 

=cut

sub search : Path {
  my( $this, $c ) = @_;

  # validate the input
  $c->forward( 'validateInput' );
  if( $c->stash->{searchError} ) {
    $c->stash->{dnaSearchError } = $c->stash->{searchError};
    return;
  }

  # build the command to run
  my $cmd;
  $cmd  =  q(pfam_scan.pl -pvm -align -d ) . $this->{blastDb};
  $cmd .=  q( --mode ) . $c->stash->{batchOpts} if( $c->stash->{batchOpts} ne 'both' and 
                                                    $c->stash->{batchOpts} ne 'bothNoMerge' );
  $cmd .=  q( --no_merge )                      if( $c->stash->{batchOpts} eq 'bothNoMerge' );
  $cmd .=  q( -e )     . $c->stash->{evalue}    if( $c->stash->{evalue} and not $c->stash->{ga} );
  $cmd .=  q( --overlap )                       if( $c->stash->{showOverlap} );
  $cmd .=  q( /tmp/) . $c->stash->{jobId} . q(.fa );

  $c->stash->{cmd} = $cmd;

  # set the queue
  $c->stash->{priority} = 'dna';

  # and submit the job...
  $c->forward( 'queueSearch' );
  if( $c->stash->{searchError} ) {
    $c->stash->{dnaSearchError } = $c->stash->{searchError};
    return;
  }

  # set a refresh URI that will be picked up by head.tt and used in a 
  # meta refresh element
  $c->stash->{refreshUri} = $c->uri_for( '/search' );

  $c->log->debug( 'Search::Dna::search: batch dna search submitted' ); 
  $c->stash->{template} = 'pages/search/sequence/batchSubmitted.tt';
}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validateInput : Private

Validate the form input. Error messages are returned in the stash as
"searchError".

=cut

sub validateInput : Private {
  my( $this, $c ) = @_;
  
  # the sequence itself

  # make sure we got a parameter first
  unless( defined $c->req->param('seq') ) {
    $c->stash->{searchError} =
      'You did not supply a valid DNA sequence. Please try again.';

    $c->log->debug( 'Search::Dna::validateInput: no DNA sequence; returning to form' );
    return;
  }

  # tidy up the sequence and make sure it's only got the valid DNA characters
  my @seqs = split /\n/, $c->req->param('seq');
  my $seq = uc( join '', @seqs );
  $seq =~ s/[\s\r]+//g;
  
  unless( $seq =~ m/^[ACGT]+$/ ) {
    $c->stash->{searchError} =
      'No valid sequence found. Please enter a valid DNA sequence and try again.';

    $c->log->debug( 'Search::Dna::validateInput: invalid DNA sequence; returning to form' );
    return;
  }

  # email address
  if( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  } else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::Dna::validateInput: bad email address; returning to form' );
    return;
  }  

  # passed !

  # store the valid sequence - no point having it in the stash if it's never
  # going to be used, but now we actually need it
  $c->log->debug( "Search::Dna::validateInput: sequence looks ok: |$seq|" );
  $c->stash->{input} = $seq;
  
  $c->log->debug( 'Search::Dna::validateInput: input parameters all validated' );
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
