
# Dna.pm
# jt6 20070731 WTSI
#
# $Id: Dna.pm,v 1.6 2008-09-03 15:39:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Dna - perform batch DNA sequence searches

=cut

package PfamWeb::Controller::Search::Dna;

=head1 DESCRIPTION

This controller is responsible for running batch DNA sequence searches.

$Id: Dna.pm,v 1.6 2008-09-03 15:39:58 jt6 Exp $

=cut

use strict;
use warnings;

use Email::Valid;

use base qw( PfamBase::Controller::Search::BatchSearch
             PfamWeb::Controller::Search ); 

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Executes a batch search. 

=cut

sub search : Path {
  my( $this, $c ) = @_;

  # validate the input
  unless ( $c->forward( 'validate_input' ) ) {
    $c->stash->{dnaSearchError } = $c->stash->{searchError};
    return;
  }
  
  #----------------------------------------
  
  # no options for a DNA search
  $c->stash->{options} = '';
  
  # before we actually run the search, check we didn't do it recently
  unless ( $c->forward( 'check_unique' ) ) {
    $c->stash->{dnaSearchError } = $c->stash->{searchError};
    return;
  }

  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();

  # set the queue
  $c->stash->{job_type} = 'dna';

  # and submit the job...
  unless ( $c->forward( 'queue_search_transaction' ) ) {
    $c->stash->{dnaSearchError } = $c->stash->{searchError};
    return;
  }
  
  #----------------------------------------
  
  # set a refresh URI that will be picked up by head.tt and used in a 
  # meta refresh element
  $c->stash->{refreshUri}   = $c->uri_for( '/search' );
  $c->stash->{refreshDelay} = 30;

  $c->log->debug( 'Search::Dna::search: batch dna search submitted' ); 
  $c->stash->{template} = 'pages/search/sequence/batchSubmitted.tt';
}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Error messages are returned in the stash as
"searchError".

=cut

sub validate_input : Private {
  my( $this, $c ) = @_;
  
  # the sequence itself

  # make sure we got a parameter first
  unless ( defined $c->req->param('seq') ) {
    $c->stash->{searchError} =
      'You did not supply a valid DNA sequence. Please try again.';

    $c->log->debug( 'Search::Dna::validate_input: no DNA sequence; returning to form' )
      if $c->debug;

    return 0;
  }

  # check it's not too long
  if ( length $c->req->param('seq') > 80_000 ) {
    $c->stash->{searchError} =
      'Your sequence was too long. We can only accept DNA sequences upto 80kb.';

    $c->log->debug( 'Search::Dna::validate_input: sequence too long; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # email address
  if ( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  }
  else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::Dna::validate_input: bad email address; returning to form' )
      if $c->debug;
      
    return 0;
  }  

  # tidy up the sequence and make sure it's only got the valid DNA characters
  my @seqs = split /\n/, $c->req->param('seq');
  my $seq = uc( join '', @seqs );
  $seq =~ s/[\s\r\n]+//g;
  
  unless ( $seq =~ m/^[ACGT]+$/ ) {
    $c->stash->{searchError} =
      'No valid sequence found. Please enter a valid DNA sequence and try again.';

    $c->log->debug( 'Search::Dna::validate_input: invalid DNA sequence; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # store the valid sequence. Up until this point there was no need to have it 
  # in the stash, since it might have been invalid. Now that it's validated, 
  # however, we actually need it
  $c->log->debug( "Search::Dna::validate_input: sequence looks ok: |$seq|" )
    if $c->debug;
    
  $c->stash->{input} = $seq;
 
  # passed ! 
  $c->log->debug( 'Search::Dna::validate_input: input parameters all validated' )
    if $c->debug;
  
  return 1;
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
