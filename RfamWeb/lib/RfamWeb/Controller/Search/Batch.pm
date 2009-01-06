
# Batch.pm
# jt6 20061108 WTSI
#
# $Id: Batch.pm,v 1.2 2009-01-06 11:53:11 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Search::Batch - perform nucleic acid sequence batch searches

=cut

package RfamWeb::Controller::Search::Batch;

=head1 DESCRIPTION

This controller is responsible for running batch searches for nucleic acid 
sequences.

$Id: Batch.pm,v 1.2 2009-01-06 11:53:11 jt6 Exp $

=cut

use strict;
use warnings;

use Scalar::Util qw( looks_like_number );
use Email::Valid;

use base qw( PfamBase::Controller::Search::BatchSearch
             RfamWeb::Controller::Search );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Executes a nucleic acid sequence batch search. 

=cut

sub search : Path {
  my ( $this, $c ) = @_;

  # validate the input
  unless ( $c->forward('validate_input') ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }

  #----------------------------------------

  # no options for an Rfam batch search
  
  # before we actually run the search, check we didn't do it recently 
  unless ( $c->forward( 'check_unique' ) ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }
  
  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  
  # set the queue
  $c->stash->{job_type} = 'rfam_batch';

  # and submit the job...
  unless ( $c->forward( 'queue_search_transaction' ) ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }

  #----------------------------------------

  # if we get to here then the job was submitted successfully. Before handing
  # off to the template, set a refresh URI that will be picked up by head.tt 
  # and used in a meta refresh element
  $c->stash->{refreshUri}   = $c->uri_for( '/search' );
  $c->stash->{refreshDelay} = 30;
  
  $c->log->debug( 'Search::Batch::search: protein batch search submitted' )
    if $c->debug; 
  $c->stash->{template} = 'pages/search/sequence/batchSubmitted.tt';
}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

sub validate_input : Private {
  my( $this, $c ) = @_;

  # do the quick checks first...
  
  # email address
  if ( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  }
  else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::Batch::validate_input: bad email address; returning to form' )
      if $c->debug;
    
    return 0;
  }  

  # finally, the most time-consuming tests: check the sequence upload itself
  unless ( $c->forward( 'parse_upload' ) ) {

    $c->stash->{searchError} = 
         $c->stash->{searchError}
      || 'No valid sequence file found. Please enter a valid FASTA-format file and try again.';

    $c->log->debug( 'Search::Batch::validate_input: bad FASTA file; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # passed !
  $c->log->debug( 'Search::Batch::validate_input: input parameters all validated' )
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
