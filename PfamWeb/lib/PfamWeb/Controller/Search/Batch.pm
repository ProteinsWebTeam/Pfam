
# Batch.pm
# jt6 20061108 WTSI
#
# $Id: Batch.pm,v 1.17 2009-10-07 12:02:06 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Batch - perform protein sequence batch searches

=cut

package PfamWeb::Controller::Search::Batch;

=head1 DESCRIPTION

This controller is responsible for running batch searches for protein sequences.
It uses the base class L<Batch|PfamWeb::Controller::Search::Batch> to take
care of queuing the search, but the validation of input etc. is here.

$Id: Batch.pm,v 1.17 2009-10-07 12:02:06 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );
use JSON;
use Scalar::Util qw( looks_like_number );
use Email::Valid;

use base qw( PfamBase::Controller::Search::BatchSearch
             PfamWeb::Controller::Search );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Executes a protein sequence batch search. 

=cut

sub search : Path {
  my( $this, $c ) = @_;

  # validate the input
  unless ( $c->forward( 'validate_input' ) ) {
    $c->stash->{batchSearchError} = $c->stash->{searchError};
    return;
  }

  #----------------------------------------

  # build the command to run
  # $c->stash->{options}  = '';
  # $c->stash->{options} .=  q( --mode ) . $c->stash->{batchOpts} if( $c->stash->{batchOpts} ne 'both' and 
  #                                                                   $c->stash->{batchOpts} ne 'bothNoMerge' );
  # $c->stash->{options} .=  q( --no_merge )                      if( $c->stash->{batchOpts} eq 'bothNoMerge' );
  # $c->stash->{options} .=  q( -e )     . $c->stash->{evalue}    if( $c->stash->{evalue} and not $c->stash->{ga} );
  # $c->stash->{options} .=  q( --overlap )                       if( $c->stash->{showOverlap} );
  
  # finally, before we actually run the search, check we didn't do it recently 
  unless ( $c->forward( 'check_unique' ) ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }
  
  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  
  # set the queue
  $c->stash->{job_type} = 'batch';

  # convert the options hash into JSON
  $c->log->debug( 'Search::Batch::search: user_options: ', dump( $c->stash->{user_options} ) )
    if $c->debug;
  $c->stash->{options} = to_json( $c->stash->{user_options} );

  # and submit the job...
  unless ( $c->forward( 'queue_search_transaction' ) ) {
    $c->stash->{batchSearchError} = $c->stash->{searchError};
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
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

sub validate_input : Private {
  my( $this, $c ) = @_;

  $c->stash->{user_options} = {};

  # do the quick checks first...
  
  # available sequence search options
  my %available_options = ( both        => 1,
                            bothNoMerge => 1,
                            ls          => 1,
                            fs          => 1 );

  # the user supplied an option; check it's valid
  if ( defined $c->req->param('batchOpts') ) {
    
    unless ( defined $available_options{ $c->req->param('batchOpts') } ) {
      $c->stash->{searchError} = 'You must use a valid search option.';

      $c->log->debug( 'Search::Batch::validate_input: bad search option; returning to form' )
        if $c->debug;

      return 0;
    }
    
    $c->stash->{user_options}->{batchOpts} = $c->req->param('batchOpts');
  }

  # default to "both"
  else {
    $c->stash->{user_options}->{batchOpts} = 'both';

    $c->log->debug( 'Search::Batch::validate_input: setting search option to default of "both"' )
      if $c->debug;
  }

  # if we have an E-value, we'll use that, otherwise we'll use the gathering
  # threshold
  if ( defined $c->req->param( 'ga' ) and $c->req->param( 'ga' ) ) {
    $c->stash->{user_options}->{ga} = 1;
  } 
  else {

    # check that the E-value is valid
    if ( defined $c->req->param( 'evalue' ) and 
         looks_like_number( $c->req->param( 'evalue' ) ) and
         $c->req->param('evalue') > 0 ) {
      $c->stash->{user_options}->{evalue} = $c->req->param( 'evalue' );
    }
    else {
      $c->stash->{searchError} = 'You did not enter a valid E-value.';

      $c->log->debug( 'Search::Batch::validate_input: bad evalue; returning to form' )
        if $c->debug;
        
      return 0;
    }
  }

  # email address
  if ( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email'); 
    # Note: this is not a "user_option" ! Needs to be dropped into it's own column in
    # the job_history table
  }
  else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::Batch::validate_input: bad email address; returning to form' )
      if $c->debug;
    
    return 0;
  }  

  # search for Pfam-Bs ?
  $c->stash->{user_options}->{searchBs} = ( defined $c->req->param('searchBs') and
                                            $c->req->param('searchBs') );

  # should we search for Pfam-As or just skip them and search only Pfam-Bs ?
  if ( defined $c->req->param('skipAs') and
       $c->req->param('skipAs') ) {
    
    $c->log->debug( 'Search::Batch::validate_input: skipping Pfam-A search' )
      if $c->debug;
    
    # flag up the fact that we want to skip Pfam-A searches
    $c->stash->{user_options}->{skipAs} = 1;
    
    # and force a search for Pfam-Bs
    $c->stash->{user_options}->{searchBs} = 1;
  }

  # finally, the most time-consuming tests: check the sequence upload itself
  unless ( $c->forward( 'parse_upload' ) ) {

    $c->stash->{searchError} = $c->stash->{searchError}
      || 'No valid sequence file found. Please enter a valid FASTA-format file and try again.';

    $c->log->debug( 'Search::Batch::validate_input: bad FASTA file; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # passed !
  $c->log->debug( 'Search::Batch::validate_input: input parameters all validated' )
    if $c->debug;
    
  $c->log->debug( 'Search::Batch::validate_input: user_options: ', dump( $c->stash->{user_options} ) )
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
