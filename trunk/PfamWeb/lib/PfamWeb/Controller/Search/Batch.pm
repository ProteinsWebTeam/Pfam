
# Batch.pm
# jt6 20061108 WTSI
#
# $Id: Batch.pm,v 1.4 2007-08-14 11:36:46 rdf Exp $

=head1 NAME

PfamWeb::Controller::Search::Batch - perform protein sequence batch searches

=cut

package PfamWeb::Controller::Search::Batch;

=head1 DESCRIPTION

This controller is responsible for running batch searches for protein sequences.
It uses the base class L<Batch|PfamWeb::Controller::Search::Batch> to take
care of queuing the search, but the validation of input etc. is here.

$Id: Batch.pm,v 1.4 2007-08-14 11:36:46 rdf Exp $

=cut

use strict;
use warnings;

use Scalar::Util qw( looks_like_number );
use Email::Valid;

use base 'PfamWeb::Controller::Search::BatchSearch';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Executes a protein sequence batch search. 

=cut

sub search : Path {
  my( $this, $c ) = @_;

  # validate the input
  $c->forward( 'validateInput' );
  if( $c->stash->{searchError} ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }

  # build the command to run
  my $opts;
  $opts .=  q( --mode ) . $c->stash->{batchOpts} if( $c->stash->{batchOpts} ne 'both' and 
                                                    $c->stash->{batchOpts} ne 'bothNoMerge' );
  $opts .=  q( --no_merge )                      if( $c->stash->{batchOpts} eq 'bothNoMerge' );
  $opts .=  q( -e )     . $c->stash->{evalue}    if( $c->stash->{evalue} and not $c->stash->{ga} );
  $opts .=  q( --overlap )                       if( $c->stash->{showOverlap} );
  
  $c->stash->{options} = $opts;

  # set the queue
  $c->stash->{job_type} = 'batch';

  # and submit the job...
  $c->forward( 'queueSearch' );
  if( $c->stash->{searchError} ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }

  # if we get to here then the job was submitted successfully. Before handing
  # off to the template, set a refresh URI that will be picked up by head.tt 
  # and used in a meta refresh element
  $c->stash->{refreshUri} = $c->uri_for( '/search' ); 
  
  $c->log->debug( 'Search::Batch::search: protein batch search submitted' ); 
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
  $c->forward( 'parseUpload' );
  
  unless( $c->stash->{input} ) {
    $c->stash->{searchError} =
      'No valid sequence file found. Please enter a valid FASTA-format file and try again.';

    $c->log->debug( 'Search::Batch::search: bad FASTA file; returning to form' );
    return;
  }

  # sequence search options
  if( defined $c->req->param( 'batchOpts' ) ) {
    $c->stash->{batchOpts} = $c->req->param( 'batchOpts' );
    unless( $c->stash->{batchOpts} eq 'both' or
            $c->stash->{batchOpts} eq 'bothNoMerge' or
            $c->stash->{batchOpts} eq 'ls' or
            $c->stash->{batchOpts} eq 'fs' ) {
      $c->stash->{searchError} = 'You must select a valid search option.';

      $c->log->debug( 'Search::Batch::search: bad search option; returning to form' );
      return;
    }
  } else {
    $c->log->debug( 'Search::Batch::search: search options not specified; returning to form' );
    $c->stash->{searchError} = 'You must select a search option.';
    return;
  }

  # if we have an evalue, we'll use that, otherwise we'll use the gathering
  # threshold
  if( defined $c->req->param( 'ga' ) and $c->req->param( 'ga' ) ) {
    $c->stash->{ga} = 1;
  } else {
    if( defined $c->req->param( 'evalue' ) and 
        looks_like_number( $c->req->param( 'evalue' ) ) ) {
      $c->stash->{evalue} = $c->req->param( 'evalue' );
    } else {
      $c->stash->{searchError} = 'You did not enter a valid E-value.';

      $c->log->debug( 'Search::Batch::search: bad evalue; returning to form' );
      return;
    }
  }

  # email address
  if( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  } else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::Batch::search: bad email address; returning to form' );
    return;
  }  

  # passed !
  $c->log->debug( 'Search::Batch::search: input parameters all validated' );
}

#-------------------------------------------------------------------------------

=head2 parseUpload : Attribute

Parses the uploaded file and, if it's valid, copies it to the stash.

=cut

sub parseUpload : Private {
  my( $this, $c ) = @_;

  # check that we can get an Catalyst::Request::Upload object from the request 
  my $u;
  return unless( $u = $c->req->upload('batchSeq') );
  
  # check that the Upload object returns us a filehandle
  my $fh;
  unless( $fh = $u->fh ) {
    $c->log->warn( 'Search::Batch::parseUpload: couldn\'t open uploaded file' );
    return;
  }

  # read through the file and bail if we find any illegal characters in it  
  my $seqs;
  while( <$fh> ) {
    unless( /^>[A-Za-z0-9\-_]+$/ or /^[A-Za-z]+$/ ) {
      $c->log->debug( 'Search::Batch::parseUpload: illegal character in upload; bailing' );
      return;
    }
    $seqs .= $_;
  }

  # the upload passed the illegal characters test; stash it as a string
  $c->stash->{input} = $seqs;
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
