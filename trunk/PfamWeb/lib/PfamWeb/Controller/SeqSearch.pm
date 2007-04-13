
# SeqSearch.pm
# jt6 20061108 WTSI
#
# $Id: SeqSearch.pm,v 1.8 2007-04-13 16:10:26 jt6 Exp $

=head1 NAME

PfamWeb::Controller::SeqSearch - perform various sequence searches

=cut

package PfamWeb::Controller::SeqSearch;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: SeqSearch.pm,v 1.8 2007-04-13 16:10:26 jt6 Exp $

=cut

use strict;
use warnings;
use HTML::Widget;
use Digest::MD5 qw(md5_hex);
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Storable qw(thaw);

use Data::Dump qw(dump );

# set the default container to be the one we've defined, which makes
# the markup a little easier to style with CSS
BEGIN {
  HTML::Widget::Element->container_class( "PfamWeb::CustomContainer" );
}

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "seqsearch" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract the query terms from the URL and de-taint them.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = "search";

  # if there's no query parameter, we're done here; drop straight to the 
  # template that will render the search forms
  return unless $c->req->param( "query" );

  # get the query
  my $terms;
  ( $terms ) = $c->req->param( "query" ) =~ /^([\w\:\;\-\.\s]+)/;

  # we're done here unless there's a query specified
  $c->log->warn( "Search::begin: no query terms supplied" ) and return
	unless defined $terms;

  # stash the de-tainted terms so we can safely display them later
  $c->stash->{rawQueryTerms} = $terms;

  # somewhere for the results of this search
  $c->stash->{results} = {};

}

#-------------------------------------------------------------------------------

=head2 index : Private

Generates the default search page.

=cut

sub default : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::default: captured a URL" );

  # build the widgets and stash them

  # search by protein name 
  #  $c->stash->{proteinForm} = $c->forward( "buildProteinNameForm" )->result;

}

#-------------------------------------------------------------------------------

=head2 unp : Local

Executes a protein search.

=cut

sub unp : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a protein search" );
}

#-------------------------------------------------------------------------------

=head2 batch : Local

Executes a batch search.

=cut

sub batch : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a batch search" );
}

#-------------------------------------------------------------------------------

=head2 domain : Local

Executes a domain query.

=cut

sub domain : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a domain search" );

  $c->log->debug( "SeqSearch::domain: |" . $c->req->param( "have" ) . "|" );

  # point at the template right away
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";

  my $list;
  if( defined $c->req->param( "have" ) ) {
	foreach ( split /\s+/, $c->req->param( "have" ) ) {
	  next unless /(PF\d{5})/;
	  $list .= "+$1 ";
	}
  }
  if( defined $c->req->param( "not" ) ) {
	foreach ( split /\s+/, $c->req->param( "not" ) ) {
	  next unless /(PF\d{5})/;
	  $list .= "-$1 ";
	}
  }

  $c->log->debug( "SeqSearch::domain: list: |$list|" );

  return unless $list;

  my @architectures = $c->model("PfamDB::Architecture")
                      	->search( {},
                          			  { join     => [ qw/ annseq / ],
                            				prefetch => [ qw/ annseq / ],
                            				order_by => "no_seqs DESC" } )
                    	  ->search_literal( "MATCH( architecture_acc ) " .
                              						"AGAINST( ? IN BOOLEAN MODE )",
                              						$list );

  my $sum = 0;
  foreach my $arch ( @architectures ) {
  	$sum += $arch->no_seqs;
  }

  $c->log->debug( "SeqSearch::domain: found " . scalar @architectures
				  . " rows, with a total of $sum sequences" );

  $c->stash->{numRows} = scalar @architectures;
  $c->stash->{numSeqs} = $sum;

  # if there are too many results, bail here and let the TT just
  # display the text summary, plus an admonition to the user to
  # restrict their search a bit
  return if scalar @architectures > 500;

  # build the mappings that we'll need to interpret all this...
  my( @seqs, %seqInfo );
  foreach my $arch ( @architectures ) {

  	# thaw out the sequence object for this architecture
  	push @seqs, thaw( $arch->annseq_storable );
  
  	# work out which domains are present on this sequence
  	my @domains = split /\~/, $arch->architecture;
  	$seqInfo{$arch->pfamseq_id}{arch} = \@domains;
  
  	# store a mapping between the sequence and the auto_architecture
  	$seqInfo{$arch->pfamseq_id}{auto_arch} = $arch->auto_architecture;
  
  	# if this is a call to retrieve all of the architectures, we don't
  	# have an auto_architecture, so this won't work
  	$seqInfo{$arch->pfamseq_id}{num} = $arch->no_seqs unless $c->stash->{auto_arch};
  }
  $c->log->debug( "found " . scalar @seqs . " storables" );

  if( scalar @seqs ) {
  	my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  	
  	$layout->layout_sequences_with_regions_and_features( \@seqs, { PfamA      => 1,
                                																   PfamB      => 1,
                                																   noFeatures => 1 } );
  	
  	my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  	$imageset->create_images( $layout->layout_to_XMLDOM );
  
  	$c->stash->{images} = $imageset;
  	$c->stash->{seqInfo}  = \%seqInfo;
  }

}

#-------------------------------------------------------------------------------

=head2 seq : Local

Queues a sequence search job and returns a page that polls the server for
results.

=cut

sub seq : Local {
  my( $this, $c ) = @_;
  
  $c->log->debug( "SeqSearch::seq: form was submitted" );

  # check the input

  # the sequence itself
  if( defined $c->req->param( "seq" ) ) {

    $c->stash->{seq} = $c->forward( "parseSequence" );
    if( $c->stash->{seq} eq "" ) {
      $c->stash->{seqSearchError} = "No valid sequence found.";
    }
  } else {
    $c->stash->{seqSearchError} = "You must supply a sequence.";
    return;
  }

  # sequence search options
  if( defined $c->req->param( "seqOpts" ) ) {
    $c->stash->{seqOpts} = $c->req->param( "seqOpts" );
    unless( $c->stash->{seqOpts} eq "both" or
            $c->stash->{seqOpts} eq "ls"   or
            $c->stash->{seqOpts} eq "fs" ) {
      $c->stash->{seqSearchError} = "You must select a valid search option.";
      return;
    }
  } else {
    $c->stash->{seqSearchError} = "You must select a search option.";
    return;
  }

  # if we have an evalue, we'll use that, otherwise we'll use the gathering
  # threshold
  if( defined $c->req->param( "evalue" ) ) {
    if( looks_like_number( $c->req->param( "evalue" ) ) ) {
      $c->stash->{evalue} = $c->req->param( "evalue" );
    } else {
      $c->stash->{seqSearchError} = "You did not enter a valid E-value.";
      return;
    }
  } else {
    $c->stash->{ga} = 1;
  }

  # try to submit the search
  my $submissionStatus = $c->forward( "queueSeqSearch" );

  # and see if we managed it...
  if( $submissionStatus > 0 ) {
    $c->log->debug( "SeqSearch::seq: sequence is pre-calculated; returning results" ); 
    $c->stash->{template} = "pages/seqSearchDone.tt";

  } elsif( $submissionStatus < 0 ) {
    $c->log->debug( "SeqSearch::seq: problem with submission; re-rendering form" ); 

  } else {
    $c->log->debug( "SeqSearch::seq: sequence search submitted; polling" ); 
    $c->stash->{template} = "pages/polling.tt";
  }
}

#-------------------------------------------------------------------------------

=head2 parseSequence : Private

Parses the sequence supplied by the CGI parameter "seq". Returns the sequence
as a single string if it's parsed successfully, or the empty string if there
was a problem parsing or if the final sequence contains a character other than
[A-Za-z].

=cut

sub parseSequence : Private {
  my( $this, $c ) = @_;

  my @seqs = split /\n/, $c->req->param( "seq" );
  shift @seqs if $seqs[0] =~ /^\>/;
  my $seq = join "", @seqs;
  $seq =~ s/\s+//g;

  $c->log->debug( "SeqSearch::parseSequence: parsed sequence: |$seq|" );
  return ( $seq =~ /^[A-Z]+$/i ) ? $seq : "";
}

#-------------------------------------------------------------------------------

=head2 queueSeqSearch : Private

Executes a protein sequence search. There are three possible return values for
this action:

=over 4

=item  1 - this sequence has been seen before so we don't need to search

=item  0 - sequence was submitted to the search queue

=item -1 - there was a problem submitting the search

=over

The calling method should capture the return value and act accordingly. In the
case of an error submitting the search, there will be an entry in the stash
C<seqSearchError> containing an error message that can be sent back to the user.

=cut

sub queueSeqSearch : Private {
  my( $this, $c ) = @_;

  # calculate the MD5 checksum for the sequence we've been handed and see if
  # we've already seen that one    
  $c->stash->{md5} = md5_hex( uc( $c->stash->{seq} ) );
  $c->log->debug( "SeqSearch:: queueSeqSearch: MD5 for user sequence is: |"
                  . $c->stash->{md5}."|." );

  my $found = $c->model("PfamDB::Pfamseq")
                ->find( { md5 => $c->stash->{md5} } );

  # yes; no need to search
  if( $found ) {

    $c->log->debug( "SeqSearch::queueuSeqSearch: we've seen this sequence before" );
    return 1;

    #get storable for sequence based on md5
    #Select regions out of database based on input i.e. take care of evalue
    #Produce same XML result a script that runs on pvm cluster
    #insert into database
  }

  # no; need to search the sequence
  
  # first, check there's room in the queue
  my $rs = $c->model( "WebUser::HmmerHistory" )
             ->find( { status => "PEND" },
                     { select => [ { count => "status" } ],
                       as     => [ "numberPending" ] } );
 
  $c->stash->{numberPending} = $rs->get_column( "numberPending" );
  $c->log->debug(   "SeqSearch::queueSeqSearch: |" . $c->stash->{numberPending}
                  . "| jobs pending" );

  if( $c->stash->{numberPending} > $this->{pendingLimit} ) {
    $c->log->debug(   "SeqSearch::queueSeqSearch: too many jobs in queue ("
                    . $c->stash->{numberPending} . ")" );
    $c->stash->{seqSearchError} = "There are currently too many jobs in the sequence search queue. Please try again in a little while.";
    return -1;
  }
 
  # ok. There's room on the queue, so we can submit the job
  
  # make a guess at the runtime for the job
  $c->stash->{estimatedTime} = int( length( $c->stash->{seq} ) / 100 );
  $c->log->debug(   "SeqSearch::queueSeqSearch: estimated search time: |"
                  . $c->stash->{estimatedTime} . "| seconds" );
    
  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();

  # build the command to run
  my $cmd = "somescript.pl" 
            . " -s " . $c->stash->{seqOpts}
            . " -evalue " .$c->stash->{evalue};

  # add this job to the tracking table
  my $resultHistory = $c->model('WebUser::HmmerHistory')
                        ->create( { command => $cmd,
                                    job_id  => $c->stash->{jobId},
                                    status  => 'PEND',
                                    opened  => \'NOW()' } );
                                    
  my $resultStream = $c->model('WebUser::HmmerStream')
                       ->create( { id    => $resultHistory->id,
                                   stdin => $c->stash->{seq} || q() } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $jobStatus = {
                    estimatedTime => $c->stash->{estimatedTime},
                    jobId         => $c->stash->{jobId},
                    interval      => $this->{pollingInterval},
                  };
  $c->stash->{jobStatus} = objToJson( $jobStatus );

  return 0;
}

#-------------------------------------------------------------------------------

=head2 checkStatus : Local

Checks the status of the specified job and returns it as a plain text response.

=cut

sub checkStatus : Local {
  my( $this, $c ) = @_;

  # build a hash that we'll convert into JSON and return
  $c->stash->{status} = {};
  
  my $jobId = $c->req->param( "jobId" );

  if( length( $jobId ) != 36 or $jobId !~ /[A-Z0-9\-]/ ) {
    $c->log->debug( "SeqSearch::checkStatus: bad job id" );
    $c->stash->{status}->{error} = "Invalid job ID";
    $c->detach( "returnStatus" );
  }

  # job ID appears to be valid; try querying for the status of that job
  my $jobStatus = $c->model( "WebUser::HmmerHistory" )
                    ->find( { job_id => $jobId } );

  # make sure the query returned *something*
  unless( defined $jobStatus ) {
    $c->log->debug( "SeqSearch::checkStatus: problem retrieving job status for job |" 
                    . $jobId . "|" );
    $c->stash->{status}->{error} = "Could not retrieve job status";
    $c->detach( "returnStatus" );
  }

  # finally, check the real status 
  if( $jobStatus->status eq "PEND" ) {
    $c->log->debug( "SeqSearch::checkStatus: job is pending" );
    $c->stash->{status}->{status} = "PEND";

  } elsif( $jobStatus->status eq "RUN" ) {
    $c->log->debug( "SeqSearch::checkStatus: job is running" );
    $c->stash->{status}->{status} = "RUN";

  } elsif( $jobStatus->status eq "DONE" ) {
    $c->log->debug( "SeqSearch::checkStatus: job is done" );
    $c->stash->{status}->{status} = "DONE";
    
  } else {
    $c->log->error( "SeqSearch::checkStatus: can't determine job status" );
    $c->stash->{status}->{status} = "UNKNOWN";
  }  
      
  # add the times to the response
  $c->stash->{status}->{opened}  = $jobStatus->opened;
  $c->stash->{status}->{started} = $jobStatus->started;
  $c->stash->{status}->{closed}  = $jobStatus->closed;

  # and hand back that status
  $c->forward( "returnStatus" );
}

#-------------------------------------------------------------------------------

sub returnStatus : Private {
  my( $this, $c ) = @_;

  # convert the status hash to a JSON object and return it
  
  my $status = objToJson( $c->stash->{status} ); 
 
  $c->log->debug( "SeqSearch::returnStatus: returning: " );
  $c->log->debug( dump( $c->stash->{status} ) );
	$c->res->content_type( "application/json" );
	$c->res->body( $status );
	  
}
#-------------------------------------------------------------------------------

=head2 jobDone : Attribute

Returns the URI of the Pfam graphic that is the result of the specified job.

=cut

sub jobDone : Local {
  my( $this, $c ) = @_;
  
	$c->res->content_type( "text/plain" );
	$c->res->body( "http://deskpro16081.dynamic.sanger.ac.uk:8000/catalyst/pfam/images/pfam_logo.gif" );
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
