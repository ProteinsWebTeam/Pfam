
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.18 2008-05-09 15:22:13 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Sequence - perform sequence searches

=cut

package PfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: Sequence.pm,v 1.18 2008-05-09 15:22:13 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::SearchIO::blast;
use File::Temp qw( tempfile );
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;

use constant SUBMISSION_ERROR   => -1;
use constant SUBMISSION_SUCCESS => 0;

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 sequence_search : Path

Queues a sequence search job and returns a page that polls the server for
results.

=cut

sub sequence_search : Path {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Search::Sequence::sequence_search: form was submitted' )
    if $c->debug;

  # check the input
  CHECK:
  {

    # parse and validate the sequence itself
    $c->forward('parse_sequence');
    unless ( defined $c->stash->{seq} ) {
      # the parse_sequence method will put the sequence into the stash if it
      # passes validation, but if the sequence looks like DNA or if it's
      # too long, we also get an error message in the stash. So, we only set 
      # a general error message here if we don't already have one
      $c->stash->{seqSearchError} ||=
        'Invalid sequence. Please try again with a valid amino-acid sequence';
      last CHECK;
    }
    
    # search for Pfam-Bs ?
    $c->stash->{searchBs} = ( defined $c->req->param('searchBs') and
                              $c->req->param('searchBs') );

    # should we search for Pfam-As or just skip them and search only Pfam-Bs ?
    if ( defined $c->req->param('skipAs') and
         $c->req->param('skipAs') ) {
      
      $c->log->debug( 'Search::Sequence::sequence_search: skipping Pfam-A search' )
        if $c->debug;

      # flag up the fact that we want to skip Pfam-A searches
      $c->stash->{skipAs} = 1;

      # and force a search for Pfam-Bs
      $c->stash->{searchBs} = 1;
      
      # no need to check these parameters unless we're actually doing a Pfam-A
      # search, so we're done checking
      last CHECK;
      
    }
    
    # sequence search options. Default to "both"
    $c->stash->{seqOpts} = $c->req->param('seqOpts') || 'both';
    unless ( $c->stash->{seqOpts} eq 'both' or
             $c->stash->{seqOpts} eq 'bothNoMerge' or
             $c->stash->{seqOpts} eq 'ls' or
             $c->stash->{seqOpts} eq 'fs' ) {
      $c->log->debug( 'Search::Sequence::sequence_search: bad search option; returning to form' )
        if $c->debug;
      $c->stash->{seqSearchError} = 'You must use a valid search option';
      last CHECK;
    }

    # if we have an evalue, we'll use that, unless we've been asked to use
    # the gathering threshold. Default to using an evalue of 1.0
    if ( defined $c->req->param('evalue') ) {
      $c->log->debug( 'Search::Sequence::sequence_search: got an evalue' )
        if $c->debug;
      
      if ( looks_like_number( $c->req->param('evalue') ) ) {
        $c->log->debug( 'Search::Sequence::sequence_search: evalue looks like a number' )
          if $c->debug;
        $c->stash->{evalue} = $c->req->param('evalue');
      }
      else {
        $c->log->debug( 'Search::Sequence::sequence_search: bad evalue; returning to form' )
          if $c->debug;
        $c->stash->{seqSearchError} = 'You did not give a valid E-value';
      }

    }
    elsif ( defined $c->req->param('ga') and 
            $c->req->param('ga') ) {
      $c->log->debug( 'Search::Sequence::sequence_search: using ga' )
        if $c->debug;
      $c->stash->{ga} = 1;

    }
    else {
      $c->log->debug( 'Search::Sequence::sequence_search: using default evalue' )
        if $c->debug;
      $c->stash->{evalue} = 1.0;
    }

  } # end of "CHECK"
  
  #----------------------------------------

  # if there was an error, decide how to show it to the user
  if ( $c->stash->{seqSearchError} ) {

    # if we're returning XML, we need to set a template to render the error
    # message. If we're emitting HTML, the end action (ultimately on Section) 
    # will take of us and return us to the HTML page containing search form 
    # and show the error message
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
    }
  
    return;
  }

  #----------------------------------------

  # no errors with the input; try to submit the search
  my $submissionStatus = $c->forward( 'queue_seq_search' );

  # and see if we managed it...
  if ( $submissionStatus < 0 ) {

    $c->log->debug( 'Search::Sequence::sequence_search: problem with submission; re-rendering form' )
      if $c->debug;       
    $c->stash->{seqSearchError} ||=
      'There was an unknown problem submitting your search';

    # point to the XML error template if emitting XML, otherwise, we're just 
    # done here 
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
    }
  
  }
  else {

    $c->log->debug( 'Search::Sequence::sequence_search: sequence search submitted; polling' )
      if $c->debug; 

    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/poll_xml.tt';
      $c->res->content_type('text/xml');
    } else {
      $c->stash->{template} = 'pages/search/sequence/polling.tt';
    }

  }

}

#-------------------------------------------------------------------------------

=head2 results : Local

Returns the result of the specified job(s).

=cut

sub results : Local {
  my ( $this, $c ) = @_;

  # try to retrieve the results for the specified jobs
  my @jobIds = $c->req->param( 'jobId' );
  foreach my $job_id ( @jobIds ) {
    next unless $job_id =~ m/^([A-F0-9\-]{36})$/;
    $c->forward( 'JobManager', 'retrieveResults', [ $job_id  ] );
    $c->forward( 'handleResults', [ $job_id  ] ) if $c->stash->{results};
  }

  # should we output XML ?
  if ( $c->stash->{output_xml} ) {
    $c->stash->{template} = 'rest/search/results_xml.tt';
    $c->res->content_type('text/xml');    
  }
  # no; render the HTML template
  else {
    if ( scalar keys %{ $c->stash->{results} } ) {
      $c->stash->{template} = 'pages/search/sequence/results.tt';
      $c->forward( 'generateGraphic' );
    } else {
      $c->log->debug( 'Search::Sequence::results: no results found' ) if $c->debug;
      $c->stash->{template} = 'pages/search/sequence/error.tt';
    }
  }

}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 parse_sequence : Private

Parses the sequence supplied by the CGI parameter "seq". Drops the sequence 
into the stash if it passed validation. Sets an error message in the stash if 
there was a specific problem.

=cut

sub parse_sequence : Private {
  my ( $this, $c ) = @_;

  # make sure we actually have a sequence...
  return unless ( defined $c->req->param('seq') and
                  $c->req->param('seq') ne '' );
  
  # break the string into individual lines and get rid of any FASTA header lines
  # before recombining
  my @seqs = split /\n/, $c->req->param('seq');
  shift @seqs if $seqs[0] =~ /^\>/;
  my $seq = uc( join '', @seqs );

  # handle various line endings. No need to worry about \n, since we got rid of
  # that with the "split" above
  $seq =~ s/[\s\r\d]+//g;

  # check the length of the sequence at this point. If it's too long, bail
  my $length = length $seq;
  if ( $length > $this->{maxSeqLength} ) {
    $c->stash->{seqSearchError} = 
      'Your sequence is too long. The maximum length of search sequences is ' .
      $this->{maxSeqLength} . '. Please try again with a shorter sequence';
    return;
  }

  # we need to make sure that the sequence is really protein and not, as we
  # commonly get, a bloody great DNA sequence. Count the number of potential 
  # nucleotides in the sequence and see what proportion of the total sequence
  # that makes
  my ( $nucleotide_count )= $seq =~ tr/ATCGU/ATCGU/;
  
  # if the sequence is more than 100 residues (or bases) and is more than
  # 95% nucleotides, there's a problem
  if ( $length > 100 and $nucleotide_count / $length > 0.95 ) {
    $c->stash->{seqSearchError} = 
      'Your sequence does not look like protein. Please upload a protein sequence';
    return;
  }

  # finally, check that the sequence string contains only letters. Bail if it 
  # has anything else in it
  unless ( $seq =~ m/^[A-Za-z]+$/ ) {
    $c->stash->{seqSearchError} = 
      'Invalid sequence. Please try again with a valid amino-acid sequence';
    return;
  }

  # passed all checks; stuff the sequence into the stash
  $c->stash->{seq} = $seq;
}

#-------------------------------------------------------------------------------

=head2 queue_seq_search : Private

Executes a protein sequence search. Queues a Pfam A search to one queue
and, if the appropriate box was checked in the submission form, a Pfam B search
to another queue.

=cut

sub queue_seq_search : Private {
  my ( $this, $c ) = @_;
  
  # first, check there's room on the queue
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->find( { status => 'PEND' },
                     { select => [ { count => 'status' } ],
                       as     => [ 'numberPending' ] } );
  
  $c->stash->{numberPending} = $rs->get_column( 'numberPending' );
  $c->log->debug( 'Search::Sequence::queue_seq_search: |' . 
                  $c->stash->{numberPending} . '| jobs pending' ) if $c->debug;
  
  if ( $c->stash->{numberPending} >= $this->{pendingLimit} ) {
    $c->log->debug( 'Search::Sequence::queue_seq_search: too many jobs in queue ('
                    . $c->stash->{numberPending} . ')' ) if $c->debug;
    $c->stash->{seqSearchError} = 
      'There are currently too many jobs in the sequence search queue. ' . 
      'Please try again in a little while';
    return SUBMISSION_ERROR;
  }
  
  #----------------------------------------
  
  # ok. There's room on the queue, so we can submit the hmmer job and, if 
  # required, the blast job
  my @jobs;
  
  if ( not $c->stash->{skipAs} ) {
    my $status = $c->forward( 'queue_pfam_a' );
    if ( ref $status ) {
      push @jobs, $status;
    }
    else {
      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-A search' )
        if $c->debug;
      $c->stash->{seqSearchError} = 'There was a problem queuing your Pfam-A search';
      return SUBMISSION_ERROR;
    } 
  } 
  
  if ( $c->stash->{searchBs} ) {
    my $status = $c->forward( 'queue_pfam_b' );
    if ( ref $status ) {
      push @jobs, $status;
    }
    else {
      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-B search' )
        if $c->debug;
      $c->stash->{seqSearchError} = 'There was a problem queuing your Pfam-B search';
      return SUBMISSION_ERROR;
    } 
  }
  
  # make sure we have at least one job...
  unless ( scalar @jobs ) {
    $c->log->warn( 'Search::Sequence::queue_seq_search: no searches submitted' )
      if $c->debug;
    $c->stash->{seqSearchError} = 'You must run at least one type of search';
    return SUBMISSION_ERROR;
  }
  
  #----------------------------------------
  
  # if we get to here, the job submissions worked. Now build a job status data 
  # structure that we'll convert to JSON and hand back to the javascript on the 
  # client side
  $c->stash->{jobStatus}     = \@jobs;
  $c->stash->{jobStatusJSON} = objToJson( \@jobs );
  
  $c->log->debug( 'Search::Sequence::queue_seq_search: json string: |' . $c->stash->{jobStatusJSON} . '|' ) 
    if $c->debug;
  
  return SUBMISSION_SUCCESS;
}

#-------------------------------------------------------------------------------

=head2 queue_pfam_a : Private

Submits a pfam A search.

=cut

sub queue_pfam_a : Private {
  my ( $this, $c ) = @_;
  
  # make a guess at the runtime for the job
  $c->log->debug( 'Search::Sequence: multiplier is '
                  . $this->{pfamA_search_multiplier} . '|' ) if $c->debug;
  $c->log->debug( 'Search::Sequence: sequence is :'
                  . length( $c->stash->{seq} ) . '| residues long' ) if $c->debug;
  my $estimated_time = int( $this->{pfamA_search_multiplier} * length( $c->stash->{seq} ) / 100 );
  ( $estimated_time *= 2 ) if ( $c->stash->{seqOpts} eq 'both' or
                                $c->stash->{seqOpts} eq 'bothNoMerge' );
  $c->log->debug(  q(Search::Sequence::queue_pfam_a: estimated search time: ) .
                  qq(|$estimated_time| seconds) ) if $c->debug;
  
  # generate a job ID
  my $job_id = Data::UUID->new()->create_str();
  
  # build the command options to run
  my $opts;
  $opts .=  q( --mode ) . $c->stash->{seqOpts} if( $c->stash->{seqOpts} ne 'both' and 
                                                   $c->stash->{seqOpts} ne 'bothNoMerge' );
  $opts .=  q( --no_merge )                    if( $c->stash->{seqOpts} eq 'bothNoMerge' );
  $opts .=  q( -e )     . $c->stash->{evalue}  if( $c->stash->{evalue} and not $c->stash->{ga} );
  $opts .=  q( --overlap )                     if( $c->stash->{showOverlap} );
  
  # describe the job
  my $job_spec = { options        => $opts,
                   job_type       => 'hmmer',
                   job_id         => $job_id,
                   estimated_time => $estimated_time,
                   opened         => \'NOW()',
                   status         => 'PEND' };
  
  # queue it up and retrieve the row in the job_history table for this job
  my $history_row = $c->forward( 'queue_job', [ $job_spec ] );
  
  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $job_status;
  
  # first, check if the submission was a success...
  if ( ref $history_row ) {
    $c->log->debug( 'Search::Sequence::queue_pfam_a: successfully queued job' )
      if $c->debug;
    
    $job_status = { checkURI      => $c->uri_for( '/jobmanager/checkStatus' )->as_string,
                    doneURI       => $c->uri_for( '/search/sequence/results' )->as_string,
                    estimatedTime => $estimated_time,
                    interval      => $this->{pollingInterval},
                    jobId         => $job_id,
                    name          => 'Pfam A search',
                    jobClass      => 'pfamASearch',
                    opened        => $history_row->opened };
  }
  
  return $job_status;
}

#-------------------------------------------------------------------------------

=head2 queue_pfam_b : Private

Submits a pfam B search.

=cut

sub queue_pfam_b : Private {
  my ( $this, $c ) = @_;

  # make a guess at the runtime for the job
  my $estimated_time = int( $this->{pfamB_search_multiplier} * length( $c->stash->{seq} ) / 100 );
  $c->log->debug(  q(Search::Sequence::queue_pfam_b: estimated search time: ) .
                  qq(|$estimated_time| seconds) ) if $c->debug;

  # generate a job ID
  my $job_id = Data::UUID->new()->create_str();

  # add this job to the tracking table
  my $job_spec = { job_type        => 'pfamb',
                   estimated_time => $estimated_time,
                   job_id         => $job_id,
                   opened         => \'NOW()',
                   status         => 'PEND' };

  # queue it up and retrieve the row in the job_history table for this job
  my $history_row = $c->forward( 'queue_job', [ $job_spec ] );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $job_status;

  # first, check if the submission was a success...
  if ( ref $history_row ) {
    $c->log->debug( 'Search::Sequence::queue_pfam_b: successfully queued job' )
      if $c->debug;
    
    $job_status = { checkURI      => $c->uri_for( '/jobmanager/checkStatus' )->as_string,
                    doneURI       => $c->uri_for( '/search/sequence/results' )->as_string,
                    estimatedTime => $estimated_time,
                    interval      => $this->{pollingInterval},
                    jobId         => $job_id,
                    name          => 'Pfam B search',
                    jobClass      => 'pfamBSearch',
                    opened        => $history_row->opened };
  }

  return $job_status;
}

#-------------------------------------------------------------------------------

=head2 queue_job : Private

Queues a search job. This requires new rows to be added to both the job_history
and the job_stream tables. We add these in a transaction block, rolling back if
either of the two fails. Returns the DBIC ResultSet from the job_history table
if the submission is successful, -1 (constant SEARCH_ERROR) otherwise.

=cut

sub queue_job : Private {
  my ( $this, $c, $job_spec ) = @_;
  
  # set up an anonymous code block to define a transaction. We want to make sure
  # that we can add a row to both tables before we know that this job has been 
  # successfully queued

  # somewhere to stuff the rows from the job_history and job_stream tables, 
  # if we get them
  my ( $job_history, $job_stream );

  my $transaction = sub {
    $c->log->debug( 'Search::Sequence::queue_job: starting transaction...' )
      if $c->debug;

    # add this job to the tracking table
    $job_history = $c->model( 'WebUser::JobHistory' )
                     ->create( $job_spec );
                                   
    die 'error: failed to add job_history row' unless defined $job_history;
    
    $c->log->debug( 'Search::Sequence::queue_job: added row to job_history' )
      if $c->debug;

    # and to the input/output table
    my $job_stream = $c->model( 'WebUser::JobStream' )
                       ->create( { id    => $job_history->id,
                                   stdin => $c->stash->{seq} || q() } );

    die 'error: failed to add job_stream row' unless defined $job_stream;
    
    $c->log->debug( 'Search::Sequence::queue_job: added row to job_stream' )
      if $c->debug;
    
    # check the submission time with a separate query. We need to do this so
    # that we get the "opened" time that is inserted by the database engine. The
    # job_history object that we have doesn't contain that at this point
    my $history_row = $c->model( 'WebUser::JobHistory' )
                        ->find( { id => $job_history->id } );

    die "error: couldn't retrieve job history row" unless defined $history_row;

    $c->log->debug( 'Search::Sequence::queue_job: job opened: |'
                    . $history_row->opened . '|' ) if $c->debug;
  
    return $history_row; # return from anonymous transaction sub 
  };
  
  my $history_row;
  eval {
    $history_row = $c->model('PfamDB')->schema->txn_do( $transaction );
  };
  if ( $@ ) {
    $c->log->error( "Search::Sequence::queue_job: error in transaction: |$@|" )
      if $c->debug;

    if ( defined $job_history ) {
      $job_history->status('FAIL');
      
      my $updated = $job_history->update;
      if ( $updated ) {
        $c->log->debug( 'Search::Sequence::queue_job: successfully rolled back job_history' )
          if $c->debug;
      }
      else {
        $c->log->warn( 'Search::Sequence::queue_job: failed to roll back job_history' )
          if $c->debug;
      }
    }

    return SUBMISSION_ERROR;
  }
 
  return $history_row
}

#-------------------------------------------------------------------------------

=head2 generateGraphic : Private

Generate the Pfam graphic from the generic results.

=cut

sub generateGraphic : Private {
  my ( $this, $c ) = @_;

  # Convert the generic results into BioPerl objects and subsequently generate 
  # the graphic.... This may seem a waste, but it abstracts us from changes to 
  # the XML.   
  
  # Generate a sequence object for the query sequence
  my $fac = Bio::Pfam::AnnSeqFactory->new;
  my $annseq = $fac->createAnnotatedSequence();
  my @seqs;
  push @seqs, $annseq;

  $annseq->sequence( 
    Bio::Pfam::SeqPfam->new( '-seq'      => $c->{stash}->{seq},
                             '-start'    => 1,
                             '-end'      => length($c->{stash}->{seq}),
                             '-id'       => 'QuerySeq',
                             '-acc'      => 'QuerySeq',
                             '-organism' => 'Unknown',
                             '-desc'     => 'QuerySeq' )
  );
  
  # for each Pfam-A region, make the PfamRegion object
  foreach my $pfamA ( @{ $c->{stash}->{genPfamARes} } ) {
    next unless $pfamA->{significant};

    $annseq->addAnnotatedRegion(
      Bio::Pfam::PfamRegion->new( '-PFAM_ACCESSION' => $pfamA->{pfama_acc},
                                  '-PFAM_ID'        => $pfamA->{pfama_id},
                                  '-SEQ_ID'         => $annseq->id,
                                  '-FROM'           => $pfamA->{start},
                                  '-TO'             => $pfamA->{end},
                                  '-MODEL_FROM'     => $pfamA->{hmm_start},
                                  '-MODEL_TO'       => $pfamA->{hmm_end},
                                  '-MODEL_LENGTH'   => $pfamA->{hmm_length},
                                  '-BITS'           => $pfamA->{bits},
                                  '-EVALUE'         => $pfamA->{evalue},
                                  '-ANNOTATION'     => $pfamA->{desc},
                                  '-REGION'         => $pfamA->{type},
                                  '-TYPE'           => 'pfama' )
    );

    # if we have active sites, we want to mark them in the final Pfam graphic
    if ( scalar @{ $pfamA->{sites} } ) {
      
      foreach my $as ( @{ $pfamA->{sites} } ) {
        
        # get the residue type
        my $as_res = substr $c->stash->{seq}, $as - 1, 1;
        $c->log->debug( "adding active site at: |$as_res|$as|" ) if $c->debug;
        
        # add a feature and let the graphics code take care of drawing it as
        # a lollipop later
        $annseq->addFeature(
          Bio::SeqFeature::Generic->new(
            -start        => $as,
            -primary      => 'Pfam predicted active site',
            -source_tag   => 'pfam_predicted_active_site',
            -display_name => "Pfam predicted active site, $as_res$as"
          )
        );
        
      } # end of "foreach active site"

    } # end of "if any active sites"

  } # end of "foreach Pfam-A region"
                                                     
  # now do the same for any Pfam-B hits  
  foreach my $pfamB ( @{ $c->{stash}->{genPfamBRes} } ) {
    $annseq->addAnnotatedRegion(
      Bio::Pfam::PfamRegion->new( '-PFAM_ACCESSION' => $pfamB->{pfamb_acc},
                                  '-PFAM_ID'        => $pfamB->{pfamb_id},
                                  '-SEQ_ID'         => $annseq->id,
                                  '-FROM'           => $pfamB->{start},
                                  '-TO'             => $pfamB->{end},
                                  '-TYPE'           => 'pfamb' )
    );  
  }
  
  # ow generate the image object that can be used for generating the graphic.
  # The actual image is printed within the template
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->layout_sequences( @seqs);

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );
  $c->stash->{images} = $imageset;
}

#-------------------------------------------------------------------------------

=head2 handleResults : Private

Parse the results and filter based on the the users defined parameters. The 
parsed results are put in a very generic format so that they can then be used 
for generating the results tables and graphics.

=cut

sub handleResults : Private {
  my ( $this, $c, $jobId ) = @_;

  $c->log->debug( "Search::Sequence::handleResults: handling results for |$jobId|" )
    if $c->debug;
  
  if( $c->{stash}->{results}->{$jobId}->{method} eq 'hmmer' ) {
    $c->forward( 'handlePfamAResults', [ $jobId ] );
  }
  elsif ( $c->{stash}->{results}->{$jobId}->{method} eq 'pfamb' ) {
    $c->forward( 'handlePfamBResults', [ $jobId ] );
  }

}

#-------------------------------------------------------------------------------

=head2 handlePfamAResults : Private

Does exactly what it says on the tin. Rob's handiwork...

=cut

sub handlePfamAResults : Private {
  my ( $this, $c, $jobId ) = @_;
  
  # we performed a hmmer search, must be a pfamA
  my ( $userEvalue ) = $c->{stash}->{results}->{$jobId}->{options} =~ m/-e (\S+)/;
  
  # are we using GA cut-offs of Evalues?
  $c->stash->{evalue} = $userEvalue ? $userEvalue : 0;
  
  # read in the pfam_scan data. This assumes that pfam_Scan is spitting out 
  # alignments so each domain is represented by 4 lines. 
  my @results = split /\n/, $c->{stash}->{results}->{$jobId}->{rawData};
  
  my @rawPfamAResults;
  while ( @results ) {
    my @set = splice( @results, 0, 4 );
    
    my ( $start, $end, $pfamA_acc, $hmmStart, $hmmEnd, $mode, $bits, 
         $evalue, $pfamA_id, $as, 
         @as_residues, $aliHmm, $aliMatch, $aliSeq, $s, $pfamData );
    foreach ( @set ) {
      
      #Line 1 is the domain positional information, lines 2-4 contain the 
      #actual alignment
      # UserSeq     33   142 PF00169.20      1    92 ls    42.8   1.2e-09  PH
      # SEQUENCE     12   290 PF00246.15      1   323 ls   474.8  1.1e-139     Peptidase_M14           predicted_active_site[241,264]
      
      if ( m/
             ^\S+\s+            #     sequence identifier
             (\d+)\s+           #  1: start residue   (sequence)  
             (\d+)\s+           #  2: HMM end residue (sequence)
             (PF\d{5})\.\d+\s+  #  3: Pfam-A accession . version
             (\d+)\s+           #  4: HMM start residue
             (\d+)\s+           #  5: HMM end residue
             (\S+)\s+           #  6: mode (fs|ls)
             (\S+)\s+           #  7: bits score
             (\S+)\s+           #  8: E-value
             (\S+)\s+           #  9: Pfam-A ID
             (\(nested\))?\s*   #     ignore the "(nested)" tag, if found
             (\S+)?             # 11: active sites
           /x ) {

        ( $start, $end, $pfamA_acc, $hmmStart, $hmmEnd, $mode, $bits, 
          $evalue, $pfamA_id, $as ) = ( $1, $2, $3, $4, $5, $6, $7, $8, $9, $11 );
          
        if ( defined $as and
             $as =~ m/\[(\d+(,\d+)+)\]/ ) {
          $c->log->debug( "Search::Sequence::handlePfamAResults: active site residues: |$1|" )
            if $c->debug;

          @as_residues = split /\s*,\s*/, $1;
        }
          
        $pfamData = $c->model( 'PfamDB::Pfam' )
                      ->find( { pfamA_acc => $pfamA_acc } );
                      
        if ( $mode eq 'ls' ) {
          $s = $pfamData->ls_domain_GA < $bits ? 1 : 0; 
        }
        elsif ( $mode eq 'fs' ) {
          $s = $pfamData->fs_domain_GA < $bits ? 1 : 0; 
        }
      }
      elsif ( m/\#HMM/ ) {
        $aliHmm = $_;
        $aliHmm .= "-*" if $aliHmm =~ m/\<$/; 
        $aliHmm .= "*"  if $aliHmm =~ m/\<\-$/; 
      }
      elsif ( m/\#MATCH/ ) {
        $aliMatch = $_; 
      }
      elsif ( m/\#SEQ/ ) {
        $aliSeq = $_; 
      }
      
    } # end of "if <massive regex>"
    
    #Now shove all of the data elements into an anonymous hash
    my $results = { pfama_id     => $pfamA_id,
                    pfama_acc    => $pfamA_acc,
                    start        => $start,
                    end          => $end,
                    hmm_start    => $hmmStart,
                    hmm_end      => $hmmEnd,
                    hmm_length   => $pfamData->model_length,
                    mode         => $mode,
                    significant  => $s,
                    bits         => $bits,
                    evalue       => $evalue,
                    type         => $pfamData->type,
                    desc         => $pfamData->description,
                    aliMatch     => $aliMatch,
                    aliHmm       => $aliHmm,
                    aliSeq       => $aliSeq,
                    sites        => \@as_residues };

    # add data that's dependent on model type
    if ( $mode eq 'ls' ) {
      $results->{ls_domain_GA} = $pfamData->ls_domain_GA;
    }
    elsif ( $mode eq 'fs' ) {    
      $results->{fs_domain_GA} = $pfamData->fs_domain_GA;
    }
    
    # store the result hash
    push @rawPfamAResults, $results; 
  }

  $c->log->debug( 'Search::Sequence::handlePfamAResults: got '
                  . scalar @rawPfamAResults . ' PfamA results' ) if $c->debug;
  
  $c->stash->{genPfamARes} = \@rawPfamAResults;
}

#-------------------------------------------------------------------------------

=head2 handlePfamBResults : Private

Does exactly what it says on the tin. Rob's handiwork...

=cut

sub handlePfamBResults : Private {
  my ( $this, $c, $jobId ) = @_;
  
  # flag the fact that we're searching PfamBs in the stash, so the template
  # can write some clever text...
  $c->stash->{searchedPfamB} = 1;
  
  #Grr - Write results to file as this is the only way we can get BioPerl to read it.
  #However, bioperl does a good jobs (most of the time, although some wu-blast errors cause expections to be thrown  
  my $tmpRoot;
  if ( $ENV{PFAM_DOMAIN_IMAGES} ) {
    $tmpRoot = $ENV{PFAM_DOMAIN_IMAGES};
  }
  elsif ( $ENV{DOCUMENT_ROOT} ) {
    $tmpRoot = "$ENV{DOCUMENT_ROOT}/tmp/pfam";
  }
  else {
    die q(Can't set a temp directory for muscle output);
  }
  ( $tmpRoot ) = $tmpRoot =~ m|([a-z0-9_\./]+)|i;
  
  my ( $tmpFh, $tmpFile ) = tempfile( DIR => $tmpRoot );
  print $tmpFh $c->stash->{results}->{$jobId}->{rawData};
  close $tmpFh;
  
  #Parse the results and remove the redundancy
  my %results;  
  my $searchio = new Bio::SearchIO::blast( -format => 'blast',
                                            -file   => $tmpFile,
                                            -signif => 0.001 );
  unlink( $tmpFile );
  
  while ( my $result = $searchio->next_result ) {
    #$c->log->debug( 'Search::Sequence::handlePfamBResults: Pfam-B Query sequence: ' . 
    #                $result->query_name ) if $c->debug;
    while ( my $hit = $result->next_hit ) {
      my ( $pfamB_acc, $pfamB_id ) = split ';', $hit->description;
       
      HIT: while( my $hsp = $hit->next_hsp ) {
        if ($results{$pfamB_acc}){
          foreach my $r (@{$results{$pfamB_acc}}){
            next HIT if ( ( $r->{start} >= $hsp->start and $r->{start} <= $hsp->end ) or
                          ( $r->{end}   >= $hsp->start and $r->{end}   <= $hsp->end ) or
                          ( $r->{start} <= $hsp->start and $r->{end}   >= $hsp->end ) );          
          }
        }
        
        #$c->log->debug( "Search::Sequence::handlePfamBResults: PfamB hit |$hsp|" . 
        #                $pfamB_acc . "\t" . 
        #                $hit->accession . "\t" . 
        #                $hit->description . "\t" . 
        #                $hit->length . "\t" . 
        #                $hit->score . "\t" . 
        #                $hsp->pvalue ) if $c->debug;
        #$c->log->debug( "Search::Sequence::handlePfamBResults: Hit string: " . 
        #                $hsp->query_string . "\t" . 
        #                $hsp->hit_string . "\t" . 
        #                $hsp->homology_string ) if $c->debug;
        push @{ $results{$pfamB_acc} }, { pfamb_acc   => $pfamB_acc, 
                                          pfamb_id    => $pfamB_id, 
                                          start       => $hsp->start, 
                                          end         => $hsp->end, 
                                          score       => $hsp->score, 
                                          pvalue      => $hsp->pvalue, 
                                          hitString   => $hsp->hit_string,
                                          homoString  => $hsp->homology_string,
                                          queryString => $hsp->query_string };
      }
    }
  }
   
  #Now make the generic results for each Pfam-B.
  my @unsortedResults;
  foreach my $pfamB_acc ( keys %results ) {
    foreach my $reg ( @{$results{$pfamB_acc}} ) {
      push @unsortedResults, $reg;
    }
  }
  
  # sort by start residue
  my @sortedResults = sort { $a->{start} <=> $b->{start} } @unsortedResults; 
  
  $c->log->debug( 'Search::Sequence::handlePfamBResults: got ' . scalar @sortedResults .
                  ' PfamB results' ) if $c->debug;
  
  $c->stash->{genPfamBRes} = \@sortedResults;
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
