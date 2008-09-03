
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.24 2008-09-03 15:39:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Sequence - perform sequence searches

=cut

package PfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: Sequence.pm,v 1.24 2008-09-03 15:39:58 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::SearchIO::blast;
use File::Temp qw( tempfile );
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;

use base 'PfamBase::Controller::Search::InteractiveSearch';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 sequence_search : Path

Queues a sequence search job and returns a page that polls the server for
results.

=cut

sub search : Path {
  my( $this, $c ) = @_;
  
  # validate the unput
  unless ( $c->forward('validate_input') ) {

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

  # no errors with the input; try to submit the search

  # success !
  if ( $c->forward( 'queue_seq_search' ) ) {

    $c->log->debug( 'Search::Sequence::search: sequence search submitted; polling' )
      if $c->debug; 

    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/poll_xml.tt';
      $c->res->content_type('text/xml');
    }
    else {
      $c->stash->{template} = 'pages/search/sequence/polling.tt';
    }

  }

  # failure...
  else {

    $c->stash->{seqSearchError} = $c->stash->{searchError} || 
                                  'There was an unknown problem when submitting your search.';

    $c->log->debug( 'Search::Sequence::search: problem with submission; re-rendering form' )
      if $c->debug;       

    # point to the XML error template if emitting XML, otherwise, we're just 
    # done here 
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
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
  my @jobIds = $c->req->param('jobId');
  
  my $completed = 0;
  foreach my $jobId ( @jobIds ) {
    
    # detaint the ID
    next unless $jobId =~ m/^[A-F0-9\-]{36}$/;

    # try to retrieve results for it
    $c->forward( 'JobManager', 'retrieveResults', [ $jobId  ] );

    # we should get *something*, even if there are no results, but let's just
    # check quickly
    next unless $c->stash->{results}->{$jobId};

    $c->log->debug( "Search::Sequence::results: looking up results for job |$jobId|" )
      if $c->debug;

    my $results = $c->stash->{results}->{$jobId};
    
    # keep track of how many jobs are actually completed
    if ( $c->stash->{results}->{$jobId}->{status} eq 'DONE' ) {
      $completed++;
      $c->log->debug( "Search::results: job |$jobId| completed" )
        if $c->debug;
    }
    
    # parse the results
    $c->forward( 'handleResults', [ $jobId  ] );

    #----------------------------------------

    # this is dumb. We don't have a record in the DB of the actual options that
    # the user selected in the submission form, and we can't have that record 
    # because of the way that the submission process works. Instead, so that
    # we can show them the options that they chose alongside their results, we
    # have to effectively parse the options string that we DO have in the DB,
    # turning it back into a list of flags. Dumb. Really dumb.

    $c->log->debug( 'Search::Sequence::results: parsing options:' )
      if $c->debug;
  
    # keep track of the job IDs. Redundant, but convenient...
    push @{ $c->stash->{job_options}->{jobIds} }, $jobId;
  
    if ( $results->{method} eq 'pfamb' ) {

      $c->log->debug( 'Search::Sequence::results:   searched Pfam-Bs' )
        if $c->debug;
      $c->stash->{job_options}->{pfamb} = $jobId;

    }
    else {
      
      # find the E-value
      if ( $results->{options} =~ m/-e (\S+)\s*/ ) {
        $c->log->debug( "Search::Sequence::results:   found an E-value: |$1|" )
          if $c->debug;
        $c->stash->{job_options}->{evalue} = $1;
      }
      else {
        $c->log->debug( 'Search::Sequence::results:   using gathering threshold' )
          if $c->debug;
        $c->stash->{job_options}->{ga} = 1;
      }
  
      # get the mode    
      if ( $results->{options} =~ m/--no_merge/ ) {
        $c->log->debug( 'Search::Sequence::results:   searching global and local separately' )
          if $c->debug;
        $c->stash->{job_options}->{both}     = 1;
        $c->stash->{job_options}->{separate} = 1;
      }
      elsif ( $results->{options} =~ m/--mode (\S+)/ ) {
        $c->log->debug( "Search::Sequence::results:   searching |$1| model" )
          if $c->debug;
        $c->stash->{job_options}->{mode} = $1;
      }
      else {
        $c->log->debug( 'Search::Sequence::results:   searching global and local merged' )
          if $c->debug;
        $c->stash->{job_options}->{both} = 1;
      }
  
    } # end of "if pfamb"

    #----------------------------------------

  }

  # if none of the jobs have actually finished, return HTTP status 204 and
  # we're done here. Don't try to render a template at all 
  unless ( $completed ) {
    $c->log->debug( 'Search::results: no results; returning 204' )
      if $c->debug;
    $c->res->status( '204' ); # 'No content'
    $c->res->body( 'Search(es) not yet complete' );
    return;
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
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

sub validate_input : Private {
  my ( $this, $c ) = @_;
  
  # parse and validate the sequence itself
  $c->forward('parse_sequence');
  unless ( defined $c->stash->{input} ) {
    # the parse_sequence method will put the sequence into the stash if it
    # passes validation, but if the sequence looks like DNA or if it's
    # too long, we also get an error message in the stash. So, we only set 
    # a general error message here if we don't already have one
    $c->stash->{searchError} ||= 'Invalid sequence. Please try again with a valid amino-acid sequence.';
    return 0;
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
    
    # no need to check the remaining parameters, since they apply only to
    # Pfam-A searches and we're not actually doing a Pfam-A search...
    return 1;
    
  }
    
  # available sequence search options
  my %available_options = ( both        => 1,
                            bothNoMerge => 1,
                            ls          => 1,
                            fs          => 1 );

  # the user supplied an option; check it's valid
  if ( defined $c->req->param('seqOpts') ) {
    
    unless ( defined $available_options{ $c->req->param('seqOpts') } ) {
      $c->stash->{searchError} = 'You must use a valid search option.';

      $c->log->debug( 'Search::Sequence::validate_inputh: bad search option; returning to form' )
        if $c->debug;

      return 0;
    }
    
    $c->stash->{seqOpts} = $c->req->param('seqOpts');
  }

  # default to "both"
  else {
    $c->stash->{seqOpts} = 'both';

    $c->log->debug( 'Search::Sequence::validate_input: setting search option to default of "both"' )
      if $c->debug;
  }

  # if we're supplied with an E-value, we'll use that, unless we've been asked 
  # to use the gathering threshold. Default to using an evalue of 1.0
  if ( defined $c->req->param('evalue') ) {
    $c->log->debug( 'Search::Sequence::sequence_search: got an evalue' )
      if $c->debug;
    
    if ( looks_like_number( $c->req->param('evalue') ) and
         $c->req->param('evalue') > 0 ) {
      $c->log->debug( 'Search::Sequence::validate_input: evalue looks like a positive number' )
        if $c->debug;
      $c->stash->{evalue} = $c->req->param('evalue');
    }
    else {
      $c->stash->{searchError} = 'You did not give a valid E-value.';

      $c->log->debug( 'Search::Sequence::validate_input: bad evalue; returning to form' )
        if $c->debug;

      return 0;
    }

  }
  elsif ( defined $c->req->param('ga') and 
          $c->req->param('ga') ) {
    $c->log->debug( 'Search::Sequence::validate_input: using ga' )
      if $c->debug;
    $c->stash->{ga} = 1;
    
  }
  else {
    $c->log->debug( 'Search::Sequence::validate_inputh: using default evalue' )
      if $c->debug;
    $c->stash->{evalue} = 1.0;
  }

  return 1;  
}

#-------------------------------------------------------------------------------

=head2 parse_sequence : Private

Parses the sequence supplied by the CGI parameter "seq". Drops the sequence 
into the stash if it passed validation. Sets an error message in the stash if 
there was a specific problem.

=cut

sub parse_sequence : Private {
  my ( $this, $c ) = @_;

  # make sure we actually have a sequence...
  unless ( defined $c->req->param('seq') and
           $c->req->param('seq') ne '' ) {
    $c->stash->{searchError} = 'Please supply a valid sequence.';

    $c->log->debug( 'Search::Sequence::parse_sequence: no sequence supplied; failed' )
      if $c->debug;

    return 0;
  }
  
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
    $c->stash->{searchError} = 
      'Your sequence is too long. The maximum length of search sequences is ' .
      $this->{maxSeqLength} . '. Please try again with a shorter sequence';

    $c->log->debug( 'Search::Sequence::parse_sequence: sequence is too long; failed' )
      if $c->debug;

    return 0;
  }

  # check that the sequence string contains only letters. Bail if it has 
  # anything else in it
  unless ( $seq =~ m/^[ABCDEFGHIKLMNPQRSTUVWXYZ\-\*\s]+\r?$/i ) {
    $c->stash->{seqSearchError} = 
      'Invalid sequence. Please try again with a valid amino-acid sequence';

    $c->log->debug( 'Search::Sequence::parse_sequence: sequence contains illegal characters; failed' )
      if $c->debug;

    return 0;
  }

  # we need to make sure that the sequence is really protein and not, as we
  # commonly get, a bloody great DNA sequence. Count the number of potential 
  # nucleotides in the sequence and see what proportion of the total sequence
  # that makes
  my ( $nucleotide_count )= $seq =~ tr/ATCGU/ATCGU/;
  
  # if the sequence is more than 100 residues (or bases) and is more than
  # 95% nucleotides, there's a problem
  if ( $length > 100 and $nucleotide_count / $length > 0.95 ) {
    $c->stash->{searchError} = 
      'Your sequence does not look like protein. Please upload a protein sequence';

    $c->log->debug( "Search::Sequence::parse_sequence: sequence doesn't look like protein; failed" )
      if $c->debug;

    return 0;
  }

  # passed all checks; stuff the sequence into the stash
  $c->stash->{input} = $seq;
  
  return 1;
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
    $c->stash->{searchError} = 
      'There are currently too many jobs in the sequence search queue. ' . 
      'Please try again in a little while.';

    $c->log->debug( 'Search::Sequence::queue_seq_search: too many jobs in queue ('
                    . $c->stash->{numberPending} . ')' ) if $c->debug;

    return 0;
  }
  
  #----------------------------------------
  
  # ok. There's room on the queue, so we can submit the hmmer job and, if 
  # required, the blast job
  my @jobs;

  # submit a Pfam-A job, unless we've been asked to skip it  
  if ( not $c->stash->{skipAs} ) {

    # submit the Pfam-A search and make sure that operation was a success
    unless ( $c->forward( 'queue_pfam_a' ) ) {
      $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-A search.';

      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-A search' )
        if $c->debug;

      return 0;
    } 

  } 
  
  # submit a Pfam-B job, if we've been asked to run one
  if ( $c->stash->{searchBs} ) {
    
    unless ( $c->forward( 'queue_pfam_b' ) ) {
      $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-B search.';

      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-B search' )
        if $c->debug;

      return 0;
    } 

  }
  
  # make sure we have at least one job...
  unless ( scalar @{ $c->stash->{jobStatus} } ) {
    $c->stash->{searchError} = 'You must run at least one type of search.';
    
    $c->log->warn( 'Search::Sequence::queue_seq_search: no searches submitted' )
      if $c->debug;
      
    return 0;
  }
  
  #----------------------------------------
  
  # if we get to here, the job submissions worked. Now convert the job status
  # structure to JSON and stash it for the javascript on the client side to use
  $c->stash->{jobStatusJSON} = objToJson( $c->stash->{jobStatus} );
  
  $c->log->debug( 'Search::Sequence::queue_seq_search: json string: |'
                  . $c->stash->{jobStatusJSON} . '|' ) if $c->debug;
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_pfam_a : Private

Submits a pfam A search.

=cut

sub queue_pfam_a : Private {
  my ( $this, $c ) = @_;
  
  # build the command options to run
  $c->stash->{options}  =  ''; 
  $c->stash->{options} .=  q( --mode ) . $c->stash->{seqOpts} if( $c->stash->{seqOpts} ne 'both' and 
                                                                  $c->stash->{seqOpts} ne 'bothNoMerge' );
  $c->stash->{options} .=  q( --no_merge )                    if( $c->stash->{seqOpts} eq 'bothNoMerge' );
  $c->stash->{options} .=  q( -e )     . $c->stash->{evalue}  if( $c->stash->{evalue} and not $c->stash->{ga} );
  $c->stash->{options} .=  q( --overlap )                     if( $c->stash->{showOverlap} );
  
  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  
  # set the queue
  $c->stash->{job_type} = 'hmmer';

  # make a guess at the runtime for the job
  $c->stash->{estimated_time} = int( $this->{pfamA_search_multiplier} * length( $c->stash->{input} ) / 100 );
  ( $c->stash->{estimated_time} *= 2 ) if ( $c->stash->{seqOpts} eq 'both' or
                                            $c->stash->{seqOpts} eq 'bothNoMerge' );

  # queue it up and retrieve the row in the job_history table for this job
  unless ( $c->forward( 'queue_search_transaction' ) ) {
    $c->stash->{searchError} = 'There was an error when registering your Pfam-A search.';
    
    $c->log->debug( 'Search::Sequence::queue_pfam_a: submission failed' )
      if $c->debug;
    
    return 0;
  }

  $c->log->debug( 'Search::Sequence::queue_pfam_a: successfully queued job' )
    if $c->debug;
  

  #----------------------------------------
  
  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side  
  push @{ $c->stash->{jobStatus} }, 
    {
      checkURI      => $c->uri_for( '/jobmanager/checkStatus' )->as_string,
      doneURI       => $c->uri_for( '/search/sequence/results' )->as_string,
      estimatedTime => $c->stash->{estimated_time},
      interval      => $this->{pollingInterval},
      jobId         => $c->stash->{jobId},
      name          => 'Pfam A search',
      jobClass      => 'pfamASearch',
      opened        => $c->stash->{history_row}->opened
    };

  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_pfam_b : Private

Submits a pfam B search.

=cut

sub queue_pfam_b : Private {
  my ( $this, $c ) = @_;

  # no options for a Pfam-B search
  $c->stash->{options} = '';

  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();

  # set the queue
  $c->stash->{job_type} = 'pfamb';

  # make a guess at the runtime for the job
  $c->stash->{estimated_time} = int( $this->{pfamB_search_multiplier} * length( $c->stash->{input} ) / 100 );

  # queue it up and retrieve the row in the job_history table for this job
  unless ( $c->forward( 'queue_search_transaction' ) ) {
    $c->stash->{searchError} = 'There was an error when registering your Pfam-B search.';
    
    $c->log->debug( 'Search::Sequence::queue_pfam_b: submission transaction failed' )
      if $c->debug;
    
    return 0;
  }

  $c->log->debug( 'Search::Sequence::queue_pfam_b: successfully queued job' )
    if $c->debug;
  
  #----------------------------------------
  
  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side  
  push @{ $c->stash->{jobStatus} }, 
    { 
      checkURI      => $c->uri_for( '/jobmanager/checkStatus' )->as_string,
      doneURI       => $c->uri_for( '/search/sequence/results' )->as_string,
      estimatedTime => $c->stash->{estimated_time},
      interval      => $this->{pollingInterval},
      jobId         => $c->stash->{jobId},
      name          => 'Pfam B search',
      jobClass      => 'pfamBSearch',
      opened        => $c->stash->{history_row}->opened
    };

  return 1;
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
        my $as_res = substr $c->stash->{input}, $as - 1, 1;
        
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
    $c->log->debug( "Search::Sequence::handleResults: job |$jobId| is a hmmer job" )
      if $c->debug;
    $c->forward( 'handlePfamAResults', [ $jobId ] );
  }
  elsif ( $c->{stash}->{results}->{$jobId}->{method} eq 'pfamb' ) {
    $c->log->debug( "Search::Sequence::handleResults: job |$jobId| is a pfamb job" )
      if $c->debug;
    $c->forward( 'handlePfamBResults', [ $jobId ] );
    $c->log->debug( 'Search::Sequence::handleResults: back from "handlePfamBResults"' )
      if $c->debug;
  }

}

#-------------------------------------------------------------------------------

=head2 handlePfamAResults : Private

Does exactly what it says on the tin. Rob's handiwork...

=cut

sub handlePfamAResults : Private {
  my ( $this, $c, $jobId ) = @_;
  
  # are we using GA cut-offs or E-values?
  if ( $c->{stash}->{results}->{$jobId}->{options} and
       $c->{stash}->{results}->{$jobId}->{options} =~ m/-e (\S+)/ ) {
    $c->stash->{evalue} = $1;
  }
  else {
    $c->stash->{evalue} = 0;
  }
  
  # read in the pfam_scan data. This assumes that pfam_scan is spitting out 
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
