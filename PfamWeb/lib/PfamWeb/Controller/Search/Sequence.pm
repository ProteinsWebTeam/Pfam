
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.11 2007-12-12 15:02:54 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Sequence - perform sequence searches

=cut

package PfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: Sequence.pm,v 1.11 2007-12-12 15:02:54 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::SearchIO::blast;
use File::Temp qw( tempfile );
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Storable qw( thaw );
use Sanger::Graphics::ColourMap;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 sequenceSearch : Path

Queues a sequence search job and returns a page that polls the server for
results.

=cut

sub sequenceSearch : Path {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Search::Sequence::sequenceSearch: form was submitted' )
    if $c->debug;

  # check the input
  CHECK:
  {

    # parse and validate the sequence itself
    $c->stash->{seq} = $c->forward('parseSequence');
    unless( $c->stash->{seq} ) {
      # the parseSequence method will return undef if there's a problem, but
      # if the sequence looks like DNA or if the sequence is too long, it also 
      # stuffs an error message into the stash. Hence, we only set a general 
      # error message here if we don't already have one from earlier
      $c->stash->{seqSearchError} ||= 'Invalid sequence. Please try again with a valid amino-acid sequence';
      last CHECK;
    }
    
    # sequence search options. Default to "both"
    $c->stash->{seqOpts} = $c->req->param('seqOpts') || 'both';
    unless( $c->stash->{seqOpts} eq 'both' or
            $c->stash->{seqOpts} eq 'bothNoMerge' or
            $c->stash->{seqOpts} eq 'ls' or
            $c->stash->{seqOpts} eq 'fs' ) {
      $c->log->debug( 'Search::Sequence::sequenceSearch: bad search option; returning to form' )
        if $c->debug;
      $c->stash->{seqSearchError} = 'You must use a valid search option';
      last CHECK;
    }

    # if we have an evalue, we'll use that, unless we've been asked to use
    # the gathering threshold. Default to using an evalue of 1.0
    if( defined $c->req->param('evalue') ) {
      $c->log->debug( 'Search::Sequence::sequenceSearch: got an evalue' )
        if $c->debug;
      
      if( looks_like_number( $c->req->param('evalue') ) ) {
        $c->log->debug( 'Search::Sequence::sequenceSearch: evalue looks like a number' )
          if $c->debug;
        $c->stash->{evalue} = $c->req->param('evalue');
      } else {
        $c->log->debug( 'Search::Sequence::sequenceSearch: bad evalue; returning to form' )
          if $c->debug;
        $c->stash->{seqSearchError} = 'You did not give a valid E-value';
        last CHECK;
      }

    } elsif( defined $c->req->param('ga') and 
             $c->req->param('ga') ) {
      $c->log->debug( 'Search::Sequence::sequenceSearch: using ga' )
        if $c->debug;
      $c->stash->{ga} = 1;

    } else {
      $c->log->debug( 'Search::Sequence::sequenceSearch: using default evalue' )
        if $c->debug;
      $c->stash->{evalue} = 1.0;
    }

    # search for Pfam-Bs too ?
    $c->stash->{searchBs} = ( defined $c->req->param('searchBs') and
                              $c->req->param('searchBs') );

  } # end of "CHECK"
  
  #----------------------------------------

  # if there was an error, decide how to show it to the user
  if( $c->stash->{seqSearchError} ) {

    # if we're returning XML, we need to set a template to render the error
    # message. If we're emitting HTML, the end action (ultimately on Section) 
    # will take of us and return us to the HTML page containing search form 
    # and show the error message
    if( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
    }
  
    return;
  }

  #----------------------------------------

  # no errors with the input; try to submit the search
  my $submissionStatus = $c->forward( 'queueSeqSearch' );

  # and see if we managed it...
  if( $submissionStatus < 0 ) {

    $c->log->debug( 'Search::Sequence::sequenceSearch: problem with submission; re-rendering form' )
      if $c->debug;       
    $c->stash->{seqSearchError} ||= 'There was an unknown problem submitting your search';

    # point to the XML error template if emitting XML, otherwise, we're just 
    # done here 
   if( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
   }
  
  } else {

    $c->log->debug( 'Search::Sequence::sequenceSearch: sequence search submitted; polling' )
      if $c->debug; 

    if( $c->stash->{output_xml} ) {
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
  my( $this, $c ) = @_;

  # try to retrieve the results for the specified jobs
  my @jobIds = $c->req->param( 'jobId' );
  foreach my $job_id ( @jobIds ) {
    next unless $job_id =~ m/^([A-F0-9\-]{36})$/;
    $c->forward( 'JobManager', 'retrieveResults', [ $job_id  ] );
    $c->forward( 'handleResults', [ $job_id  ] ) if $c->stash->{results};
  }

  if( $c->stash->{output_xml} ) {
    $c->stash->{template} = 'rest/search/results_xml.tt';
    $c->res->content_type('text/xml');    
  } else {

    if( scalar keys %{ $c->stash->{results} } ) {
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

=head2 parseSequence : Private

Parses the sequence supplied by the CGI parameter "seq". Returns the sequence
as a single string if it's parsed successfully, or undef otherwise. Sets an
error message in the stash if there was a specific problem.

=cut

sub parseSequence : Private {
  my( $this, $c ) = @_;

  return undef unless( defined $c->req->param( 'seq' ) and
                       $c->req->param('seq') ne '' );
  
  my @seqs = split /\n/, $c->req->param( 'seq' );
  shift @seqs if $seqs[0] =~ /^\>/; # strip off FASTA header lines
  my $seq = uc( join '', @seqs );
  $seq =~ s/[\s\r]+//g; # handle various line endings
  
  # check the length of the sequence at this point. If it's too long, bail
  my $length = length $seq;
  if( $length > $this->{maxSeqLength} ) {
    $c->stash->{seqSearchError} = 'Your sequence is too long. The maximum length of search sequences is ' .
                                  $this->{maxSeqLength} . 
                                  '. Please try again with a shorter sequence';
    return undef;
  }

  # we need to make sure that the sequence is really protein and not, as we
  # commonly get, a bloody great DNA sequence. Count the number of potential 
  # nucleotides in the sequence and see what proportion of the total sequence
  # that makes
  my( $nucleotide_count )= $seq =~ tr/ATCGU/ATCGU/;
  
  # if the sequence is more than 100 residues (or bases) and is more than
  # 95% nucleotides, there's a problem
  if( $length > 100 and $nucleotide_count / $length > 0.95 ) {
    $c->stash->{seqSearchError} = 'Your sequence does not look like protein. Please upload a protein sequence';
    return undef;
  }

  # finally, check that the sequence string contains only letters. Return undef
  # if it has anything else in it
  unless( $seq =~ m/^[A-Za-z]+$/ ) {
    $c->stash->{seqSearchError} = 'Invalid sequence. Please try again with a valid amino-acid sequence';
    return undef;
  }
  
  return $seq;
}

#-------------------------------------------------------------------------------

=head2 queueSeqSearch : Private

Executes a protein sequence search. Queues a Pfam A search to one queue
and, if the appropriate box was checked in the submission form, a Pfam B search
to another queue.

=cut

sub queueSeqSearch : Private {
  my( $this, $c ) = @_;

  # taken out MD5 check as we need to search the sequence as the results are 
  # different.

  # first, check there's room in the queue
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->find( { status => 'PEND' },
                     { select => [ { count => 'status' } ],
                       as     => [ 'numberPending' ] } );

  $c->stash->{numberPending} = $rs->get_column( 'numberPending' );
  $c->log->debug( 'Search::Sequence::queueSeqSearch: |' . $c->stash->{numberPending} .
                  '| jobs pending' ) if $c->debug;

  if( $c->stash->{numberPending} >= $this->{pendingLimit} ) {
    $c->log->debug( 'Search::Sequence::queueSeqSearch: too many jobs in queue (' .
                    $c->stash->{numberPending} . ')' ) if $c->debug;
    $c->stash->{seqSearchError} = 'There are currently too many jobs in the sequence search queue. Please try again in a little while.';
    return -1;
  }

  # ok. There's room on the queue, so we can submit the hmmer job and the 
  # blast job
  my @jobs; 
  push @jobs, $c->forward( 'queuePfamA' );
  push @jobs, $c->forward( 'queuePfamB' ) if $c->stash->{searchBs};
  
  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  $c->log->debug( dump( \@jobs ) ) if $c->debug;

  $c->stash->{jobStatus}     = \@jobs;
  $c->stash->{jobStatusJSON} = objToJson( \@jobs );

  $c->log->debug( 'json string: |' . $c->stash->{jobStatusJSON} . '|' ) 
    if $c->debug;

  return 0;
}

#-------------------------------------------------------------------------------

=head2 queuePfamA : Private

Submits a pfam A search.

=cut

sub queuePfamA : Private {
  my( $this, $c ) = @_;

  # make a guess at the runtime for the job
  my $estimatedTime = int( length( $c->stash->{seq} ) / 100 );
  ( $estimatedTime *= 2 ) if( $c->stash->{seqOpts} eq 'both' or
                             $c->stash->{seqOpts} eq 'bothNoMerge' );
  $c->log->debug(  q(Search::Sequence::queuePfamA: estimated search time: ) .
                  qq(|$estimatedTime| seconds) ) if $c->debug;

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # build the command options to run
  my $opts;
  $opts .=  q( --mode ) . $c->stash->{seqOpts} if( $c->stash->{seqOpts} ne 'both' and 
                                                   $c->stash->{seqOpts} ne 'bothNoMerge' );
  $opts .=  q( --no_merge )                    if( $c->stash->{seqOpts} eq 'bothNoMerge' );
  $opts .=  q( -e )     . $c->stash->{evalue}  if( $c->stash->{evalue} and not $c->stash->{ga} );
  $opts .=  q( --overlap )                     if( $c->stash->{showOverlap} );
  
  # add this job to the tracking table
  my $jobHistory = $c->model('WebUser::JobHistory')
                     ->create( { options        => $opts,
                                 job_type       => 'hmmer',
                                 estimated_time => $estimatedTime,
                                 job_id         => $jobId,
                                 opened         => \'NOW()',
                                 status         => 'PEND' } );

  my $jobStream = $c->model('WebUser::JobStream')
                    ->create( { id    => $jobHistory->id,
                                stdin => $c->stash->{seq} || q() } );

  # check the submission time with a separate query
  my $historyRow = $c->model( 'WebUser::JobHistory' )
                     ->find( { id => $jobHistory->id } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $jobStatus = {
                    checkURI      => $c->uri_for( '/jobmanager/checkStatus' )->as_string,
                    doneURI       => $c->uri_for( '/search/sequence/results' )->as_string,    
                    estimatedTime => $estimatedTime,
                    interval      => $this->{pollingInterval},
                    jobId         => $jobId,
                    name          => 'Pfam A search',
                    jobClass      => 'pfamASearch',
                    opened        => $historyRow->opened,
                  };
  return $jobStatus;
}

#-------------------------------------------------------------------------------

=head2 queuePfamB : Private

Submits a pfam B search.

=cut

sub queuePfamB : Private {
  my( $this, $c ) = @_;

  # make a guess at the runtime for the job
  my $estimatedTime = int( length( $c->stash->{seq} ) / 100 );
  $c->log->debug( "Search::Sequence::queuePfamB: estimated search time: |$estimatedTime| seconds" )
    if $c->debug;

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # add this job to the tracking table
  my $resultHistory = $c->model('WebUser::JobHistory')
                        ->create( { job_type        => 'pfamb',
                                    estimated_time => $estimatedTime,
                                    job_id         => $jobId,
                                    opened         => \'NOW()',
                                    status         => 'PEND' } );

  my $jobStream = $c->model('WebUser::JobStream')
                       ->create( { id    => $resultHistory->id,
                                   stdin => $c->stash->{seq} || q() } );

  # check the submission time with a separate query
  my $historyRow = $c->model( 'WebUser::JobHistory' )
                     ->find( { id => $resultHistory->id } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $jobStatus = {
                    checkURI      => $c->uri_for( '/jobmanager/checkStatus' )->as_string,
                    doneURI       => $c->uri_for( '/search/sequence/results' )->as_string,    
                    estimatedTime => $estimatedTime,
                    interval      => $this->{pollingInterval},
                    jobId         => $jobId,
                    name          => 'Pfam B search',
                    jobClass      => 'pfamBSearch',
                    opened        => $historyRow->opened,
                  };
  return $jobStatus;
}

#-------------------------------------------------------------------------------

=head2 generateGraphic : Private

Generate the Pfam graphic from the generic results.

=cut

sub generateGraphic : Private {
  my( $this, $c ) = @_;

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
                             '-start'    => '1',
                             '-end'      => length($c->{stash}->{seq}),
                             '-id'       => 'QuerySeq',
                             '-acc'      => 'QuerySeq',
                             '-organism' => 'Unknown',
                             '-desc'     => 'QuerySeq' )
  );

  # For each pfamA region, make the PfamRegion object
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
                                  '-TYPE'           => "pfama" )
    );
  }
                                                     
  #Now do the same for any PfamB hits  
  foreach my $pfamB ( @{ $c->{stash}->{genPfamBRes} } ) {
    $annseq->addAnnotatedRegion(
      Bio::Pfam::PfamRegion->new( '-PFAM_ACCESSION' => $pfamB->{pfamb_acc},
                                  '-PFAM_ID'        => $pfamB->{pfamb_id},
                                  '-SEQ_ID'         => $annseq->id,
                                  '-FROM'           => $pfamB->{start},
                                  '-TO'             => $pfamB->{end},
                                  '-TYPE'           => "pfamb" )
    );  
  }
  
  #Now generate the image object that can be used for generating the graphic.
  #The actual image is printed within the tt.
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->layout_sequences( @seqs);
  #$c->log->debug($layout->layout_to_XMLDOM->toString(1));

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
  my( $this, $c, $jobId ) = @_;

  $c->log->debug( "Search::Sequence::handleResults: handling results for |$jobId|" )
    if $c->debug;
  
  if( $c->{stash}->{results}->{$jobId}->{method} eq 'hmmer' ) {
    $c->forward( 'handlePfamAResults', [ $jobId ] );
  } elsif( $c->{stash}->{results}->{$jobId}->{method} eq 'pfamb' ) {
    $c->forward( 'handlePfamBResults', [ $jobId ] );
  }

}

#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub handlePfamAResults : Private {
  my( $this, $c, $jobId ) = @_;
  
  #We have performed a hmmer search, must be a pfamA
  my( $userEvalue ) = $c->{stash}->{results}->{$jobId}->{options} =~ /-e (\S+)/;
  
  #Are we using GA cut-offs of Evalues?
  $c->stash->{evalue} = $userEvalue ? $userEvalue : 0;
  
  #Read in the pfam_scan data. This assumes that pfam_Scan is spitting out 
  #alignments so each domain is represented by 4 lines. 
  my @results = split(/\n/, $c->{stash}->{results}->{$jobId}->{rawData});
  
  my @rawPfamAResults;
  while( @results ) {
    my @set = splice( @results, 0, 4 );
    
    my( $start, $end, $pfamA_acc, $hmmStart, $hmmEnd, $mode, $bits, 
        $evalue, $pfamA_id, $aliHmm, $aliMatch, $aliSeq, $s, $pfamData );
    foreach ( @set ) {
      
      #Line 1 is the domain positional information, lines 2-4 contain the 
      #actual alignment
      # UserSeq     33   142 PF00169.20      1    92 ls    42.8   1.2e-09  PH
      if( /^\S+\s+(\d+)\s+(\d+)\s+(PF\d{5})\.\d+\s+(\d+)\s+(\d+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/ ) {
        
        ( $start, $end, $pfamA_acc, $hmmStart, $hmmEnd, $mode, $bits, 
          $evalue, $pfamA_id ) = ( $1, $2, $3, $4, $5, $6, $7, $8, $9 );
          
        unless( $3 =~ /^PF\d{5}$/ ) {
          $c->log->warn( "Search::Sequence::handlePfamAResults: couldn't find a Pfam accession: |$3|" );
          next;
        } 
        
        $pfamData = $c->model('PfamDB::Pfam')
                      ->find( { pfamA_acc => $pfamA_acc } );
                      
        if( $mode eq 'ls' ) {
          $s = $pfamData->ls_domain_GA < $bits ? 1 : 0; 
        }elsif( $mode eq 'fs' ) {
          $s = $pfamData->fs_domain_GA < $bits ? 1 : 0; 
        }
      } elsif( /\#HMM/ ) {
        $aliHmm = $_;
        $aliHmm .= "-*" if $aliHmm =~ m/\<$/; 
        $aliHmm .= "*"  if $aliHmm =~ m/\<\-$/; 
      } elsif( /\#MATCH/ ) {
        $aliMatch = $_; 
      } elsif( /\#SEQ/ ) {
        $aliSeq = $_; 
      }
    }
    
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
                             aliSeq       => $aliSeq };

    # add data that's dependent on model type
    if( $mode eq 'ls' ) {
      $results->{ls_domain_GA} = $pfamData->ls_domain_GA;
    } elsif( $mode eq 'fs' ) {    
      $results->{fs_domain_GA} = $pfamData->fs_domain_GA;
    }
    
    # store the result hash
    push @rawPfamAResults, $results; 
  }

  $c->log->debug( 'Search::Sequence::handlePfamAResults: got ' . scalar @rawPfamAResults . 
                  ' PfamA results' ) if $c->debug;
  #$c->log->debug( dump @rawPfamAResults );
  
  $c->stash->{genPfamARes} = \@rawPfamAResults;
}

#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub handlePfamBResults : Private {
  my( $this, $c, $jobId ) = @_;
  
  # flag the fact that we're searching PfamBs in the stash, so the template
  # can write some clever text...
  $c->stash->{searchedPfamB} = 1;
  
  #Grr - Write results to file as this is the only way we can get BioPerl to read it.
  #However, bioperl does a good jobs (most of the time, although some wu-blast errors cause expections to be thrown  
  my $tmpRoot;
  if( $ENV{PFAM_DOMAIN_IMAGES} ) {
    $tmpRoot = $ENV{PFAM_DOMAIN_IMAGES};
  } elsif( $ENV{DOCUMENT_ROOT} ) {
    $tmpRoot = "$ENV{DOCUMENT_ROOT}/tmp/pfam";
  } else {
    die q(Can't set a temp directory for muscle output);
  }
  ( $tmpRoot ) = $tmpRoot =~ m|([a-z0-9_\./]+)|i;
  
  my( $tmpFh, $tmpFile ) = tempfile( DIR => $tmpRoot );
  print $tmpFh $c->{stash}->{results}->{$jobId}->{rawData};
  close $tmpFh;
  
  #Parse the results and remove the redundancy
  my %results;  
  my $searchio = new Bio::SearchIO::blast( -format => 'blast',
                                            -file   => $tmpFile,
                                            -signif => 0.001 );
  unlink( $tmpFile );
  
  while( my $result = $searchio->next_result ) {
    #$c->log->debug( 'Search::Sequence::handlePfamBResults: Pfam-B Query sequence: ' . 
    #                $result->query_name ) if $c->debug;
    while( my $hit = $result->next_hit ) {
      my( $pfamB_acc, $pfamB_id ) = split ';', $hit->description;
       
      HIT: while( my $hsp = $hit->next_hsp ) {
        if($results{$pfamB_acc}){
          foreach my $r (@{$results{$pfamB_acc}}){
            next HIT if( ( $r->{start} >= $hsp->start and $r->{start} <= $hsp->end ) or
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
  #$c->log->debug( dump @rawPfamBResults );
  
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
