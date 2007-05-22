
# SeqSearch.pm
# jt6 20061108 WTSI
#
# $Id: SeqSearch.pm,v 1.17 2007-05-22 15:33:55 rdf Exp $

=head1 NAME

PfamWeb::Controller::SeqSearch - perform various sequence searches

=cut

package PfamWeb::Controller::SeqSearch;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: SeqSearch.pm,v 1.17 2007-05-22 15:33:55 rdf Exp $

=cut

use strict;
use warnings;

use Digest::MD5 qw(md5_hex);
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Storable qw(thaw);
use Sanger::Graphics::ColourMap;

use Data::Dump qw(dump );

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
  
  # tell the layout template to disable the summary icons
  $c->stash->{iconsDisabled} = 1;

  # if there's no query parameter, we're done here; drop straight to the 
  # template that will render the search forms
  return unless $c->req->param( "query" );

  # get the query
  my( $terms ) = $c->req->param( "query" ) =~ /^([\w\:\;\-\.\s]+)/;

  # we're done here unless there's a query specified
  $c->log->warn( "Search::begin: no query terms supplied" ) and return
  unless defined $terms;

  # stash the de-tainted terms so we can safely display them later
  $c->stash->{rawQueryTerms} = $terms;

  # somewhere for the results of this search
  $c->stash->{results} = {};

}

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 batch : Local

Executes a batch search.

=cut

sub batch : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a batch search" );
}

#-------------------------------------------------------------------------------

=head2 funshift : Local

Executes a functional similarity search.

=cut

sub funshift : Local {
  my( $this, $c ) = @_;

  return unless $c->req->param( "entry" ) =~ m/^([\w_-]+)$/;
  $c->log->debug( "SeqSearch::funshift: executing a functional similarity search for |$1|" );

  # check for an accession or an ID
  if( $c->req->param( "entry" ) =~ /^(PF\d{5})$/i ) {

    $c->log->debug( "SeqSearch::funshift: might be a Pfam accession" );
    $c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );

  } elsif( $c->req->param( "entry" ) =~ /^([\w_-]+)$/ ) {

    $c->log->debug( "SeqSearch::funshift: might be a Pfam accession" );
    $c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_id => $1 } );

  } else {
    
    $c->log->debug( "SeqSearch::funshift: can't figure out whether it's an ID or acc" );
    return;
    
  }
  
  # make sure the query actually found an entry
  if( not defined $c->stash->{pfam} ) {
    $c->log->debug( "SeqSearch::funshift: can't find a Pfam family |$1|" );
    return;
  }
  
  # yes; now do the funshift search
  my @fs = $c->model("PfamDB::Funshift")
             ->search( { auto_pfamA_A => $c->stash->{pfam}->auto_pfamA,
                         auto_pfamA_B => { '!=' => $c->stash->{pfam}->auto_pfamA },
                         rfunSim      => { '>'  => 0.75 } },
                       { join     => [ qw/ pfam clan / ],
                         prefetch => [ qw/ pfam clan / ], 
                         order_by => "rfunSim DESC" } );

  $c->log->debug( "SeqSearch::funshift: found |" . scalar @fs . "| rows" );

  if( scalar @fs ) {

    $c->stash->{template} = "pages/fsResults.tt";
    $c->stash->{results} = \@fs;
  
    # generate a gradient for this many rows
    my $cm = new Sanger::Graphics::ColourMap;
    my @grad = $cm->build_linear_gradient( scalar @fs, "008000", "C00000" );
    $c->stash->{gradient} = \@grad;

  } else {
    # see if there are any GO terms for this family
    $c->stash->{goTerms} = $c->model("PfamDB::GO")
                             ->search( { "me.auto_pfamA" => $c->stash->{pfam}->auto_pfamA } );
    
    $c->stash->{template} = "pages/fsError.tt";    
  }
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
  $c->stash->{seq} = $c->forward( "parseSequence" );
  unless( $c->stash->{seq} ) {
    $c->stash->{seqSearchError} = "No valid sequence found. Please enter a valid amino-acid sequence and try again.";
    return;
  }
  
  # sequence search options
  if( defined $c->req->param( "seqOpts" ) ) {
    $c->stash->{seqOpts} = $c->req->param( "seqOpts" );
    unless( $c->stash->{seqOpts} eq "both" or
            $c->stash->{seqOpts} eq "ls"   or
            $c->stash->{seqOpts} eq "fs" ) {
      $c->stash->{seqSearchError} = "You must select a valid search option.";

      $c->log->debug( "SeqSearch::seq: bad search option; returning to form" );
      return;
    }
  } else {
    $c->log->debug( "SeqSearch::seq: search options not specified; returning to form" );
    $c->stash->{seqSearchError} = "You must select a search option.";
    return;
  }

  # if we have an evalue, we'll use that, otherwise we'll use the gathering
  # threshold
  if( defined $c->req->param( "ga" ) and $c->req->param( "ga" ) ) {
    $c->stash->{ga} = 1;
  } else {
    if( defined $c->req->param( "evalue" ) and 
      looks_like_number( $c->req->param( "evalue" ) ) ) {
      $c->stash->{evalue} = $c->req->param( "evalue" );
    } else {
      $c->log->debug( "SeqSearch::seq: bad evalue; returning to form" );
      $c->stash->{seqSearchError} = "You did not enter a valid E-value.";
      return;
    }
  }

  # try to submit the search
  my $submissionStatus = $c->forward( "queueSeqSearch" );

  # and see if we managed it...
  if( $submissionStatus > 0 ) {
    $c->log->debug( "SeqSearch::seq: sequence is pre-calculated; returning results" ); 
    $c->stash->{template} = "pages/seqSearchDone.tt";

  } elsif( $submissionStatus < 0 ) {
    $c->log->debug( "SeqSearch::seq: problem with submission; re-rendering form" ); 
    return;

  } else {
    $c->log->debug( "SeqSearch::seq: sequence search submitted; polling" ); 
    $c->stash->{template} = "pages/polling.tt";
  }
}

#-------------------------------------------------------------------------------

=head2 checkStatus : Local

Returns the status of the specified job. Used by the javascript that polls for
the status via XMLHttpRequest calls.

=cut

sub checkStatus : Local {
  my( $this, $c ) = @_;

  # build a hash that we'll convert into JSON and return
  $c->stash->{status} = {};

  my $jobId = $c->req->param( "jobId" );

  if( length( $jobId ) != 36 or $jobId !~ /[A-F0-9\-]/ ) {
    $c->log->debug( "SeqSearch::checkStatus: bad job id" );
    $c->stash->{status}->{error} = "Invalid job ID";
    $c->detach( "returnStatus" );
  }

  # job ID appears to be valid; try querying for the status of that job
  my $jobStatus = $c->model( "WebUser::JobHistory" )
                    ->find( { job_id => $jobId } );

  # make sure the query returned *something*
  if( not defined $jobStatus ) {
    $c->log->debug( "SeqSearch::checkStatus: problem retrieving job status for job |$jobId|" );
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

  } elsif( $jobStatus->status eq "FAIL" ) {
    $c->log->debug( "SeqSearch::checkStatus: job failed" );
    $c->stash->{status}->{status} = "FAIL";

  } else {
    $c->log->error( "SeqSearch::checkStatus: can't determine job status" );
    $c->stash->{status}->{status} = "UNKNOWN";
  }

#  $c->log->debug( "SeqSearch::checkStatus: opened:  |" . $jobStatus->opened ."|" );
#  $c->log->debug( "SeqSearch::checkStatus: started: |" . $jobStatus->started ."|" );
#  $c->log->debug( "SeqSearch::checkStatus: closed:  |" . $jobStatus->closed ."|" );

  # see how many jobs are pending
  my $rs = $c->model( "WebUser::JobHistory" )
             ->search( { status => "PEND",
                         id     => { '<',  $jobStatus->id },
                         job_id => { 'not like', $jobStatus->job_id } },
                       { select => [
                                     { count => "id" },
                                     { sum   => "estimated_time" }
                                   ],
                         as     => [ qw( num wait ) ] }
                     );
  $c->stash->{status}->{numPending} = $rs->first()->get_column( "num" );
  $c->stash->{status}->{waitTime}   = $rs->first()->get_column( "wait" ) || 0;

  $c->log->debug( "SeqSearch::checkStatus: found      |" .
                  $c->stash->{status}->{numPending} . "| pending jobs" );
  $c->log->debug( "SeqSearch::checkStatus: wait time: |" .
                  $c->stash->{status}->{waitTime} );

  # add the times to the response
  $c->stash->{status}->{opened}  = $jobStatus->opened;
  $c->stash->{status}->{started} = $jobStatus->started;
  $c->stash->{status}->{closed}  = $jobStatus->closed;

  # and hand back that status
  $c->forward( "returnStatus" );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 parseSequence : Private

Parses the sequence supplied by the CGI parameter "seq". Returns the sequence
as a single string if it's parsed successfully, or the empty string if there
was a problem parsing or if the final sequence contains a character other than
[A-Za-z].

=cut

sub parseSequence : Private {
  my( $this, $c ) = @_;

  return unless defined $c->req->param( "seq" );
  
  my @seqs = split /\n/, $c->req->param( "seq" );
  shift @seqs if $seqs[0] =~ /^\>/;
  my $seq = uc( join "", @seqs );
  $seq =~ s/[\s\r]+//g;

  $c->log->debug( "SeqSearch::parseSequence: parsed sequence: |$seq|" );
  return ( $seq =~ /^[A-Z]+$/ ) ? $seq : "";
}

#-------------------------------------------------------------------------------

=head2 queueSeqSearch : Private

Executes a protein sequence search. Queues a Pfam A search to one queue
and, if the appropriate box was checked in the submission form, a Pfam B search
to another queue.

=cut

sub queueSeqSearch : Private {
  my( $this, $c ) = @_;

  # calculate the MD5 checksum for the sequence we've been handed and see if
  # we've already seen that one
  $c->stash->{md5} = md5_hex( uc( $c->stash->{seq} ) );
  $c->log->debug( "SeqSearch::queueSeqSearch: MD5 for user sequence is: |"
                  . $c->stash->{md5}."|" );

  my $found = $c->model("PfamDB::Pfamseq")
                ->find( { md5 => $c->stash->{md5} } );

  # yes; no need to search
  if( $found ) {

    $c->log->debug( "SeqSearch::queueuSeqSearch: we've seen this sequence before" );
    if( $c->req->param( "searchBs" ) ) {
      $c->stash->{pfambSearch} = 1;
    }
    $c->detach( 'getSeqGraphicOnMd5');
    return 1;
    
    #get storable for sequence based on md5
    #Select regions out of database based on input i.e. take care of evalue
    #Produce same XML result a script that runs on pvm cluster
    #insert into database
  }

  # no; we need to search the sequence

  # first, check there's room in the queue
  my $rs = $c->model( "WebUser::JobHistory" )
             ->find( { status => "PEND" },
                     { select => [ { count => "status" } ],
                       as     => [ "numberPending" ] } );

  $c->stash->{numberPending} = $rs->get_column( "numberPending" );
  $c->log->debug(   "SeqSearch::queueSeqSearch: |" . $c->stash->{numberPending}
                  . "| jobs pending" );

  if( $c->stash->{numberPending} > $this->{pendingLimit} ) {
    $c->log->debug( "SeqSearch::queueSeqSearch: too many jobs in queue ("
                    . $c->stash->{numberPending} . ")" );
    $c->stash->{seqSearchError} = "There are currently too many jobs in the sequence search queue. Please try again in a little while.";
    return -1;
  }

  # ok. There's room on the queue, so we can submit the hmmer job and the blast job

  my @jobs; 
  push @jobs, $c->forward( "queuePfamA" );
  if( $c->req->param( "searchBs" ) ) {
    push @jobs, $c->forward( "queuePfamB" );
  }
  
  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $jobStatus = {
                    checkURI => $c->uri_for( "/seqsearch/checkstatus" )->as_string,
                    doneURI  => $c->uri_for( "/seqsearch/jobDone" )->as_string,
                    jobs     => \@jobs,
                  };

  $c->log->debug( dump( $jobStatus ) );
  $c->stash->{jobStatus} = objToJson( $jobStatus );
  $c->log->debug( "json string: |" . $c->stash->{jobStatus} . "|" );
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
  $c->log->debug( "SeqSearch::queuePfamA: estimated search time: "
                  . "|$estimatedTime| seconds" );

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # build the command to run
  my $cmd = "/home/pfamweb/scripts/pfam_scan.pl -pvm -d /data/blastdb/Pfam/data";
  $cmd .= " --mode " . $c->stash->{seqOpts} if $c->stash->{seqOpts} ne "both";
  $cmd .= " -e " . $c->stash->{evalue}      if $c->stash->{evalue} && !$c->stash->{ga};
  $cmd .= " --overlap "                     if $c->stash->{showOverlap};
  $cmd .= " /tmp/$jobId.fa";

  # add this job to the tracking table
  my $resultHistory = $c->model('WebUser::JobHistory')
                        ->create( { command        => $cmd,
                                    priority       => "hmmer",
                                    estimated_time => $estimatedTime,
                                    job_id         => $jobId,
                                    opened         => \'NOW()',
                                    status         => 'PEND' } );

  my $resultStream = $c->model('WebUser::JobStream')
                       ->create( { id    => $resultHistory->id,
                                   stdin => $c->stash->{seq} || q() } );

  # check the submission time with a separate query
  my $historyRow = $c->model( "WebUser::JobHistory" )
                     ->find( { id => $resultHistory->id } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $jobStatus = {
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
  $c->log->debug( "SeqSearch::queuePfamB: estimated search time: "
                  . "|$estimatedTime| seconds" );

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # build the command to run
  my $cmd = "/data/bin/wublastp /data/blastdb/Pfam/data/Pfam-B.fasta";
  $cmd .= " /tmp/$jobId.fa";
  $cmd .= " -cpus 2 -gapE=2000 -T=12";

  # add this job to the tracking table
  my $resultHistory = $c->model('WebUser::JobHistory')
                        ->create( { command        => $cmd,
                                    priority       => "fast",
                                    estimated_time => $estimatedTime,
                                    job_id         => $jobId,
                                    opened         => \'NOW()',
                                    status         => 'PEND' } );

  my $resultStream = $c->model('WebUser::JobStream')
                       ->create( { id    => $resultHistory->id,
                                   stdin => $c->stash->{seq} || q() } );

  # check the submission time with a separate query
  my $historyRow = $c->model( "WebUser::JobHistory" )
                     ->find( { id => $resultHistory->id } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $jobStatus = {
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

=head2 getSeqGraphicOnMd5 : Private

  Get the graphic for the MD5

=cut

sub getSeqGraphicOnMd5 : Private {
  my( $this, $c ) = @_;
  # code here
  
  my $md5 = $c->stash->{md5};
  my (@rs, @rawPfamAResults);
  if ( $c->stash->{evalue} && !$c->stash->{ga} ){
    #get the region information from database
    $c->log->debug( "SeqSearch::getGraphicOnMD5 using evalue cut off:".$c->stash->{evalue});
    if($c->stash->{seqOpts} eq 'both'){
      @rs = $c->model( "PfamDB::PfamA_reg_full")
              ->search( { md5      => $md5},
                       { join     => [ qw( pfamseq pfamA ) ],
                         prefetch => [ qw( pfamseq pfamA ) ] } );
    }elsif($c->stash->{seqOpts}){
       @rs = $c->model( "PfamDB::PfamA_reg_full")
              ->search( { md5      => $md5,
                          mode     => $c->stash->{seqOpts}},
                       { join     => [ qw( pfamseq pfamA ) ],
                         prefetch => [ qw( pfamseq pfamA ) ] } );
    }else{
      $c->log->debug( "SeqSearch::getGraphicOnMD5 no mode set");
      $c->stash->{seqSearchError} = "You did not enter a valid a mode.";
      return;
    }
    
    foreach my $reg (@rs){
      if($c->stash->{evalue} >= $reg->domain_evalue_score){

       push(@rawPfamAResults, {  pfama_id    => $reg->pfamA_id,
                                 pfama_acc   => $reg->pfamA_acc,
                                 start       => $reg->seq_start,
                                 end         => $reg->seq_end,
                                 hmm_start   => $reg->model_start,
                                 hmm_end     => $reg->model_end,
                                 hmm_length  => $reg->model_length,
                                 significant => $reg->significant,
                                 mode        => $reg->mode,
                                 in_full     => $reg->in_full,
                                 evalue      => $reg->domain_evalue_score,
                                 bits        => $reg->domain_bits_score,
                                 desc        => $reg->description,
                                 type        => $reg->type});
      }
    }                           
    
  }elsif($c->stash->{ga}){
    $c->log->debug( "SeqSearch::getGraphicOnMD5 using GA cut off");
    if($c->stash->{seqOpts} eq 'both'){
     @rs = $c->model( "PfamDB::PfamA_reg_full_significant")
             ->search( { md5      => $md5 },
                       { join     => [ qw( pfamseq ) ],
                         prefetch => [ qw( pfamseq ) ] } );
    }elsif($c->stash->{seqOpts}){
      $c->log->debug( "SeqSearch::getGraphicOnMD5 mode:".$c->stash->{seqOpts} );
      @rs = $c->model( "PfamDB::PfamA_reg_full_significant")
             ->search( { md5      => $md5,
                         mode     => $c->stash->{seqOpts} },
                       { join     => [ qw( pfamseq ) ],
                         prefetch => [ qw( pfamseq ) ] } );
    }else{
      $c->log->debug( "SeqSearch::getGraphicOnMD5 no mode set");
      $c->stash->{seqSearchError} = "You did not enter a valid a mode.";
      return;
    }        
    foreach my $reg (@rs){
       push(@rawPfamAResults, {  pfama_id    => $reg->pfamA_id,
                                 pfama_acc   => $reg->pfamA_acc,
                                 start       => $reg->seq_start,
                                 end         => $reg->seq_end,
                                 hmm_start   => $reg->model_start,
                                 hmm_end     => $reg->model_end,
                                 hmm_length  => $reg->model_length,
                                 significant => 1,
                                 mode        => $reg->mode,
                                 in_full     => $reg->in_full,
                                 evalue      => $reg->domain_evalue_score,
                                 bits        => $reg->domain_bits_score,
                                 desc        => $reg->description,
                                 type        => $reg->type });
    }      
  }
  
  my @rawPfamBResults;  
  if($c->stash->{pfambSearch}){
      my @rs = $c->model( "PfamDB::PfamB_reg")
                  ->search( { md5 => $md5 },
                            { join => [ qw( pfamseq pfamB) ],
                              prefetch => [ qw( pfamseq pfamB) ] } );
      foreach my $reg (@rs) {
          push(@rawPfamBResults, {  pfamb_id  => $reg->pfamB_id,
                                    pfamb_acc => $reg->pfamB_acc,
                                    start     => $reg->seq_start,
                                    end       => $reg->seq_end });

      }    
  } 
  $c->log->debug( "SeqSearch::getGraphicOnMD5: Got ".scalar(@rawPfamAResults)." PfamA results" );
  $c->log->debug( dump(@rawPfamAResults));
  $c->log->debug( "SeqSearch::getGraphicOnMD5: Got ".scalar(@rawPfamBResults)." PfamB results" );
  $c->log->debug( dump(@rawPfamBResults));
  
  $c->stash->{genPfamARes} = \@rawPfamAResults;
  $c->stash->{genPfamBRes} = \@rawPfamBResults;
  
  $c->forward( "generateGraphic" );
  $c->stash->{template} = "pages/jobDone.tt";
}

#-------------------------------------------------------------------------------

=head2 returnStatus : Local

Returns the status of a polled job as a JSON snippet. Short-circuits the default
end action because we're adding content directly to the response body. We also
take care to set apropriate headers to avoid this response being cached on the
client side.

=cut

sub returnStatus : Private {
  my( $this, $c ) = @_;

  # convert the status hash to a JSON object and return it
  my $status = objToJson( $c->stash->{status} );

  $c->log->debug( "SeqSearch::returnStatus: returning: " );
  $c->log->debug( dump( $c->stash->{status} ) );

  $c->res->content_type( "application/json" );
  $c->res->body( $status );

  # make damned sure this isn't cached...
  $c->res->header( 'Pragma' => 'no-cache' );
  $c->res->header( 'Expires' => 'Thu, 01 Jan 1970 00:00:00 GMT' );
  $c->res->header( 'Cache-Control' => 'no-store, no-cache, must-revalidate,'.
                                      'post-check=0, pre-check=0, max-age=0' );

}

#-------------------------------------------------------------------------------

=head2 jobDone : Local

Returns the URI of the Pfam graphic that is the result of the specified job.

=cut

sub jobDone : Local {
  my( $this, $c ) = @_;

  # extract the list of IDs from the URI
  my @jobIds = $c->req->param( "jobId" );
  foreach ( @jobIds ) {

    # detaint the job ID
    ( my $jobId ) = $_ =~ m/^([A-F0-9\-]+)$/;
    $c->log->debug( "SeqSearch::jobDone: looking up details for job ID: |$jobId|" );

    next unless defined $jobId;

    # job ID *looks* valid; try looking for that job
    my $job = $c->model( "WebUser::JobHistory" )
                ->find( { job_id => $jobId },
                        { join     => [ qw( job_stream ) ],
                          prefetch => [ qw( job_stream ) ] } );

    # bail unless it exists
    next unless defined $job;

    # retrieve the results of the job and stash them
    $c->stash->{results}->{$jobId}->{rawData} = $job->stdout;
    $c->stash->{results}->{$jobId}->{method}  = $job->priority;
    $c->stash->{results}->{$jobId}->{command} = $job->command;
    $c->{stash}->{seq} = $job->stdin;
  }

  # do something interesting with the results
  $c->forward( "handleResults" );
  $c->forward( "generateGraphic" );
  $c->stash->{template} = "pages/jobDone.tt";
}

#-------------------------------------------------------------------------------

=head2 handleResults : Private

Do something interesting with job results. This is where we would mess with the
results of the searches and generate some useful graphics for the user.

=cut

sub handleResults : Private {
  my( $this, $c ) = @_;
#TODO Add mode to hash of rawResults when mode is both!
  my(@rawPfamAResults, @rawPfamBResults);
  
  
  foreach my $jobId ( keys %{ $c->stash->{results} } ) {
    $c->log->debug( "SeqSearch::handleResults: handling results for |$jobId|" );
    if($c->{stash}->{results}->{$jobId}->{method} eq 'hmmer'){
      my ($mode)   = $c->{stash}->{results}->{$jobId}->{command} =~ /--mode (\S+)/;
      my ($userEvalue) = $c->{stash}->{results}->{$jobId}->{command} =~ /-e (\S+)/;
      $c->stash->{evalue} = $userEvalue ? $userEvalue : 0; 
      foreach my $line (split(/\n/, $c->{stash}->{results}->{$jobId}->{rawData})){
        if(my($start, $end, $pfamA_acc, $hmmStart, $hmmEnd, $bits, $evalue, $pfamA_id) = 
            $line =~ /\S+\s+(\d+)\s+(\d+)\s+(PF\d{5})\.\d+\s+(\d+)\s+(\d+)\s+(\S+)\s+(\S+)\s+(\S+)/){
            my $pfamData = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $pfamA_acc});
            my $s;
            if($mode eq 'ls'){
              $s = ($pfamData->ls_domain_GA < $bits ? 1 : 0); 
            }elsif($mode eq 'fs'){
              $s = ($pfamData->fs_domain_GA < $bits ? 1 : 0); 
            }
            push(@rawPfamAResults, {   pfama_id    => $pfamA_id,
                                       pfama_acc   => $pfamA_acc,
                                       start       => $start,
                                       end         => $end,
                                       hmm_start   => $hmmStart,
                                       hmm_end     => $hmmEnd,
                                       hmm_length  => $pfamData->model_length,
                                       mode        => $mode,
                                       significant => $s,
                                       bits        => $bits,
                                       evalue      => $evalue,
                                       type        => $pfamData->type,
                                       desc        => $pfamData->description });
        }  
      } 
    }elsif($c->{stash}->{results}->{$jobId}->{method} eq 'fast'){
       
    }
  }
  $c->log->debug( "SeqSearch::getGraphicOnMD5: Got ".scalar(@rawPfamAResults)." PfamA results" );
  $c->log->debug( dump(@rawPfamAResults));
  $c->log->debug( "SeqSearch::getGraphicOnMD5: Got ".scalar(@rawPfamBResults)." PfamB results" );
  $c->log->debug( dump(@rawPfamBResults));
  
  $c->stash->{genPfamARes} = \@rawPfamAResults;
  $c->stash->{genPfamBRes} = \@rawPfamBResults;
  
  
}

#-------------------------------------------------------------------------------

=head2 generateGraphic : Private

Empty action stub.

=cut

sub generateGraphic : Private {
  my( $this, $c ) = @_;

  # Convert the generic results into BioPerl objects and subsequently generate the graphic....   
  my $fac = Bio::Pfam::AnnSeqFactory->new;
  my $annseq = $fac->createAnnotatedSequence();
  my @seqs;
  push(@seqs, $annseq);
  
  $annseq->sequence(
    Bio::Pfam::SeqPfam->new('-seq' => $c->{stash}->{seq},
                            '-start' => '1',
                            '-end' => length($c->{stash}->{seq}),
                            '-id'=> "QuerySeq",
                            '-acc' => "QuerySeq",
                            '-organism' => "Unknown",
                            '-desc' => "QuerySeq"));

  foreach my $pfamA (@{$c->{stash}->{genPfamARes}}){
    if($pfamA->{significant}){
    $annseq->addAnnotatedRegion(
      Bio::Pfam::PfamRegion->new( '-PFAM_ACCESSION' => $pfamA->{pfama_acc},
                                  '-PFAM_ID' => $pfamA->{pfama_id},
                                  '-SEQ_ID' => $annseq->id,
                                  '-FROM' => $pfamA->{start},
                                  '-TO' => $pfamA->{end},
                                  '-MODEL_FROM' => $pfamA->{hmm_start},
                                  '-MODEL_TO' => $pfamA->{hmm_end},
                                  '-MODEL_LENGTH'=>$pfamA->{hmm_length},
                                  '-BITS' => $pfamA->{bits},
                                  '-EVALUE' => $pfamA->{evalue},
                                  '-TYPE' => "pfama",
                                  '-ANNOTATION' => $pfamA->{desc},
                                  '-REGION' => $pfamA->{type} ));
    }
  }
                                                       
  foreach my $pfamB (@{$c->{stash}->{genPfamBRes}}){
    $annseq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $pfamB->{pfamb_acc},
                                                              '-PFAM_ID' => $pfamB->{pfamb_id},
                                                              '-SEQ_ID' => $annseq->id,
                                                              '-FROM' => $pfamB->{start},
                                                              '-TO' => $pfamB->{end},
                                                              '-TYPE' => "pfamb"));  
  }
  
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->layout_sequences( @seqs);
  #$c->log->debug($layout->layout_to_XMLDOM->toString(1));
  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );  
  $c->stash->{images} = $imageset;

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
