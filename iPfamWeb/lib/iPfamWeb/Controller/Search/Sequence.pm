
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.2 2008-01-15 13:58:07 rdf Exp $

=head1 NAME

PfamWeb::Controller::Sequence - perform sequence searches

=cut

package iPfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: Sequence.pm,v 1.2 2008-01-15 13:58:07 rdf Exp $

=cut

use strict;
use warnings;

use Bio::SearchIO::blast;
use File::Temp qw( tempfile );
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Storable qw( thaw );

use Data::Dump qw( dump );

use base 'iPfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 sequenceSearch : Path

Queues a sequence search job and returns a page that polls the server for
results.

=cut

sub sequenceSearch : Path {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Search::Sequence::sequenceSearch: form was submitted' );

  # check the input

  # the sequence itself
  $c->stash->{seq} = $c->forward( 'parseSequence' );
  unless( $c->stash->{seq} ) {
    $c->stash->{seqSearchError} = 'No valid sequence found. Please enter a valid amino-acid sequence and try again.';
    return;
  }
  
  # sequence search options
  if( defined $c->req->param( 'seqOpts' ) ) {
    $c->stash->{seqOpts} = $c->req->param( 'seqOpts' );
    unless( $c->stash->{seqOpts} eq 'both' or
            $c->stash->{seqOpts} eq 'bothNoMerge' or
            $c->stash->{seqOpts} eq 'ls' or
            $c->stash->{seqOpts} eq 'fs' ) {
      $c->stash->{seqSearchError} = 'You must select a valid search option.';

      $c->log->debug( 'Search::Sequence::sequenceSearch: bad search option; returning to form' );
      return;
    }
  } else {
    $c->log->debug( 'Search::Sequence::sequenceSearch: search options not specified; returning to form' );
    $c->stash->{seqSearchError} = 'You must select a search option.';
    return;
  }

  # if we have an evalue, we'll use that, otherwise we'll use the gathering
  # threshold
  if( defined $c->req->param( 'ga' ) and $c->req->param( 'ga' ) ) {
    $c->stash->{ga} = 1;
  } else {
    if( defined $c->req->param( 'evalue' ) and 
      looks_like_number( $c->req->param( 'evalue' ) ) ) {
      $c->stash->{evalue} = $c->req->param( "evalue" );
    } else {
      $c->log->debug( 'Search::Sequence::sequenceSearch: bad evalue; returning to form' );
      $c->stash->{seqSearchError} = 'You did not enter a valid E-value.';
      return;
    }
  }

  # try to submit the search
  my $submissionStatus = $c->forward( 'queueSeqSearch' );

  # and see if we managed it...
  if( $submissionStatus > 0 ) {
    $c->log->debug( 'Search::Sequence::sequenceSearch: sequence is pre-calculated; returning results' ); 
    $c->stash->{template} = 'pages/search/sequence/results.tt';

  } elsif( $submissionStatus < 0 ) {
    $c->log->debug( 'Search::Sequence::sequenceSearch: problem with submission; re-rendering form' ); 
    return;

  } else {
    $c->log->debug( 'Search::Sequence::sequenceSearch: sequence search submitted; polling' ); 
    $c->stash->{template} = 'pages/search/sequence/polling.tt';
  }
}

#-------------------------------------------------------------------------------

=head2 results : Local

Returns the URI of the Pfam graphic that is the result of the specified job.

=cut

sub results : Local {
  my( $this, $c ) = @_;

  # the template that will format the results  
  $c->stash->{template} = 'pages/search/sequence/results.tt';

  # try to retrieve the results for the specified jobs
  $c->forward( 'JobManager', 'retrieveResults' );
  unless( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Search::Sequence::results: no results found' );
    $c->stash->{template} = 'pages/search/sequence/error.tt';
    return;
  } 

  # we got some results, so format them appropriately and generate a Pfam
  # graphic
  $c->forward( 'handleResults' );
  $c->forward( 'generateGraphic' );
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

  return unless defined $c->req->param( 'seq' );
  
  my @seqs = split /\n/, $c->req->param( 'seq' );
  shift @seqs if $seqs[0] =~ /^\>/;
  my $seq = uc( join '', @seqs );
  $seq =~ s/[\s\r]+//g;

  $c->log->debug( "Search::Sequence::parseSequence: parsed sequence: |$seq|" );
  return ( $seq =~ /^[A-Z]+$/ ) ? $seq : '';
}

#-------------------------------------------------------------------------------

=head2 queueSeqSearch : Private

Executes a protein sequence search. Queues a Pfam A search to one queue
and, if the appropriate box was checked in the submission form, a Pfam B search
to another queue.

=cut

sub queueSeqSearch : Private {
  my( $this, $c ) = @_;

  # Taken out MD% check as we need to search the sequence as the results are different.

  # first, check there's room in the queue
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->find( { status => 'PEND' },
                     { select => [ { count => 'status' } ],
                       as     => [ "numberPending" ] } );

  $c->stash->{numberPending} = $rs->get_column( 'numberPending' );
  $c->log->debug( 'Search::Sequence::queueSeqSearch: |' . $c->stash->{numberPending} .
                  '| jobs pending' );

  if( $c->stash->{numberPending} > $this->{pendingLimit} ) {
    $c->log->debug( 'Search::Sequence::queueSeqSearch: too many jobs in queue (' .
                    $c->stash->{numberPending} . ')' );
    $c->stash->{seqSearchError} = 'There are currently too many jobs in the sequence search queue. Please try again in a little while.';
    return -1;
  }

  # ok. There's room on the queue, so we can submit the hmmer job and the blast job

  my @jobs; 
  push @jobs, $c->forward( 'queuePfamA' );
  push @jobs, $c->forward( 'queuePfamB' ) if $c->req->param( 'searchBs' );
  
  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  $c->log->debug( dump( \@jobs ) );

  $c->stash->{jobStatus} = objToJson( \@jobs );

  $c->log->debug( 'json string: |' . $c->stash->{jobStatus} . '|' );

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
  $c->log->debug(  q(Search::Sequence::queuePfamA: estimated search time: ) . 
                  qq(|$estimatedTime| seconds) );
  ($estimatedTime *= 2 ) if( $c->stash->{seqOpts} eq 'both' or
                             $c->stash->{seqOpts} eq 'bothNoMerge' );
  $c->log->debug(  q(Search::Sequence::queuePfamA: estimated search time: ) .
                  qq(|$estimatedTime| seconds) );

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
  $c->log->debug( "Search::Sequence::queuePfamB: estimated search time: |$estimatedTime| seconds" );

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

=head2 handleResults : Private

Parse the results and filter based on the the users defined parameters. The 
parsed results are put in a very generic format so that they can then be used 
for generating the results tables and graphics.

=cut

sub handleResults : Private {
  my( $this, $c ) = @_;

  #There two arrays will be populated with each hit represented as a hash.
  #We go through these step for two reasons
  #1. Allows us to potentially use the data in the database rather than 
  #   searching every sequence
  #2. Removes redundancy in the pfamB results.
  my(@rawPfamAResults, @rawPfamBResults);

  foreach my $jobId ( keys %{ $c->stash->{results} } ) {
    $c->log->debug( "Search::Sequence::handleResults: handling results for |$jobId|" );

    #Identify What sort of job we have performed.....
    if( $c->{stash}->{results}->{$jobId}->{method} eq 'hmmer' ){
      
      #We have performed a hmmer search, must be a pfamA
      my( $userEvalue ) = $c->{stash}->{results}->{$jobId}->{options} =~ /-e (\S+)/;
      
      #Are we using GA cut-offs of Evalues?
      $c->stash->{evalue} = $userEvalue ? $userEvalue : 0;
      
      #Read in the pfam_scan data. This assumes that pfam_Scan is spitting out 
      #alignments so each domain is represented by 4 lines. 
      my @results = split(/\n/, $c->{stash}->{results}->{$jobId}->{rawData});

      while( @results ) {
        my @set = splice( @results, 0, 4 );
        
        my( $start, $end, $pfamA_acc, $hmmStart, $hmmEnd, $mode, $bits, 
            $evalue, $pfamA_id, $aliHmm, $aliMatch, $aliSeq, $s, $pfamData );
        foreach ( @set ) {
          $c->log->debug( "Search::Sequence::handleResults: line: |$_|" );

          #Line 1 is the domain positional information, lines 2-4 contain the 
          #actual alignment
          # UserSeq     33   142 PF00169.20      1    92 ls    42.8   1.2e-09  PH
          if( /^\S+\s+(\d+)\s+(\d+)\s+(PF\d{5})\.\d+\s+(\d+)\s+(\d+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/ ) {

            ( $start, $end, $pfamA_acc, $hmmStart, $hmmEnd, $mode, $bits, 
              $evalue, $pfamA_id ) = ( $1, $2, $3, $4, $5, $6, $7, $8, $9 );
              
            unless( $3 =~ /^PF\d{5}$/ ) {
              $c->log->warn( "Search::Sequence::handleResults: couldn't find a Pfam accession: |$3|" );
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
        push @rawPfamAResults, { pfama_id     => $pfamA_id,
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

      } 

    } elsif( $c->{stash}->{results}->{$jobId}->{method} eq 'pfamb' ) {
      #Okay, looks like we have a pfamB result
      
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
        $c->log->debug( 'Pfam-B Query sequence: ' . $result->query_name );
        while( my $hit = $result->next_hit ) {
          my( $pfamB_acc, $pfamB_id ) = split ';', $hit->description;
           
          HIT: while( my $hsp = $hit->next_hsp ) {
            if($results{$pfamB_acc}){
              foreach my $r (@{$results{$pfamB_acc}}){
                 
                next HIT if( ( $r->{'start'} >= $hsp->start and $r->{'start'} <= $hsp->end ) or
                             ( $r->{'end'}   >= $hsp->start and $r->{'end'}   <= $hsp->end ) or
                             ( $r->{'start'} <= $hsp->start and $r->{'end'}   >= $hsp->end ) );          
              }
            }

            $c->log->debug( "PfamB hit |$hsp|".$pfamB_acc."\t".$hit->accession."\t".$hit->description."\t".$hit->length."\t".$hit->score."\t".$hsp->pvalue);
            push @{ $results{$pfamB_acc} }, { pfamB_acc   => $pfamB_acc, 
                                              pfamB_id    => $pfamB_id, 
                                              start       => $hsp->start, 
                                              end         => $hsp->end, 
                                              score       => $hsp->score, 
                                              pvalue      => $hsp->pvalue, 
                                              hitString   => $hsp->hit_string,
                                              homoString  => $hsp->homology_string,
                                              queryString => $hsp->query_string };
            
            $c->log->debug("Hit string". $hsp->query_string ."\t". $hsp->hit_string."\t". $hsp->homology_string);
          }
        }
      }
       
      #Now make the generic results for each Pfam-B.
      foreach my $pfamB_acc (keys %results){
        foreach my $reg (@{$results{$pfamB_acc}}){
          push @rawPfamBResults, { pfamb_id    => $reg->{pfamB_id},
                                   pfamb_acc   => $reg->{pfamB_acc},
                                   start       => $reg->{start},
                                   end         => $reg->{end},
                                   score       => $reg->{score},
                                   pvalue      => $reg->{pvalue},
                                   hitString   => $reg->{hitString},
                                   homoString  => $reg->{homoString},
                                   queryString => $reg->{queryString} };
        }            
      }
    }
  }

  $c->log->debug( 'Search::Sequence::handleResults: got ' . scalar @rawPfamAResults . 
                  ' PfamA results' );
  #$c->log->debug( dump @rawPfamAResults );
  $c->log->debug( 'Search::Sequence::handleResults: got ' . scalar @rawPfamBResults .
                  ' PfamB results' );
  #$c->log->debug( dump @rawPfamBResults );
  
  $c->stash->{genPfamARes} = \@rawPfamAResults;
  $c->stash->{genPfamBRes} = \@rawPfamBResults;

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
