
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.8 2009-06-09 14:00:11 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Search::Sequence - perform single sequence searches

=cut

package RfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for submitting single sequence searches for Rfam.

$Id: Sequence.pm,v 1.8 2009-06-09 14:00:11 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::SearchIO::blast;
use File::Temp qw( tempfile );
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;

use Data::Dump qw( dump );

use base qw( PfamBase::Controller::Search::InteractiveSearch
             RfamWeb::Controller::Search );
             
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Queues a sequence search job and returns a page that polls the server for
results.

=cut

sub search : Path {
  my ( $this, $c ) = @_;

  # validate the input
  unless ( $c->forward('validate_input') ) {

    # copy the error message into the slot in the stash where the templates
    # expect to find it
    $c->stash->{seqSearchError} = $c->stash->{searchError}
      || 'There was an unknown problem when validating your sequence.';

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
  if ( $c->forward('queue_seq_search') ) {

    $c->log->debug(
      'Search::Sequence::search: sequence search submitted; polling')
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

    $c->stash->{seqSearchError} = $c->stash->{searchError}
      || 'There was an unknown problem when submitting your search.';

    $c->log->debug(
      'Search::Sequence::search: problem with submission; re-rendering form')
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
  my @jobIds = $c->req->param( 'jobId' );
  
  my $completed = 0;
  foreach my $job_id ( @jobIds ) {
    
    # detaint the ID
    next unless $job_id =~ m/^([A-F0-9\-]{36})$/i;

    # try to retrieve results for it
    $c->forward( 'JobManager', 'retrieveResults', [ $job_id  ] );

    # we should get *something*, even if there are no results, but let's just
    # check quickly
    next unless $c->stash->{results}->{$job_id};

    $c->log->debug( "Search::Sequence::results: looking up results for job |$job_id|" )
      if $c->debug;

    my $results = $c->stash->{results}->{$job_id};
    
    # keep track of how many jobs are actually completed
    if ( $c->stash->{results}->{$job_id}->{status} eq 'DONE' ) {
      $completed++;
      $c->log->debug( "Search::Sequence::results: job |$job_id| completed" )
        if $c->debug;
    }
    
    # parse the results
    $c->forward( 'handleResults', [ $job_id  ] );

  }

  # if none of the jobs have actually finished, return HTTP status 204 and
  # we're done here. Don't try to render a template at all 
  unless ( $completed ) {
    $c->log->debug( 'Search::Sequence::results: no results; returning 204' )
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
      #$c->forward( 'generateGraphic' );
    } else {
      $c->log->debug( 'Search::Sequence::results: no results found' ) if $c->debug;
      $c->stash->{template} = 'pages/search/sequence/error.tt';
    }
  }

}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

sub validate_input : Private {
  my ( $this, $c ) = @_;
  
  # parse and validate the sequence itself
  unless ( $c->forward('parse_sequence') ) {
    $c->stash->{searchError} ||= 'Invalid sequence. Please try again with a valid RNA sequence.';

    $c->log->debug( 'Search::Sequence::validate_input: sequence parsing failed' )
      if $c->debug;

    return 0;
  }

  # no options to check right now

  $c->log->debug( 'Search::Sequence::parse_sequence: validating input was successful' )
    if $c->debug;
      
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
    $c->stash->{searchError} = 'You did not supply a nucleic-acid sequence.';
    
    $c->log->debug( 'Search::Sequence::parse_sequence: no sequence; failed' )
      if $c->debug;
      
    return 0;
  }
  
  # break the string into individual lines and get parse any FASTA header lines
  # before recombining. If there is no user-supplied FASTA header, one will be
  # supplied for them
  my @seqs = split /\n/, $c->req->param('seq');

  my $header;
  if ( $seqs[0] =~ /^\>([\w\s]+)/ ) {
    $c->log->debug( 'Search::Sequence::parse_sequence: found a user-supplied FASTA header; stripping it' )
      if $c->debug;
    
    shift @seqs;
  }

  my $seq = uc( join '', @seqs );

  # handle various line endings. No need to worry about \n, since we got rid of
  # that with the "split" above
  $seq =~ s/[\s\r\d]+//g;

  # check the length of the sequence at this point. If it's too long, bail
  my $length = length $seq;
  if ( $length > $this->{maxSeqLength} ) {
    $c->stash->{searchError} = 
        'Your sequence is too long. The maximum length of search sequences is '
      . $this->{maxSeqLength} . ' bases. Please try again with a shorter '
      . 'sequence, or use the batch search form and get your results by email.';
    
    $c->log->debug( 'Search::Sequence::parse_sequence: sequence is too long; failed' )
      if $c->debug;
    
    return 0;
  }

  # check that the sequence string contains only the appropriate letters. Bail
  # if it has anything else in it
  unless ( $seq =~ m/^[ACGUTSWMKRYBDHVN\-\.]+$/ ) {
    $c->stash->{searchError} = 
      'Invalid sequence. Please try again with a valid nucleic-acid sequence';
    
    $c->log->debug( 'Search::Sequence::parse_sequence: sequence contains illegal characters' )
      if $c->debug;
    
    return 0;
  }

  # passed all checks; stuff the header (either user-supplied or generated here)
  # and the sequence into the stash
  $c->stash->{input} = "> UserSeq\n" . $seq;

  $c->log->debug( 'Search::Sequence::parse_sequence: parsing sequence was successful' )
    if $c->debug;
      
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_seq_search : Private

Queues an Rfam search.

=cut

sub queue_seq_search : Private {
  my ( $this, $c ) = @_;
  
  # first, check there's room on the queue
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->find( { status   => 'PEND',
                       job_type => 'rfam' },
                     { select => [ { count => 'status' } ],
                       as     => [ 'numberPending' ] } );
  
  $c->stash->{numberPending} = $rs->get_column( 'numberPending' );
  $c->log->debug( 'Search::Sequence::queue_seq_search: |' . 
                  $c->stash->{numberPending} . '| jobs pending' ) if $c->debug;
  
  if ( $c->stash->{numberPending} >= $this->{pendingLimit} ) {
    $c->stash->{searchError} = 
      'There are currently too many Rfam jobs in the sequence search queue. ' . 
      'Please try again in a little while';

    $c->log->debug( 'Search::Sequence::queue_seq_search: too many Rfam jobs in queue ('
                    . $c->stash->{numberPending} . ')' ) if $c->debug;

    return 0;
  }
  
  #----------------------------------------

  # ok. There's room on the queue, so we can submit the hmmer job and, if 
  # required, the blast job
  my @jobs;
  
  unless ( $c->forward('queue_rfam') ) {
    $c->stash->{searchError} ||= 'There was a problem queuing your Rfam search';

    $c->log->debug( 'Search::Sequence::queue_seq_search: problem submitting Rfam search' )
      if $c->debug;
    
    return 0;
  }

  #----------------------------------------

  # if we get to here, the job submissions worked. Now build a job status data 
  # structure that we'll convert to JSON and hand back to the javascript on the 
  # client side
  $c->stash->{jobStatusJSON} = objToJson( $c->stash->{jobStatus} );
  
  $c->log->debug( 'Search::Sequence::queue_seq_search: json string: |' 
                  . $c->stash->{jobStatusJSON} . '|' ) if $c->debug;
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_rfam : Private

Submits an Rfam batch search.

=cut

sub queue_rfam : Private {
  my ( $this, $c ) = @_;
  
  # no user-configurable options for these jobs. Yet.

  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  
  # set the queue
  $c->stash->{job_type} = 'rfam';

  # make a guess at the runtime for the job
  $c->stash->{estimated_time} = int( $this->{search_multiplier} * length( $c->stash->{input} ) / 100 ); 

  # make sure we're not going to claim to have this search done without
  # at least one polling interval going by
  if ( $c->stash->{estimated_time} < $this->{pollingInterval} ) {
    $c->log->debug( 'Search::Sequence::queue_rfam: resetting estimated search time to polling interval' )
      if $c->debug;
    $c->stash->{estimated_time} = $this->{pollingInterval};
  }

  $c->log->debug( 'Search::Sequence::queue_rfam: estimated search time: |'
                  . $c->stash->{estimated_time} . '| seconds' ) if $c->debug;
  

  unless ( $c->forward('queue_search_transaction') ) {
    $c->stash->{searchError} = 'There was an error when registering your Rfam search.';
    
    $c->log->debug( 'Search::Sequence::queue_rfam: submission failed' )
      if $c->debug;
    
    return 0;
  }

  $c->log->debug( 'Search::Sequence::queue_rfam: successfully queued job' )
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
      name          => 'Rfam search',
      jobClass      => 'rfamSearch',
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

  # parse the log into a sensible data structure  
  $c->forward( 'parse_log', [ $jobId ] );
#  $c->log->debug( 'Search::Sequence::handleResults: results data structure: ' .
#                  dump( $c->stash->{hits} ) ) if $c->debug;  

  foreach my $hit ( @{ $c->stash->{hits} } ) {
    $c->log->debug( 'hit: ', dump( $hit ) )
      if $c->debug;
    
    $hit->{alignment}->{ss}       = '           ';
    $hit->{alignment}->{hit_seq}  = sprintf '%10d ', $hit->{blocks}->[0]->{hit}->{start};
    $hit->{alignment}->{match}    = '           ';
    $hit->{alignment}->{user_seq} = sprintf '%10d ', $hit->{start};
        
    foreach my $block ( @{ $hit->{blocks} } ) {
      $hit->{alignment}->{ss}       .= $block->{ss};
      $hit->{alignment}->{hit_seq}  .= $block->{hit}->{seq};
      $hit->{alignment}->{match}    .= $block->{match};
      $hit->{alignment}->{user_seq} .= $block->{user}->{seq};
    }
        
    $hit->{alignment}->{ss}       .= '           ';
    $hit->{alignment}->{hit_seq}  .= sprintf ' %-10d', $hit->{blocks}->[-1]->{hit}->{end};
    $hit->{alignment}->{match}    .= '           ';
    $hit->{alignment}->{user_seq} .= sprintf ' %-10d', $hit->{end};
  }
  
  $c->log->debug( 'Search::Sequence::handleResults: modified results data structure: ' .
                  dump( $c->stash->{hits} ) ) if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 parse_log : Private

Parses the output from the rfam_scan script and dumps the resulting data 
structure into the stash.

=cut

sub parse_log : Private {
  my ( $this, $c, $jobId ) = @_;
  
  # split the log into individual lines and parse them 
  my @lines = split /\n/, $c->{stash}->{results}->{$jobId}->{rawData};
  
  my $hits     = []; # everything...
  my $hit;           # an individual hit
  my $family   = ''; # the current family
  my $strand   = ''; # the current strand, plus or minus
  my ( $seq_start, $seq_end ); # sequence start/end positions
  my ( $hit_start, $hit_end ); # hit start/end positions

  for ( my $n = 0; $n < scalar @lines; $n++ ) {
    my $line = $lines[$n];
    
    $c->log->debug( sprintf "Search::Sequence::parse_log: line % 3d: %s",
                            $n, $line ) if $c->debug;
    
    # store the name of the family for this hit
    if ( $line =~ m/^CM: ([\w\-]+)/ ) {
      $c->log->debug( "Search::Sequence::parse_log: results for |$1|" )
        if $c->debug;
      $family = $1;
    }

    # sequence start/end
    elsif ( $line =~ m|^>.*?/(\d+)\-(\d+)$| ) {
      $c->log->debug( "Search::Sequence::parse_log: sequence start-end: |$1-$2|" )
        if $c->debug;

      # store the ID for later
      $seq_start = $1;
      $seq_end   = $2;
    }

    # plus or minus strand
    elsif ( $line =~ m/^\s*(.*?) strand results/ ) {
      $c->log->debug( "Search::Sequence::parse_log: results for |$1| strand" )
        if $c->debug;

      # store the strand
      $strand = $1 eq 'Plus' ? '+' : '-';
    }

    # get the sequence, start and end
    elsif ( $line =~ m|Query = (\d+) - (\d+), Target = (\d+) - (\d+)| ) {
      if ( $c->debug ) {
        $c->log->debug( "Search::Sequence::parse_log: query start-end:  |$1 - $2|" );
        $c->log->debug( "Search::Sequence::parse_log: target start-end: |$3 - $4|" );
      }

      if ( $4 > $3 ) {
        $hit_start = $3;
        $hit_end   = $4;
      }
      else {
        $hit_start = $4;
        $hit_end   = $3;
      }
    }

    # get the score, etc.
    elsif ( $line =~ m|Score = (.*?), E = (.*?), P = (.*?), GC =\s+(.*)$| ) {
      if ( $c->debug ) {
        $c->log->debug( "Search::Sequence::parse_log: score: |$1|" );
        $c->log->debug( "Search::Sequence::parse_log: E:     |$2|" );
        $c->log->debug( "Search::Sequence::parse_log: P:     |$3|" );
        $c->log->debug( "Search::Sequence::parse_log: GC:    |$4|" );
      }

      $hit = {
        family => $family,
        strand => $strand,
        start  => $hit_start + $seq_start - 1,
        end    => $hit_end   + $seq_start - 1,
        score  => $1,
        E      => $2,
        P      => $3,
        GC     => $4,
        blocks => []
      };

      push @$hits, $hit;
      
      # parse the alignment blocks
      for ( my $b = $n + 2; $b < scalar @lines; $b += 5 ) {
        last unless $lines[$b+1] =~ m/^\s+\d+.*?\s+\d+\s*$/;
        
        $c->log->debug( 'Search::Sequence::parse_log: block constitutes lines ' .
                        "$b - " . ( $b + 3 ) ) if $c->debug; 
        
        my $block = read_block( [ @lines[ $b .. $b+3 ] ] );
  
        $c->log->debug( 'Search::Sequence::parse_log: block: ' .
                        dump( $block ) ) if $c->debug; 
        
        push @{ $hit->{blocks} }, $block;
      }
      
    }
  
  }
  
  # stash the parsed results
  $c->stash->{hits} = $hits;
  
}

#-------------------------------------------------------------------------------
#- subroutines -----------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 NON-ACTION METHODS

=head2 read_block

Given a reference to an array containing the four lines of an alignment block,
this function parses the block and returns a reference to a hash containing all 
of the relevant information.

=cut

sub read_block {
  my $lines = shift;

  my $block = {};

  $lines->[0] =~ s/^\s*(.*?)\s*$/$1/;
  $block->{ss} = $1;

  $lines->[1] =~ m/^\s+(\d+)\s+(.*?)\s+(\d+)\s*$/;
  $block->{hit}->{seq}   = $2;
  $block->{hit}->{start} = $1;
  $block->{hit}->{end}   = $3;

  $block->{match} = substr $lines->[2], 11, length $2;

  $lines->[3] =~ m/^\s+(\d+)\s+(.*?)\s+(\d+)\s*$/;
  $block->{user}->{seq}   = $2;
  $block->{user}->{start} = $1;
  $block->{user}->{end}   = $3;

  return $block;
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

__END__

CM: mir-2
>UserSeq/79-459

  Plus strand results:

 Query = 1 - 72, Target = 73 - 144
 Score = 58.80, E = 7.87e-16, P = 8.842e-18, GC =  50

           ::::<<<<<<<<<<<<<<<<<<--<<<___________.______>>>>>>>>>>>>->>
         1 uaacuCaUCAAAgUGGCUGUGAaaUguuguuauuuuuu.aaaaUCauAUCACAGCCAGcU 59
             +CUC UCAAAGUGG:UGUGAAAUG ++UU    UUU      C UAUCACA:CCAGCU
        73 GGGCUC-UCAAAGUGGUUGUGAAAUGCAUUUCCGCUUUgCGCGGCAUAUCACAGCCAGCU 131

           >>>>>>>::::::
        60 UUGAuGaGcUugg 72
           UUGAUGAGCUU G
       132 UUGAUGAGCUUAG 144


 Query = 1 - 72, Target = 247 - 311
 Score = 38.62, E = 2.022e-10, P = 2.271e-12, GC =  45

           ::::<<<<<<<<<.<<<<<<<<<--<<<_________________>>>>>>>>>>>>->>
         1 uaacuCaUCAAAg.UGGCUGUGAaaUguuguuauuuuuuaaaaUCauAUCACAGCCAGcU 59
           UA+CUCAUCAAAG UGG UGUGA+AUG        UU   +A+ C UAUCACA CCA C
       247 UAGCUCAUCAAAGcUGGCUGUGAUAUGC------GUUGGGUAUCCAUAUCACAACCA-C- 298

           >>>>>>>::::::
        60 UUGAuGaGcUugg 72
           UUGAUGAG  U
       299 UUGAUGAGGCUUU 311


  Minus strand results:

 Query = 1 - 72, Target = 309 - 245
 Score = 56.13, E = 4.102e-15, P = 4.609e-17, GC =  48

           ::::<<<<<<<<<<<<<<<<<<--<<<_________________>>>>>>>>>>>>->>>
         1 uaacuCaUCAAAgUGGCUGUGAaaUguuguuauuuuuuaaaaUCauAUCACAGCCAGcUU 60
           +  CUCAUCAA  UGG:UGUGA+AUG:      +U+   AA  C:UAUCACA:CCAGC U
       309 AGCCUCAUCAAG-UGGUUGUGAUAUGG------AUACCCAACGCAUAUCACAGCCAGCUU 257

           >>>>>>::::::
        61 UGAuGaGcUugg 72
           UGAUGAGCU GG
       256 UGAUGAGCUAGG 245


 Query = 1 - 72, Target = 142 - 71
 Score = 34.10, E = 3.29e-09, P = 3.697e-11, GC =  50

           ::::<<<<<<<<<.<<<<<<<<<--<<<___________.______>>>>>>>>>>>>->
         1 uaacuCaUCAAAg.UGGCUGUGAaaUguuguuauuuuuu.aaaaUCauAUCACAGCCAGc 58
           +A+CUCAUCAAAG UGG UGUGA+A G  G   +++    AAA+ C U UCACA CCA C
       142 AAGCUCAUCAAAGcUGGCUGUGAUAUGCCGCGCAAAGCGgAAAUGCAUUUCACAACCA-C 84

           >>>>>>>>::::::
        59 UUUGAuGaGcUugg 72
           UUUGA GAGC
        83 UUUGA-GAGCCCUC 71

