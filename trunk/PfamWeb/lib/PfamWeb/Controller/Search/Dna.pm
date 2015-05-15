
# Dna.pm
# jt6 20070731 WTSI
#
# $Id: Dna.pm,v 1.6 2008-09-03 15:39:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Dna - perform batch DNA sequence searches

=cut

package PfamWeb::Controller::Search::Dna;

=head1 DESCRIPTION

This controller is responsible for running batch DNA sequence searches.

$Id: Dna.pm,v 1.6 2008-09-03 15:39:58 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Drawing::Layout::LayoutManager;
use JSON qw( -convert_blessed_universally );
use Storable qw( thaw );
use Data::Dump qw(dump);

use base qw( PfamBase::Controller::Search::BatchSearch
             PfamWeb::Controller::Search ); 

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 results : Local

Shows the results of a DNA search.

=cut

sub results : Local {
  my ( $this, $c, $arg ) = @_;

  my $job_id = $c->req->param('jobId') ||
               $arg || 
               '';

  $c->stash->{template} = 'pages/search/dna/results.tt';

  unless ( $job_id =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->stash->{seqSearchError} = 'No valid job ID.';

    $c->log->debug( "Search::Dna::results: not a valid job ID: |$job_id|" )
      if $c->debug;

    return;
  }

  $c->log->debug( "Search::Dna::results: found valid job ID: $job_id"  )
    if $c->debug;

  $c->stash->{jobId} = $job_id;

  my @jobs = $c->model( 'WebUser::JobHistory' )
               ->search( { job_id => $job_id }, {} );

  unless ( scalar @jobs ) {
    $c->stash->{seqSearchError} = 'We could not find any jobs with that ID';

    $c->log->debug( 'Search::Dna::results: no jobs with that ID' )
      if $c->debug;

    return;
  }
  
  # stash the DNA sequence for the template
  $c->stash->{sequences} = [ map { $_->stdin} @jobs ];

  $c->log->debug( 'Search::Dna::results: found ' . scalar @jobs . ' frame jobs' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

sub resultset : Local {
  my ( $this, $c, $arg ) = @_;

  my $job_id = $c->req->param('jobId') ||
               $arg || 
               '';

  unless ( $job_id =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->stash->{json}->{error} = 'No valid job ID.';

    $c->log->debug( "Search::Dna::resultset: not a valid job ID: |$job_id|" )
      if $c->debug;

    $c->forward('View::JSON');

    return;
  }

  $c->log->debug( "Search::Dna::resultset: found valid job ID: $job_id"  )
    if $c->debug;

  $c->stash->{job_id} = $job_id;

  my @jobs = $c->model( 'WebUser::JobHistory' )
               ->search( { job_id   => $job_id,
                           job_type => 'A' },
                         { prefetch => [ 'job_stream' ] } );

  unless ( scalar @jobs ) {
    $c->stash->{json}->{error} = 'We could not find any jobs with that ID';

    $c->log->debug( 'Search::Dna::results: no jobs with that ID' )
      if $c->debug;

    $c->forward('View::JSON');

    return;
  }

  $c->stash->{json}->{jobId}    = $job_id;
  $c->stash->{json}->{sequence} = $jobs[0]->stdin;
  
  JOB: foreach my $job ( @jobs ) {
    my $status = $job->status;
    my $options = from_json( $job->options || '{}' );
    next unless $options->{dna};

    my $frame = $options->{frame};

    $c->stash->{json}->{results}->[$frame]->{status} = $status;

    if ( $status eq 'DONE' ) {

      # thaw out and stash the actual results
      my $results;
      eval {
        $results = thaw( $job->stdout );
      };
      if ( $@ ) {
        $c->stash->{seqSearchError} = 'There was a problem retrieving one of your job results';

        $c->log->debug( "Search::Dna::results: error thawing results: $@" )
          if $c->debug;

        next JOB;
      }

      $c->stash->{json}->{results}->[$frame]->{hits}    = $results;
      $c->stash->{json}->{results}->[$frame]->{graphic} = 
        $c->forward( 'layout_dg', [ $results ] );
    }
  }

  $c->log->debug( 'Search::Dna::results: found ' 
                  . scalar @{ $c->stash->{json}->{results} } . ' frame jobs' )
    if $c->debug;

  $c->forward('View::JSON');
}

#-------------------------------------------------------------------------------

sub resulttable : Local {
  my ( $this, $c, $job_id, $frame ) = @_;
  
  $c->stash->{template} = 'pages/search/dna/results_table.tt';

  unless ( $job_id =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->stash->{seqSearchError} = 'No valid job ID.';

    $c->log->debug( "Search::Dna::resulttable: not a valid job ID: |$job_id|" )
      if $c->debug;

    return;
  }

  unless ( defined $frame and $frame =~ m/^\d$/ ) {
    $c->stash->{seqSearchError} = 'No valid frame number.';

    $c->log->debug( "Search::Dna::resulttable: not a valid frame number: |$frame|" )
      if $c->debug;

    return;
  }

  my @jobs = $c->model( 'WebUser::JobHistory' )
              ->search( { job_id => $job_id },
                        { prefetch => [ 'job_stream' ] } );

  my $results;
  JOB: foreach my $job ( @jobs ) {

    # get the results and make absolutely sure we're handling a DNA search here
    my $options = from_json( $job->options || '{}' );
    next JOB unless $options->{dna};

    # we can only build a result table if there are results...
    my $status = $job->status;
    next JOB unless $status eq 'DONE';

    # and we only want to do it for the specified frame
    my $job_frame = $options->{frame};
    next JOB unless $job_frame == $frame;

    # should be safe to cache this now. Shouldn't it...?
    # $c->cache_page( 604800 );

    # thaw out and stash the actual results
    eval {
      $results = thaw( $job->stdout );
    };
    if ( $@ ) {
      $c->stash->{seqSearchError} = "There was a problem retrieving results for job $job_id";

      $c->log->debug( "Search::Dna::results: error thawing results: $@" )
        if $c->debug;

      return;
    }

  } # JOB

  unless ( $results ) {
    $c->stash->{seqSearchError} = 'We did not find any results.';

    $c->log->debug( "Search::Dna::resulttable: no results for job |$job_id|" )
      if $c->debug;

    return;
  }

  # push the config setting for the maximum desc string length into the stash
  $c->stash->{max_desc_length} = $this->{max_desc_length};

  $c->stash->{results} = $results;
  $c->stash->{frame}   = $frame;
}


# sub search : Path {
#   my( $this, $c ) = @_;
# 
#   # validate the input
#   unless ( $c->forward( 'validate_input' ) ) {
#     $c->stash->{dnaSearchError } = $c->stash->{searchError};
#     return;
#   }
#   
#   #----------------------------------------
#   
#   # no options for a DNA search
#   $c->stash->{options} = '';
#   
#   # before we actually run the search, check we didn't do it recently
#   unless ( $c->forward( 'check_unique' ) ) {
#     $c->stash->{dnaSearchError } = $c->stash->{searchError};
#     return;
#   }
# 
#   # generate a job ID
#   $c->stash->{jobId} = Data::UUID->new()->create_str();
# 
#   # set the queue
#   $c->stash->{job_type} = 'dna';
# 
#   # and submit the job...
#   unless ( $c->forward( 'queue_search_transaction' ) ) {
#     $c->stash->{dnaSearchError } = $c->stash->{searchError};
#     return;
#   }
#   
#   #----------------------------------------
#   
#   # set a refresh URI that will be picked up by head.tt and used in a 
#   # meta refresh element
#   $c->stash->{refreshUri}   = $c->uri_for( '/search' );
#   $c->stash->{refreshDelay} = 30;
# 
#   $c->log->debug( 'Search::Dna::search: batch dna search submitted' )
#     if $c->debug;
#   $c->stash->{template} = 'pages/search/sequence/batchSubmitted.tt';
# }

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Error messages are returned in the stash as
"searchError".

=cut

sub validate_input : Private {
  my( $this, $c ) = @_;
  
  # the sequence itself

  # make sure we got a parameter first
  unless ( defined $c->req->param('seq') ) {
    $c->stash->{seqSearchError} =
      'You did not supply a valid DNA sequence. Please try again.';

    $c->log->debug( 'Search::Dna::validate_input: no DNA sequence; returning to form' )
      if $c->debug;

    return 0;
  }

  # check it's not too long
  if ( length $c->req->param('seq') > $this->{maxDnaSeqLength} ) {
    $c->stash->{searchError} =
      'Your sequence was too long. We can only accept DNA sequences upto '. $this->{maxDnaSeqLength} / 1000 . 'kb.';

    $c->log->debug( 'Search::Dna::validate_input: sequence too long; returning to form' )
      if $c->debug;
      
    return 0;
  }
  # check it's not too long
    $c->log->debug( 'Search::Dna::validate_input: check length curr: '.length $c->req->param('seq').' <'.$this->{minDnaSeqLength} );
  if ( length $c->req->param('seq') < $this->{minDnaSeqLength} ) {
    $c->stash->{searchError} =
      'Your sequence was too short. We can only accept DNA sequences of >  '. $this->{minDnaSeqLength} / 1000 . 'kb.';

    $c->log->debug( 'Search::Dna::validate_input: sequence too short; returning to form' )
      if $c->debug;
      
    return 0;
  }


  # email address
  if ( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  }
  else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::Dna::validate_input: bad email address; returning to form' )
      if $c->debug;
      
    return 0;
  }  

  # tidy up the sequence and make sure it's only got the valid DNA characters
  my @seqs = split /\n/, $c->req->param('seq');
  shift @seqs if $seqs[0] =~ m/^\>/;
  my $seq = uc( join '', @seqs );
  $seq =~ s/[\s\r\n]+//g;
  
  unless ( $seq =~ m/^[ACGTRYKMSWBDHVN]+$/ ) {
    $c->stash->{searchError} =
      'No valid sequence found. Please enter a valid DNA sequence and try again.';

    $c->log->debug( 'Search::Dna::validate_input: invalid DNA sequence; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # store the valid sequence. Up until this point there was no need to have it 
  # in the stash, since it might have been invalid. Now that it's validated, 
  # however, we actually need it
  $c->log->debug( "Search::Dna::validate_input: sequence looks ok: |$seq|" )
    if $c->debug;
    
  $c->stash->{input} = $seq;
 
  # passed ! 
  $c->log->debug( 'Search::Dna::validate_input: input parameters all validated' )
    if $c->debug;
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 layout_dg : Private

Generates a JSON description of the domain graphics for the given results.

=cut

sub layout_dg : Private {
  my ( $this, $c, $hits ) = @_;

  my $seq = $c->stash->{json}->{sequence};
  $seq =~ s/^>.*?\n(.*)/$1/;

  my ( @regions, @markups );

  HIT: foreach my $hit ( @$hits ) {

    # only add significant hits to the domain graphic
    next unless $hit->{sig};

    push @regions, new Bio::Pfam::Sequence::Region( {
      start       => $hit->{env}->{from},
      end         => $hit->{env}->{to},
      aliStart    => $hit->{seq}->{from},
      aliEnd      => $hit->{seq}->{to},
      modelStart  => $hit->{hmm}->{from},
      modelEnd    => $hit->{hmm}->{to},
      modelLength => $hit->{model_length},
      type        => 'pfama',
      metadata    => new Bio::Pfam::Sequence::MetaData( {
        accession   => $hit->{acc},
        identifier  => $hit->{name},
        type        => $hit->{type},
        description => $hit->{desc},
        score       => $hit->{evalue},
        scoreName   => 'e-value',
        start       => $hit->{env}->{from},
        end         => $hit->{env}->{to},
        aliStart    => $hit->{seq}->{from},
        aliEnd      => $hit->{seq}->{to},
        database    => 'pfam'
      } )
    } );

    if ( $hit->{act_site} ) {
      # $c->log->debug( 'Search::Sequence::layout_dg: adding active sites' )
      #   if $c->debug;

      foreach my $site_residue ( @{ $hit->{act_site} } ) {
        push @markups, new Bio::Pfam::Sequence::Markup( {
          start    => $site_residue,
          residue  => substr( $seq, $site_residue - 1, 1 ),
          type     => 'Pfam predicted active site',
          metadata => Bio::Pfam::Sequence::MetaData->new( {
            start       => $site_residue,
            type        => 'Pfam predicted active site',
            description => 'Residue ' . substr( $seq, $site_residue - 1, 1 ) . $site_residue,
            database    => 'pfam'
          } )
        } );
      }

    }

  } # end of "foreach hit"

  my $sequence = new Bio::Pfam::Sequence( {
    length  => length( $seq ),
    regions => \@regions,
    motifs  => [],
    markups => \@markups 
  } );

  $c->log->debug( 'Search::Dna::layout_dg: sequence object: '
                  . dump( $sequence ) ) if $c->debug;

  my $lm = new Bio::Pfam::Drawing::Layout::LayoutManager;
  my $sequences = [ $sequence ];
  $lm->layoutSequences( $sequences );

  # we only build one graphic at a time, so hand back only one
  return $sequences->[0];
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
