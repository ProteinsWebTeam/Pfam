#!/software/bin/perl

use strict;
use warnings;

use JSON;
use File::Temp qw( tempfile);
use Bio::Pfam::Scan::PfamScan;
use Bio::Pfam::WebServices::PfamQueue;
use Storable qw( freeze nfreeze );

use Data::Dump qw( dump );

my $DEBUG = defined($ENV{DEBUG}) ? $ENV{DEBUG} : 1;
$ENV{PFAMOFFLINE_CONFIG} ||= $ENV{HOME} . '/perl/pfam_scan/pfam_backend.conf';

my $pq = Bio::Pfam::WebServices::PfamQueue->new( 'h3' );
$pq->daemonise unless $DEBUG;

my $ps = Bio::Pfam::Scan::PfamScan->new();

JOB: while ( 1 ) {

  my $job = $pq->satisfy_pending_job;

  unless ( $job->{id} ) {
    $DEBUG && print STDERR "dequeuer: no job; sleeping\n";
    sleep 2; # TODO make the sleep delay a configuration variable
    next JOB;
  }
  $DEBUG && print STDERR 'dequeuer: job specification: ' . dump( $job ) . "\n";

  my $sequence = ">UserSeq\n" . $job->{stdin};
  my ($fh, $filename) = tempfile();
  print $fh $sequence;
  close($fh);

  my $input = {
    -dir      => $pq->dataFileDir,
    -as       => 1,
    #-sequence => $sequence,
    -fasta    => $filename,
  };

  if ( defined $pq->cpus ) {
    $DEBUG && print STDERR 'dequeuer: using ' . $pq->cpus . " cpus\n";
    $input->{-cpu} = $pq->cpus;
  }

  if(defined( $job->{options} ) and $job->{options} =~ /\S+/ ){
    my $opts = from_json($job->{options});
    $DEBUG && print STDERR 'dequeuer: options from db:'.dump( $opts )."\n";
    if(defined( $opts->{evalue}) ){
      $input->{-e_dom} = $opts->{evalue};
      $input->{-e_seq} = $opts->{evalue};
    }
  }
  if(defined( $job->{job_type} )){
    if($job->{job_type} eq 'A'){
      push(@{$input->{-hmmlib}},  'Pfam-A.hmm');
    }elsif( $job->{job_type} eq 'B'){
      push(@{$input->{-hmmlib}},  'Pfam-B.hmm');
    }
  }

  $DEBUG && print STDERR 'dequeuer: PfamScan params: ' . dump( $input ) . "\n";

  $DEBUG && print STDERR "dequeuer: created a PfamScan object\n";

  # TODO update the job_history.opened column appropriately

  my $results;
  eval {
    $DEBUG && print STDERR "dequeuer: running a search...\n";
    $ps->search( $input );
    $DEBUG && print STDERR "dequeuer: done\n";
    $results = nfreeze( $ps->results( $input->{-e_dom} ) );
  };
  if ( $@ ) {
    $DEBUG && print STDERR "ERROR: search failed: $@\n";
    $pq->update_job_stream( $job->{id}, 'stderr', $@ );
    $pq->update_job_status( $job->{id}, 'FAIL' );
    next JOB;
  }

  if ( $results ) {
    $DEBUG && print STDERR "dequeuer: updating job stream with results\n";
    $pq->update_job_stream( $job->{id}, 'stdout', $results );
    $DEBUG && print STDERR "dequeuer: done\n";
  }
  else {
    $DEBUG && print STDERR "dequeuer: updating job stream with empty result set\n";
    $pq->update_job_stream( $job->{id}, 'stdout', 'no matches found' );
    $DEBUG && print STDERR "dequeuer: done\n";
  }

  $DEBUG && print STDERR "dequeuer: updating job status\n";
  $pq->update_job_status( $job->{id}, 'DONE' );
  $DEBUG && print STDERR "dequeuer: done\n";
  
  $DEBUG && print STDERR "dequeuer: cleaning up temp file\n";
  unlink $filename
    or warn "WARNING: couldn't remove temp file '$filename': $!";
  $DEBUG && print STDERR "dequeuer: done\n";
  
  $DEBUG && print STDERR "dequeuer: restarting event loop\n";
}

exit;

