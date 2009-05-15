#!/software/bin/perl

use strict;
use warnings;

use Bio::Pfam::Scan::PfamScan;
use Bio::Pfam::WebServices::PfamQueue;
use Storable qw( freeze );

use Data::Dump qw( dump );

my $DEBUG = $ENV{DEBUG} || 1;
$ENV{PFAMOFFLINE_CONFIG} = $ENV{HOME} . '/perl/pfam_scan/pfam_backend.conf';

my $pq = Bio::Pfam::WebServices::PfamQueue->new( 'h3' );
$pq->daemonise unless $DEBUG;

JOB: while ( 1 ) {

  my $job = $pq->satisfy_pending_job;

  unless ( $job->{id} ) {
    $DEBUG && print STDERR "no job; sleeping\n";
    sleep 2; # TODO make the sleep delay a configuration variable
    next JOB;
  }
  $DEBUG && print STDERR 'job specification: ' . dump( $job ) . "\n";

  my $sequence = ">UserSeq\n" . $job->{stdin};

  my %input = (
    -dir      => $pq->dataFileDir, # TODO make "dir" configurable
    -sequence => $sequence,
  );

  $DEBUG && print STDERR 'job params: ' . dump( \%input ) . "\n";

  my $ps = Bio::Pfam::Scan::PfamScan->new( %input );
  $DEBUG && print STDERR "created a PfamScan object\n";
  # TODO make the PfamScan object stick around; add a "set_input" method

  my $results;
  eval {
    $DEBUG && print STDERR "running a search...\n";
    $ps->search;
    $DEBUG && print STDERR "done\n";
    $results = freeze( $ps->results );
  };
  if ( $@ ) {
    $DEBUG && print STDERR "ERROR: search failed: $@\n";
    $pq->update_job_stream( $job->{id}, 'stderr', $@ );
    $pq->update_job_status( $job->{id}, 'FAIL' );
    next JOB;
  }

  $DEBUG && print STDERR "updating job status\n";
  $pq->update_job_status( $job->{id}, 'DONE' );
  $DEBUG && print STDERR "done\n";

  if ( $results ) {
    $DEBUG && print STDERR "updating job stream with results\n";
    $pq->update_job_stream( $job->{id}, 'stdout', $results );
    $DEBUG && print STDERR "done\n";
  }
  else {
    $DEBUG && print STDERR "updating job stream with empty result set\n";
    $pq->update_job_stream( $job->{id}, 'stdout', 'no matches found' );
    $DEBUG && print STDERR "done\n";
  }

  $DEBUG && print STDERR "restarting event loop\n";
}

exit;

