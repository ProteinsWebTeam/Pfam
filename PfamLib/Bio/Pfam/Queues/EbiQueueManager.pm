
# EbiQueueMananger.pm
# jgt 20131202

package Bio::Pfam::Queues::EbiQueueManager;

use Moose;
use Log::Log4perl qw(get_logger :levels);
use POSIX qw(setsid);
use File::Basename;
use File::Find;
use File::Temp;
use Carp;
use JSON;
use Storable qw( nfreeze );
use Data::Printer;
use Mail::Send;
use Parallel::ForkManager;
use Try::Tiny;
use IPC::Cmd qw( run );
# use IPC::Open3;
# use IO::Handle;
use Text::Wrap qw( wrap );

use WebUser;
use Bio::Pfam::EbiRestClient;

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------

# public attributes

has 'config' => (
  is       => 'ro',
  isa      => 'Bio::Pfam::Config',
  required => 1,
);

has 'queue' => (
  is       => 'ro',
  isa      => 'Str',
  required => 1,
);

#-------------------------------------------------------------------------------
# private attributes

has '_log' => (
  is      => 'ro',
  default => sub {
    my $log = get_logger();
    if ( $ENV{DEBUG_DEQUEUER} ) {
      $log->level($DEBUG);
      $log->debug( 'enabling verbose logging (in response to DEBUG_DEQUEUER env variable)' );
    }
    return $log;
  },
  # TODO this needs to be more robust when the calling script doesn't init
  # Log4perl
);

has '_schema' => (
  is      => 'ro',
  lazy    => 1,
  default => sub {
    my $self = shift;
    my $db_params = $self->config->{Model}->{WebUser};
    WebUser->connect(
      'dbi:' . $db_params->{driver} . ':'
             . $db_params->{database} . ':'
             . $db_params->{host} . ':'
             . $db_params->{port},
      $db_params->{user},
      $db_params->{password}
    );
  },
);

# convenience slot to hold the queue specification from the configuration
has '_queue_spec' => (
  is => 'rw',
  isa => 'HashRef',
);

# convenience slot for the jobs-per-cycle config value
has '_jobs_per_cycle' => (
  is => 'rw',
  isa => 'HashRef',
  default => sub { { submission => 10, results => 50 } },
);

# what type of job this queue manager is responsible for
has '_job_type' => (
  is => 'rw',
  isa => 'Str',
);

# RESTful search interface client
has '_rc' => (
  is => 'ro',
  isa => 'Bio::Pfam::EbiRestClient',
  default => sub { Bio::Pfam::EbiRestClient->new },
  lazy => 1,
);

#-------------------------------------------------------------------------------
# post-construction checks

sub BUILD {
  my $self = shift;

  # see if we should enable debugging messages
  $self->_log->level($DEBUG) if $ENV{DEBUG};

  # check that the specified queue is valid, i.e. available in the config
  my $queue_config = $self->config->{search}->{queues};
  my %known_queues = map { $_ => 1 } keys %$queue_config;

  croak 'ERROR: not a valid queue; known queues (from configuration) are: '
        . join( ', ', keys %known_queues ) . "\n"
    unless $known_queues{$self->queue};

  # as a convenience: retrieve the queue specification from the configuration,
  # based on the queue name that we get from arguments; store the job
  # type directly; get the job_per_cycle setting
  $self->_queue_spec( $queue_config->{$self->queue} );
  $self->_job_type( $self->_queue_spec->{job_type} );
  $self->_jobs_per_cycle( {
    submission => $self->config->{search}->{jobs_per_submission_cycle},
    results    => $self->config->{search}->{jobs_per_results_cycle},
  } );

  # set up the REST client
  $self->_rc->base_url( $self->_queue_spec->{base_url} )
    if defined $self->_queue_spec->{base_url};
}

#-------------------------------------------------------------------------------
#- public methods --------------------------------------------------------------
#-------------------------------------------------------------------------------

# background the manager

sub daemonise {
  my $self = shift;

  my $lock_dir = $self->config->{search}->{lock_dir};
  my( $pid_file, $dir, $suffix ) = fileparse($0);
  $pid_file = "${lock_dir}/${pid_file}.pid";

  $self->_log->info( "daemonising script; lock file '$pid_file'" );

  umask 0;
  open STDIN,   '/dev/null' or $self->_log->logdie( "can't read from /dev/null: $!" );
  open STDOUT, '>/dev/null' or $self->_log->logdie( "can't write to /dev/null: $!" );
  open STDERR, '>/dev/null' or $self->_log->logdie( "can't write to /dev/null: $!" );
  defined( my $pid = fork ) or $self->_log->logdie( "can't fork: $!" );

  if ( $pid ) {
    open PIDFILE, ">$pid_file"
      or $self->_log->logdie( "can't open $pid_file: $!\n" );
    print PIDFILE $pid;
    close PIDFILE;
    exit 0;
  }

  setsid or $self->_log->logdie( "can't start a new session: $!" );
}

#-------------------------------------------------------------------------------

# start the (endless) polling loop

sub start_polling {
  my $self = shift;

  $self->_log->info( 'starting polling loop' );
  my $delay = $self->_queue_spec->{polling_interval} || 2;

  while ( 1 ) {

    $self->_log->info( 'polling for jobs of type "' . $self->_queue_spec->{job_type} . '"' );

    $self->_enqueue_pending_jobs;
    $self->_check_running_jobs;

    $self->_log->debug( "sleeping for $delay seconds" );
    sleep $delay

  } # end of endless while loop

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# check for any PENDing jobs of the appropriate type in our tracking table
# and queue them to the EBI web service as specified by the queue configuration

sub _enqueue_pending_jobs {
  my $self = shift;

  my @jobs = $self->_schema
               ->resultset('JobHistory')
                 ->get_pending_jobs( $self->_job_type );

  $self->_log->info( 'found ' . scalar @jobs . ' pending jobs' )
    if scalar @jobs;

  my $job_number = 0;
  JOB: foreach my $job ( @jobs[ 0 .. ( $self->_jobs_per_cycle->{submission} - 1 ) ] ) {
    # since we're asking for an array slice, we might get undef for $job when
    # the number of running jobs is less than the number of jobs-per-cycle
    next unless defined $job;

    # these are the options specified by the user who submitted the search.
    # We need to catch a potential exception from "from_json", because
    # the alignment jobs have their options specified as a non-JSON string
    # like "-acc RF12345"

    my $job_options;
    try {
      $job_options = from_json( $job->options );
    }
    catch {
      $self->_log->debug( 'job options is not a JSON string' );
    };
    $job_options ||= {};

    $self->_log->debug( 'job options: ', p( $job_options ) );

    # these are the options that we're going to hand to the EBI search service.
    # It spits the dummy over 'key=<undef>' pairs, so we have to be careful to
    # add only those keys that actually have a value
    my $search_options = {};
    $search_options->{database} = $self->_queue_spec->{database}
      if defined $self->_queue_spec->{database};
    $search_options->{evalue}   = $job_options->{evalue}
      if defined $job_options->{evalue};

    $self->_log->info( 'search options: ', p( $search_options ) );

    # submit the job

    # it's a batch job...
    if ( $self->_queue_spec->{synchronisation} eq 'async' ) {

      my $error_message = $self->_submit_batch_job( $job, $search_options );

      if ( defined $error_message ) {
        $job->update( { status => 'FAIL',
                        closed => \'NOW()', } );
        $job->job_stream->update ( { stderr => $error_message } );

        $self->_mail_error_message( $job, $error_message );

        $self->_log->warn( 'failed to submit a chunk for job ' . $job->job_id
                           . '; added error to job_stream and mailed user' );
      }
      else {
        $job->update( { status => 'RUN',
                        started => \'NOW()', } );

        $self->_log->debug( 'successfully submitted job and updated job_history' );
      }
    }

    # it's an interactive job
    elsif ( $self->_queue_spec->{synchronisation} eq 'sync' ) {

      my $error_message = $self->_submit_interactive_job( $job, $search_options );

      if ( $error_message ) {
        $job->update( { status => 'FAIL',
                        closed => \'NOW()', } );
        $job->job_stream->update ( { stderr => $_ } );

        $self->_log->warn( 'failed to submit interactive job ' . $job->job_id
                           . "; added error to job_stream: $error_message" );
      }
      else {
        $job->update( { status => 'RUN',
                        started => \'NOW()', } );

        $self->_log->debug( 'successfully submitted job and updated job_history' );
      }
    }

    # and everything else is a job that runs on the machine where the dequeuer sits...
    else {
      $self->_run_local_job( $job );
    }

    $job_number++;
    if ( $job_number >= $self->_jobs_per_cycle->{submission} ) {
      $self->_log->debug( 'reached jobs-per-cycle limit (' . $self->_jobs_per_cycle->{submission} . ')' );
    }
  }

}

#-------------------------------------------------------------------------------
# coordinates the splitting of a batch input file and the submission of the
# resulting chunks of sequence.
#
# This method submits jobs to the ES service in parallel. The number of forked
# clients is controlled by the queue config.

sub _submit_batch_job {
  my ( $self, $job, $search_options ) = @_;

  # bail unless there's a valid email address
  my $email = $job->email;
  unless ( $email and Email::Valid->address( $email ) ) {
    $self->_log->logwarn( 'no valid email address supplied' );
    die 'No valid email address supplied.';
  }

  $search_options->{format} = 'txt';
  $search_options->{email}  = 'xfam@ebi.ac.uk';

  my $chunks = $self->_split_batch_file( $job );

  my $jh = $self->_schema->resultset('JobHistory');
  my $js = $self->_schema->resultset('JobStream');

  my $pfm;
  $pfm = Parallel::ForkManager->new( $self->_queue_spec->{num_submission_forks} )
    unless $ENV{SINGLE_THREADED_DEQUEUER};

  my $error_message;

  CHUNK: foreach my $chunk ( @$chunks ) {
    unless ( $ENV{SINGLE_THREADED_DEQUEUER} ) {
      $pfm->start and next;
    }

    # ignore the child processes when running under the perl debugger
    $DB::inhibit_exit = 0;

    $search_options->{sequence} = $chunk;
    $search_options->{format}   = $self->_queue_spec->{output_format}
      if defined $self->_queue_spec->{output_format};

    # queue the search to the ES web service
    my $ebi_id;
    eval {
      $ebi_id = $self->_rc->search( $search_options );
      $self->_log->debug( "got job ID $ebi_id" );
    };
    if ( $@ ) {
      $self->_log->logwarn( 'there was a problem submitting a chunk for job '
                            . $job->job_id . ": $@" );
      $error_message = 'There was a problem queueing one of your sequences.';

      $pfm->finish unless $ENV{SINGLE_THREADED_DEQUEUER};
      last CHUNK;
    }

    $self->_log->debug( "submitted batch chunk, given EBI job ID $ebi_id" );

    # store the job in the tracking tables
    my $jh_row = $jh->create( {
      job_id   => $job->job_id,
      ebi_id   => $ebi_id,
      status   => 'RUN',
      options  => $job->options,
      opened   => $job->opened,
      started  => \'NOW()',
      job_type => 'chunk',
    } );

    unless ( $jh_row ) {
      $self->_log->logwarn( 'there was a problem storing job info for chunk for batch job '
                            . $job->job_id );
      $error_message = 'There was a problem recording the details of your search.';

      $pfm->finish unless $ENV{SINGLE_THREADED_DEQUEUER};
      last CHUNK;
    }

    my $rv = $js->create( {
      id    => $jh_row->id,
      stdin => $chunk,
    } );

    unless ( $rv ) {
      $self->_log->logwarn( 'there was a problem storing the sequence for chunk for batch job '
                            . $job->job_id );
      $error_message = 'There was a problem storing one of your sequences.';

      $pfm->finish unless $ENV{SINGLE_THREADED_DEQUEUER};
      last CHUNK;
    }

    $pfm->finish unless $ENV{SINGLE_THREADED_DEQUEUER};
  }
  $pfm->wait_all_children unless $ENV{SINGLE_THREADED_DEQUEUER};

  my $chunks_rs = $jh->search( { job_id   => $job->job_id,
                                 job_type => 'chunk' } );

  if ( $error_message or
       $chunks_rs->count != scalar @$chunks ) {
    $self->_log->debug( 'found ' . scalar @$chunks . ' chunks, but submitted only '
                        . $chunks_rs->count . ' jobs' );
    return $error_message;
  }

  $self->_log->debug( 'no errors and correct number of chunk jobs submitted' );

  return;
}

#-------------------------------------------------------------------------------
# retrieves and splits up the input sequences

sub _split_batch_file {
  my ( $self, $job ) = @_;

  my $fasta = $job->job_stream->stdin;

  my @sequences = map { ">$_\n" } split( m/\n\>/, "\n".$fasta );
  shift @sequences;

  my @chunks;
  while ( scalar @sequences > 0 ) {
    push @chunks, join( '', splice( @sequences, 0, $self->_queue_spec->{chunk_size} ) );
  }

  $self->_log->info( 'batch file was split into ' . scalar @chunks . ' chunks' );

  return \@chunks;
}

#-------------------------------------------------------------------------------
# runs a job locally on the machine where this dequeuer is running

sub _run_local_job {
  my ( $self, $job ) = @_;

  my ( $command, $tmp_file );

  if ( $self->_queue_spec->{job_type} eq 'align' ) {
    $self->_log->debug( 'got a Pfam sunburst alignment job' );

    try {
      $tmp_file = $self->_write_alignment_to_tmp_file( $job );
    }
    catch {
      $self->_log->warn( "couldn't write temp file: $_" );
      return;
    };

    # build the command that we're going to run
    $command =
      'align2seed.pl'
    . ' -in ' . $job->job_id . '.fa'
    . ' -tmp ' . $self->config->{tempDir}
    . ' -data ' . $self->_queue_spec->{data_dir} . ' '
    . $job->options;
  }
  elsif ( $self->_queue_spec->{job_type} eq 'rfalign' ) {
    $self->_log->debug( 'got an Rfam sunburst alignment job' );

    try {
      $tmp_file = $self->_write_list_to_tmp_file( $job );
    }
    catch {
      $self->_log->warn( "couldn't write temp file: $_" );
      return;
    };

    $command =
         'esl-afetch ' . $self->_queue_spec->{data_dir} . '/Rfam.full ' . $job->options
    . " | esl-alimanip --informat stockholm --seq-k $tmp_file - "
    . ' | esl-alimask --informat stockholm -g --gapthresh 0.9999999 - ';
  }

  $self->_log->debug( "running command: |$command|" );

  # and execute the command. IPC::Cmd will use IPC::Open3 behind the scenes
  # to capture STDOUT and STDERR from the executed command
  my ( $success, $error_msg, $full_buf, $stdout_buf, $stderr_buf ) =
    run( command => $command, verbose => 0 );

  if ( $success ) {
    $job->update( { status => 'DONE',
                    closed => \'NOW()' } );
  }
  else {
    $job->update( { status => 'FAIL',
                    closed => \'NOW()' } );
    $self->_log->warn( "couldn't run command: $error_msg" );
    return;
  }

  # store the output from the job
  my $results = {};
  $results->{stdout} = join '', @$stdout_buf if scalar @$stdout_buf;
  $results->{stderr} = join '', @$stderr_buf if scalar @$stderr_buf;
  $job->job_stream->update( $results );

  # clean up
  unlink $tmp_file
    or $self->_log->warn( "couldn't remove temp file: $!" );
}

#-------------------------------------------------------------------------------
# writes the alignment (stdin) for a job to a temporary file

sub _write_alignment_to_tmp_file {
  my ( $self, $job ) = @_;

  my $stdin = $job->job_stream->stdin;

  my $filename = $self->config->{tempDir} . '/' . $job->job_id . '.fa';
  $self->_log->debug( "writing stdin to '$filename'" );

  open ( FA, ">$filename" )
    or die "Couldn't write alignment to temporary file: $!";

  print FA ">UserSeq\n" unless $stdin =~ m/^>/;
  print FA $stdin . "\n";

  close FA
    or die "Couldn't close temporary file: $!";

  return $filename;
}

#-------------------------------------------------------------------------------
# strips out a list of accessions from the job's stdin and writes it to file

sub _write_list_to_tmp_file {
  my ( $self, $job ) = @_;

  my $stdin = $job->job_stream->stdin;

  my $filename = $self->config->{tempDir} . '/' . $job->job_id . '.list';
  $self->_log->debug( "writing list to '$filename'" );

  open ( LIST, ">$filename" )
    or die "Couldn't write list to temporary file: $!";

  foreach ( split /\n/, $stdin ) {
    print LIST $1 . "\n" if m/^\>(\S+)/;
  }

  close LIST
    or die "Couldn't close temporary file: $!";

  return $filename;
}

#-------------------------------------------------------------------------------
# submits a single sequence, interactive search

sub _submit_interactive_job {
  my ( $self, $job, $search_options ) = @_;

  my $sequence = $job->job_stream->stdin;
  $self->_log->debug( "sequence to search: $sequence" );

  $search_options->{sequence} = $sequence;
  $search_options->{format}   = $self->_queue_spec->{output_format}
    if defined $self->_queue_spec->{output_format};

  my $ebi_id = $self->_rc->search( $search_options );

  unless ( $ebi_id ) {
    return 'There was a problem submitting your interactive search.';
  }

  my $rv = $job->update( { ebi_id  => $ebi_id,
                           status  => 'RUN',
                           started => \'NOW()' } );
  unless ( $rv ) {
    return 'There was a problem storing the details of your search.';
  }

  $self->_log->debug( "submitted search; internal EBI job ID: $ebi_id" );

  return;
}

#-------------------------------------------------------------------------------
# polls the tracking table for any currently running jobs

sub _check_running_jobs {
  my $self = shift;

  my @jobs = $self->_schema->resultset('JobHistory')
               ->get_running_jobs( $self->_job_type );

  $self->_log->info( 'found ' . scalar @jobs . ' running jobs' )
    if scalar @jobs;

  my $job_number = 0;
  JOB: foreach my $job ( @jobs[ 0 .. ( $self->_jobs_per_cycle->{results} - 1 ) ] ) {
    # since we're asking for an array slice, we might get undef for $job when
    # the number of running jobs is less than the number of jobs-per-cycle
    next unless defined $job;

    my $job_id = $job->job_id;

    if ( $self->_queue_spec->{synchronisation} eq 'async' ) {
      $self->_handle_batch_job( $job );
    }
    else {
      $self->_handle_interactive_job( $job );
    }

    $job_number++;
    if ( $job_number >= $self->_jobs_per_cycle->{results} ) {
      $self->_log->debug( 'reached jobs-per-cycle limit (' . $self->_jobs_per_cycle->{results} . ')' );
    }
  }
}

#-------------------------------------------------------------------------------
# looks for currently running chunk searches in the ES service and handles the
# concatenation of the results when all chunks have been run

sub _handle_batch_job {
  my ( $self, $job ) = @_;

  my $job_id = $job->job_id;

  my $all_chunks_rs  = $self->_schema->resultset('JobHistory')
                         ->get_chunks_for_job( $job_id );
  my $num_chunks = $all_chunks_rs->count;

  # there should always be at least one chunk for a running batch job, so fail
  # if we can't find any
  unless ( $num_chunks > 0 ) {
    $self->_log->logwarn( "no chunks found for job $job_id" );

    $job->update( { status => 'FAIL',
                    closed => \'NOW()' } );
    $job->job_stream->update( { stderr => 'There was a problem running your batch job' } );

    return;
  }

  my $running_chunks_rs = $all_chunks_rs->search( { status => 'RUN' } );

  # check for status of running chunks
  if ( $running_chunks_rs->count > 0 ) {
    $self->_log->debug( "batch job $job_id still has "
                        . $running_chunks_rs->count . ' running chunks' );

    $self->_handle_running_chunks( $running_chunks_rs );

    my $still_running = $all_chunks_rs->search( { status => 'RUN' } )
                                      ->count;

    $self->_log->debug( "still $still_running chunks running" );

    # if there are still jobs running, bail out and come back to check
    # their status on the next polling cycle
    return if $still_running > 0;
  }

  $self->_log->debug( "all chunks complete for batch job $job_id" );

  my $failed_chunks = $all_chunks_rs->search( { status => 'FAIL' } );

  if ( my $num_failed = $failed_chunks->count ) { 
    # there were failed chunks; mail an error message

    $self->_log->debug( "found $num_failed failed chunks; marking job as failed" );

    # format the error message that comes back from the job itself
    my $error_message = $failed_chunks->first->job_stream->stderr ||
                        'There was a problem searching one or more of your sequences';
    chomp $error_message;
    
    # update the row describing the parent search, flagging it as failed
    $job->update( { status => 'FAIL',
                    closed => \'NOW()' } );
    $job->job_stream->update( { stderr => $error_message } );

    # mail error message
    $self->_mail_error_message( $job, $error_message );
  }
  else {
    # no errors; mail the results

    my $results = $self->_assemble_chunk_results( $all_chunks_rs, $job );

    # update the row describing the parent search, flagging it as done
    $job->update( { status => 'DONE',
                    closed => \'NOW()' } );
    $job->job_stream->update( { stdout => $results } );

    # mail results
    $self->_mail_batch_results( $job, $results )
      if $results;
  }

  # since all chunks are done, delete the rows from the DB
  $all_chunks_rs->delete;
}

#-------------------------------------------------------------------------------
# checks the status of all chunks that are flagged as running in the tracking
# table. Any that are complete are flagged as such and the chunk output is
# retrieved from the ES service and stored in the tracking table.
#
# This method submits requests to the ES service in parallel. The number of
# forked processes is controlled by the queue config.

sub _handle_running_chunks {
  my ( $self, $running_chunks_rs ) = @_;

  my $pfm;
  $pfm = Parallel::ForkManager->new( $self->_queue_spec->{num_submission_forks} )
    unless $ENV{SINGLE_THREADED_DEQUEUER};

  foreach my $chunk ( $running_chunks_rs->all ) {
    unless ( $ENV{SINGLE_THREADED_DEQUEUER} ) {
      $pfm->start and next;
    }

    # ignore the child processes when running under the perl debugger
    $DB::inhibit_exit = 0;

    my $ebi_id = $chunk->ebi_id;
    my $chunk_status = $self->_rc->status( $ebi_id );
    $self->_log->debug( "chunk $ebi_id has status $chunk_status" );

    # update status for completed chunks
    if ( $chunk_status eq 'FINISHED' ) {
      # see if the queue config specifies a renderer
      my $chunk_results = defined $self->_queue_spec->{renderer}
                        ? $self->_rc->result( $ebi_id, $self->_queue_spec->{renderer} )
                        : $self->_rc->result( $ebi_id );

      $chunk->update( { status => 'DONE',
                        closed => \'NOW()' } );
      $chunk->job_stream->update( { stdout => $chunk_results } );

      $self->_log->debug( "marked chunk $ebi_id as DONE" );
    }
    elsif ( $chunk_status eq 'FAILURE' ) {
      
      my $error_msg = $self->_rc->error_msg( $ebi_id );

      $chunk->update( { status => 'FAIL',
                        closed => \'NOW()' } );
      $chunk->job_stream->update( { stderr => $error_msg } );

      $self->_log->debug( "marked chunk $ebi_id as FAIL: $error_msg" );
    }

    $pfm->finish unless $ENV{SINGLE_THREADED_DEQUEUER};
  }
  $pfm->wait_all_children unless $ENV{SINGLE_THREADED_DEQUEUER};
}

#-------------------------------------------------------------------------------
# retrieves the output for each batch chunk from the tracking table and
# concatenates them to generate the final output. The method of concatenating
# the individual logs is dependent on the queue, so there's a separate bit of
# code to handle different queues, currently "batch" (Pfam batch) and
# "rfam_batch".

sub _assemble_chunk_results {
  my ( $self, $all_chunks_rs, $job ) = @_;

  my $job_id = $job->job_id;
  my $results;
  my @chunks = $all_chunks_rs->all;

  if ( $self->_queue_spec->{job_type} eq 'batch' ) {

    my $header = '';
    if ( $self->_queue_spec->{header} ) {
      $header = "\n" . $self->_queue_spec->{header} . "\n";
      $header =~ s/\$JOBID/$job_id/;
      my $input = join "\n", ( split /\n/, $job->job_stream->stdin )[0..9];
      $header =~ s/\$INPUT/$input\n.../;
    }

    # get the header text from the first result chunk. This should be possible
    # with a single regex and a lot less faffing about, but hey
    my @comment_rows = grep( /^#/, split( /\n/, $chunks[0]->job_stream->stdout || '' ) );
    $header .= join( "\n", @comment_rows ) . "\n\n";

    # collect the actual results from all chunks
    my @matches;
    foreach my $chunk ( @chunks ) {
      my $ebi_id = $chunk->ebi_id;
      $self->_log->debug( "getting results for chunk $ebi_id" );

      my $chunk_results = $chunk->job_stream->stdout || '';
      # TODO check for failed job

      $self->_log->debug( 'results for chunk with EBI job ID ' . $ebi_id
                          . ":\n$chunk_results" );

      push @matches, grep !/^(#.*|\s*)?$/, split( /\n/, $chunk_results );
    }

    $results = $header . join "\n", sort @matches;
  }
  elsif ( $self->_queue_spec->{job_type} eq 'rfam_batch' ) {

    # keep the first two rows of the first result log as the header for the
    # combined output, and then the remainder as the footer
    my @rows   = grep( /^#/, split( /\n/, $chunks[0]->job_stream->stdout ) );
    $results   = join "\n", splice( @rows, 0, 2 );
    $results  .= "\n";

    my $footer = "\n";
    $footer   .= join "\n", @rows;

    foreach my $chunk ( @chunks ) {
      my $stdout = $chunk->job_stream->stdout;
      next unless defined $stdout;
      my $result = join( "\n", grep( !/^#/, split( /\n/, $stdout ) ) );
      next unless $result;
      $results .= "$result\n";
    }
    $results .= $footer;

    $results =~ s/^\s*\n//mg; # strip blank lines
  }

  return $results;
}

#-------------------------------------------------------------------------------
# sends an email to the job submitter with the results of their search

sub _mail_batch_results {
  my ( $self, $job, $results ) = @_;

  my $job_id = $job->job_id;
  my $email  = $job->email;

  # there might be multiple admin email addresses
  my $admin_emails = ref $self->_queue_spec->{admin_email}
                   ?   $self->_queue_spec->{admin_email}
                   : [ $self->_queue_spec->{admin_email} ];

  $self->_log->info( "mailing results of job $job_id to $email" );

  my $msg = Mail::Send->new;
  $msg->to( $email );
  $msg->bcc( @{ $admin_emails } );
  $msg->subject( "Results for batch search job $job_id" );

  my $body_fh = $msg->open;
  print $body_fh $results;
  $body_fh->close
    or $self->_log->warn( "failed to send email to $email with results of job $job_id" );
}

#-------------------------------------------------------------------------------
# sends an email to the job submitter with an error message

sub _mail_error_message {
  my ( $self, $job, $error_message ) = @_;

  my $job_id = $job->job_id;
  my $email  = $job->email;

  # there might be multiple admin email addresses
  my $admin_emails = ref $self->_queue_spec->{admin_email}
                   ?   $self->_queue_spec->{admin_email}
                   : [ $self->_queue_spec->{admin_email} ];

  $self->_log->info( "mailing error message from job $job_id to '$email'" );

  my $msg = Mail::Send->new;
  $msg->to( $email );
  $msg->bcc( @{ $admin_emails } );
  $msg->subject( "There was a problem with your batch search job $job_id" );

  my $body_fh = $msg->open;
  print $body_fh <<EOF_mail;

There was a problem when submitting your batch search with the job ID
$job_id. The error message that we
received from the queueing system was:

    $error_message

Please check the input file that you uploaded and make sure that it meets
the format requirements in the batch search documentation.

If you think that the problem is with the search system rather than your file,
please let us know. You can submit a help-desk ticket using the email at the
bottom of the search page.

EOF_mail

  $body_fh->close
    or $self->_log->warn( "failed to send email to $email with results of job $job_id" );
}

#-------------------------------------------------------------------------------
# coordinates the submission of an interactive search job to the ES service

sub _handle_interactive_job {
  my ( $self, $job ) = @_;

  my $ebi_id = $job->ebi_id;
  $self->_log->debug( "found running job with internal EBI job ID $ebi_id" );

  # retrieve the status of the job from the EBI queuing system
  my $status = $self->_rc->status( $ebi_id );

  my ( $results, $new_status );

  if ( $status eq 'RUNNING' ) {
    $self->_log->debug( "interactive job $ebi_id is still RUNNING" );
  }
  elsif ( $status eq 'NOT_FOUND' ) {
    $self->_log->debug( 'job was not found on EBI system; flagging as DEL in job_history' );
    $job->update( { status => 'DEL',
                    closed => \'NOW()' } );
  }
  elsif ( $status eq 'FINISHED' ) {
    $self->_log->info( "interactive job $ebi_id is FINISHED; retrieving results" );

    # see if the queue config specifies a renderer
    my $results = defined $self->_queue_spec->{renderer}
                ? $self->_rc->result( $ebi_id, $self->_queue_spec->{renderer} )
                : $self->_rc->result( $ebi_id );

    # the raw results in the table may or may not be JSON...
    if ( defined $self->_queue_spec->{output_format} and
         $self->_queue_spec->{output_format} eq 'json' ) {
      $self->_log->debug( "handling JSON output for job $ebi_id" );

      my $json_results = from_json $results;

      unless ( defined $json_results ) {
        $self->_log->warn( "there was a problem deserialising the JSON results for EBI job $ebi_id" );
        $job->update( { status => 'FAIL',
                        closed => \'NOW()' } );
        return;
      }

      $results = nfreeze $json_results;
    }

    unless ( $results ) {
      $self->_log->warn( "there was a problem retrieving (or freezing) the results for EBI job $ebi_id" );
      $job->update( { status => 'FAIL',
                      closed => \'NOW()' } );
      return;
    }

    $self->_log->debug( "successfully retrieved results for EBI job $ebi_id" );

    # temporarily turn off query debugging, because job_stream can contain
    # binary strings that will screw up the terminal
    $self->_schema->storage->debug(0);

    $job->job_stream->update( { stdout => $results } );

    # re-enable debugging if necessary
    $self->_schema->storage->debug(1) if $ENV{DBIC_TRACE};

    $job->update( { status => 'DONE',
                    closed => \'NOW()' } );

    $self->_log->debug( "successfully retrieved results for EBI job $ebi_id" );
  }
  else {
    $self->_log->warn( "something went wrong with job $ebi_id" );

    $job->update( { status => 'FAIL',
                    closed => \'NOW()' } );
  }

  return $results;
}

#-------------------------------------------------------------------------------

no Moose;
__PACKAGE__->meta->make_immutable;

