
package Bio::Rfam::View::Dequeuer;

=head1 NAME

Bio::Rfam::View::Dequeuer - poll for and submit Rfam View process jobs

=head1 SYNOPSIS

 my $job_dequeuer = Bio::Rfam::View::Dequeuer->new( 'family' );
 $job_dequeuer->daemonise;
 $job_dequeuer->start_polling;

=head1 DESCRIPTION

This module implements a simple job queue manager for Rfam "view process" jobs.
It connects to the B<RfamJobs> database using connection details obtained from
L<Bio::Rfam::Config> and polls for pending jobs of the specified type. If a
pending job is found, a job command is built and submitted to the farm using
LSF.

This module makes no attempt to track the progress of the LSF job. Once it has
been submitted, the polling loop restarts and the details of the submitted job
are not maintained. The job itself is responsible for updating the tracking
information where necessary.

=cut

use Moose;
use Moose::Util::TypeConstraints;
use namespace::autoclean;

use Log::Log4perl qw( get_logger );
use POSIX qw( setsid );
use File::Basename;
use Data::Dump qw( dump );
use LSF::Job;

use RfamJobs;

#-------------------------------------------------------------------------------
#- configure logging -----------------------------------------------------------
#-------------------------------------------------------------------------------

my $logger_conf = q(
  log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout                   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
  log4perl.logger.Bio.Rfam.View.Dequeuer            = ERROR, Screen
);

has '_log' => (
  is      => 'ro',
  isa     => 'Log::Log4perl::Logger',
  lazy    => 1,
  default => sub {
    my $self = shift;
    Log::Log4perl->init_once( \$logger_conf );
    return Log::Log4perl->get_logger( ref $self );
  }
);

#-------------------------------------------------------------------------------
#- private attributes ----------------------------------------------------------
#-------------------------------------------------------------------------------
 
has '_schema' => (
  is      => 'ro',
  lazy    => 1,
  default => sub {
    my $self = shift;
    my $db_params = $self->_config->{Model}->{RfamJobs};
    RfamJobs->connect(
      'dbi:' . $db_params->{driver} . ':'
             . $db_params->{database} . ':'
             . $db_params->{host} . ':'
             . $db_params->{port},
      $db_params->{user},
      $db_params->{password}
    );
  },
  trigger => sub { $_->_jobs_table( $_->_schema->resultset('RfamJobs') ) },
);

has '_jobs_table' => (
  is  => 'rw',
  isa => 'DBIx::Class::ResultSet',
);

has '_config' => (
  is  => 'rw',
  isa => 'HashRef',
);

#-------------------------------------------------------------------------------
#- public attributes -----------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 PUBLIC ATTRIBUTES

=head2 config

An instance of L<Bio::Rfam::Config>, the object containing the configuration
for the dequeuer. Required.

=cut

has 'config' => (
  is       => 'rw',
  isa      => 'Bio::Rfam::Config',
  required => 1,
  trigger  => sub { 
    my ( $self, $config_object ) = @_;
    $self->_config( $config_object->config );
  },
  # update the _config attribute with the raw configuration hash
);

=head2 job_type

Sets or gets the type of job to handle. Must be either 'family' or 'clan'.
Required.

=cut

has 'job_type' => (
  is       => 'rw',
  isa      => enum( [ qw( family clan ) ] ),
  required => 1,
);

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 PUBLIC METHODS

=head2 start_polling

Starts the object polling for jobs of the specified type. The polling loop
run indefinitely. The polling interval should be specified in the configuration,
as in:

 <job_dequeuer>
   polling_interval 2
 </job_dequeuer>

=cut

sub start_polling {
  my ( $self ) = shift;

  my $delay = $self->_config->{job_dequeuer}->{polling_interval} || 2;
  $self->_log->info( "starting submission loop for '" . $self->job_type .
                     "' jobs, with $delay second polling interval" );

  while (1) {

    # the delay goes at the beginning of the loop, so that we can use "next" if
    # there's a problem submitting the previous job
    $self->_log->debug( "sleeping for $delay seconds " );
    sleep $delay;

    # look for pending jobs in the tracking table
    my $jobs = $self->_jobs_table
                    ->get_pending_jobs( $self->job_type );

    if ( $self->_log->is_debug ) {
      # (don't call $jobs->count unless we're actually debugging, since it
      # runs an extra "SELECT COUNT(*)" query to get the total)
      $self->_log->debug( 'found ' . $jobs->count . ' pending jobs' );
    }

    # get the row for the next pending job from the tracking table
    next unless my $job = $jobs->next;

    # build a hash with the parameters describing the LSF job
    my $job_spec = $self->_build_job_spec( $job );

    # and actually submit that job to LSF
    my $lsf_id = $self->_submit_lsf_job( $job_spec );

    # make sure it submitted successfully
    unless ( $lsf_id and $lsf_id =~ m/^\d+$/ ) {
      $self->_log->error( 'there was a problem submitting the view process for '
                          . $job->job_type . ' '
                          . $job->entity_acc );
      $job->fail;
      next;
    }

    $self->_log->debug( "job submitted with LSF ID $lsf_id" );

    # update the job row with the LSF ID for the farm job
    $job->lsf_id( $lsf_id ); 

    # and flag the job as running. Also sets the start time
    $job->run;
  }

}

#-------------------------------------------------------------------------------

=head2 daemonise

Runs the dequeuer as a background process. The default behaviour is to run it
as an interactive, foreground process.

=cut

sub daemonise {
  my $self = shift;

  my $lock_dir = $self->_config->{job_dequeuer}->{lock_dir};
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
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 PRIVATE METHODS

=head2 _build_job_spec

Collects together the details of the job from the configuration and the
tracking database. Decides on the amount of memory that should be requested
from LSF when submitting to the farm, based on the B<entity_size> column in
the tracking table.

=cut

sub _build_job_spec {
  my ( $self, $job ) = @_;

  $self->_log->debug( 'building farm command for job ', $job->job_id );

  # this is the basic job specification, which we'll use to build the LSF
  # submission command later. We need to add to it the resource requirements,
  # which is done below
  my $job_spec = {
    tmp_dir    => $self->_config->{job_dequeuer}->{tmp_dir},
    lsf_queue  => $self->_config->{job_dequeuer}->{lsf_queue},
    lsf_user   => $self->_config->{job_dequeuer}->{lsf_user},
    job_id     => $job->job_id,
    entity_acc => $job->entity_acc,
    memory     => 1_000, # Mbytes; defaults to 4Gb on the farm
  };
  
  my $entity_size = $job->entity_size;

  # TODO these limits are entirely made up and should be adjusted before
  # TODO anything goes into production !

  # adjust the memory requirement based on the size of the family/clan that is
  # being run
  if ( $entity_size > 10000 ) {
    $self->_log->debug( 'large job; requesting maximum memory requirement' );
    $job_spec->{memory} = 10_000;
  }
  elsif ( $entity_size > 5000 ) {
    $self->_log->debug( 'medium large job; requesting medium memory requirement' );
    $job_spec->{memory} = 5_000;
  }
  elsif ( $entity_size < 1000 ) {
    $self->_log->debug( 'small job; requesting reduced memory requirement' );
    $job_spec->{memory} = 500;
  }

  $self->_log->debug( 'job spec: ', dump( $job_spec ) );
  
  return $job_spec;
}

#-------------------------------------------------------------------------------

=head2 _submit_lsf_job

Given the job spec generated by L<_build_job_spec>, this method builds the 
shell command needed to run the view process and submits it to LSF. Return 
value is the LSF job ID.

=cut

sub _submit_lsf_job {
  my ( $self, $job_spec ) = @_;

  $self->_log->debug( "building LSF job command" );

  my $working_dir =   $job_spec->{tmp_dir} . '/' 
                    . $job_spec->{lsf_user} . '/' 
                    . $job_spec->{job_id};
  $self->_log->debug( "LSF working directory: $working_dir" );

  my $view_script = $self->_config->{job_dequeuer}->{view_script}->{ $self->job_type };
  $self->_log->debug( "view script: |$view_script|" );

  my $view_command =   $view_script
                     . ' -id '                      . $job_spec->{job_id} 
                     . ' -' . $self->job_type . ' ' . $job_spec->{entity_acc};
  $self->_log->debug( "view command: |$view_command|" );

  my $lsf_command =   "mkdir -p $working_dir"
                    . " && cd $working_dir"
                    . " && $view_command"
                    . " && rm -rf $working_dir";
  $self->_log->debug( "LSF command: |$lsf_command|" );

  my $log_file =   $job_spec->{tmp_dir} . '/' 
                 . $job_spec->{lsf_user} . '/' 
                 . $job_spec->{job_id} . '.log';
  $self->_log->debug( "writing log to: |$log_file|" );

  my $memory_resource = 'rusage[mem=' . $job_spec->{memory} . ']';
  $self->_log->debug( "memory resource string: |$memory_resource|" );

  my $tmp_resource = 'rusage[tmp=' . $job_spec->{tmp_space} . ']';
  $self->_log->debug( "tmp space resource string: |$tmp_resource|" );

  $self->_log->debug( "submitting LSF job" );

  my $lsf_id = LSF::Job->submit(
    -o => $log_file,
    -q => $job_spec->{lsf_queue},
    -R => $memory_resource,
    -R => $tmp_resource,
    -M => $job_spec->{memory},
    $lsf_command
  );
  
  return $lsf_id;
}

#-------------------------------------------------------------------------------

__PACKAGE__->meta->make_immutable;

1;

