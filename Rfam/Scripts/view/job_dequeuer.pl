#!/usr/bin/env perl

use strict;
use warnings;

use Log::Log4perl qw( get_logger :levels );
use Getopt::Long;
use Pod::Usage;

use Bio::Rfam::Config;
use Bio::Rfam::View::Dequeuer;

#-------------------------------------------------------------------------------
# configure logging

my $logger_conf = q(
  log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout                   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
  log4perl.rootLogger                               = INFO, Screen
  log4perl.logger.Bio.Rfam.View.Dequeuer            = ERROR
);

Log::Log4perl->init( \$logger_conf );

my $log = get_logger;

#-------------------------------------------------------------------------------
# process command line options

my ( $config_file, $job_type, $view_set, $daemonise, $help );
GetOptions( 'config=s'   => \$config_file,
            'job_type=s' => \$job_type,
            'viewset=s'  => \$view_set,
            'daemonise'  => \$daemonise,
            'verbose'    => sub { $log->level( $DEBUG ) },
            'help|?'     => \$help )
  or pod2usage( -exitval => 1, -verbose => 1, -output => \*STDERR );

pod2usage( -verbose => 2, -output => \*STDERR ) if $help;

$log->logdie( 'ERROR: you must supply a job type' )
 unless defined $job_type;

$log->logdie( 'ERROR: you must supply the name of a view plugin set to run' )
 unless defined $view_set;

$log->logdie( 'ERROR: you must supply the name of an Rfam configuration file' )
 unless ( defined $config_file and -f $config_file );

#-------------------------------------------------------------------------------
# main

my $config = Bio::Rfam::Config->new( $config_file );;

my $job_dequeuer = Bio::Rfam::View::Dequeuer->new( job_type => $job_type,
                                                   view_set => $view_set,
                                                   config   => $config );

if ( $daemonise ) {
  $log->info( 'running dequeuer in daemon mode' );
  $job_dequeuer->daemonise;
}

$job_dequeuer->start_polling;

exit;

#-------------------------------------------------------------------------------
#- pod -------------------------------------------------------------------------
#-------------------------------------------------------------------------------

__END__

=head1 NAME

job_dequeuer.pl - poll for and submit Rfam view process jobs

=head1 SYNOPSIS

  job_dequeuer.pl [-d] [-verbose] -c rfam.conf -j family -viewset family

=head1 DESCRIPTION

This script polls the "RfamJobs" database for new view process jobs to run. If
it finds a pending job, it builds a view process command and submits it to the
farm using LSF. 

Once a job is submitted, the dequeuer goes back to polling the database; it
does not attempt to keep track of the progress or status of jobs once they have
been submitted.

=head1 OPTIONS

=over 8

=item B<-config | -c> <viewset name> [REQUIRED]

the path to the configuration file

=item B<-jobtype | -j> <job type> [REQUIRED]

the type of view process to run. Must be either B<family> or B<clan>. 

=item B<-viewset> <viewset name> [REQUIRED]

the name of a set of view plugins to run

=item B<-daemonise | -d>

run the dequeuer as a daemon [default: run interactively]

=item B<-verbose>

show debug messages

=item B<-help | -h | -?>

print this help text

=back

If the script is run in daemon mode, a PID file is written to
F</tmp/job_dequeuer.pl.pid>.

=head1 CONFIGURATION

The script obtains its configuration from the C<Bio::Rfam::Config> module, 
which reads the location of the Apache-style configuration file from the 
specified configuration file.

=head1 SEE ALSO

C<Bio::Rfam::View::Dequeuer>
C<Rfam/Scripts/view/rfam_family_view.pl>

=cut

