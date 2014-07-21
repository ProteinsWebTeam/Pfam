#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Pod::Usage;

use Bio::Rfam::Config;
use Bio::Rfam::View;
use RfamJobs;

# this script should generally be submitted to the farm, using a command
# something like this:
# mkdir -p /tmp/rfamprod/5EF780FE-2D0E-11E3-8C47-A34D104C94EC/RF00504 && cd /tmp/rfamprod/5EF780FE-2D0E-11E3-8C47-A34D104C94EC/RF00504 && rfam_family_view.pl -id 5EF780FE-2D0E-11E3-8C47-A34D104C94EC -family RF00504 && rm -rf /tmp/rfamprod/5EF780FE-2D0E-11E3-8C47-A34D104C94EC/RF00504

#-------------------------------------------------------------------------------
# handle options

my ( $job_uuid, $rfam_acc, $help );
GetOptions( 'id=s'     => \$job_uuid,
            'family=s' => \$rfam_acc,
            'help|?'   => \$help )
  or die "ERROR: there was a problem with your command line arguments\n";

pod2usage( -exitval => 1, -verbose => 2 ) if $help;

die "ERROR: you must specify both a job UUID (-id) and a family accession (-f)\n"
  unless ( defined $job_uuid and defined $rfam_acc );

die "not a valid UUID ($job_uuid)\n"
  unless ( $job_uuid =~ m/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/i );

die "not a valid Rfam family accession ($rfam_acc)\n"
  unless ( $rfam_acc =~ m/^RF\d{5}$/i );

#-------------------------------------------------------------------------------
# setup 

my $config  = Bio::Rfam::Config->new;
my $plugins = $config->viewPluginSets('family');

my $view = Bio::Rfam::View->new ( {
  plugins   => $plugins,
  config    => $config,
  family    => $rfam_acc,
  job_uuid  => $job_uuid,
  seqdb     => 'rfamseq'
} );

# we need to get the row in the job_history table for this job, so that we can
# update its status before and after running the plugins. It's also a good
# validation for the UUID, so we'll check it before trying to run anything.

my $db_params = $config->config->{Model}->{RfamJobs};
my $dsn = 'dbi:' . $db_params->{driver} . ':'
          . $db_params->{database} . ':'
          . $db_params->{host} . ':'
          . $db_params->{port};

my $rfam_jobs = RfamJobs->connect(
  $dsn,
  $db_params->{user},
  $db_params->{password}
);

die "couldn't connect to the 'rfam_jobs' tracking database\n"
  unless $rfam_jobs;

my $job = $rfam_jobs->resultset('JobHistory')
                    ->search( { job_id => $job_uuid } )
                    ->single;

die "couldn't find a row for this job (job ID $job_uuid) in the tracking table"
  unless $job;
                         
#-------------------------------------------------------------------------------
# call the plugins

$job->run;

foreach my $plugin ( @{ $view->plugin_list } ) {
  $plugin->process;
}

$job->done;

# (the $job row object has methods "run", "fail" and "done, which set the
# status for that row and update the "closed" time stamp too. See
# RfamJobs/Result/JobHistory.pm.)

exit;

#-------------------------------------------------------------------------------
#- pod -------------------------------------------------------------------------
#-------------------------------------------------------------------------------

__END__

=head1 NAME

rfam_family_view.pl - run the "view process" for an Rfam family

=head1 SYNOPSIS

  rfam_family_view.pl -id 5EF780FE-2D0E-11E3-8C47-A34D104C94EC -family RF00504

=head1 DESCRIPTION

This is a script to run the Rfam view process for a given family. It's intended
to be run by the job dequeuer, which polls the rfam_jobs.job_history table for
pending view process jobs, and runs this script on the farm via "bsub".

=head1 OPTIONS

=over 8

=item B<-id>

the UUID for the given job, e.g. C<5EF780FE-2D0E-11E3-8C47-A34D104C94EC>

=item B<-family>

the Rfam family accession, e.g. C<RF005004>

=item B<--help | -h | -?>

print this help text

=back

The UUID should normally come from the tracking database, having been generated
by the commit script that commissioned the view process job originally.

=head1 CONFIGURATION

The script obtains its configuration from the C<Bio::Rfam::Config> module, 
which reads the location of the Apache-style configuration file from the 
C<RFAM_CONFIG> environment variable.

The script reads the C<view_sets> section in the configuration to figure out
which view plugins should be run. It also gets database connection parameters
for the C<rfam_jobs> database from C<Model/RfamJobs>.

=head1 SEE ALSO

C<Rfam/Scripts/view/job_dequeuer.pl>
C<Bio::Rfam::View::Dequeuer>

=cut

