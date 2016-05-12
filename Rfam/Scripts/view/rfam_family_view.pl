#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Pod::Usage;
use Data::UUID;
use Try::Tiny;

use Bio::Rfam::Config;
use Bio::Rfam::View;

use RfamLive;
# use RfamJobs;

# this script should generally be submitted to the farm, using a command
# something like this:
# mkdir -p /tmp/rfamprod/5EF780FE-2D0E-11E3-8C47-A34D104C94EC/RF00504 && cd /tmp/rfamprod/5EF780FE-2D0E-11E3-8C47-A34D104C94EC/RF00504 && rfam_family_view.pl -id 5EF780FE-2D0E-11E3-8C47-A34D104C94EC -family RF00504 && rm -rf /tmp/rfamprod/5EF780FE-2D0E-11E3-8C47-A34D104C94EC/RF00504

# This script has been modified for Rfam Release 12.1 to use _post_process table
# from rfamlive

#-------------------------------------------------------------------------------
# handle options

my ( $job_uuid, $rfam_acc, $no_db, $help );
GetOptions( 'id=s'     => \$job_uuid,
            'family=s' => \$rfam_acc,
            'nodb'     => \$no_db,
            'help|?'   => \$help )
  or die "ERROR: there was a problem with your command line arguments\n";

pod2usage( -exitval => 1, -verbose => 2 ) if $help;

if ( $no_db ) {
  $job_uuid ||= Data::UUID->new->create_str;
}
else {
  die "ERROR: you must specify a job UUID (-id) unless you specify no DB (-nodb)\n"
    unless ( defined $job_uuid and defined $rfam_acc );

  die "ERROR: not a valid UUID ($job_uuid)\n"
    unless ( $job_uuid =~ m/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/i );
}

die "ERROR: you must specify a family accession (-f)\n"
  unless defined $rfam_acc;

die "ERROR: not a valid Rfam family accession ($rfam_acc)\n"
  unless ( $rfam_acc =~ m/^RF\d{5}$/i );

# anything else on the command line will be taken to be the name of a plugin 
# set
die "ERROR: you must specify at least one plugin set\n"
  unless scalar @ARGV;

#-------------------------------------------------------------------------------
# setup 

my $config  = Bio::Rfam::Config->new;

# we need to get the row in the pos_process table for this job, so that we can
# update its status before and after running the plugins. It's also a good
# validation for the UUID, so we'll check it before trying to run anything.

my $job;
# unless ( $no_db ) {
my $rfam_jobs = $config->rfamlive;

die "couldn't connect to the 'rfam_live' tracking database\n"
  unless $rfam_jobs;

$job = $rfam_jobs->resultset('PostProcess')
                 ->search( { rfam_acc => $rfam_acc, #new line
                             uuid => $job_uuid
                             #status => 'PEND' 
                             } )
                 ->single; #make sure this is a single row...??
                 


die "couldn't find a row for this job (job ID $job_uuid) in the tracking table"
  unless $job;
# }
                         
#-------------------------------------------------------------------------------
# call the plugins

foreach my $plugin_set ( @ARGV ) {
  print "\nPlugin Sets: $plugin_set\n";
  try{
  my $plugins = $config->viewPluginSets( $plugin_set );
  print "test\n";
  print "Plugins:\n$plugins\n";
  print "Config: $config\n";
  print "Rfam_acc: $rfam_acc\n";
  print "Job uuid: $job_uuid\n";
 
  my $view = Bio::Rfam::View->new ( {
    plugins   => $plugins,
    config    => $config,
    family    => $rfam_acc,
    job_uuid  => $job_uuid,
    #job => $job,
    seqdb     => 'rfamseq'
  } );

  print "\nView object: $view\n";
  print "bp1\n";

  $job->run; # unless $no_db; #modified PostProcess.pm in RfamLive ResultSet 
  

  print "bp2\n";


  foreach my $plugin ( @{ $view->plugin_list } ) {
    print $plugin;
    #move try catch at this point
    $plugin->process; #call the subroutine to process the plugin
    print "bp3\n";
    # TODO could wrap the call to "process" in a try/catch and store the
    # message from the exception in the tracking DB, along with the name of the
    # plugin that failed
  }
  print "bp3.1\n";
  #marked as DONE if all plugins execute successfully
  $job->done; # unless $no_db; #this one will now work after modifying the 
  #PostProcess.pm in RfamLive ResultSet
  print "bp4\n";

  # (the $job row object has methods "run", "fail" and "done, which set the
  # status for that row and update the "closed" time stamp too. See
  # RfamJobs/Result/JobHistory.pm.) 
  # modified PostProcess.pm file to include those too.. currently modified under 
  # result set and may need to move those to result/PostProcess.pm
}
catch{
   print "bp5\n";
   #print "Failed on: $p_var\n";
   $job->failure; #this will set the job status to 'FAIL' upon failure
};
print "bp6\n";
}

exit;

#-------------------------------------------------------------------------------
#- pod -------------------------------------------------------------------------
#-------------------------------------------------------------------------------

__END__

=head1 NAME

rfam_family_view.pl - run the "view process" for an Rfam family

=head1 SYNOPSIS

  # runs the plugins in the "family" plugin set for Rfam family RF00504
  rfam_family_view.pl -id 5EF780FE-2D0E-11E3-8C47-A34D104C94EC -f RF00504 family

=head1 DESCRIPTION

This is a script to run the Rfam view process for a given family. It's intended
to be run by the job dequeuer, which polls the rfam_jobs.job_history table for
pending view process jobs, and runs this script on the farm via "bsub".

We have the concept of plugin sets, so that we can group together view plugins
which have common features. For example, there may be plugins that are based on
sequence data, versus others that use genome data. These can be collected
together in separate sets and then run separately or as part of the same view
process job. 

The configuration file should define the view sets in a section like this:

  <view_sets> 
    rfamseq Alignment
    rfamseq Species
    rfamseq SecondaryStructure
    rfamseq Tree
    genome Search
    genome Tree
  </view_sets> 

This block defined two plugin sets, I<rfamseq> and I<genome>. To run both sets
in the same view process run, add both to the command line arguments:

  rfam_family_view.pl -id 5EF780FE-2D0E-11E3-8C47-A34D104C94EC -f RF00504 rfamseq genome

=head1 OPTIONS

=over 8

=item B<-id>

the UUID for the given job, e.g. C<5EF780FE-2D0E-11E3-8C47-A34D104C94EC>

=item B<-family>

the Rfam family accession, e.g. C<RF005004>

=item B<-nodb>

flag to tell the script not to try to record job details like start and end
time in the tracking database. If you specify B<-no_db>, the UUID is optional.
If you don't supply a UUID for the job, the script will generate one, though it
will be used only for creating a temporary directory on the farm node, not for
tracking the job in the database.

=item B<--help | -h | -?>

print this help text

=back

The UUID should normally come from the tracking database, having been generated
by the commit script that commissioned the view process job originally.

=head1 ARGUMENTS

The script needs to know which sets of plugins to run. The available sets
should be configured in the C<view_sets> section of the configuration. You
can run more than one set of plugins by adding multiple view set names to
the command line.

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

