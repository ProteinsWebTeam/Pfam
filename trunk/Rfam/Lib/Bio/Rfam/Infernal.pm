package Bio::Rfam::Infernal;

#TODO: add pod documentation to all these functions

# Wrappers for Infernal executables called by rfsearch and rfmake.

use strict;
use warnings;
use Sys::Hostname;
use File::stat;

use Cwd;
use Carp;
use Data::Dumper;
use Data::Printer;
use Mail::Mailer;
use File::Copy;
use vars qw( @ISA
             @EXPORT
);

@ISA    = qw( Exporter );

our $CMCALIBRATE_NCPU = 81;

=head1 SUBROUTINES/METHODS
=cut
    
=head2 cmbuild_wrapper

    Title    : cmbuild_wrapper
    Incept   : EPN, Sun Feb  3 17:17:40 2013
    Usage    : Bio::Rfam::Infernal::cmbuild_wrapper($cmbuildPath, $options, $cmPath, $alnPath)
    Function : Runs Infernals cmbuild.
    Args     : <cmbuildPath>: path to cmbuild
             : <opts>:        option string for cmbuild (-F will be appended to this)
             : <cmPath>:      path to output CM (often 'CM')
             : <seedPath>:    path to input alignment (often 'SEED')
    Returns  : void
    Dies     : if cmbuild command fails

=cut

sub cmbuild_wrapper {
    my ($config, $options, $cmPath, $seedPath) = @_;
    
    if($options !~ m/\-F/) { $options = "-F " . $options; }
    $options =~ s/\s+$//; # remove trailing whitespace
    $options =~ s/^\s+//; # remove leading  whitespace
    my $cmbuildPath = $config->infernalPath . "/cmbuild";
    my $cmd = $cmbuildPath . ' ' . $options . ' ' . $cmPath . ' ' . $seedPath . '> /dev/null';
    system($cmd);
    if($? != 0) { die "$cmd failed"; }
    if (! -e $cmPath) {  die "ERROR CM does not exist after apparently successful cmbuild [cmd: $cmd]"; }
    return;
}

=head2 cmcalibrate_wrapper

  Title    : cmcalibrate_wrapper
  Incept   : EPN, Sun Feb  3 19:58:11 2013
  Usage    : Bio::Rfam::Infernal::cmcalibrate_wrapper($cmcalibratePath, $options, $cmPath)
  Function : Runs Infernals cmcalibrate.
  Args     : <cmcalibratedPath>: path to cmcalibrate
           : <opts>:             option string for cmcalibrate
           : <cmPath>:           path to CM (often 'CM')
           : <ncpu>:             number of CPUs to use, usually undefined
  Returns  : void
  Dies     : if cmcalibrate command fails

=cut

sub cmcalibrate_wrapper {
  my ($cmcalibratePath, $options, $cmPath, $ncpu) = @_;
  
  # ensure $cmPath exists
  if (! -e $cmPath) {
    croak "CM file $cmPath does not exist";
  }
  
  # set number of CPUs to use, currently hard-coded
  if (! defined $ncpu) {
    $ncpu = $CMCALIBRATE_NCPU;
  }
  
  # predict how long job will take
  my $cmd = "cmcalibratePath --forecast --nforecast $ncpu $cmPath";
  my $predicted_seconds;
  system($cmd);
  if ($? != 0) {
    croak "$cmd failed";
  }
  my $forecast_out = "cmcalibrate-forecast.out";
  open(IN, $forecast_out) || croak "unable to open $forecast_out";
  while (my $line = <IN>) { 
    if ($line !~ m/^\#/) { 
      $line =~ s/^\S+\s+//;
      my ($h, $m, $s) = split(":", $line);
      $predicted_seconds = 3600. * $h + 60. * $m + $s;
      last;
    }
  }
  if (! defined $predicted_seconds) {
    croak "cmcalibrate prediction failed";
  }
  unlink $forecast_out;
  
  # submit MPI job
  my $mpi_output = "cmcalibrate-mpi.out";
  my $job_name   = "cmcal" . $$ . hostname;
  system("module load openmpi-x86_64");
  system("bsub -J $job_name -q mpi -I -n $ncpu -a openmpi mpirun.lsf -np $ncpu -mca btl tcp,self $cmcalibratePath --mpi $cmPath > $mpi_output");
  
  Bio::Rfam::Utils::wait_for_farm($job_name, 'cmcalibrate', $ncpu, (2 * $predicted_seconds) + 180); #wait an extra few mins then the job will be killed, assuming MPI+Farm badness.
  return;
}


################################

# cmsearch_wrapper: takes a CM and sequence file and submits a cmsearch job to the cluster

sub cmsearch_wrapper {
  my $config  = shift; # config, used for cmalign path, location
  my $cmfile  = shift; # CM file
  my $seqfile = shift; # sequence file with seqs to align
  my $tblO    = shift; # tblout output file
  my $cmsO    = shift; # cmsearch output file 
  my $options = shift; # string of cmalign options
  my $cpus    = shift; # number of CPUs to run cmsearch with (SHOULD ALREADY BE PART OF $options)
  my $acc     = shift; # Rfam accession, for constructing job id
  my $idx     = shift; # idx, for constructing job id

  if($options !~ m/\-\-cpu $cpus/) { die "ERROR cmsearch_wrapper option string ($options) does not contain --cpu $cpus"; }

  my $cmsearchPath = $config->infernalPath . "/cmsearch";

  my $location = $config->location;
  my $command = "";
  my $jobname = "$acc.cms.$idx";
  my $errfile = "$jobname.err";
  my $search_command = "$cmsearchPath $options --tblout $tblO $cmfile $seqfile > $cmsO";
  if($location eq "EBI") { 
    # pick smallest 500 Mb multiple that satisfies required memory estimate
    my $requiredMb = $cpus * 3 * 1000.0; # ~3 Gb per thread
    my $ebi_cpus   = ($cpus == 0) ? 1 : $cpus;
    $command = "bsub -q research-rh6 -n $ebi_cpus -J $jobname -o /dev/null -e $errfile -M $requiredMb -R \"rusage[mem=$requiredMb]\" \"$search_command\"";
  }
  elsif($location eq "JFRC") { 
    $command = "qsub -N $acc.cms.$idx -o /dev/null -e $acc.cms.$idx.err -b y -cwd -V -l excl=true \"$search_command\"";
  }
  else { 
    die "ERROR unknown location $location in cmsearch_wrapper"; 
  }
  # submit job
  printf STDERR "about to run command $command\n";
  system("$command");
  if ($?) { die "FAILED: $command"; }
  print STDERR "search job $idx is now running";

  return;
}

################################

#cmalign_wrapper: takes a CM and sequence file and runs cmalign, either locally or on the farm using MPI

#Systems MPI help documentation:
#http://scratchy.internal.sanger.ac.uk/wiki/index.php/How_to_run_MPI_jobs_on_the_farm

# For MPI to work, you need to make sure Infernal has been compiled correctly, with --enable-mpi flag to configure.
# See Infernal user's guide. It should be compiled correctly, or else rfsearch wouldn't work (MPI cmcalibrate is used there).
# Also, ssh keys need to be correct. Remove all ^"bc-*" entries from your ~/.ssh/known_hosts file.
# Optionally add "StrictHostKeyChecking no" to your ~/.ssh/config

sub cmalign_wrapper {
  my $config      = shift; # config, used for cmalign path, location
  my $cmfile      = shift; # CM file
  my $seqfile     = shift; # sequence file with seqs to align
  my $alnfile     = shift; # alignment output file 
  my $outfile     = shift; # cmalign output file 
  my $options        = shift; # string of cmalign options
  my $nseq        = shift; # number of sequences in $seqfile
  my $tot_len     = shift; # total number of nucleotides in $seqfile
  my $always_farm = shift; # 1 to always use farm, 0 to only use farm if > 4 CPUs needed
  my $never_farm  = shift; # 0 to never  use farm, 1 to only use farm if > 4 CPUs needed
  my $dirty       = shift; # 1 to leave files, else remove them 

  # TODO remove unlinkA if ebi farm works without copying files
  my @unlinkA = ();

  my $cmalignPath = $config->infernalPath . "/cmalign";

  ####################################################################
  # Predict running time and memory requirement of cmalign
  # and use them to determine number of CPUs for MPI cmcalibrate call.
  ####################################################################
  # Get a rough estimate of running time on 1 CPU based on $tot_len (passed in)
  my $sec_per_Kb = 4;
  my $estimatedCpuSeconds = ($tot_len / 1000.) * $sec_per_Kb;

  # Determine number of CPUs to use: target running time is 1 minute.
  my $targetSeconds = 60;
  my $cpus = int($estimatedCpuSeconds / $targetSeconds) + 1;
  my $farm_max_ncpu  = 100; 
  my $farm_min_ncpu  = 4;
  my $local_max_ncpu = 4;
  my $local_min_ncpu = 2;

  my $use_farm;
  if    ($always_farm) { $use_farm = 1; }
  elsif ($never_farm)  { $use_farm = 0; }
  elsif ($cpus > 4)    { $use_farm = 1; }
  else                 { $use_farm = 0; }
	
  if ($use_farm) { 
    if ($cpus > $farm_max_ncpu)  { $cpus = $farm_max_ncpu; }
    if ($cpus < $farm_min_ncpu)  { $cpus = $farm_min_ncpu; }
  } else { 
    if ($cpus > $local_max_ncpu) { $cpus = $local_max_ncpu; }
    if ($cpus < $local_min_ncpu) { $cpus = $local_min_ncpu; }
  }

  my $estimatedWallSeconds = $estimatedCpuSeconds / $cpus;

  # Memory requirement is easy, cmalign caps DP matrix size at 1024 Mb
  my $requiredMb = $cpus * 1024.0;

  my $hrs = int($estimatedWallSeconds/3600);
  my $min = int(($estimatedWallSeconds - ($hrs * 3600)) / 60);
  my $sec = int($estimatedWallSeconds - ($hrs * 3600 + $min * 60));
    
  my $rounded_requiredMb = 500;
  # pick smallest 500 Mb multiple that satisfies required memory estimate
  while ($rounded_requiredMb < $requiredMb) { 
    $rounded_requiredMb += 500; 
  }
  $requiredMb = $rounded_requiredMb;
  my $requiredKb = $requiredMb * 1000;

  printf("Aligning %7d sequences %s on %d cpus; predicted time (h:m:s): %02d:%02d:%02d %s", 
         $nseq, 
         ($use_farm) ? "on farm" : "locally",
         $cpus, $hrs, $min, $sec+0.5, 
         ($use_farm) ? "\n" : " ... ");

  if (! $use_farm) { 
    # run locally, location-independent
    my $command = "$cmalignPath --cpu $cpus $options -o $alnfile $cmfile $seqfile > $outfile";
    system("$command");
    if ($?) {
      die "FAILED: $command";
    }
    printf("done.\n");
    open(OUT, $outfile);
  } 
  else { 
    # run on farm with MPI 
    my $command = "qsub -N J$$ -o /dev/null -e $$.err -b y -cwd -V -pe impi $cpus \"mpirun -np $cpus $cmalignPath --mpi $options -o $alnfile $cmfile $seqfile > $outfile\"\n";
    system("$command");
    if ($?) {
      die "FAILED: $command";
    }
    die "alignment job is now running on the farm.";
  }

  my $alignTime=0;
  while (<OUT>) {
    if (/\#\s+CPU\s+time\:\s+(\S+)u\s+(\S+)s/) {
      $alignTime=$1+$2;
      last;
    }
  }
  close(OUT);

  # clean up
  if (! $dirty) { 
    foreach my $file (@unlinkA) { 
      unlink $file;
    }
  }

  $alignTime *= $cpus;
  return $alignTime;
}

=head2 cm_evalue2bitsc()

  Title    : cm_evalue2bitsc()
  Incept   : EPN, Tue Jan 29 17:18:43 2013
  Usage    : cm_evalue2bitsc($cm, $evalue, $Z)
  Function : Returns bit score for a given E-value
  Args     : <cm>:     Bio::Rfam::Family::CM object
           : <evalue>: E-value we want bit score for
           : <Z>:      database size (both strands) for E-value->bitsc calculation
  Returns  : bit score for E-value for CM in db of $Z residues 
           : (where $Z includes BOTH strands of target seqs)
  
=cut
  
sub cm_evalue2bitsc { 
  my ($cm, $evalue, $Z) = @_;

  # this subroutine corresponds to infernal's cmstat.c line 295 ('else if(output_mode == OUTMODE_BITSCORES_E) {')
  my $bitsc;  # bit score to return;

  if(! $cm->{is_calibrated}) {  
    die "ERROR CM is not calibrated, and we're trying to convert an E-value to a bit score"; 
  }
  
  # TODO, only use HMM stat line if --nohmmonly was NOT used in SM
  if ($cm->{match_pair_node}) { # use CM stats
    # TODO, read SM in desc, and pick appropriate E-value line based on that
    my ($lambda, $mu_extrap, $mu_orig, $dbsize, $nhits, $tailp) = @{$cm->{cmHeader}->{ecmli}};
    my $cur_eff_dbsize = (($Z * 1000000.) / $dbsize) * $nhits;
    $bitsc = $mu_extrap + ((log($evalue / $cur_eff_dbsize)) / (-1 * $lambda));
  } else { 
    my ($tau, $lambda) = @{$cm->{cmHeader}->{efp7gf}};
    my $maxlen = $cm->{cmHeader}->{maxl};
    $bitsc = $tau + ((log($evalue / ($Z / $maxlen))) / (-1 * $lambda));
  }
  
  # printf("in cm_evalue2bitsc() converted E-value $evalue to bit $bitsc (Z: $Z)\n");
  return $bitsc;
}

=head2 stringize_infernal_cmdline_options()

  Title    : stringize_infernal_cmdline_options()
  Incept   : EPN, Thu Jan 31 16:52:35 2013
  Usage    : stringize_infernal_cmdline_options($sdashAR, $ddashAR)
  Function : Returns option string including single dash and double dash
           : options in $sdashAR and $ddashAR.
  Args     : Array ref to array of single dash options
           : Array ref to array of double dash options
  Returns  : string of options
  
=cut

sub stringize_infernal_cmdline_options {
  my ($sdashAR, $ddashAR) = @_;
  
  my $optstring = "";
  my $opt;
  
  foreach $opt (@{$sdashAR}) {
    $optstring .= "\-$opt ";
  }
  foreach $opt (@{$ddashAR}) {
    $optstring .= "\--$opt ";
  }
  
  $optstring =~ s/\s+$//;
  
  return $optstring;
}

######################################################################

1;



