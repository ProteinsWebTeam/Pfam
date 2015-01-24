package Bio::Rfam::Infernal;

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
    Usage    : Bio::Rfam::Infernal::cmbuild_wrapper($config, $options, $cmPath, $seedPath, $outPath)
    Function : Runs Infernals cmbuild.
    Args     : $config:   Rfam config, with infernalPath
             : $options:  option string for cmbuild (-F will be appended to this, if it is not already there)
             : $cmPath:   path to output CM (often 'CM')
             : $seedPath: path to input alignment (often 'SEED')
             : $outPath:  path for output alignment (must be defined)
    Returns  : Number of seconds elapsed for all (from cmbuild output)
    Dies     : if cmbuild command fails

=cut

sub cmbuild_wrapper { 
  my ($config, $options, $cmPath, $seedPath, $outPath) = @_;

  if($options !~ m/\-F/) { $options = "-F " . $options; }
  $options =~ s/\s+/ /g; # change multi-spaces into single spaces
  $options =~ s/\s+$//;  # remove trailing whitespace
  $options =~ s/^\s+//;  # remove leading  whitespace
  my $cmbuildPath = $config->infernalPath . "cmbuild";
  my $cmd = $cmbuildPath . ' ' . $options . ' ' . $cmPath . ' ' . $seedPath . '>' . $outPath;

  Bio::Rfam::Utils::run_local_command($cmd);

  # get running time (and verify output, this call will die if no 'CPU time' lines exist in output)
  my $elapsed_secs;
  Bio::Rfam::Infernal::process_cpu_times($outPath, "# CPU time:", undef, undef, undef, \$elapsed_secs);

  if (! -e $cmPath) {  die "ERROR CM does not exist after apparently successful cmbuild [cmd: $cmd]"; }

  return $elapsed_secs;
}

=head2 cmcalibrate_wrapper

  Title    : cmcalibrate_wrapper()
  Incept   : EPN, Sun Feb  3 19:58:11 2013
  Usage    : Bio::Rfam::Infernal::cmcalibrate_wrapper($config, $jobname, $options, $cmPath, $outPath, $errPath, $nproc)
  Function : Submit MPI cmcalibrate job to cluster.
           : Command used is location-dependent. We 
           : first predict how long the calibration 
           : will take, then we submit the command 
           : function only submits command it does not
           : wait for it to finish. The predicted time
           : for calibration in minutes is returned.
  Args     : $config:  Rfam config, with infernalPath
           : $jobname: name for MPI job we submit
           : $options: option string for cmcalibrate (SHOULD NOT CONTAIN '--mpi')
           : $cmPath:  path to CM (often 'CM')
           : $outPath: path to output file, must be defined
           : $errPath: path to error output file, must be defined
           : $nproc:   number of CPUs to use, if undefined $CMCALIBRATE_NCPU is used
           : $queue:   queue to submit to, "" for default, ignored if location eq 'EBI'
           : $doMPI:   TRUE to run using MPI, FALSE not to
  Returns  : Predicted number of minutes the calibration should take.
  Dies     : if any command fails, including prediction or cluster submission

=cut

sub cmcalibrate_wrapper {
  my ($config, $jobname, $options, $cmPath, $outPath, $errPath, $nproc, $queue, $doMPI) = @_;
  
  # ensure $cmPath exists
  if (! -e $cmPath) { die "CM file $cmPath does not exist"; }
  
  # set number of CPUs to use, currently hard-coded
  if (! defined $nproc) { $nproc = $CMCALIBRATE_NCPU; }
  
  my $cmcalibratePath = $config->infernalPath . "cmcalibrate";

  # run cmcalibrate --forecast to predict how long job will take
  my $forecast_out = "cfc.$$.out";
  Bio::Rfam::Utils::run_local_command("$cmcalibratePath --forecast --nforecast $nproc $cmPath > $forecast_out");

  # parse cmcalibrate output
  my $predicted_seconds;
  open(IN, $forecast_out) || die "unable to open $forecast_out";
  while (my $line = <IN>) { 
    if ($line !~ m/^\#/) { 
      $line =~ s/^\s+\S+\s+//;
      my ($h, $m, $s) = split(":", $line);
      $predicted_seconds = 3600. * $h + 60. * $m + $s;
      last;
    }
  }
  if (! defined $predicted_seconds) { die "cmcalibrate prediction failed"; }
  unlink $forecast_out;
  
  # submit MPI job
  if($doMPI) { 
    Bio::Rfam::Utils::submit_mpi_job($config->location, "$cmcalibratePath --mpi $cmPath > $outPath", $jobname, $errPath, $nproc, $queue); 
  }
  else { 
    my $gbPerThread = 3.0;
    my $requiredMb = $nproc * $gbPerThread * 1000.; 
    Bio::Rfam::Utils::submit_nonmpi_job($config->location, "$cmcalibratePath --cpu $nproc $cmPath > $outPath", $jobname, $errPath, $nproc, $requiredMb, undef, $queue); 
  }
  return ($predicted_seconds / 60);
}

=head2 cmsearch_wrapper

  Title    : cmsearch_wrapper
  Incept   : EPN, Mon Apr  1 10:20:32 2013
  Usage    : Bio::Rfam::Infernal::cmsearch_wrapper($config, $jobname, $options, $cmPath, $seqfilePath, $outPath, $errPath, $submitExStr, $queue)
  Function : Submit cmsearch job (non-MPI) either to the cluster, or run it locally,
           : by calling cmsearch_or_cmscan_wrapper
           : All options should already be specified in $options,
           : including '--cpu <n>' and '--tblout <tblout>'.
  Args     : $config:       Rfam config, with infernalPath
           : $jobname:      name for job we submit
           : $options:      option string for cmsearch (must contain --tblout and --cpu)
           : $cmPath:       path to CM (often 'CM')
           : $seqfilePath:  path to sequence file to search
           : $outPath:      file to save standard output to, if undefined send to /dev/null.
           : $errPath:      file to save standard error output to
           : $submitExStr:  extra string to add to qsub/bsub command
           : $queue:        queue to submit to, "" for default
           : $do_locally:   '1' to run locally, else run on cluster
           : $gbPerThread:  number of Gb of memory to request per thread
  Returns  : void
  Dies     : if cmsearch command fails

=cut

sub cmsearch_wrapper { 
  cmsearch_or_cmscan_wrapper("cmsearch", @_);
}

=head2 cmscan_wrapper

  Title    : cmscan_wrapper
  Incept   : EPN, Mon Dec  9 17:17:32 2013
  Usage    : Bio::Rfam::Infernal::cmscan_wrapper($config, $jobname, $options, $cmPath, $seqfilePath, $outPath, $errPath, $submitExStr, $queue)
  Function : Submit cmscan job (non-MPI) either to the cluster, or run it locally,
           : by calling cmsearch_or_cmscan_wrapper
           : All options should already be specified in $options,
           : including '--cpu <n>' and '--tblout <tblout>'.
  Args     : $config:       Rfam config, with infernalPath
           : $jobname:      name for job we submit
           : $options:      option string for cmsearch (must contain --tblout and --cpu)
           : $cmPath:       path to CM (often 'CM')
           : $seqfilePath:  path to sequence file to search
           : $outPath:      file to save standard output to, if undefined send to /dev/null.
           : $errPath:      file to save standard error output to
           : $submitExStr:  extra string to add to qsub/bsub command
           : $queue:        queue to submit to, "" for default
           : $do_locally:   '1' to run locally, else run on cluster
           : $do_locally:   '1' to run locally, else run on cluster
           : $gbPerThread:  number of Gb of memory to request per thread
  Returns  : void
  Dies     : if cmsearch command fails

=cut

sub cmscan_wrapper { 
  cmsearch_or_cmscan_wrapper("cmscan", @_);
}

=head2 cmsearch_or_cmscan_wrapper

  Title    : cmsearch_or_cmscan_wrapper
  Incept   : EPN, Mon Apr  1 10:20:32 2013
  Usage    : Bio::Rfam::Infernal::cmsearch_or_cmscan_wrapper($config, $jobname, $options, $cmPath, $seqfilePath, $outPath, $errPath, $submitExStr, $queue)
  Function : Submit cmsearch or cmscan job (non-MPI) either to the cluster, or run it locally.
           : All options should already be specified in $options,
           : including '--cpu <n>' and '--tblout <tblout>'.
  Args     : $program:      "cmsearch" or "cmscan"
           : $config:       Rfam config, with infernalPath
           : $jobname:      name for job we submit
           : $options:      option string for cmsearch (must contain --tblout and --cpu)
           : $cmPath:       path to CM (often 'CM')
           : $seqfilePath:  path to sequence file to search
           : $outPath:      file to save standard output to, if undefined send to /dev/null.
           : $errPath:      file to save standard error output to
           : $submitExStr:  extra string to add to qsub/bsub command
           : $queue:        queue to submit to, "" for default
           : $do_locally:   '1' to run locally, else run on cluster
           : $gbPerThread:  number of Gb of memory to request per thread
  Returns  : void
  Dies     : if cmsearch/cmscan command fails

=cut

sub cmsearch_or_cmscan_wrapper { 
  my ($program, $config, $jobname, $options, $cmPath, $seqfilePath, $outPath, $errPath, $submitExStr, $queue, $do_locally, $gbPerThread) = @_;
  my $cpus;
  if(! defined $outPath || $outPath eq "") { $outPath = "/dev/null"; }
  if(! defined $gbPerThread || $gbPerThread eq "") { $gbPerThread = 3.0; }

  # contract check: $program must be cmsearch or cmscan, $options must include --tblout and --cpu
  if($program ne "cmsearch" && $program ne "cmscan") { 
    die "ERROR, do_and_display_cmsearch_or_cmscan, program eq $program"; 
  }
  if($options !~ m/\-\-tblout/) { 
    die "ERROR cmsearch_or_cmscan_wrapper() option string ($options) does not contain --tblout"; 
  }
  if($options =~ /\-\-cpu (\d+)/) { 
    $cpus = $1; 
  }
  else { 
    die "ERROR cmsearch_or_cmscan_wrapper() option string ($options) does not contain --cpu"; 
  }

  # run job locally or submit non-MPI job to cluster
  if($do_locally) { 
    Bio::Rfam::Utils::run_local_command($config->infernalPath . "$program $options $cmPath $seqfilePath > $outPath"); 
  }
  else { # submit to cluster
    my $ncpu = ($cpus == 0) ? 1 : $cpus; # --cpu 0 actually means 'use 1 CPU'
    my $requiredMb = $ncpu * $gbPerThread * 1000.; # 
    Bio::Rfam::Utils::submit_nonmpi_job($config->location, $config->infernalPath . "$program $options $cmPath $seqfilePath > $outPath", $jobname, $errPath, $ncpu, $requiredMb, $submitExStr, $queue);
  }

  return;
}

=head2 cmalign_wrapper
  Title    : cmalign_wrapper
  Incept   : EPN, Mon Apr  1 10:20:32 2013
  Usage    : Bio::Rfam::Infernal::cmalign_wrapper($config, $jobname, $options, $cmPath, $seqfilePath, $outPath, $errPath, $nseq, $tot_len, $always_local, $always_cluster, $queue, $nproc, $logFH, $do_stdout)
  Function : Run cmalign job or submit it to the cluster if its big.
           : All options should already be specified in $options,
           : including '-o <f>', if desired, EXCEPT for --cpu
           : bc the number of CPUs to use is autodetermined 
           : based on predicted running time. If we submit an
           : MPI job, we wait for it to finish before returning.
           : (This is different from cmsearch_wrapper() and cmcalibrate_wrapper())
  Args     : $config:         Rfam config, with infernalPath
           : $uname:          user name
           : $jobname:        name for job we submit
           : $options:        option string for cmalign
           : $cmPath:         path to CM (often 'CM')
           : $seqfilePath:    path to sequence file to search
           : $outPath:        file to save standard output to, if undefined send to /dev/null.
           : $errPath:        file to save standard error output to
           : $nseq:           number of sequences in $seqfilePath file
           : $tot_len:        total number of residues in $seqfilePath
           : $always_local:   TRUE to always run job locally
           : $always_cluster: TRUE to always run job on the cluster with MPI
           : $queue:          queue to submit to, "" for default
           : $nproc:          number of processors to align with, -1 to autodetect
           : $logFH:          file handle for log output
           : $do_stdout:      1 to output log to stdout, 0 not to
  Returns  : void
  Dies     : if cmalign command fails (if running locally)
           : if job submit command fails or MPI job fails (if running with MPI on cluster)

=cut

sub cmalign_wrapper {
  my ($config, $uname, $jobname, $options, $cmPath, $seqfilePath, $outPath, $errPath, $nseq, $tot_len, $always_local, $always_cluster, $queue, $nproc, $logFH, $do_stdout) = @_;

  # contract check, --cpu MUST NOT be specified and -o <f> MUST be specified
  if($options =~ m/\-\-cpu/)  { die "ERROR cmalign_wrapper() option string ($options) contains --cpu"; }
  if($options !~ /\-o\s+\S+/) { die "ERROR cmalign_wrapper() option string ($options) does not contain -o <f>"; }
  if($nproc > 8 && $always_local) { die "ERROR cmalign_wrapper() nproc ($nproc) > 8 and always_local=1"; }
  
  my $cmalignPath = $config->infernalPath . "cmalign";

  if((! defined $outPath) || ($outPath eq "")) { $outPath = "/dev/null"; }
  
  ####################################################################
  # Predict running time and memory requirement of cmalign
  # and use them to determine number of CPUs for MPI cmcalibrate call.
  ####################################################################
  # Get a rough estimate of running time on 1 CPU based on $tot_len (passed in)
  my $sec_per_Kb = 4;
  my $estimatedCpuSeconds = ($tot_len / 1000.) * $sec_per_Kb;

  my $cluster_max_ncpu = 200; 
  my $cluster_min_ncpu = 4;
  my $local_max_ncpu = 4;
  my $local_min_ncpu = 2;
  my $nproc_passed_in;
  
  # Determine number of CPUs to use: target running time is 1 minute.
  my $targetSeconds = 60;
  if($nproc == -1) { 
    $nproc = int($estimatedCpuSeconds / $targetSeconds) + 1;
  }
  else { 
    $nproc_passed_in = 1;
  }
  
  my $use_cluster;
  if    ($always_cluster) { $use_cluster = 1; }
  elsif ($always_local)   { $use_cluster = 0; }
  elsif ($nproc > 4)      { $use_cluster = 1; }
  else                    { $use_cluster = 0; }

  if(! $nproc_passed_in) { 
    if ($use_cluster) { 
      if ($nproc > $cluster_max_ncpu)  { $nproc = $cluster_max_ncpu; }
      if ($nproc < $cluster_min_ncpu)  { $nproc = $cluster_min_ncpu; }
    } else { 
      if ($nproc > $local_max_ncpu) { $nproc = $local_max_ncpu; }
      if ($nproc < $local_min_ncpu) { $nproc = $local_min_ncpu; }
    }
  }

  my $estimatedWallSeconds = $estimatedCpuSeconds / $nproc;
  
  # Memory requirement is easy, cmalign caps DP matrix size at 1024 Mb
  my $requiredMb = $nproc * 1024.0;
  
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
  
  my $align_start_time = time();
  if ($use_cluster) { 
    # submit MPI job
    my $jobname = "a.$$";
    my $errPath = "a.$$.err";
    Bio::Rfam::Utils::submit_mpi_job($config->location, "$cmalignPath --mpi $options $cmPath $seqfilePath > $outPath", "a.$$", "a.$$.err", $nproc, $queue);
    my @jobnameA = ($jobname);
    my @outnameA = ($outPath);
    Bio::Rfam::Utils::wait_for_cluster($config->location, $uname, \@jobnameA, \@outnameA, "\# CPU time:", "cmalign-mpi", $logFH, "[$nproc processors]", -1, $do_stdout);
    unlink $errPath;
  }
  else { 
    if(defined $logFH) { Bio::Rfam::Utils::log_output_progress_local($logFH, "cmalign", time() - $align_start_time, 1, 0, "[$nproc CPUs]", $do_stdout); }
    Bio::Rfam::Utils::run_local_command("$cmalignPath --cpu $nproc $options $cmPath $seqfilePath > $outPath"); 
    if(defined $logFH) { Bio::Rfam::Utils::log_output_progress_local($logFH, "cmalign", time() - $align_start_time, 0, 1, "", $do_stdout); }
  }

  return;
}

=head2 cmemit_wrapper
  Title    : cmemit_wrapper
  Incept   : EPN, Wed Feb 12 10:19:39 2014
  Usage    : Bio::Rfam::Infernal::cmemit_wrapper($config, $options, $cmPath, $logFH, $do_stdout)
  Function : Run cmemit job locally.
           : All options should already be specified in $options,
           : including '-o <f>', if desired. If '-o <f>' is not
           : in $options we will output to stdout.
  Args     : $config:         Rfam config, with infernalPath
           : $options:        option string for cmsearch (must contain --tblout and --cpu)
           : $cmPath:         path to CM (often 'CM')
           : $logFH:          file handle for log output, if ! defined, do not output to log output
           : $do_stdout:      1 to output log to stdout, 0 not to
  Returns  : void
  Dies     : if cmemit command fails

=cut

sub cmemit_wrapper {
  my ($config, $options, $cmPath, $logFH, $do_stdout) = @_;

  my $cmemitPath = $config->infernalPath . "cmemit";
  my $emit_start_time = time();

  if(defined $logFH) { Bio::Rfam::Utils::log_output_progress_local($logFH, "cmemit", time() - $emit_start_time, 1, 0, "", $do_stdout); }
  Bio::Rfam::Utils::run_local_command("$cmemitPath $options $cmPath"); 
  if(defined $logFH) { Bio::Rfam::Utils::log_output_progress_local($logFH, "cmemit", time() - $emit_start_time, 0, 1, "", $do_stdout); }

  return;
}

=head2 cm_evalue2bitsc()

  Title    : cm_evalue2bitsc()
  Incept   : EPN, Tue Jan 29 17:18:43 2013
  Usage    : cm_evalue2bitsc($cm, $evalue, $Z, $opts)
  Function : Returns bit score for a given E-value
  Args     : <cm>:     Bio::Rfam::Family::CM object
           : <evalue>: E-value we want bit score for
           : <Z>:      database size in Mb (both strands) for E-value->bitsc calculation
           : <opts>:   string with cmsearch options (could be from DESC)
  Returns  : bit score for E-value for CM in db of $Z residues 
           : (where $Z includes BOTH strands of target seqs)
  
=cut
  
sub cm_evalue2bitsc { 
  my ($cm, $evalue, $Z, $opts) = @_;

  # this subroutine corresponds to infernal 1.1's cmstat.c line 295 ('else if(output_mode == OUTMODE_BITSCORES_E) {')
  my $bitsc;  # bit score to return;

  # two seperate blocks: are we using an HMM or a CM to search?
  #######
  # HMM #
  #######
  if($opts =~ m/\-\-hmmonly/) { 
    # HMM only search, use HMM E-value parameters
    my ($hitlen, $tau, $lambda) = Bio::Rfam::Infernal::search_opts_to_hmm_evalue_stats($cm);
    $bitsc = $tau + ((log($evalue / (($Z * 1000000.) / $hitlen))) / (-1 * $lambda));
    # print STDERR ("in cm_evalue2bitsc() converted HMM E-value $evalue to bit $bitsc (Z: $Z)\n");
  }
  ######
  # CM #
  ######
  else { # --hmmonly not in $opts, we want CM stats:
    if(! $cm->{is_calibrated}) {  
      die "ERROR CM is not calibrated, and we're trying to convert an E-value to a bit score"; 
    }
  
    my ($lambda, $mu_extrap, $mu_orig, $dbsize, $nhits, $tailp) = Bio::Rfam::Infernal::search_opts_to_cm_evalue_stats($cm, $opts);
    my $cur_eff_dbsize = (($Z * 1000000.) / $dbsize) * $nhits;
    $bitsc = $mu_extrap + ((log($evalue / $cur_eff_dbsize)) / (-1 * $lambda));
    # print STDERR ("in cm_evalue2bitsc() converted CM E-value $evalue to bit $bitsc (Z: $Z)\n");
  }

  return $bitsc;
}


=head2 cm_bitsc2evalue()

  Title    : cm_bitsc2evalue()
  Incept   : EPN, Thu Apr 25 14:01:51 2013
  Usage    : cm_bitsc2evalue($cm, $bitsc, $Z, $opts)
  Function : Returns E-value for a given bit score
  Args     : <cm>:     Bio::Rfam::Family::CM object
           : <bitsc>:  bit score we want E-value for
           : <Z>:      database size in Mb (both strands) for E-value->bitsc calculation
           : <opts>:   string with cmsearch options (could be from DESC)
  Returns  : E-value for bit score for CM in db of $Z residues 
           : (where $Z includes BOTH strands of target seqs)
  
=cut
  
sub cm_bitsc2evalue { 
  my ($cm, $bitsc, $Z, $opts) = @_;

  # this subroutine corresponds to infernal's cmstat.c line 295 ('else if(output_mode == OUTMODE_BITSCORES_E) {')
  my $evalue;  # evalue to return;
  my $surv;    # survivor function value (calc'ed same as in esl_exp_surv())


  # two seperate blocks: are we using an HMM or a CM to search?
  #######
  # HMM #
  #######
  if($opts =~ m/\-\-hmmonly/) { 
    # HMM only search, use HMM E-value parameters
    my ($hitlen, $tau, $lambda) = Bio::Rfam::Infernal::search_opts_to_hmm_evalue_stats($cm);
    # See C code esl_exp_surv && Infernal 1.1 stats.c:Score2E
    my $cur_eff_dbsize = ($Z * 1000000.) / $hitlen;
    $surv = ($bitsc < $tau) ? 1.0 : exp((-1 * $lambda) * ($bitsc - $tau));
    $evalue = $surv * $cur_eff_dbsize;
    # print STDERR ("in cm_bitsc2eevalue() converted HMM E-value $evalue to bit $bitsc (Z: $Z)\n");
  }
  ######
  # CM #
  ######
  else { 
    if(! $cm->{is_calibrated}) {  
      die "ERROR CM is not calibrated, and we're trying to convert a bit score to an E-value";
    }
    my ($lambda, $mu_extrap, $mu_orig, $dbsize, $nhits, $tailp) = Bio::Rfam::Infernal::search_opts_to_cm_evalue_stats($cm, $opts);

    my $cur_eff_dbsize = (($Z * 1000000.) / $dbsize) * $nhits;
    # from easel's esl_exponential.c:esl_exp_surv
    $surv = ($bitsc < $mu_extrap) ? 1.0 : exp((-1 * $lambda) * ($bitsc - $mu_extrap));
    $evalue = $surv * $cur_eff_dbsize;
    # print STDERR ("in cm_bitsc2evalue() converted CM bit score $bitsc to E-value $evalue (Z: $Z)\n");
  }
  return $evalue;
}

=head2 search_opts_to_cm_evalue_stats()

  Title    : search_opts_to_cm_evalue_stats()
  Incept   : EPN, Tue Aug 27 14:42:06 2013
  Usage    : search_opts_to_cm_evalue_stats($cm, $opts)
  Function : Returns appropriate array of E-value stat parameters
           : (lambda, mu_extrap, mu_orig, dbsize, nhits, tailp),
           : from CM file (either ECMLI, ECMLC, ECMGI, ECMGC)
           : based on cmsearch options from $opts (could be 
           : from DESC file).
  Args     : <cm>:     Bio::Rfam::Family::CM object
           : <opts>:   search-method string, from DESC
  Returns  : $lambda:    appropriate CM lambda exp tail parameter
           : $mu_extrap: appropriate CM extrapolated mu exp tail parameter
           : $mu_orig:   appropriate CM original mu exp tail parameter  
           : $dbsize:    appropriate CM dbsize exp tail parameter  
           : $nhits:     appropriate CM nhits exp tail parameter  
           : $tailp:     appropriate CM tail prob exp tail parameter  
=cut
  
sub search_opts_to_cm_evalue_stats {
  my ($cm, $opts) = @_;

  if($opts =~ m/\s+\-g\s+/) { # glocal mode
    if($opts =~ m/\s+\-\-cyk\s+/) { # cyk mode
      return @{$cm->{cmHeader}->{ecmgc}};
    }
    else { # inside (default)
      return @{$cm->{cmHeader}->{ecmgi}};
    }
  }
  else { # local mode (default)
    if($opts =~ m/\s+\-\-cyk\s+/) { # cyk mode
      return @{$cm->{cmHeader}->{ecmlc}};
    }
    else { # inside (default)
      return @{$cm->{cmHeader}->{ecmli}};
    }
  }
}

=head2 search_opts_to_cm_evalue_stats()

  Title    : search_opts_to_hmm_evalue_stats()
  Incept   : EPN, Tue Apr 22 10:49:08 2014
  Usage    : search_opts_to_hmm_evalue_stats($cm)
  Function : Returns appropriate array of E-value stat parameters
           : for the CMs filter p7 HMM
           : (hitlen, mu, lambda),
           : from CM file (STATS LOCAL FORWARD and MAXL lines)
  Args     : <cm>:     Bio::Rfam::Family::CM object
  Returns  : $hitlen:    from MAXL line in filter HMM section of the CM file
           : $tau:       tau from STATS LOCAL FORWARD line of CM file
           : $lambda:    lambda from STATS LOCAL FORWARD line of CM file
=cut
  
sub search_opts_to_hmm_evalue_stats {
  my ($cm) = @_;

  return ($cm->{hmmHeader}->{maxl},
          $cm->{hmmHeader}->{forwardStats}->{tau}, 
          $cm->{hmmHeader}->{forwardStats}->{lambda});
}

=head2 stringize_infernal_cmdline_options()

  Title    : stringize_infernal_cmdline_options()
  Incept   : EPN, Thu Jan 31 16:52:35 2013
  Usage    : stringize_infernal_cmdline_options($sdashAR, $ddashAR)
  Function : Returns option string including single dash and double dash
           : options in $sdashAR and $ddashAR.
  Args     : $sdashAR: ref to array of single dash options
           : $ddashAR: ref to array of double dash options
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

=head2 process_cpu_times()

  Title    : process_cpu_times()
  Incept   : EPN, Tue Apr  2 14:37:02 2013
  Usage    : process_cpu_times($file)
  Function : Sums CPU and elapsed run times in an Infernal output file
           : (or a file of concatenated Infernal output files)
           : and returns them, along with max values.
  Args     : $file: file with "# CPU time:" lines 
           : $time_string:       string that indicates timing line
           : $ret_max_cpu_secsR: RETURN: number of max CPU seconds
           : $ret_max_elp_secsR: RETURN: number of max elapsed seconds
           : $ret_tot_cpu_secsR: RETURN: number of total CPU seconds (summed)
           : $ret_tot_elp_secsR: RETURN: number of total elapsed seconds (summed)
  Returns  : Maximum number of CPU     seconds in $ret_max_cpu_secsR.
           : Maximum number of elapsed seconds in $ret_max_elp_secsR.
           : Total   number of CPU     seconds in $ret_tot_cpu_secsR.
           : Total   number of elapsed seconds in $ret_tot_elp_secsR.
  Dies     : if no "CPU time" lines were found
=cut

sub process_cpu_times { 
  my ($file, $time_string, $ret_max_cpu_secsR, $ret_max_elp_secsR, $ret_tot_cpu_secsR, $ret_tot_elp_secsR) = @_;

  my $max_cpu_secs = 0;
  my $max_elp_secs = 0;
  my $tot_cpu_secs = 0;
  my $tot_elp_secs = 0;

  my $found_cpu = 0;
  my ($cpu_secs, $elp_secs);
  open(IN, $file) || die "process_cpu_times() can't open $file"; 
  while(<IN>) { 
     ## CPU time: 382.47u 117.85s 00:08:20.32 Elapsed: 00:01:57.46
     ## Total CPU time:298.97u 8.23s 00:05:07.20 Elapsed: 00:01:32.74
    if(s/\Q$time_string//) { 
      /\s*\S+u \S+s (\d\d)\:(\d\d)\:(\S+)\s+Elapsed\:\s+(\d\d)\:(\d\d)\:(\S+)/;
      $cpu_secs = (3600 * $1) + (60 * $2) + $3;
      $elp_secs = (3600 * $4) + (60 * $5) + $6;
      if($cpu_secs > $max_cpu_secs) { $max_cpu_secs = $cpu_secs; }
      if($elp_secs > $max_elp_secs) { $max_elp_secs = $elp_secs; }
      $tot_cpu_secs += $cpu_secs;
      $tot_elp_secs += $elp_secs;
      $found_cpu = 1;
    }
  }
  close(IN);

  if(defined $ret_max_cpu_secsR) { $$ret_max_cpu_secsR = $max_cpu_secs; }
  if(defined $ret_max_elp_secsR) { $$ret_max_elp_secsR = $max_elp_secs; }
  if(defined $ret_tot_cpu_secsR) { $$ret_tot_cpu_secsR = $tot_cpu_secs; }
  if(defined $ret_tot_elp_secsR) { $$ret_tot_elp_secsR = $tot_elp_secs; }

  if(! $found_cpu) { die "process_cpu_times() no time lines were found in $file"; }

  return;
}

######################################################################

1;



