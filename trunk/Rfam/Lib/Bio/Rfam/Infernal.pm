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
  Usage    : Bio::Rfam::Infernal::cmbuild_wrapper($cmbuildPath, $opts, $cmPath, $alnPath)
  Function : Runs Infernal's cmbuild.
  Args     : <cmbuildPath>: path to cmbuild
           : <opts>:        option string for cmbuild (-F will be appended to this)
           : <cmPath>:      path to output CM (often 'CM')
           : <seedPath>:    path to input alignment (often 'SEED')
  Returns  : void
  Dies     : if cmbuild command fails
=cut

sub cmbuild_wrapper {
    my ($cmbuildPath, $opts, $cmPath, $seedPath) = @_;
    
    if($opts !~ m/\-F/) { $opts = "-F " . $opts; }
    $opts =~ s/\s+$//; # remove trailing whitespace
    $opts =~ s/^\s+//; # remove leading  whitespace
    my $cmd = $cmbuildPath . ' ' . $opts . ' ' . $cmPath . ' ' . $seedPath . '> /dev/null';
    system($cmd);
    if($? != 0) { croak "$cmd failed"; }
    return;
}


=head2 cmbuild_wrapper
  Title    : cmbuild_wrapper
  Incept   : EPN, Sun Feb  3 19:58:11 2013
  Usage    : Bio::Rfam::Infernal::cmcalibrate_wrapper($cmcalibratePath, $opts, $cmPath)
  Function : Runs Infernal's cmcalibrate.
  Args     : <cmcalibratedPath>: path to cmcalibrate
           : <opts>:             option string for cmcalibrate
           : <cmPath>:           path to CM (often 'CM')
           : <ncpu>:             number of CPUs to use, usually undefined
  Returns  : void
  Dies     : if cmcalibrate command fails
=cut

sub cmcalibrate_wrapper {
    my ($cmcalibratePath, $opts, $cmPath, $ncpu) = @_;

    # ensure $cmPath exists
    if(! -e $cmPath) { croak "CM file $cmPath does not exist"; }

    # set number of CPUs to use, currently hard-coded
    if(! defined $ncpu) { $ncpu = $CMCALIBRATE_NCPU; }

    # predict how long job will take
    my $cmd = "cmcalibratePath --forecast --nforecast $ncpu $cmPath";
    my $predicted_seconds;
    system($cmd);
    if($? != 0) { croak "$cmd failed"; }
    my $forecast_out = "cmcalibrate-forecast.out";
    open(IN, $forecast_out) || croak "unable to open $forecast_out";
    while(my $line = <IN>) { 
	if($line !~ m/^\#/) { 
	    $line =~ s/^\S+\s+//;
	    my ($h, $m, $s) = split(":", $line);
	    $predicted_seconds = 3600. * $h + 60. * $m + $s;
	    last;
	}
    }
    if(! defined $predicted_seconds) { croak "cmcalibrate prediction failed"; }
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

#cmAlign: takes a CM and sequence file and runs cmalign, either locally or on the farm using MPI

#Systems MPI help documentation:
#http://scratchy.internal.sanger.ac.uk/wiki/index.php/How_to_run_MPI_jobs_on_the_farm

# For MPI to work, you need to make sure Infernal has been compiled correctly, with --enable-mpi flag to configure.
# See Infernal user's guide. It should be compiled correctly, or else rfsearch wouldn't work (MPI cmcalibrate is used there).
# Also, ssh keys need to be correct. Remove all ^"bc-*" entries from your ~/.ssh/known_hosts file.
# Optionally add "StrictHostKeyChecking no" to your ~/.ssh/config

sub cmAlign {
    my $cmalignPath = shift; #Path to cmalign
    my $cmfile      = shift; #Handed a CM file
    my $seqfile     = shift; #sequence file with seqs to align
    my $alnfile     = shift; #alignment output file 
    my $outfile     = shift; #cmalign output file 
    my $opts        = shift; #string of cmalign options
    my $nseq        = shift; #number of sequences in $seqfile
    my $tot_len     = shift; #total number of nucleotides in $seqfile
    my $always_farm = shift; # 1 to always use farm, 0 to only use farm if > 4 CPUs needed
    my $never_farm  = shift; # 0 to never  use farm, 1 to only use farm if > 4 CPUs needed
    my $dirty       = shift; # 1 to leave files on farm, else remove them 

    my @unlinkA = ();

    die "cmfile \42$cmfile\42 either does not exist or is empty!" if !(-s $cmfile);

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
    my $farm_max_ncpu  = 27; # (1028 * 27) = 27,756 (max memory usage is 28 Gb for any 1 job, cmalign requires up to 1028 Mb per CPU...)
    my $farm_min_ncpu  = 4;
    my $local_max_ncpu = 4;
    my $local_min_ncpu = 2;

    my $use_farm;
    if   ($always_farm) { $use_farm = 1; }
    elsif($never_farm)  { $use_farm = 0; }
    elsif($cpus > 4)    { $use_farm = 1; }
    else                { $use_farm = 0; }
	
    if($use_farm) { 
	if($cpus > $farm_max_ncpu) { $cpus = $farm_max_ncpu; }
	if($cpus < $farm_min_ncpu) { $cpus = $farm_min_ncpu; }
    }
    else { 
	if($cpus > $local_max_ncpu) { $cpus = $local_max_ncpu; }
	if($cpus < $local_min_ncpu) { $cpus = $local_min_ncpu; }
    }

    my $estimatedWallSeconds = $estimatedCpuSeconds / $cpus;

    # Memory requirement is easy, cmalign caps DP matrix size at 1024 Mb
    my $requiredMb = $cpus * 1024.0;

    my $hrs = int($estimatedWallSeconds/3600);
    my $min = int(($estimatedWallSeconds - ($hrs * 3600)) / 60);
    my $sec = int($estimatedWallSeconds - ($hrs * 3600 + $min * 60));
    
    my $rounded_requiredMb = 500;
    # pick smallest 500 Mb multiple that satisfies required memory estimate
    while($rounded_requiredMb < $requiredMb) { 
	$rounded_requiredMb += 500; 
    }
    $requiredMb = $rounded_requiredMb;
    my $requiredKb = $requiredMb * 1000;

    printf("Aligning %7d sequences %s on %d cpus; predicted time (h:m:s): %02d:%02d:%02d %s", 
	   $nseq, 
	   ($use_farm) ? "on farm" : "locally",
	   $cpus, $hrs, $min, $sec+0.5, 
	   ($use_farm) ? "\n" : " ... ");

    if(! $use_farm) { 
	# run locally
	# NOWTODO update with config infernal path, probably need to change cmAlign() subroutine args
	my $command = "$cmalignPath --cpu $cpus $opts -o $alnfile $cmfile $seqfile > $outfile";
	system("$command");
	if($?) { die "FAILED: $command"; }
	printf("done.\n");
	open(OUT, $outfile);
    }
    else { 
	# run on farm with MPI 
	# create directory on lustre to use, copy CM and seqfile there:
	my $user =  getlogin() || getpwuid($<);
	my $pwd = getcwd;
	die "FATAL: failed to run [getlogin or getpwuid($<)]!\n[$!]" if not defined $user or length($user)==0;
	my $lustre = "$Rfam::scratch_farm/$user/$$"; #path for dumping data to on the farm
	mkdir("$lustre") or die "FATAL: failed to mkdir [$lustre]\n[$!]";

	# don't worry about calling 'lfs setstripe' here like we do in rfsearch.pl
	# because we won't be accessing a few very large files only (see: http://scratchy.internal.sanger.ac.uk/wiki/index.php/Farm_II_User_notes#Striping_options)

	my $lustre_cmfile  = "$lustre/CM";
	my $lustre_seqfile = "$lustre/$seqfile";
	my $lustre_alnfile = "$lustre/$alnfile";
	my $lustre_outfile = "$lustre/$outfile";
	copy("$cmfile",  "$lustre_cmfile") or die "FATAL: failed to copy [$cmfile] to [$lustre_cmfile]\n[$!]";
	copy("$seqfile", "$lustre_seqfile") or die "FATAL: failed to copy [$seqfile] to [$lustre_seqfile]\n[$!]";
	push(@unlinkA, $lustre_cmfile);
	push(@unlinkA, $lustre_seqfile);

	my $alignCommand = "mpirun --mca mpi_paffinity_alone 1 --hostfile /tmp/hostfile.\$LSB_JOBID --np \$CPUS $cmalignPath --mpi $opts -o $lustre_alnfile $lustre_cmfile $lustre_seqfile > $lustre_outfile";
	
	#Generate a MPI script:
	my $mpiScript = "#!/bin/bash
# An OPENMPI LSF script for running cmalign
# Submit this script via bsub to use.
 
# Parse the LSF hostlist into a format openmpi understands and find the number of CPUs we are running on.
echo \$LSB_MCPU_HOSTS | awk '{for(i=1;i <=NF;i=i+2) print \$i \" slots=\" \$(i+1); }' >> /tmp/hostfile.\$LSB_JOBID
CPUS=`echo \$LSB_MCPU_HOSTS | awk '{for(i=2;i <=NF;i=i+2) { tot+=\$i; } print tot }'` 

# Now run our executable 
$alignCommand
";
	my $mpiScriptFile = $lustre . '/' . $$ . '_cmalign_mpi_script.sh'; #be cleverer here
	open(MS, "> $mpiScriptFile") or die "FATAL: failed to open file: $mpiScriptFile\n[$!]";
	print MS $mpiScript;
	close(MS);
	
	chmod 0775,  $mpiScriptFile or die "FATAL: failed to run [chmod 0775,  $mpiScriptFile]\n[$!]";
	
	my $mpiScriptOut =  $$ . '_cmalign_mpi_script.out';
	my $bjobName = "cmaln" . $$ . hostname;
	my $bsubOpts = "-o $lustre/$mpiScriptOut -q mpi  -J\"$bjobName\" -n$cpus -a openmpi -R \'select[mem>$requiredMb] rusage[mem=$requiredMb]\' -M $requiredKb";
	
	print "Running: bsub $bsubOpts $mpiScriptFile\n";
	system("bsub $bsubOpts $mpiScriptFile > $lustre/$mpiScriptOut\.std") and die "FATAL: failed to run to run: bsub $bsubOpts $mpiScriptFile\n[$!]";
	Bio::Rfam::Utils::wait_for_farm($bjobName, 'cmalign', $cpus, 100*$estimatedWallSeconds+120, undef ); #100*$estimatedWallSeconds() our timing estimates may be way off because job may be PENDING for a long time
	push(@unlinkA, $lustre_alnfile);
	push(@unlinkA, $lustre_outfile);
	
	#copy("$lustre/$mpiScriptOut", "$pwd/$mpiScriptOut") or die "FATAL: failed to copy $lustre/$mpiScriptOut to $pwd/$mpiScriptOut\n[$!]";
	
	open(OUT, "< $lustre/$mpiScriptOut") or die "FATAL: failed to open $lustre/$mpiScriptOut for reading\n[$!]";

	copy("$lustre_alnfile",  "$alnfile") or die "FATAL: failed to copy [$lustre_alnfile] to [$alnfile]\n[$!]";
	copy("$lustre_outfile",  "$outfile") or die "FATAL: failed to copy [$lustre_outfile] to [$outfile]\n[$!]";
    }	
    my $alignTime=0;
    while(<OUT>){
	if(/\#\s+CPU\s+time\:\s+(\S+)u\s+(\S+)s/){
	    $alignTime=$1+$2;
	    last;
	}
    }
    close(OUT);

    # clean up
    if(! $dirty) { 
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
  Usage    : cm_evalue2bitsc($cm, $evalue)
  Function : Returns bit score for a given E-value
  Args     : <cm>:     Bio::Rfam::Family::CM object
           : <evalue>: E-value we want bit score for
           : <Z>:      database size (both strands) for E-value->bitsc calculation
  Returns  : bit score for E-value for CM in db of $Z residues 
           : (where $Z includes BOTH strands of target seqs)
  
=cut

sub cm_evalue2bitsc { 
    my ($cm, $evalue, $Z) = @_;
    
    # following from infernal's cmstat.c line 295 ('else if(output_mode == OUTMODE_BITSCORES_E) {')
    my $eline; # E-value stat line from CM file
    my ($lambda, $mu_extrap, $mu_orig, $dbsize, $nhits, $tailp); # E-value stat components from CM file
    my $cur_eff_dbsize; # current effective dbsize
    my $bitsc; # bit score to return;

    # variables only used if HMM stats apply
    my $tau; # used if HMM stats are used
    my $maxlen; # maximum lenght

    # TODO, only use HMM stat line if --nohmmonly was NOT used in SM
    if($cm->{match_pair_node}) { # use CM stats
	# TODO, read SM in desc, and pick appropriate E-value line based on that
	$eline = $cm->{cmHeader}->{ecmli};
	($lambda, $mu_extrap, $mu_orig, $dbsize, $nhits, $tailp) = split(/s+/, $eline);
	$cur_eff_dbsize = ($Z / $dbsize) * $nhits;
	$bitsc = $mu_extrap + ((log($evalue / $cur_eff_dbsize)) / (-1 * $lambda));
    }
    else { 
	$eline = $cm->{cmHeader}->{efp7gf};
	($tau, $lambda) = split(/\s+/, $eline);
	$maxlen = $cm->{cmHeader}->{maxl};
	$bitsc = $tau + ((log($evalue / ($Z / $maxlen))) / (-1 * $lambda));
    }
    
    printf("in cm_evalue2bitsc() converted E-value $evalue to bit $bitsc (Z: $Z)\nEline: $eline\n");
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

    foreach $opt (@{$sdashAR}) { $optstring .= "\-$opt ";  }
    foreach $opt (@{$ddashAR}) { $optstring .= "\--$opt "; }
    
    $optstring =~ s/\s+$//;

    return $optstring;
}

######################################################################

1;



