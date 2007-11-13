#!/software/bin/perl -w

use strict;
use Rfam;
use RfamQC;
use Rfam::RfamAlign;
use IO::File;
use File::Copy;
use Sys::Hostname;
use Cwd;

my $family = shift;

## Step 1 - Create working directory

#First lsf is to create a working directory on the farm! 
my $queue      = 'small -R \"select[type==X86_64]\"';
my $lustre = "/lustre/scratch1/sanger/rfam/$$"; #path for dumping data to on the farm
my $blastdb_path = "/data/blastdb/Pfam/";

#Set the stripe pattern on the lustre file system:
my $fh0 = new IO::File;
$fh0 -> open("| bsub -I -q $queue") or die "FATAL: bsub -I -q $queue\n$!";
print( "Making lustre run directory on the farm: $lustre" );
$fh0->print("mkdir -p $lustre\n") or die "FATAL: mkdir -p $lustre\n$!";
$fh0->print("lfs setstripe $lustre 0 4 4\n") or die "FATAL: lfs setstripe $lustre 0 4 4\n$!";
$fh0->print("ls -1 $blastdb_path/pfamseq.*.xnd >$lustre/blastdb.txt");
$fh0->close;

## Step 2 - split SEED 
my $max_seqfiles = 5; #number of files to split jobs across - each are run on 42 Pfam blastdb's...

open( SEED, "$family/SEED" ) or die ("FATAL: Couldn't open SEED!\n $!\n");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
close(SEED);
my @list = $seed->each_seq();
my $seqs_per_file = int(scalar(@list)/$max_seqfiles)+1;
my ($i, $seqcount) = (0, 1);
my %seq_files;
foreach my $seqobj ( @list ) {
    my $outfile = "$family/query.$i.fa";
    $seq_files{$outfile}=1;
    open( OUT, ">>$outfile" ) or die ("FATAL: Couldn't open $outfile!\n $!\n");
    my $seq = $seqobj->seq;
    $seq =~ s/[.,:_-]//g;
    my $seqname = $seqobj->id;
    my $start = $seqobj->start;
    my $end = $seqobj->end;
    $seqname = "$seqname/$start-$end";
    printf OUT ">$seqname\n$seq\n";
    close(OUT);
    if (($seqcount % $seqs_per_file) == 0){
	$i++;
    }
    $seqcount++;
}

#SCP sequence files over & blast DB names back
foreach my $filename (keys %seq_files){
    system("/usr/bin/scp $filename farm-login:$lustre/") 
	and die "Failed to copy $filename to lustre directory:[$!]\n";
}
system("/usr/bin/scp farm-login:$lustre/blastdb.txt ./") and die "Failed to copy farm-login:$lustre/blastdb.txt:[$!]\n";

open(BLASTDB,"<blastdb.txt") || die "Could not open file blastdb.txt!\n [$!]";
my %blastdb;
while( my $line = <BLASTDB> ) {
    if ($line =~ /pfamseq.\d+.xnd/){
	$line = s/\.xnd//;
	$blastdb{$line}=1;
    }
}

## Step 3 - BLAST
print "Running blast...\n";
# We need to know where we are so that we can scp files back and forth.
my $phost = hostname();
my $wqueue      = "long -n4 -R \"span[hosts=1] && select[type==X86_64]\"";
my $nobjobs = scalar(@list);

my $k=0;
foreach my $seqfile (keys %seq_files){
    foreach my $blastdbfile (keys %blastdb){
	
	my $blastfile  = "wublast_pfamseq_vs_query.$k.dat";
	my $blastcmd = "wublastx $blastdb_path/$blastdbfile $lustre/$seqfile E=0.01 mformat=3  hspmax=1000 hspsepSmax=200 cpus=4 -filter=seg+xnu";
	$ENV{"WUBLASTMAT"} = "/software/rfam/src/wublast/matrix";
	$ENV{"WUBLASTFILTER"} = "/software/rfam/src/wublast/filter";
	
	my $fh = new IO::File;
	$fh -> open("| bsub -q $wqueue -J\"rf$$\" -o $family/$$\.berr.$i") or die "FATAL: bsub of blastcmd failed!\n$!";
	#print "Sending: $blastcmd > $lustre/$blastfile\n";
	$fh -> print("$blastcmd  > $lustre/$blastfile\n");
	$fh -> print("/usr/bin/scp $lustre/$blastfile $phost:$family/\n") or die "error scp-ing farm-login:$lustre/$blastfile to $phost:$family/\n$!\n"; 
	$fh -> close;
	$k++;
    }
}

  print "Waiting for blast jobs";
  my $wait = 1;
  my $bjobcount = 1;
  my $bjobinterval = 15;
  my $jobs = $nobjobs;
  while($wait){
	 sleep($bjobinterval); 
	 $jobs = 0;
	 open(LOG, "bjobs -J rf$$ |") || die "Failed to open blast log\n";
	 while(<LOG>){
	   if(/rf$$/){
		   $jobs++;
	   }
	 }
	 close(LOG);
	 if ($jobs < int($nobjobs*(1-0.95)) ){#Once 95% of jobs are finished, check frequently.
	    $bjobinterval=15;
	 }elsif ($jobs < int($nobjobs*(1-0.80)) ){#Once 80% of jobs are finished, check a little more frequently.
	    $bjobinterval=15 + int(log($bjobcount/2)); 
	 }else {#otherwise check less & less frequently (max. interval is ~150 secs/2.5 mins).
	    if ($bjobinterval<150){ $bjobinterval = $bjobinterval + int(log($bjobcount));}
	 }
	
	 if($jobs){
	    print "There are $jobs blast job still running after $bjobcount checks. Check interval is now $bjobinterval secs.\n"; 
	 }else{
	    $wait = 0;
	 }
	 $bjobcount++;
}

## Step 4 - Parsing & summarising BLAST output:
print "Blast finished, parsing results.\n";

my $blastfile = "$family/wublast_pfamseq_vs_query.dat";
system("cat $family/wublast_pfamseq_vs_query.*.dat > $blastfile");
open(OUT,"<$blastfile") || die "Could not open file $blastfile!\n $!";

my $linenumber = 0;
my( $qname, $name, $qstart, $qend, $sstart, $send, $start, $end, $strand, @bline, %protnames, %rnanames, $evalue );
while( <OUT> ) {
    $linenumber++;
    next if( /^\#/ );
    
    @bline = split(/\t/,$_);
    if(scalar(@bline) == 22 ){
	if ($bline[0]  =~ /\S+/){$qname  = $bline[0];}  else {printf STDERR "qname error   =\'$bline[0]\'  in $blastfile line number $linenumber\n"};
	if ($bline[1]  =~ /\S+/){$name   = $bline[1];}  else {printf STDERR "name error    =\'$bline[1]\'  in $blastfile line number $linenumber\n"};
	if ($bline[17] =~ /\d+/){$qstart = $bline[17];} else {printf STDERR "qstart error  =\'$bline[17]\' in $blastfile line number $linenumber\n"};
	if ($bline[18] =~ /\d+/){$qend   = $bline[18];} else {printf STDERR "qend error    =\'$bline[18]\' in $blastfile line number $linenumber\n"};
	if ($bline[20] =~ /\d+/){$sstart = $bline[20];} else {printf STDERR "sstart error  =\'$bline[20]\' in $blastfile line number $linenumber\n"};
	if ($bline[21] =~ /\d+/){$send   = $bline[21];} else {printf STDERR "send error    =\'$bline[21]\' in $blastfile line number $linenumber\n"};
	if ($bline[16] =~ /\S+/ && $bline[19] =~ /\S+/){$strand = $bline[16]*$bline[19];}  else {printf STDERR "strand error =\'$bline[16]\' and \'$bline[19]\' in $blastfile line number $linenumber\n"};
	if ($bline[3]  =~ /\S+/){$evalue   = $bline[3];}  else {printf STDERR "evalue error    =\'$bline[3]\'  in $blastfile line number $linenumber\n"};
	
	print "$qname\t$qstart\t$qend\t$name\t$sstart\t$send\t$evalue\n";
	$protnames{$name}=1;
	$rnanames{$name}=1;
    }
}

#print "Protein hits:\n";
#if (-e "$family/wublast_pfamseq_vs_query.seq"){
#    system("rm $family/wublast_pfamseq_vs_query.seq");
#}

#foreach my $n (keys %protnames){
#    system("mfetch -d uniprot -v full $n >>$family/wublast_pfamseq_vs_query.seq");
#}

#foreach my $n (keys %rnanames){
#    system("mfetch -d embl -v full $n >>$family/wublast_pfamseq_vs_query.seq");
#}

## Step 5 - Clean up:
my $fhbs2 = new IO::File;
$fhbs2 -> open("| bsub -q $queue -w\'done(rf$$)\'") or die "$!";
$fhbs2 -> print("rm -fr $lustre\n");
$fhbs2 -> close;
system("rm $family/wublast_pfamseq_vs_query.*.dat $family/query.*.fa");


#
