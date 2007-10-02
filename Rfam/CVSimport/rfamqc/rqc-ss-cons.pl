#! /software/bin/perl -w

# A program to run checks on alignment consistency with structure.

use strict;
use Rfam;
use RfamQC;
use Rfam::RfamAlign;
use Getopt::Long;
#use Data::Dumper; #Great for printing diverse data-structures.
use Tie::IxHash;  #Use to return hash keys in the order they were added

my( $nolog );

&GetOptions("n!"   => \$nolog );

if( $#ARGV == -1 ) {
    print "rqc-ss-con.pl - calculate statistics for how well the ";
    print "                structure annotation corresponds with \n";
    print "                the sequences.\n";
    print "Usage:    rqc-ss-con.pl <directory>\n";
    print "Options:\n";
    print "  -n             no log file\n";
    exit(1);
}


my $family_dir = shift; # family dir to use
my @family_dir = split(/\//, $family_dir);
my $shortname_family_dir = pop(@family_dir); # family name for printing

my $perfamily = 0;
my %persequence = ();
my %perbasepair = ();
tie %persequence, "Tie::IxHash"; #keys returns elements in the same order they were added.
tie %perbasepair, "Tie::IxHash";

#Open log files:
if( ! defined $nolog ) {
    open(LOGpf,">$family_dir/ss-stats-perfamily") || die "Could not open log file $family_dir/ss-stats-perfamily - can use -n option (but you'd be mental to do this!) $!";
    open(LOGps,">$family_dir/ss-stats-persequence") || die "Could not open log file $family_dir/ss-stats-persequence - can use -n option (but you'd be mental to do this!) $!";
    open(LOGpb,">$family_dir/ss-stats-perbasepair") || die "Could not open log file $family_dir/ss-stats-perbasepair - can use -n option (but you'd be mental to do this!) $!";
#Print headers:
    printf LOGps     "FAMILY\tSEQID\tFRACTN_CANONICAL_BPs\n";
    printf LOGpf     "FAMILY\tMEAN_FRACTN_CANONICAL_BPs\tNO_SEQs\tALN_LENGTH\tNO_BPs\tmean_PID\tmax_PID\tmin_PID\tCOVARIATION\n";
    printf LOGpb      "FAMILY\tBP_COORDS\tFRACTN_CANONICAL_BPs\n";
}
else {
    printf "You're stupidly not writing to log-file!\n";
}

open( SEED, "$family_dir/SEED" ) or die;
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
my @list = $seed->each_seq();
my $ss_cons = $seed->ss_cons->getInfernalString(); #This is damned confusing!

#Make a pair table & initialise hashes:
my @table = make_pair_table($ss_cons);
my $noseqs = @list;
my $len = $table[0];
my $loop = 0;
my $nopairs = 0;
for( my $i = 1; $i<$len+1; $i++ ){
    if ($table[$i]){
	my $j = $table[$i];
	if ($i<$j){
	    my $bpstr = "$i:$j";
	    $perbasepair{$bpstr} = 0;
	    $nopairs++;
	}
    }
    else {
	$loop++;
    }
}

if ($nopairs==0 && scalar(@list)<2){
    die("FATAL: $shortname_family_dir has less than two sequences and no structure! NEEDS FIXED!\n");
}

if ($nopairs==0){
    printf STDERR "WARNING: $shortname_family_dir is boring and has no secondary structure (or the structure is munged)! Computing sequence stats only.\n";
}

if (scalar(@list)<2){
    printf STDERR "WARNING: $shortname_family_dir is boring and less than two sequences! Computing base-pair stats only.\n";
}

my @list2 = @list;
my $mean_pid = 0.0;
my $min_pid  = 1.0;
my $max_pid  = 0.0;
my $nocomps = 0;
my $nocovs = 0;
my $covariation = 0.0;
#Calculate persequence info:
foreach my $seqobj ( @list ) {
    my $seq = $seqobj->seq;
    my $seqname = $seqobj->id;
    my $start = $seqobj->start;
    my $end = $seqobj->end;
    $seqname = "$seqname/$start-$end";
    my @seq = split(//,$seq);
    $persequence{$seqname} = 0;
    #Count canonical base-pairs:
    if($nopairs>0){
	foreach my $bpposns ( keys %perbasepair ) {
	    my @bpposns = split(/:/,$bpposns);
	    my $ispair = is_complementary($seq[$bpposns[0]-1],$seq[$bpposns[1]-1]);
	    $perbasepair{$bpposns} += $ispair;
	    $persequence{$seqname} += $ispair;
	    $perfamily += $ispair;
	}
	$persequence{$seqname} = $persequence{$seqname}/$nopairs;
    }
    else {
	$persequence{$seqname} = 1.0;
    }
    
    if( ! defined $nolog ) {
	printf LOGps "$shortname_family_dir\t$seqname\t%0.4f\n", $persequence{$seqname};
    }
    else {
	printf "PERSEQUENCE: $shortname_family_dir\t$seqname\t%0.4f\n", $persequence{$seqname};
    }

    #Computing pairwise stats (PID & Covariation):
    shift(@list2);    
    foreach my $seqobj2 ( @list2 ) {
	my $seq2 = $seqobj2->seq;
	my $seqname2 = $seqobj2->id;
	my $start2 = $seqobj2->start;
	my $end2 = $seqobj2->end;
	$seqname2 = "$seqname2/$start2-$end2";
	my @seq2 = split(//,$seq2);
	my $pid = 0;
	my $len1 = 0;
	my $len2 = 0;
	for (my $i=0; $i<$len; $i++){
	    if ( is_nucleotide($seq[$i]) && is_nucleotide($seq2[$i]) && ($seq[$i] eq $seq2[$i]) ){
		$pid += 1.0;
	    }

	    if ( is_nucleotide($seq[$i]) ){
		$len1++;
	    }

	    if ( is_nucleotide($seq2[$i]) ){
		$len2++;
	    }
	}
	my $maxlen = max($len1,$len2);
	if ($maxlen>0){
	    $pid = $pid/$maxlen;
	}
	else {
	    printf STDERR "WARNING: $shortname_family_dir $seqname has length \'$len1\' and $seqname2 has length \'$len2\' in $family_dir/SEED!\n";
	}
	
	$mean_pid += $pid;
	
	if ($pid>$max_pid){
	    $max_pid = $pid;
	}

	if ($pid<$min_pid){
	    $min_pid = $pid;
	}
	$nocomps++;
	
	#Covariation statistic:
	if($nopairs>0){
	    my $PI = 0;
	    my $OM = 0;
	    foreach my $bpposns ( keys %perbasepair ) {
		my @bpposns = split(/:/,$bpposns);
		my $ispair  = is_complementary( $seq[$bpposns[0]-1], $seq[$bpposns[1]-1]);
		my $ispair2 = is_complementary($seq2[$bpposns[0]-1],$seq2[$bpposns[1]-1]);
		if (is_nucleotide($seq[$bpposns[0]-1]) && is_nucleotide($seq[$bpposns[1]-1]) && is_nucleotide($seq2[$bpposns[0]-1]) && is_nucleotide($seq2[$bpposns[1]-1]) ){
		    if ($ispair && $ispair2){
			$PI=1;
			$OM=0;
		    }
		    else {
			$OM=1;
			$PI=0;
		    }
		    my $d1 = dist($seq[$bpposns[0]-1], $seq2[$bpposns[0]-1]);
		    my $d2 = dist($seq[$bpposns[1]-1], $seq2[$bpposns[1]-1]);
		    my $d = $d1 + $d2;
		    $nocovs++;
		    $covariation += ($PI*$d - $OM*$d);
		}
	    }
	}
	
    }
    
}

if ($nocomps>0){
    $mean_pid = $mean_pid/$nocomps;
}
if($nopairs>0 && $nocomps>0){
	$covariation = $covariation/($nopairs*$nocomps); #$nocovs
}

if ($noseqs>0 && $nopairs>0){
    foreach my $bpposns ( keys %perbasepair ) {
	$perbasepair{$bpposns} = $perbasepair{$bpposns}/$noseqs;
	
	if( ! defined $nolog ) {
	    printf LOGpb "$shortname_family_dir\t$bpposns\t%0.4f\n", $perbasepair{$bpposns};
	}
	else {
	    printf "PERBASEPAIR: $shortname_family_dir\t$bpposns\t%0.4f\n", $perbasepair{$bpposns};
	}
	
	if ($perbasepair{$bpposns}<6/16){
	    printf STDERR "WARNING: $shortname_family_dir base-pair $bpposns has an FCbp of only %0.3f (<6/16). NEEDS FIXED!\n", $perbasepair{$bpposns};
	}
	
    }
    $perfamily = $perfamily/($noseqs*$nopairs);
}
else {
    $perfamily = 1.0;
}

if( ! defined $nolog ) {
    printf LOGpf "$shortname_family_dir\t%0.5f\t$noseqs\t$len\t$nopairs\t%0.3f\t%0.3f\t%0.3f\t%0.5f\n", $perfamily, $mean_pid, $max_pid, $min_pid, $covariation;
}
else {
    printf "PERFAMILY:   $shortname_family_dir\t%0.5f\t$noseqs\t$len\t$nopairs\t%0.3f\t%0.3f\t%0.3f\t%0.5f\n", $perfamily, $mean_pid, $max_pid, $min_pid, $covariation;    
}

close(LOGpb);
close(LOGpf);
close(LOGps);

######################################################################
#End of main
######################################################################


######################################################################
#make_pair_table: takes a structure string and returns an array/pair_table.
#                 pair_table[0] contains the length of the string.
#                 pair_table[1..pair_table[0]] contains 0 if no basepair exists 
#                 for this position, otherwise it contains the index for the 
#                 corresponding pair.
#                 Eg. make_pair_table(".((...))") returns:
#                 (8,0,8,7,0,0,0,3,2)
######################################################################
sub make_pair_table {

    my $str = shift;
    
    my $unbalanced = 0;
    my %bpsymbs5p3p = (
	'(' => ')',
	'<' => '>',
	'[' => ']',
	'{' => '}',
	A => 'a',
	B => 'b',
	C => 'c',
	D => 'd',
	E => 'e',
	F => 'f',
	G => 'g',
	H => 'h',
	I => 'i'
	);

    my %bpsymbs5p3p_counts = (
	'(' => 0,
	'<' => 0,
	'[' => 0,
	'{' => 0,
	A => 0,
	B => 0,
	C => 0,
	D => 0,
	E => 0,
	F => 0,
	G => 0,
	H => 0,
	I => 0
	);

    my %bpsymbs3p5p = ();
    my %bpsymbs3p5p_counts = ();
    
    foreach my $symb5p (keys %bpsymbs5p3p){
	my $symb3p = $bpsymbs5p3p{$symb5p};
	$bpsymbs3p5p{$symb3p} = $symb5p;
	$bpsymbs3p5p_counts{$symb3p} = 0;
    }
    
    my %unpairedsymbs = (
	'.' => 1,
	',' => 2,
	'-' => 3,
	':' => 4,
	'_' => 5
	);
    
    my %bpsymbs_posns = ();
    
    my @pair_table;
    my $prime5;
    my $prime3;
    my $count = 0;
    
    my @ss = split(//,$str);
    $pair_table[0]  = length($str);
    
    my $j=0;
    foreach my $char (@ss){
	$j++;
	$pair_table[$j] = 0;
	
	if ( defined( $unpairedsymbs{$char} ) ) {
	    next;
	}
	elsif ( defined( $bpsymbs5p3p{$char}) ){#Record position of open bps:
	    push( @{ $bpsymbs_posns{$char} }, $j);
	    ++$count;
	    $bpsymbs5p3p_counts{$char}++;
	}
	elsif ( defined( $bpsymbs3p5p{$char}) ){#close bp, save positions of matches:
	    my $mchar = $bpsymbs3p5p{$char};
	    $prime5 = pop( @{ $bpsymbs_posns{$mchar} } );
	    $prime3 = $j;
	    $pair_table[$prime3] = $prime5;
	    $pair_table[$prime5] = $prime3;
	    --$count;
	    $bpsymbs3p5p_counts{$char}++;
	}
       	else {
	    printf STDERR "Strange character \"$char\" in secondary structure:\n$str\n";
	}
    }
    
    foreach my $symb5p (keys %bpsymbs5p3p_counts){
	my $symb3p = $bpsymbs5p3p{$symb5p};
	my $diff = $bpsymbs5p3p_counts{$symb5p} - $bpsymbs3p5p_counts{$symb3p};
	if ($diff!=0){
	    printf STDERR "BAD EVIL AND NASTY: Unbalanced brackets in secondary structure:\n$bpsymbs5p3p_counts{$symb5p}x\'$symb5p\' and $bpsymbs3p5p_counts{$symb3p}x\'$symb3p\'\n";
	    $unbalanced = 1;
	}
    }
    
    
    if ($count != 0 || $unbalanced){
	printf STDERR "Unbalanced brackets in secondary structure:\n$str\n";
	return ();
    }    
    else {
	return @pair_table;
    }
    
}

######################################################################
#returns true if input character is a nucleotide
sub is_nucleotide {
    my $a = shift;
    $a =~ tr/a-z/A-Z/;
    
    if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
	return 1;
    }
    else {
	return 0;
    }
    
}

######################################################################
#returns true if the two input characters can form a canonical basepair
sub is_complementary {
    my $a = shift;
    my $b = shift;
    $a =~ tr/a-z/A-Z/;
    $b =~ tr/a-z/A-Z/;
    
    my $ab = $a . $b;
    
    my %canonical_basepair = (
	AU => 1,
	AT => 1,
	UA => 1,
	TA => 1,
	CG => 1,
	GC => 1,
	UG => 1,
	GU => 1,
	TG => 1,
	GT => 1,
	RY => 1,
	YR => 1,
	MK => 1,
	KM => 1,
	SS => 1,
	WW => 1
	);
    
    if ( defined($canonical_basepair{$ab} ) ) {
	return 1;
    }
    else {
        return 0;
    }
	 
}

######################################################################
#returns true if the two input characters are different (One day I'll make a generic hamming distance function...)
sub dist {
    my $a = shift;
    my $b = shift;
    $a =~ tr/a-z/A-Z/;
    $b =~ tr/a-z/A-Z/;

    if ( defined($a) && defined($b) && (length($a)==1) && (length($b)==1) && ($a ne $b) ) {
	return 1;
    }
    else {
        return 0;
    }

}

#######
sub max {
  return $_[0] if @_ == 1;
  $_[0] > $_[1] ? $_[0] : $_[1]
}

sub min {
  return $_[0] if @_ == 1;
  $_[0] < $_[1] ? $_[0] : $_[1]
}
