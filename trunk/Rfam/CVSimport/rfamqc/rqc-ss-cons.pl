#!/software/bin/perl -w 
#-d:DProf 
#dprofpp -u tmon.out

# A program to run checks on alignment consistency with structure.

use strict;
use Rfam;
use RfamQC;
use Rfam::RfamAlign;
use Getopt::Long;
#use Data::Dumper; #Great for printing diverse data-structures.
use Tie::IxHash;  #Use to return hash keys in the order they were added
use bigint;
use Math::BigFloat;
use Cwd;

my( $nolog, $help, $file );

&GetOptions(
    "f=s"  => \$file,
    "n!"   => \$nolog,
    "h|help" => \$help
    );

##$|++;
if (defined($help) ) {
    help();
    exit(1);
}

my $family_dir;

if( $#ARGV == 0 ) {
    $family_dir = shift; # family dir to use
    #print "$#ARGV family_dir: $family_dir\n";
    
}
else {
    $family_dir = getcwd;
}

#print "ARGV $#ARGV family_dir: $family_dir\n";

if ( !(-e "$family_dir/SEED")){
    print "FATAL: missing essential input file: [$family_dir/SEED]\n";
    help();
    exit(1);    
}

my @family_dir = split(/\//, $family_dir);
my $shortname_family_dir = pop(@family_dir); # family name for printing

print "family_dir: $shortname_family_dir\n";
#print "shortname family dir: $shortname_family_dir\n";

my (%persequence, %perbasepair, %persequence_lens, %composition, %perbasepaircovariation, %perbasepaircovcounts);
my $perfamily=0;
tie %persequence, "Tie::IxHash"; #keys returns elements in the same order they were added.
tie %perbasepair, "Tie::IxHash";

#Open log files:
if( ! defined $nolog ) {
    open(LOGpf,">$family_dir/ss-stats-perfamily") || die "Could not open log file $family_dir/ss-stats-perfamily - can use -n option (but you'd be mental to do this!) $!";
    open(LOGps,">$family_dir/ss-stats-persequence") || die "Could not open log file $family_dir/ss-stats-persequence - can use -n option (but you'd be mental to do this!) $!";
    open(LOGpb,">$family_dir/ss-stats-perbasepair") || die "Could not open log file $family_dir/ss-stats-perbasepair - can use -n option (but you'd be mental to do this!) $!";
#Print headers:
    printf LOGps     "FAMILY\tSEQID\tFRACTN_CANONICAL_BPs\tLEN\tFRAC_A\tFRAC_C\tFRAC_G\tFRAC_U\tMAX_DINUC\tCG_CONTENT\n";
    printf LOGpf     "FAMILY\tMEAN_FRACTN_CANONICAL_BPs\tCOVARIATION\tNO_SEQs\tALN_LENGTH\tNO_BPs\tNO_NUCs\tmean_PID\tmax_PID\tmin_PID\tmean_LEN\tmax_LEN\tmin_LEN\tFRACTN_NUCs\tFRAC_A\tFRAC_C\tFRAC_G\tFRAC_U\tMAX_DINUC\tCG_CONTENT\n";
    printf LOGpb      "FAMILY\tBP_COORDS\tFRACTN_CANONICAL_BPs\tCOVARIATION\n";
}
else {
    printf "You're stupidly not writing to log-file!\n";
}

if (defined($file)){
    open( SEED, "$file" ) or die ("FATAL: Couldn't open $file!\n $!\n");
}
else {
    open( SEED, "$family_dir/SEED" ) or die ("FATAL: Couldn't open SEED!\n $!\n");
}

my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
close(SEED);
my @list = $seed->each_seq();
my $ss_cons = $seed->ss_cons->getInfernalString(); #This is damned confusing!

#Make a pair table & initialise hashes:
my @table = make_pair_table($ss_cons);
my $noseqs = scalar(@list);
my $len = $table[0];
my ($loop, $nopairs) = (0,0);
for( my $i = 1; $i<$len+1; $i++ ){
    if ($table[$i]){
	my $j = $table[$i];
	if ($i<$j){
	    my $bpstr = "$i:$j";
	    $perbasepair{$bpstr} = 0;
	    $perbasepaircovariation{$bpstr} = 0;
	    $perbasepaircovcounts{$bpstr} = 0;
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
my ($min_pid, $max_pid, $nocomps, $nocovs, $covariation, $mean_length, $min_length, $max_length, $nonucleotides) = (1.0, 0.0, 0, 0, 0.0, 0.0, inf, 0, 0);
my $mean_pid = Math::BigFloat->new(0.00);

#Calculate persequence info:
foreach my $seqobj ( @list ) {
    my $seq = uc($seqobj->seq);
    my $seqname = $seqobj->id . "/" . $seqobj->start . "-" . $seqobj->end;
    my @seq = split(//,$seq);
    my %seq_composition = ();
    
    #Compute, number of nucleotides, composition & seq length stats:
    my $nuccount = 0;
    for (my $i=0; $i<$len; $i++){
	my $c = $seq[$i];
	if ( is_nucleotide($c) ){
	    $nonucleotides++;
	    $nuccount++;
	    
	    if ( defined($composition{$c}) ){
		$composition{$c}++;
		$seq_composition{$c}++;
	    }
	    else {
		$composition{$c}=1;
		$seq_composition{$c}=1;
	    }
	}
    }
    
    $persequence_lens{$seqname} = $nuccount;
    $mean_length += $nuccount;
    if ($min_length>$nuccount){
	$min_length=$nuccount;
    }
    
    if ($max_length<$nuccount){
	$max_length=$nuccount;
    }
    
    $persequence{$seqname} = 0;
    #Count the canonical base-pairs in each sequence:
    if($nopairs>0){
	foreach my $bpposns ( keys %perbasepair ) {
	    my @bpposns = split(/:/,$bpposns);
	    my $ispair = is_complementary($seq[$bpposns[0]-1],$seq[$bpposns[1]-1]);
	    $perbasepair{$bpposns} += $ispair;
	    $persequence{$seqname} += $ispair;
	    $perfamily += $ispair;
	}
	my $a  = new Math::BigFloat $persequence{$seqname};
	my $b  = new Math::BigFloat $nopairs;
	$persequence{$seqname} = new Math::BigFloat $a/$b; 
    }
    else {
	$persequence{$seqname} = 1.0;
    }

    my ($seq_refmononuccounts, $seq_maxdinucstr, $seq_maxdinuc, $seq_CGcontent) = compute_compositions( \%seq_composition, $nuccount);
    
    if( ! defined $nolog ) {
	printf LOGps "$shortname_family_dir\t$seqname\t%0.4f\t%d \t%0.3f\t%0.3f\t%0.3f\t%0.3f\t$seq_maxdinucstr:%0.3f\t%0.3f\n", $persequence{$seqname}, $persequence_lens{$seqname}, $seq_refmononuccounts->{'A'}, $seq_refmononuccounts->{'C'}, $seq_refmononuccounts->{'G'}, $seq_refmononuccounts->{'U'}, $seq_maxdinuc, $seq_CGcontent;
    }
    else {
	printf "PERSEQUENCE: $shortname_family_dir\t$seqname\t%0.4f\t%d \t%0.3f\t%0.3f\t%0.3f\t%0.3f\t$seq_maxdinucstr:%0.3f\t%0.3f\n", $persequence{$seqname}, $persequence_lens{$seqname}, $seq_refmononuccounts->{'A'}, $seq_refmononuccounts->{'C'}, $seq_refmononuccounts->{'G'}, $seq_refmononuccounts->{'U'}, $seq_maxdinuc, $seq_CGcontent;
    }

    #Computing pairwise stats (PID & Covariation):
    shift(@list2);    #shift current list1 seq off list2 - don't want to compare the sequence to itself.
    foreach my $seqobj2 ( @list2 ) {
	my $seq2 = uc($seqobj2->seq);
	my $seqname2 = $seqobj2->id . "/" . $seqobj2->start . "-" . $seqobj2->end;
	
	my @seq2 = split(//,$seq2);
	my $pid  = 0;
	my $len1 = 0;
	my $len2 = 0;
	#Compute sequence identity measures (Could've used alistat, it does too much rounding tho):
	for (my $i=0; $i<$len; $i++){
	    my $check1 = is_nucleotide($seq[$i]);
	    my $check2 = is_nucleotide($seq2[$i]);
	    
	    if ( $check1 && $check2 && ($seq[$i] eq $seq2[$i]) ){
		$pid += 1.0;
	    }

	    if ( $check1 ){
		$len1++;
	    }

	    if ( $check2 ){
		$len2++;
	    }
	}
	my $maxlen = max($len1,$len2);
	if ($maxlen>0){
	    my $a  = new Math::BigFloat $pid;
	    my $b  = new Math::BigFloat $maxlen;
	    $pid = new Math::BigFloat $a/$b; 
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
		my $nuccount = is_nucleotide(  $seq[$bpposns[0]-1]) + is_nucleotide( $seq[$bpposns[1]-1]);
		my $nuccount2 = is_nucleotide($seq2[$bpposns[0]-1]) + is_nucleotide($seq2[$bpposns[1]-1]);
		
		if ($nuccount>0 && $nuccount2>0){#Only compute covariation if there is at least 1 nucleotide 
                                                 #in each pair. Agnostic metric wrt gap-gap comparisons:
		    my $ispair  = is_complementary( $seq[$bpposns[0]-1], $seq[$bpposns[1]-1]);
		    my $ispair2 = is_complementary($seq2[$bpposns[0]-1],$seq2[$bpposns[1]-1]);
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
		    $perbasepaircovariation{$bpposns} += ($PI*$d - $OM*$d);
		    $perbasepaircovcounts{$bpposns}++;
		}
	    }
	}
    }
}

if ($nocomps>0){
    my $a  =  $mean_pid;
    my $b  = new Math::BigFloat $nocomps;
    $mean_pid = new Math::BigFloat $a/$b;
}

#if($nopairs>0 && $nocomps>0){
if($nocovs>0){
    my $a  = new Math::BigFloat $covariation;
    my $b  = new Math::BigFloat $nocovs;
    $covariation = new Math::BigFloat $a/$b; #($nopairs*$nocomps); #$nocovs
}

if ($noseqs>0){
    my $a  = new Math::BigFloat $mean_length;
    my $b  = new Math::BigFloat $noseqs;
    $mean_length = new Math::BigFloat $a/$b; 
}

my $fracnuc;
if($noseqs>0 && $len>0){
    my $a  = new Math::BigFloat $nonucleotides;
    my $b  = new Math::BigFloat $noseqs*$len;
    $fracnuc = new Math::BigFloat $a/$b; 
}

#my $msg = Dumper(%composition);
#print $msg;

my ($refmononuccounts, $maxdinucstr, $maxdinuc, $CGcontent) = compute_compositions( \%composition, $nonucleotides);
my $threshold = new Math::BigFloat (new Math::BigFloat 6)/(new Math::BigFloat 16);

#Print data to file and warnings for dodgy pairs:
if ($noseqs>0 && $nopairs>0){
    foreach my $bpposns ( keys %perbasepair ) {

	my $a  = new Math::BigFloat $perbasepair{$bpposns};
	my $b  = new Math::BigFloat $noseqs;
	$perbasepair{$bpposns} = new Math::BigFloat $a/$b;

	my $c  = new Math::BigFloat $perbasepaircovariation{$bpposns};
	my $d  = new Math::BigFloat $perbasepaircovcounts{$bpposns};
	$perbasepaircovariation{$bpposns}  = new Math::BigFloat $c/$d;
	
	if( ! defined $nolog ) {
	    printf LOGpb "$shortname_family_dir\t$bpposns\t%0.4f\t%0.4f\n", $perbasepair{$bpposns}, $perbasepaircovariation{$bpposns};
	}
	else {
	    printf "PERBASEPAIR: $shortname_family_dir\t$bpposns\t%0.4f\t%0.4f\n", $perbasepair{$bpposns}, $perbasepaircovariation{$bpposns};
	}
	
	if ($perbasepair{$bpposns}<$threshold){
	    printf STDERR "WARNING: $shortname_family_dir base-pair $bpposns has an FCbp of only %0.3f (<6/16). NEEDS FIXED!\n", $perbasepair{$bpposns};
	}
	
    }
    my $e  = new Math::BigFloat $perfamily;
    my $f  = new Math::BigFloat $noseqs*$nopairs;
    $perfamily =  new Math::BigFloat $e/$f;
}
else {
    $perfamily = 1.0;
}

if( ! defined $nolog ) {
    printf LOGpf "$shortname_family_dir\t%0.5f\t%0.5f\t$noseqs\t$len\t$nopairs\t$nonucleotides\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%d\t%d\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t$maxdinucstr:%0.3f\t%0.3f\n", $perfamily, $covariation, $mean_pid, $max_pid, $min_pid, $mean_length, $max_length, $min_length, $fracnuc, $refmononuccounts->{'A'}, $refmononuccounts->{'C'}, $refmononuccounts->{'G'}, $refmononuccounts->{'U'}, $maxdinuc, $CGcontent;    
}
else {
    printf "PERFAMILY:   $shortname_family_dir\t%0.5f\t%0.5f\t$noseqs\t$len\t$nopairs\t$nonucleotides\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%d\t%d\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t$maxdinucstr:%0.3f\t%0.3f\n", $perfamily, $covariation, $mean_pid, $max_pid, $min_pid, $mean_length, $max_length, $min_length, $fracnuc, $refmononuccounts->{'A'}, $refmononuccounts->{'C'}, $refmononuccounts->{'G'}, $refmononuccounts->{'U'}, $maxdinuc, $CGcontent;
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
#                 pair_table[1..pair_table[0]] contains 0's if no basepair exists 
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
	    next; #Boring unpaired region.
	}
	elsif ( defined( $bpsymbs5p3p{$char}) ){#Record position of open bps:
	    push( @{ $bpsymbs_posns{$char} }, $j);
	    ++$count;
	    $bpsymbs5p3p_counts{$char}++;
	}
	elsif ( defined( $bpsymbs3p5p{$char}) ){#close bp, save positions of matches in pair_table:
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
    
    #Check basepair symbols are all matched:
    foreach my $symb5p (keys %bpsymbs5p3p_counts){
	my $symb3p = $bpsymbs5p3p{$symb5p};
	my $diff = $bpsymbs5p3p_counts{$symb5p} - $bpsymbs3p5p_counts{$symb3p};
	if ($diff!=0){
	    printf STDERR "BAD EVIL AND NASTY: Unbalanced brackets in secondary structure:\n$bpsymbs5p3p_counts{$symb5p}x\'$symb5p\' and $bpsymbs3p5p_counts{$symb3p}x\'$symb3p\'\n";
	    $unbalanced = 1;
	}
    }
    
    
    if ($count != 0 || $unbalanced){
	printf STDERR "Unbalanced basepair symbols in secondary structure:\n$str\n";
	return ();
    }    
    else {
	return @pair_table;
    }
    
}

######################################################################
#returns true if input character is a nucleotide (IUPAC codes):
sub is_nucleotide {
    my $a = shift;
    
#    if (defined($a)){
#	$a =~ tr/a-z/A-Z/;
#    }
	
    if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
	return 1;
    }
    else {
	return 0;
    }
    
}

######################################################################
#returns true if the two input characters can form a canonical/watson-crick basepair
sub is_complementary {
    my $a = shift;
    my $b = shift;

#    if (defined($a)){
#	$a =~ tr/a-z/A-Z/;
#    }

#    if (defined($b)){
#	$b =~ tr/a-z/A-Z/;
#    }
    
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

#    if (defined($a)){
#	$a =~ tr/a-z/A-Z/;
#    }

#    if (defined($b)){
#	$b =~ tr/a-z/A-Z/;
#    }
    
    if ( defined($a) && defined($b) && (length($a)==1) && (length($b)==1) && ($a ne $b) ) {
	return 1;
    }
    else {
        return 0;
    }

}

######################################################################
#
sub compute_compositions {
    my $nuc_counts = shift;
    my $total = shift;
    
    my %nuc_counts = %$nuc_counts;
    #This is a monstrous hash of hashes that standardises any possible IUPAC char:
    my %IUPAC2counts = (
	A => {
	    A => 1,
	    C => 0,
	    G => 0,
	    U => 0,
	},
	C => {
	    A => 0,
	    C => 1,
	    G => 0,
	    U => 0,
	},
	G => {
	    A => 0,
	    C => 0,
	    G => 1,
	    U => 0,
	},
	U => {
	    A => 0,
	    C => 0,
	    G => 0,
	    U => 1,
	},
	T => {
	    A => 0,
	    C => 0,
	    G => 0,
	    U => 1,
	},
	R => {
	    A => 0.5,
	    C => 0,
	    G => 0.5,
	    U => 0,
	},
	Y => {
	    A => 0,
	    C => 0.5,
	    G => 0,
	    U => 0.5,
	},
	S => {
	    A => 0,
	    C => 0.5,
	    G => 0.5,
	    U => 0,
	},
	W => {
	    A => 0.5,
	    C => 0,
	    G => 0,
	    U => 0.5,
	},
	M => {
	    A => 0.5,
	    C => 0.5,
	    G => 0,
	    U => 0,
	},
	K => {
	    A => 0,
	    C => 0,
	    G => 0.5,
	    U => 0.5,
	},
	B => {
	    A => 0,
	    C => 0.3,
	    G => 0.3,
	    U => 0.3,
	},
	D => {
	    A => 0.3,
	    C => 0,
	    G => 0.3,
	    U => 0.3,
	},
	H => {
	    A => 0.3,
	    C => 0.3,
	    G => 0,
	    U => 0.3,
	},
	V => {
	    A => 0.3,
	    C => 0.3,
	    G => 0.3,
	    U => 0,
	},
	N => {
	    A => 0.25,
	    C => 0.25,
	    G => 0.25,
	    U => 0.25,
	},
	);
        
    my %mononuc2dinuc = (
	A => ['R', 'W', 'M'],
	C => ['Y', 'S', 'M'],
	G => ['R', 'S', 'K'],
	U => ['Y', 'W', 'K']
	);
    
    my %mononuccounts = (
	A => 0,
	C => 0,
	G => 0,
	U => 0
	);
    
    my %dinuccounts = (
	R => 0,
	Y => 0,
	S => 0,
	W => 0,
	M => 0,
	K => 0
	);
    
    
    foreach my $nucchar (keys %nuc_counts){
	foreach my $n (keys %{ $IUPAC2counts{$nucchar} } ){
	    if ( defined( $IUPAC2counts{$nucchar}{$n} ) ){
		$mononuccounts{$n} += $IUPAC2counts{$nucchar}{$n}*$nuc_counts{$nucchar};
		#print "mononuccounts:$n = $mononuccounts{$n} nuc_counts:$nucchar $nuc_counts{$nucchar} IUPAC2counts:$nucchar:$n $IUPAC2counts{$nucchar}{$n} total:$total\n";
	    }
	}
    }
    
    foreach my $nuc (keys %mononuc2dinuc){
	foreach my $dinuc ( @{ $mononuc2dinuc{$nuc} } ){
	    $dinuccounts{$dinuc} += $mononuccounts{$nuc};
	}
    }


    foreach my $mononuc ( keys %mononuccounts ) {
	#printf "mononuc:$mononuc mononuccounts:$mononuccounts{$mononuc} total:$total mononuccounts:%0.4f\n", $mononuccounts{$mononuc};
	my $a  = new Math::BigFloat $mononuccounts{$mononuc};
	my $b  = new Math::BigFloat $total;
	#my $rat  = new Math::BigFloat $a/$b;
	$mononuccounts{$mononuc} = new Math::BigFloat $a/$b;

	#printf "mononuc:$mononuc mononuccounts:$mononuccounts{$mononuc} total:$total mononuccounts:%0.4f rat:$rat\n", $mononuccounts{$mononuc};
    }
    
    my $maxdinuc = new Math::BigFloat "0.00";
    my $maxdinucstr =  "";
    foreach my $dinuc ( keys %dinuccounts ) {
	my $a  = new Math::BigFloat $dinuccounts{$dinuc};
	my $b  = new Math::BigFloat $total;
	#print "dinuc:$dinuc dinuccounts:$dinuccounts{$dinuc} total:$total\n";
	$dinuccounts{$dinuc} = new Math::BigFloat $a/$b;
	#print "dinuc:$dinuc dinuccounts:$dinuccounts{$dinuc} total:$total\n";
	if ($maxdinuc < $dinuccounts{$dinuc}) {
	    $maxdinuc = $dinuccounts{$dinuc};
	    $maxdinucstr = $dinuc;
	}
	#print "dinuc:$dinuc dinuccounts:$dinuccounts{$dinuc} total:$total maxdinucstr:$maxdinucstr maxdinuc:$maxdinuc\n";
    }
    
#    my $msg = Dumper(%IUPAC2counts, "\n\n", %mononuc2dinuc);
#    print "$msg\n\n";

#    $msg = Dumper(%mononuccounts, "\n\n", %dinuccounts);
#    print "$msg\n\n";
 
   
    return (\%mononuccounts, $maxdinucstr, $maxdinuc, $dinuccounts{'S'} );
    
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

######################################################################
sub help {
    
    print STDERR <<EOF;

rqc-ss-con.pl - calculate statistics for how well the structure annotation 
                corresponds with the SEED sequences.

Usage:    rqc-ss-con.pl <directory>
Options:
  -n             no log file
  -f <stkfile>   calculate statistics on \47stkfile\47 instead of SEED

EOF
}
