package RfamUtils; 

#Occasionally useful Rfam utilities

use strict;
use warnings;
use Sys::Hostname;
use File::stat;
use RfamUtils; 
use Rfam::RfamSearch;
use Cwd;
use Data::Dumper;
use Mail::Mailer;

use vars qw( @ISA
             @EXPORT
);

@ISA    = qw( Exporter );

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
        
    my (%bpsymbs_posns, @pair_table, $prime5, $prime3, %bpsymbs3p5p, %bpsymbs3p5p_counts);
    my ($count, $unbalanced) = (0,0);

    #Match up the 5' and 3' basepair simbols:
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
    
    #We also need the reverse of the above hashes (ie. Match the 3' symbols with the 5'):
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
	return \@pair_table;
    }
    
}
######################################################################
#species2shortspecies: Given a species string eg. "Homo sapiens
#                      (human)" generate a nicely formated short name
#                      with no whitespace eg. "H.sapiens".
sub species2shortspecies {
    my $species = shift;
    my $shortSpecies;
    
    if ($species=~/(.*)\s+sp\./){
	$shortSpecies = $1;
    }
    elsif ($species=~/metagenome/i or $species=~/uncultured/i){
	$species=~s/metagenome/metag\./gi;
	$species=~s/uncultured/uncult\./gi;
	my @w = split(/\s+/,$species);
	if(scalar(@w)>2){
	    foreach my $w (@w){
		$shortSpecies .= substr($w, 0, 5) . '.';
	    }
	}
	else {
	    $shortSpecies = $species;
	    $shortSpecies =~ s/\s+/_/g;
	}
    }#lots of conditions here. Need else you get some ridiculous species names.
    elsif($species=~/^(\S+)\s+(\S{4,})/ && $species!~/[\/\-\_0-9]/ && $species!~/^[a-z]/ && $species!~/\svirus$/ && $species!~/\svirus\s/ && $species!~/^Plasmid\s/i && $species!~/\splasmid\s/i){
	$shortSpecies = substr($1,0,1) . "." . $2; 
    }
    else {
	$shortSpecies = $species;
    }
    
    $shortSpecies =~ s/\s+/_/g;
    $shortSpecies =~ s/[\'\(\)\:\/]//g;
    $shortSpecies = substr($shortSpecies,0,20) if (length($shortSpecies) > 20);
    
#   H.P 
    return $shortSpecies;
}


######################################################################
#returns true if input character is a nucleotide (IUPAC codes):
sub is_nucleotide {
    my $a = shift;
    
    if (defined($a)){
	$a =~ tr/a-z/A-Z/;
    }
    
    if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
	return 1;
    }
    else {
	return 0;
    }
    
}
######################################################################
#reorder: given 2 integers, return the smallest first & the largest last:
sub reorder {
    my ($x,$y)=@_;
    
    if ($y<$x){
	my $tmp = $x;
	$x = $y;
	$y = $tmp;
    }
    return ($x,$y);
}
######################################################################
#nses2array: takes name/start-end:strand -- returns an array of (name, start, end, strand)
sub nses2array {
    my $idline=shift;
    
    if($idline=~/(\S+)\/(\d+)-(\d+):(1|-1)/){
	return ($1,$2,$3,$4);
    }
    return (0,0,0,0);
}
######################################################################
#nse2array: takes name/start-end -- returns an array of (name, start, end)
sub nse2array {
    my $idline=shift;
    
    if($idline=~/(\S+)\/(\d+)-(\d+)/){
	return ($1,$2,$3);
    }
    return (0,0,0);
}
######################################################################
#nvse2array: takes name.version/start-end -- returns an array of (name, version, start, end)
sub nvse2array {
    my $idline=shift;
    
    if($idline=~/(\S+)\.(\d+)\/(\d+)-(\d+)/){
	return ($1,$2,$3,$4);
    }
    return (0,0,0,0);
}

######################################################################
#Returns true if the coordinates for two regions ($x1, $y1) and ($x2, $y2) overlap:
# - assumes that $x1 < $y1 and $x2 < $y2.
sub overlap {
    my($x1, $y1, $x2, $y2) = @_;
    
    if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
        return 1;
    }
    else {
        return 0;
    }
}

######################################################################
#Returns the extent of overlap between two regions A=($x1, $y1) and B=($x2, $y2):
# - assumes that $x1 < $y1 and $x2 < $y2.
#
# D = 2*|A n B|/(|A|+|B|)
#
sub overlapExtent {
    my($x1, $y1, $x2, $y2) = @_;
    
    ($x1, $y1) = RfamUtils::reorder($x1, $y1);
    ($x2, $y2) = RfamUtils::reorder($x2, $y2);
    # 1.
    # x1                   y1
    # |<---------A--------->|
    #    |<------B------>|
    #    x2             y2
    #    XXXXXXXXXXXXXXXXX
    # 2.  x1                     y1
    #     |<---------A----------->|
    # |<-------------B------>|
    # x2                    y2
    #     XXXXXXXXXXXXXXXXXXXX
    # 3. x1             y1
    #    |<------A------>|
    # |<---------B--------->|
    # x2                   y2
    #    XXXXXXXXXXXXXXXXX
    # 4. x1                    y1
    #    |<-------------A------>|
    #        |<---------B----------->|
    #        x2                     y2
    #        XXXXXXXXXXXXXXXXXXXX
    my $D=0;
    my $int=0;
    my $L1=$y1-$x1+1;
    my $L2=$y2-$x2+1;
    my $minL = RfamUtils::min($L1,$L2);
    if ( ($x1<=$x2 && $x2<=$y1) && ($x1<=$y2 && $y2<=$y1) ){    #1.
	$D = $L2;
    }
    elsif ( ($x2<=$x1) && ($x1<=$y2 && $y2<=$y1) ){              #2.
	$D = $y2-$x1+1;
    }
    elsif ( ($x2<=$x1 && $x1<=$y2) && ($x2<=$y1 && $y1<=$y2) ){ #3.
	$D = $L1;
    }
    elsif ( ($x1<=$x2 && $x2<=$y1) && ($y1<=$y2) ){              #4.
	$D = $y1-$x2+1;
    }
    return $D/$minL;
}

######################################################################
#pid: compute the identity between two sequences.
sub pid {
    my $a = shift;
    my $b = shift;
    
    my @a = split(//, $a);
    my @b = split(//, $b);
    my ($sim, $lena, $lenb) = (0, 0, 0);
    
    if (scalar(@a) != scalar(@b)){
	return 0;
    }
    else {
	
 	for (my $i=0; $i<scalar(@b); $i++){
	    if ( (RfamUtils::is_nucleotide($a[$i]) || RfamUtils::is_nucleotide($b[$i])) && $a[$i] eq $b[$i] ){
		$sim++;
	    }
	    
	    if ( RfamUtils::is_nucleotide($a[$i]) ){
		$lena++;
	    }

	    if ( RfamUtils::is_nucleotide($b[$i]) ){
		$lenb++;
	    }
	}
    }
    
    my $maxlen = RfamUtils::max($lena, $lenb);
    if ($maxlen>0){
	return $sim/$maxlen;
    }
    else {
	return 0;
    }
}


######################################################################
#generate a unique-ish filename:
sub genFileName {
    my ($suffix, $try) = @_;
    $try = 0 if not defined $try;
    $suffix = 'tmpFile.' if not defined $suffix;
    my $host = hostname;
    my $random = int(rand(1000000));
    my $filename =  "$$\.$host\.$random\.$suffix";
    die "FATAL: could not generate a unique filename after $try attempts. Last try was \'$filename\'. Too many files in your area?" if $try > 1000;
    if (-e $filename){
	$filename =  genFileName($suffix, $try++);
    }
    return $filename;
}

######################################################################
#Max and Min
#max
sub max {
  return $_[0] if @_ == 1;
  $_[0] > $_[1] ? $_[0] : $_[1]
}

#min
sub min {
  return $_[0] if @_ == 1;
  $_[0] < $_[1] ? $_[0] : $_[1]
}
######################################################################
#Max and Min for arrays:
#max
sub maxA {
    my $max = $_[0];
    foreach my $a (@_){
	$max = max($max, $a) if isNumeric($a);
    }
    return $max;
}

#min
sub minA {
    my $min = $_[0];
    foreach my $a (@_){
	$min = min($min, $a) if isNumeric($a);
    }
    return $min;
}

######################################################################
sub isNumeric {
    my $num = shift;
    if ($num=~/^-?\d+\.?\d*$/) { 
	return 1; 
    }
    else {
	return 0;
    }
}

######################################################################
sub isInteger {
    
    my $test = shift;
    if (defined($test) && $test =~ /^(\-|\+)?(0|[1-9])\d*$/){
	return 1;
    }
    else {
	return 0;
    }
}

######################################################################
#function for checking the status of jobs on the farm - returns true when the job is finished:
#eg.
#wait_for_farm("rf$blastpname", "blast")
sub wait_for_farm {
    
    my ($bjobname, $jobtype, $nobjobs, $toKill, $debug) = @_;
    
    my $wait = 1;
    my $bjobcount = 1;
    my $bjobinterval = 15;
    my $jobs = $nobjobs;
    my $startTime = time();
    my $nowTime=0;
    my $firstRun;
    while($wait){
	
	sleep($bjobinterval); 
	
	$jobs = 0;
	my ($bjpend, $bjrun)  = (0, 0);
	open(S, "bjobs -J $bjobname |") or die "FATAL: failed to open pipe \'bjobs -J $bjobname |\'\n[$!]";
	while(<S>) {
	    $bjpend++ if (/PEND/);
	    $bjrun++ if (/RUN/);
	    if (/RUN/ && not defined $firstRun){
		$firstRun = time();
	    }
	}
	close(S);
	$jobs = $bjpend + $bjrun;
	
	$jobs = $nobjobs if $jobtype eq 'cmcalibrate' && $bjrun == 1;
	
	if ($jobs < int($nobjobs*(1-0.95)) ){#Once 95% of jobs are finished, check frequently.
	    $bjobinterval=15;
	}      
	elsif ($jobs < int($nobjobs*(1-0.90)) ){#Once 90% of jobs are finished, check a little more frequently.
	    $bjobinterval=15 + int(log($bjobcount/2)); 
	}
	else {#otherwise check less & less frequently (max. interval is ~300 secs = 5 mins).
	    if ($bjobinterval<300){ $bjobinterval = $bjobinterval + int(log($bjobcount));}
	}
	
	if($jobs){
	    $nowTime = time();
	    my @humanTime = gmtime($nowTime-$startTime);
	    printf STDERR "There are $jobs $jobtype jobs of $nobjobs still running after $bjobcount checks (PEND:$bjpend,RUN:$bjrun). Check interval:$bjobinterval secs [TotalTime=%dD:%dH:%dM:%dS].\n", @humanTime[7,2,1,0]; 
	    if(defined $toKill && defined $firstRun && ($nowTime-$firstRun)>$toKill){
		printf STDERR "WARNING: this job is taking too long therefore I am backing out. Hopefully it'll restart nicely.\n";
		#########
		#DEBUG -- MOSTLY TO TRACK DOWN MPI PROBLEMS:
		#MAKES A FILE FOR EACH MPIRUN NODE CONTAINING STRACE INFO AND PS.
		if (defined $debug){
		    printf STDERR "DEBUG start!\n";
		    
		    my $user =  'pg5';
		    $user =  getlogin() if defined getlogin();
		    my $pwd = getcwd;
		    print "DEBUG: pwd=$pwd, user=$user\n";
		    open(S, "bjobs -J $bjobname |") or die "FATAL: failed to open pipe \'bjobs -J $bjobname |\'\n[$!]";
		    while(<S>) {
			if(/(\d+)\*(bc-\S+)/){
			    my $node = $2;
			    print "node: $node\n";
			    open(B, "ssh $node ps -U $user -o pid,user,\%cpu,\%mem,cmd --sort cmd |") or warn "FATAL: failed to open pipe [ssh $node ps -U $user -o pid,user,\%cpu,\%mem,cmd --sort cmd]\n[$!]";
			    while(my $b=<B>) {
				print $b;
				if($b=~/(\d+)\s+\S+\s+(\S+)\s+(\S+).+$debug/){
				    # 6456 pg5      99.7  0.1 /software/rfam/share/infernal-1.0/bin/cmcalibrate --mpi -s 1 CM
				    my $debugFile="\$HOME/$$\.mpi.debug.$debug.$1.$node";
				    print("ssh $node \'strace -p $1 -o $debugFile\'");
				}
			    }	    
			    close(B);
			}
		    }
		    close(S);
		}
		printf STDERR "DEBUG end!\n";
		#DEBUG ENDS
		########
		system("bkill -J $bjobname ") and die "FATAL: failed to run [bkill -J $bjobname]\n[$!]";
	    }
	}else{
	    $wait = 0;
	}
	$bjobcount++;
    }
    
    return 1;
    
}

######################################################################
#secs2human: given a number of seconds returns the total time in a human readable string: 
sub secs2human {
    my $secs = shift;
    my @humanTime = gmtime($secs);
    my $humanTime = sprintf "TotalTime=%dD:%dH:%dM:%dS", @humanTime[7,2,1,0];
    return $humanTime;
}

######################################################################

sub slurpDesc {
    
    my $descFile=shift;
    $descFile='DESC' if not defined $descFile;
    my %desc;
    
    my @tags = qw(
    **
AC
AU
BM
CC
DE
DR
GA
ID
NC
PI
RN
SE
SS
TC
TP
WK);
    
    my @litTags = qw(
RA
RL
RM
RT
);

    foreach my $t (@tags){
	$desc{$t}='';
    }
    
    my %litTags;
    foreach my $t (@litTags){
	$litTags{$t}='';
    }
    
    open(DE, "< $descFile") or die "FATAL: failed to open $descFile file!\n[$!]";
    my ($refNo, @refs);
    while(<DE>){
	chomp;
	if(/^RN\s+\[(\d+)\]/){
	    $refNo=int($1)-1 if defined $1 && $1>0 && length($1)>0;
	}
	elsif(/^(R\S)\s+(.+)/ && defined $refNo){
	    next if length($refNo) == 0 or length($1) == 0 or length($2) == 0;
	    if (not defined $refs[$refNo]{$1}){
		$refs[$refNo]{$1} = $2;
	    }
	    else {
		$refs[$refNo]{$1} .= "\n$1   " . $2;
	    }
	}
	elsif(/^(R\S)\s+(.+)/ && not defined $refNo){
	    print STDERR "WARNING: your references are munged, you need an RN line first!\n";
	}
	elsif(/^(\S{2})\s+(.+)/){#elsif(/^(\S{2})\s+(.+)\s+/ || /^(\S{2})\s+(.+)/){
	    if (defined $desc{$1} && length($desc{$1}) == 0){
		$desc{$1}.=$2;
	    }
	    elsif(defined $desc{$1}){
		$desc{$1}.="\n$1   " . $2;
	    }
	    print STDERR "WARNING: Strange Desc File line, [$1] is an undefined tag: [$_]\n" if not defined $desc{$1};
	}
    }
    print STDERR "WARNING: your DESC file contains no references!\n" if not defined $refNo;
    print STDERR "WARNING: your DESC file is missing SO DR lines!\n" if ($desc{'DR'} !~ /SO/);
    print STDERR "WARNING: your DESC file is missing GO DR lines!\n" if ($desc{'DR'} !~ /GO/);
    print STDERR "WARNING: your DESC file is missing a WK entry!\n\tFORMAT is \47DR   http://en.wikipedia.org/wiki/RNA\47\n" if  $desc{'WK'} !~ /http:\/\/en.wikipedia.org\/wiki\/(\S+)/;
    
    
    $desc{'RN'}=\@refs if defined $refNo;
    return \%desc;
}

###################################
#Add a check that essential tags have been written:
sub writeDesc {
    my $desc = shift;
    my $fp = shift;
    my @tags = qw(
AC
ID
DE
PI
AU
SE
SS
GA
TC
NC
TP
BM
DR
RN
CC
WK
**
);

    my @litTags = qw(
RA
RL
RM
RT
);
    my %allowed;
    foreach my $t (@tags, @litTags){
	$allowed{$t}='';
    }
    
    foreach my $t (@tags){
	
	#References are awkward:
	if ($t eq 'RN' && defined $desc->{'RN'} && length($desc->{'RN'}) > 0){
	    for (my $i=0; $i< scalar(@{$desc->{'RN'}}); $i++ ){
		my $ref = ${$desc->{'RN'}}[int($i)];
		printf $fp "RN   [%d]\n", $i+1;
		foreach my $rTag (qw(RM RT RA RL) ){
		    if (defined $ref->{$rTag} && length($ref->{$rTag})>0){
			$ref->{$rTag} =~ s/\t/ /g;
			print $fp "$rTag   " . $ref->{$rTag} . "\n";
			print STDERR "WARNING: a tab character or a terminal whitespace is screwing up one of your $rTag lines in your DESC file!\n" if ($ref->{$rTag}=~/\s$/ || $ref->{$rTag}=~/\t/);
		    }
		    else {
			printf STDERR "WARNING: no $rTag entry for reference [%d]\n", $i+1;
		    }
		}
	    }
	}
	else {
	    #All the other tags are fairly easy to take care of:
	    if (defined $desc->{$t} && length($desc->{$t})>0){
		$desc->{$t} =~ s/\t/ /g;
		if ($t eq 'GA' || $t eq 'TC' || $t eq 'NC'){#Thresholds are slightly awkward:
		    if(isNumeric($desc->{$t})){
			printf $fp "$t   %0.2f\n", $desc->{$t};
		    }
		    else {
			printf $fp "$t   %s\n", $desc->{$t};
		    }
		}
		else {
		    print $fp "$t   " . $desc->{$t} . "\n";
		}
		print STDERR "WARNING: a tab character or a terminal whitespace is screwing up one of your $t lines in your DESC file!\n" if ($desc->{$t}=~/\s$/ || $desc->{$t}=~/\t/);
	    }
	}
    }
}

###################################
#Add a check that essential tags have been written:
sub writeDescString {
    my $desc = shift;
    my $string='';
    my @tags = qw(
AC
ID
DE
PI
AU
SE
SS
GA
TC
NC
TP
BM
DR
RN
CC
WK
);

    my @litTags = qw(
RA
RL
RM
RT
);
    my %allowed;
    foreach my $t (@tags, @litTags){
	$allowed{$t}='';
    }
    
    my %seen; 
    foreach my $t (@tags){
	
	$seen{$t}=1;
	#References are awkward:
	if ($t eq 'RN' && defined $desc->{'RN'} && length($desc->{'RN'}) > 0){
	    for (my $i=0; $i< scalar(@{$desc->{'RN'}}); $i++ ){
		my $ref = ${$desc->{'RN'}}[int($i)];
		$string .= sprintf "#=GF RN   [%d]\n", $i+1;
		foreach my $rTag (qw(RM RT RA RL) ){
		    if (defined $ref->{$rTag} && length($ref->{$rTag})>0){
			$ref->{$rTag} =~ s/\t/ /g;
			
			my @rT = split(/\n$rTag\s+/, $ref->{$rTag});
			foreach my $rT (@rT){
			    $string .= "#=GF $rTag   " . $rT . "\n";
			}
			print STDERR "WARNING: a tab character or a terminal whitespace is screwing up one of your $rTag lines in your DESC file!\n" if ($ref->{$rTag}=~/\s$/ || $ref->{$rTag}=~/\t/);
		    }
		    else {
			printf STDERR "WARNING: no $rTag entry for reference [%d]\n", $i+1;
		    }
		}
	    }
	}
	else {
	    #All the other tags are fairly easy to take care of:
	    if (defined $desc->{$t} && length($desc->{$t})>0){
		$desc->{$t} =~ s/\t/ /g;
		if ($t eq 'GA' || $t eq 'TC' || $t eq 'NC'){#Thresholds are slightly awkward:
		    if(isNumeric($desc->{$t})){
			$string .= sprintf "#=GF $t   %0.2f\n", $desc->{$t};
		    }
		    else {
			$string .= "#=GF $t   " . $desc->{$t} . "\n";
		    }
		}
		else {
		    my @descT = split(/\n$t\s+/, $desc->{$t});
		    foreach my $descT (@descT){
			$string .= "#=GF $t   " . $descT . "\n";
		    }
		}
		print STDERR "WARNING: a tab character or a terminal whitespace is screwing up one of your $t lines in your DESC file!\n" if ($desc->{$t}=~/\s$/ || $desc->{$t}=~/\t/);
	    }
	}
    }
    
    my @compulsoryTags = qw(
AC
ID
DE
AU
SE
SS
GA
TC
NC
TP
BM
DR
WK
);
    
    foreach my $tag (@compulsoryTags){
	print STDERR "WARNING: you are missing the compulsory tag: [$tag] in your DESC file!" if not defined $seen{$tag};
    }
    
    return $string;
}


###################################
sub generateDesc {
    
    my %desc;
    my %user2name = (
	agb  => 'Bateman A',
	jd7  => 'Daub J',
	pg5  => 'Gardner PP',
	sgj  => 'Griffiths-Jones SR',
	io3  => 'Osuch I',
	aw10 => 'Wilkinson A'
	);
    my $user =  getlogin() if defined getlogin();
    $user =  'pg5' if not defined $user2name{$user}; #This is soo evil! ;-)
    
    die "FATAL: DESC file exists but may be empty, delete or fix script..." if -e 'DESC';
    
    my $pwd = getcwd; 
    my @pwd = split(/\//,$pwd); 
    my $id=pop(@pwd);
    $id = 'ncRNA' if not defined $id or length($id)==0;
    $desc{'DE'}=$id;
    $desc{'AU'}=$user2name{$user};
    $desc{'SE'}=$user2name{$user};
    $desc{'SS'}=$user2name{$user};
    $desc{'GA'}=20.00;
    $desc{'TC'}=20.00;
    $desc{'NC'}=20.00;
    $desc{'TP'}='Gene;';
    $desc{'BM'}='cmbuild -F CM SEED; cmcalibrate -s 1 CM';
    my $dbsize  = Rfam::RfamSearch::getDbSize();
    $dbsize  = int($dbsize/1000000);    
    $desc{'BM'}.="\nBM   cmsearch -Z $dbsize -E 1000 --toponly CM SEQDB";
    $desc{'WK'}='http://en.wikipedia.org/wiki/Non-coding_RNA';
    
    return \%desc;
}


######################################################################

#Given two pointers to score vectors, return a pointer to hash (keys
#are scores/thresholds, values are MCC's), return the highest & lowest
#optimal threshold
sub scores2mcc {
    my $true = shift;
    my $false = shift;
    
    #Sort?
    my @true = sort { $a <=> $b } (@$true);
    my @false = sort { $a <=> $b } (@$false);

    
    #Find ranges:
    my $maxT = maxA(@true);
    my $maxF = maxA(@false);
    my $minT = minA(@true);
    my $minF = minA(@false);
    my $max  = max($maxT, $maxF);
    my $min  = min($minT, $minF);
    
    #print "(maxT, maxF)=($maxT, $maxF), (minT, minF)=($minT, $minF), max=$max, min=$min true=[@true] false=[@false]\n";
    
    $min = int($min-1);
    $max = int($max+1);
    
    #print "minNew=$min, maxNew=$max\n";
    
    my (%mcc, %highLow);
    #$highLow{'high'} = (max(thresh), max(mcc))
    #$highLow{'low'}  = (min(thresh), max(mcc))
    push(@{$highLow{'high'}}, (0,0.00) );
    push(@{$highLow{'low'}}, (0,0.00));
    for (my $thr = $min; $thr<=$max; $thr++){
	#compute TP, TN, FP and FN:
	my ($tp, $tn, $fp, $fn) = (0,0,0,0);
	
        true: foreach my $t (@true){
	  if ($t<$thr){
		$fn++;
	    }
	    else {
		last true;
	    }
	}
	$tp = scalar(@true) - $fn;
	
	false: foreach my $f (@false){
	    if ($f<$thr){
		$tn++;
	    }
	    else {
		last false;
	    }
	}
	$fp = scalar(@false) - $tn;
	$mcc{$thr} = calcMCC($tp, $tn, $fp, $fn);
	
	@{$highLow{'high'}} = ($thr,$mcc{$thr}) if ($mcc{$thr} >= ${$highLow{'high'}}[1]) && ($thr > ${$highLow{'high'}}[0]);
	@{$highLow{'low' }} = ($thr,$mcc{$thr}) if ($mcc{$thr} >= ${$highLow{'low' }}[1]); 
	#printf "thr: $thr, mcc: %0.3f, (tp, tn, fp, fn) = ($tp, $tn, $fp, $fn)\n", $mcc{$thr};
    }
    
    return (\%mcc, \%highLow);
}


#Compute the Mathew's correlation coefficient from TP, TN, FP & FN counts:
sub calcMCC {
    my ($tp, $tn, $fp, $fn) = @_;
    #Array contains TP, TN, FP and FN:
    
    my $denom = sqrt( ($tp + $fp)*($tp + $fn)*($tn + $fp)*($tn + $fn) );
    my $mcc = 0.00;
    $mcc = ($tp * $tn - $fp * $fn)/$denom if $denom > 0;
    return $mcc;
}

######################################################################
#youngerThan(file1, file2): test if file1 is younger than file2 
sub youngerThan {
    my ($file1, $file2) = @_;
    my ($t1,$t2) = (0,0);
    $t1 = stat($file1)->mtime if -e $file1;
    $t2 = stat($file2)->mtime if -e $file2;
    
    if($t1>$t2 or $t1==0 or $t2==0){
	return 1;
    }
    else {
	return 0;
    }
}
######################################################################
#mailUser: sends an email to the specified user:
sub mailUser {
  my($user, $title, $message) = @_;
	my %header = ( 	To => "$user\@sanger.ac.uk",
					From => "$user\@sanger.ac.uk",
					Subject => $title );
  
	my $mailer = Mail::Mailer->new;
	$mailer->open(\%header);
  print $mailer $message;
  $mailer->close;
#  exit(1);
  return 1;
}


######################################################################

=head1 AUTHOR

Paul Gardner, C<pg5@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2009: Genome Research Ltd.

Authors: Paul Gardner (pg5@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;



