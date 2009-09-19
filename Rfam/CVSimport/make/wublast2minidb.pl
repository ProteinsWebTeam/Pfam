#!/software/bin/perl -w
###
use strict;
use Getopt::Long;
use SeqFetch;
my ($blastfile, $outfile, $limits, $window, $blastdatabase, $help);

$limits=2000;
$window = 200;

&GetOptions( 
    "f|b|blastfile=s"   => \$blastfile,
    "o|file=s"          => \$outfile,
    "l|limits=s"        => \$limits,
    "w|window=s"        => \$window,
    "d|db|database=s"   => \$blastdatabase,
    "h|help"            => \$help 
    );

if ($help){
    &help();
    exit(1);    
}

if (!defined($blastfile)){
    $blastfile = shift @ARGV;
}

if (!(-e $blastfile) || !(-e glob( "$blastdatabase*"))){
    die "FATAL: problem with blastfile = [$blastfile] or blastdatabase = [$blastdatabase], needs fixed\n";
    #&help();
    #exit(1);
}

if (!defined($outfile)){
    $outfile = $blastfile . ".minidb";
}

my (%forward, %reverse, %seq_list);
open(BLAST,"$blastfile") || die "cannot open $blastfile\n[$!]";

#Check top line for "# BLASTN".
#Naughty! HEHEHEHE.
my $head = `head -n 1 $blastfile`;
#my $tail = `tail -n 1 $blastfile`;
#if ($head !~ /\# BLASTN/ || $tail !~ /\# EXIT/){
if ($head !~ /\# BLASTN/){#Checking the tails doesn't seem to work consistently - needs checking...
    open(BLASTERR,">$blastfile\.error") || die "cannot open $blastfile\.error\n[$!]";
    print BLASTERR "head = [$head]\n";
    close(BLASTERR);
    die "FATAL: poorly formatted BLAST output!\nhead=[$head]";
}

while (my $line = <BLAST>){
    
    next if !defined($line);
    if ($line =~ /^\#\s+FATAL/){
	open(BLASTERR,">$blastfile\.error") || die "cannot open $blastfile\.error\n[$!]";
	print BLASTERR "fatal line = [$line]\n";
	close(BLASTERR);
	die "FATAL: your BLAST job returned a fatal error!\n[$line]";
    }
    next if $line =~ /^\#/;
    
    chomp($line);
    my @bline = split(/\t/,$line);
    
    if(scalar(@bline) == 22 ){
	my ($name, $strand, $start, $end);
	if ($bline[1]  =~ /\S+/){$name   = $bline[1];}  else {printf STDERR "name error    =\'$bline[1]\' in $blastfile \n"};
	if ($bline[16] =~ /\S+/ && $bline[19] =~ /\S+/){$strand = $bline[16]*$bline[19];}  else {printf STDERR "strand error =\'$bline[16]\' and \'$bline[19]\' \n"};
	if ($bline[20] =~ /\d+/){$start = $bline[20];} else {printf STDERR "sstart error  =\'$bline[20]\' in $blastfile \n"};
	if ($bline[21] =~ /\d+/){$end   = $bline[21];} else {printf STDERR "send error    =\'$bline[21]\' in $blastfile \n"};
	my $already = 0;
	
	#print "$name\t$start\t$end\t$strand\n";

#                     BLAST HIT
#-------------------XXXXXXXXXXXXXX----------------------------
#        |<----------------------|
#                   window
#                   |---------------------->|
#                            window
	
	my $tmp = $end;
	$end   =  max($start + $window,$end);  #This looks strange but is correct. See diagram above.
	$start =  min($tmp   - $window,$start);  #Ditto.
	$start = 1 if( $start < 1 );
	
	if( exists($seq_list{$name}) ) {
	    
	    foreach my $se ( @{ $seq_list{$name} } ){
		my $start2  = $se->{'start'};
		my $end2    = $se->{'end'};
		
		my $ov = overlap($start, $end, $start2, $end2);
		if ($ov && $strand == $se->{'strand'}){
		    my $mn0 = min($start, $start2);
		    my $mn1 = min($end, $end2);
		    my $mx0 = max($start, $start2);
		    my $mx1 = max($end, $end2);
		    
		    $se->{'start'} = min($mn0, $mn1);
		    $se->{'end'}   = max($mx0, $mx1);
		    $already = 1;
		}
	    }
	}
	
	if( !$already ) {

	    push( @{ $seq_list{$name} }, { 'start'  => $start,
					   'end'    => $end,
					   'strand' => $strand} );

	}
    }
}

my $seqcounter=0;
my $filecount=0;
#merging overlaps that may have grown together and writing to file if seqcounter is over the limit
foreach my $n ( keys %seq_list ){
    my $iindx=0;
    foreach my $se ( @{ $seq_list{$n} } ){
	my $start = $se->{'start'};
	my $end = $se->{'end'};
	my $strand = $se->{'strand'};
	my @temp = @{ $seq_list{$n} };
	for (my $j = $iindx+1; $j < scalar( @{ $seq_list{$n} } ); $j++ ){
	    last if !defined(${ $seq_list{$n} }[$j]);
	    my $se2 = ${ $seq_list{$n} }[$j];
	    my $start2  = $se2->{'start'};
	    my $end2    = $se2->{'end'};
	    my $strand2 = $se2->{'strand'};
	    
	    my $ov = overlap($start, $end, $start2, $end2);
	    if ($ov && $strand == $strand2){
		my $mn0 = min($start, $start2);
		my $mn1 = min($end, $end2);
		my $mx0 = max($start, $start2);
		my $mx1 = max($end, $end2);
		
		$start = min($mn0, $mn1);
		$end   = max($mx0, $mx1);
		${ $seq_list{$n} }[$iindx] = $se;
		splice(@{ $seq_list{$n} }, $j, 1);
		$j--;
	    }
	}
	
	if ($strand==1){
	    push( @{ $forward{$n} }, { 'start'  => $start,
				       'end'    => $end,
				       'strand' => $strand} );
	}
	else {
	    push( @{ $reverse{$n} }, { 'start'  => $start,
				       'end'    => $end,
				       'strand' => $strand} );
	}
	
	#print "$n\t$start\t$end\t$strand ####MERGE####\n";

	if ($seqcounter>$limits-1){
	    
	    if (defined($outfile)){
		open(OUT,">$outfile\.$filecount\.$seqcounter") || die "cannot open $outfile\.$filecount\.$seqcounter\n[$!]";
	    }
	    else {
		*OUT = *STDOUT;
	    }
	    
	    SeqFetch::fetchSeqs(\%forward, $blastdatabase, 0, \*OUT);
	    SeqFetch::fetchSeqs(\%reverse, $blastdatabase, 1, \*OUT);
	    undef %forward;
	    undef %reverse;
	    $seqcounter=0;
	    $filecount++;
	    close(OUT);
	}

	$iindx++;
	$seqcounter++;
    }
}

if ($seqcounter>0){ 
    if (defined($outfile)){
	open(OUT,">$outfile\.$filecount\.$seqcounter") || die "cannot open $outfile\.$filecount\.$seqcounter\n[$!]";
    }
    else {
	*OUT = *STDOUT;
    }
    
    SeqFetch::fetchSeqs(\%forward, $blastdatabase, 0, \*OUT);
    SeqFetch::fetchSeqs(\%reverse, $blastdatabase, 1, \*OUT);
}

exit(0);

######################################################################
sub overlap {
    my($x1, $y1, $x2, $y2) = @_;
    
    if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
        return 1;
    }
    else {
        return 0;
    }
}

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
sub help {
    print STDERR <<EOF;

wublast2minidb.pl: this script takes as input wu-blast output and a blast database (formatted with xdformat). 
                   It merges/collapses overlapping blast hits and fetches unique sequences from the db, creating 
		   minidbs. 

USAGE:   wublast2minidb.pl <options>
OPTIONS:       -h                  show this help
	       -f|-b|-blastfile <str> Required: wu-blast output filename
	       -d|-db|-database <str> Required: Name of the database wu-blast output was derived from
	       -o|-file         <str> Output filename for writing sequences corresponding to collapsed 
	                              blast hits [Default: \42blastfilename\42.minidb]
	       -l|-limits       <num> Maximum number of sequences to write to file [Default: $limits]
	       -w|-window       <num> Maximum expected length for a hit - shorter hits are extended to 
	                              this if possible, usually derived from the CM [Default: $window]

DESCRIPTION:

Input is a wu-blast output file in tabular (mformat=3) format.  This
script merges/collases overlapping blast hits and fetches the
sequences from the database - extended short hits to a maximal length
defined by the \42window\42 parameter. These sequence are written to
minidb files, if there are more hits than defined by the limits
variable ($limits) then these are split across many files. Minidb filenames
are in the format:
\42outfile\.filecount\.seqcounter\42
Where outfile is a prefix defined by the user, filecount is an integer
between 0 and N where N is mod(number-of-unique-hits,$limits) and
seqcounter is the number of sequences written to the file.

EOF
}
