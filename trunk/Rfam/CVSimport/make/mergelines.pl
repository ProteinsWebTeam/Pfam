#!/software/bin/perl -w

use strict;
use Getopt::Long;
my ($blastfile, $outfile, $help);

&GetOptions( "f|b|blastfile=s"   => \$blastfile,
	     "o|file=s"          => \$outfile,
	     "h|help"          => \$help );

if (!defined($blastfile)){
    $blastfile = shift @ARGV;
}

if (!(-e $blastfile) || $help){
    &help();
    exit(1);
}

my %seq_list;
open(BLAST,"$blastfile") || die "cannot open $blastfile\n[$!]";

if (defined($outfile)){
    open(OUT,">$outfile") || die "cannot open $outfile\n[$!]";
}
else {
    *OUT = *STDOUT;
}

while (my $line = <BLAST>){
    
    next if( !defined($line) || $line =~ /^\#/ );
    
    chomp($line);
    my @bline = split(/\t/,$line);
    
    if(scalar(@bline) == 22 ){
	my ($name, $strand, $start, $end);
	if ($bline[1]  =~ /\S+/){$name   = $bline[1];}  else {printf STDERR "name error    =\'$bline[1]\' in $blastfile \n"};
	if ($bline[16] =~ /\S+/ && $bline[19] =~ /\S+/){$strand = $bline[16]*$bline[19];}  else {printf STDERR "strand error =\'$bline[16]\' and \'$bline[19]\' \n"};
	if ($bline[20] =~ /\d+/){$start = $bline[20];} else {printf STDERR "sstart error  =\'$bline[20]\' in $blastfile \n"};
	if ($bline[21] =~ /\d+/){$end   = $bline[21];} else {printf STDERR "send error    =\'$bline[21]\' in $blastfile \n"};
	my $already = 0;
	if( exists($seq_list{$name}) ) {
	    
	    foreach my $se ( @{ $seq_list{$name} } ){
		if( $se->{'strand'} == $strand and $start <= $se->{'start'} and $se->{'start'} <= $end ) {
		    $se->{'start'} = $start;
		    $already = 1;
		}
		
		if( $se->{'strand'} == $strand and $start <= $se->{'end'} and $se->{'end'} <= $end ) {
			$se->{'end'} = $end;
			$already = 1;
		}
		
		if( $se->{'strand'} == $strand and $se->{'start'} <= $start and $end <= $se->{'end'} ) {
		    $already = 1;
		    next;
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

foreach my $n ( keys %seq_list ){
    foreach my $se ( @{ $seq_list{$n} } ){
	my $start = $se->{'start'};
	my $end = $se->{'end'};
	my $strand = $se->{'strand'};
	print OUT "$n\t$strand\t$start\t$end\n";
    }
}

sub help {
    print STDERR <<EOF;

mergelines.pl: this is 

USAGE:   mergelines.pl <options>
OPTIONS:       -h                  show this help
	       -f <str>            blast filename
	       -o <str>            output filename
DESCRIPTION:
Where blastfile is a wu-blast output file in tabular (mformat=3) format.
This script merges overlapping blast hits, returning just 3 columns:
1. SEQID
2. STRAND
3. START COORDINATE
4  END COORDINATE

EOF
}
