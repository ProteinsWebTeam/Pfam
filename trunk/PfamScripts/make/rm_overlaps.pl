#! /usr/local/bin/perl

# A script to remove overlap sequence from an alignment

my $align=shift @ARGV;
my $overlap=shift @ARGV;;

if (! -e $overlap and ! -e $align){
    print STDERR <<"EOF";
Usage: $0 <align file> <overlap file>

This program will take a stockholm/mul format alignment and 
overlap file and remove any overlapping alignment segments 
and print the result to STDOUT. 

EOF
exit 0;
}

if (! -s $align){die "align file $align has no size!"}
if (! -e $overlap){die "overlap file $overlap does not exist!"}

my %id;
open (OVERLAP, "$overlap") or die "Cannot open $overlap file";
while(<OVERLAP>){
    if (/\[(\S+)\].*SEED with/){
	$id{$1}=1;
#	print "Yay $1\n";
    } elsif (/\[(\S+)\].*JALIGN with/){
	$id{$1}=1;
#	print "Yay $1\n";
    } elsif (/\[(\S+)\].*FULL with/){
	$id{$1}=1;
#	print "Yay $1\n";
    } else {
	warn "Unrecognized line in overlap file $_";
    }
}
close OVERLAP;

if ($id{"B9KL46"}){
    warn "B9KL46 exists\n";
}#

#print %id,"\n";

open (ALIGN, "$align") or die "Cannot open $align file";
while(<ALIGN>){
    if (/^(\S+).\d+\/\d+-\d+\s+\S+$/){
	my $name=$1;

#	if ($name eq "B9KL46"){
    #die "B9KL46 exists in alignment\n";
	#}

	if ($id{$name}){
	 #   print;
	} else {
	  #  print "removing $name\n";
	    print;
	}
    } else {
	warn "Unrecognized line $_";
    }
}
close OVERLAP;


