#!/usr/local/bin/perl

use strict;
use Rfam::RfamAlign;
use Getopt::Long;

my ($compalign, $help);
&GetOptions (
    'h' => \$help,
    'compalign=s'=> \$compalign
); 

if ($help ){
$help = 1;
}
if ($help){
print STDERR <<EOF;
Useage: compRNAalign.pl trusted.ali test.ali
EOF
        exit;
}

my $sensitivity;
my $specificity;
my $j=0;
my $TP;
my $FP;
my $TN;
my $FN;
my $family = $ARGV[0];
my $comparison = $ARGV[1];
&compare($family, $comparison);
my $pass=0;

sub compare {
my $family  = shift @_;
my $comparison = shift @_;
my @outseq;
my @structure;
chomp $comparison;
	while ($pass < 2){
	my $aln = Rfam::RfamAlign -> new();
		if ($pass ==0){
		open (SEED, "$family") or die "Can't open trusted SEED";
		$aln -> read_stockholm(*SEED);
		close SEED;
		}
		elsif($pass == 1) {
		open (SEED, "$comparison") or die "Can't open test SEED";
		$aln -> read_stockholm(*SEED);
		close SEED;
		}
	my $ss_gapped = $aln -> ss_cons();
	$aln -> each_seq();
	my @ss;
		foreach my $pair ($ss_gapped-> eachPair()){
			my $left = $pair -> left;
			$left = $left - 1;
			my $right = $pair -> right;
			$right = $right -1;
			$ss[$left] = "<";
			$ss[$right] = ">";
		}
		foreach my $seq ( sort{ $a->id cmp $b->id || $a->start <=> $b->start } $aln -> each_seq()) {
		my $qwe = ($seq -> seq());
		my @ary = split( //, $qwe );
		my $i=-1;

			foreach my $el (@ary){
			chomp $el;
			$i++;
	      			if ($el ne "."){
				push (@{$outseq[$j]}, $el);
				push (@{$structure[$j]}, $ss[$i]);
				}
	
				else {
				next
				}
			}
			push (@{$outseq[$j]}, "\n");
			push (@{$structure[$j]}, "\n");
		}
		$pass++;
		$j++;
	}

	my $count = 0;
	my $TP;
	my $FP;
	my $TN;
	my $FN;
	
 	foreach my $el (@{$structure [0]}) {
		
		if ($el eq "\n"){
#		print "\n";
		}
		elsif ((($el eq @{$structure[1]}[$count]) ) && (($el eq "<") || ($el eq ">"))){
#		print "$el";
		$TP++;
		}
		elsif ((($el ne @{$structure[1]}[$count]) ) && (($el eq "<") || ($el eq ">"))){
			if (( @{$structure[1]}[$count] ne "<") || (@{$structure[1]}[$count] ne ">"))
			{
#			print "!";
			$FN++;
			}
			else{
#			print ".";
			$TN++;
			}
			
		}
		elsif ( ($el ne @{$structure[1]}[$count]) && ( ( @{$structure[1]}[$count] eq "<") || (@{$structure[1]}[$count] eq ">") ) ){
			if (($el ne "<") || ($el ne ">")){
#			print "@{$outseq[1]}[$count]";
			$FP++;
			}
		}
		else{
#		print ".";
		$TN++;
		}	
		$count++;
	}
	print "\n";
	if ($TP !=0){
	$sensitivity = (($TP)/($TP + $FN));
	$specificity = (($TP)/($TP + $FP));
	}
	else {
	$sensitivity = 0;
	$specificity = 0;
	}
#	print "$family\n";
	print "SENSITIVITY: $sensitivity\n";
	print "SPECIFICITY: $specificity\n\n";
}

