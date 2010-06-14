#! /software/bin/perl -w

use strict;
use Bio::Pfam::HMM::HMMResultsIO;
use Date::Object;
use Getopt::Long;

#The algorithm
#Take two overlapping families and check if overlap still exists.
#If it does, see if the highest bits score overlap for first family is <= $bits_threshold.
#If it is, and if raising the threshold would lose <= ($allowed_loss + num overlaps between the 2 families), do the pfmake.
#If it isn't <= $bits_threshold, or it is <= $bits_threshold but a pfamke loses > ($allowed_loss + num overlaps between 2 families), move to the next highest bits score for that family and repeat the process.
#Then do the same for the second family (of course check overlap still exists first)


#To run redirect to a filename

my $allowed_loss;   #Actual allowed loss with be this number + number of overlaps between the two families
my $bits_threshold; #Only consider overlaps which are below this threshold
my $num_overlaps;   #Only consider resolving the overlaps for a family if total number of overlaps for a family less than this number


my ($status_dir,$families_dir,$help);
GetOptions( 'status_dir=s'     => \$status_dir,
            'family_dir=s'     => \$families_dir,
	    'loss=i'           => \$allowed_loss,
	    'bits_threshold=i' => \$bits_threshold,
	    'num_overlaps=i'   => \$num_overlaps,
	    'h'                => \$help,
	    'help'             => \$help);

if (! $status_dir){
    warn "You need to specify a status directory (which contains the overlap files)\n";
    $help=1;
}

if (! $families_dir){
    warn "You need to specify a family directory\n";
    $help=1;
}


if($help) {
    help();
}


#These are the default parameters, use these unless user says otherwise
$allowed_loss = "25" unless($allowed_loss);
$bits_threshold = "30" unless($bits_threshold);
$num_overlaps = "200" unless($num_overlaps);

print STDERR "Allowing (number of overlap between two families + $allowed_loss) to be lost from a family\n";
print STDERR "Only working on families with less than $num_overlaps overlaps\n";
print STDERR "Only working on sequence overlaps where the bit score is less than $bits_threshold\n\n";

print "#Allowing (number of overlap between two families + $allowed_loss) to be lost from a family\n";
print "#Only working on families with less than $num_overlaps overlaps\n";
print "#Only working on sequence overlaps where the bit score is less than $bits_threshold\n\n";



my $date = new Date::Object( time() );
#my $file1 = $root . $date->year.$date->month.$date->day."overlaps.overlaps";
#my $file2 = $root . $date->year.$date->month.$date->day."overlaps.familyOverlaps";
my $file1 = $status_dir . "20100607"."overlaps.overlaps";
my $file2 = $status_dir . "20100607"."overlaps.familyOverlaps";

die "Can't find [$file1]\n" unless(-s $file1);
die "Can't find [$file2]\n" unless(-s $file2);

my (%data, %ga, %clan, %desc, %possible_clan, %ignore);
my (%fam_overlaps, %total_overlaps);

#Parse overlaps file and store overlaps
print STDERR "Parsing $file1...\n";
open(FH, $file1) or die "$!";
while(<FH>) {
    if(/(PF\d\d\d\d\d)\s?\:? (\S+).+?(\S+\.\d+)\/(\d+)\-(\d+) \((\S+) bits\).+(PF\d\d\d\d\d) (\S+) \S+\.\d+\/(\d+)\-(\d+) \((\S+) bits\)/) {

	my ($fam1, $type1, $seq, $st1, $en1, $bits1, $fam2, $type2, $st2, $en2, $bits2) = ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);

        #Store number of overlaps between any two families, and total number of overlaps between two families
        $fam_overlaps{"$fam1:$fam2"}++;
        $fam_overlaps{"$fam2:$fam1"}++;

	$total_overlaps{$fam1}++;
	$total_overlaps{$fam2}++;


	#Some fams have already been killed - make sure family directories exist
        next unless(-e "$families_dir/$fam1" and -e "$families_dir/$fam2");

        #See if seq and ga thresholds are the same	
	$ga{$fam1} = check_desc($fam1, \%clan, \%ignore) unless(exists($ga{$fam1}));
	$ga{$fam2} = check_desc($fam2, \%clan, \%ignore) unless(exists($ga{$fam2}));


	#Store the overlap data in a hash
	unless($ignore{$fam1}) {  #Ignore fams with diff seq and dom ga
            next if($clan{$fam1} and $clan{$fam2} and $clan{$fam1} eq $clan{$fam2}); #If they're in the same clan it's not an overlap          

	    if($type1 eq "ALIGN") { #Don't store any SEED family overlaps
		$data{$fam1}{$fam2}{$bits1}{'start'}=$st1;
		$data{$fam1}{$fam2}{$bits1}{'end'}=$en1;                    
		$data{$fam1}{$fam2}{$bits1}{'seq'}=$seq;

		$data{$fam1}{$fam2}{$bits1}{'ov_start'}=$st2;
		$data{$fam1}{$fam2}{$bits1}{'ov_end'}=$en2;                    
	    }
	}
     
        
	unless($ignore{$fam2}) { #Ignore fams with diff seq and dom ga
	    next if($clan{$fam1} and $clan{$fam2} and $clan{$fam1} eq $clan{$fam2}); #If they're in the same clan it's not an overlap

	    if($type2 eq "ALIGN") { #Don't store any SEED family overlaps
		$data{$fam2}{$fam1}{$bits2}{'start'}=$st2;
		$data{$fam2}{$fam1}{$bits2}{'end'}=$en2;
		$data{$fam2}{$fam1}{$bits2}{'seq'}=$seq;

		$data{$fam2}{$fam1}{$bits2}{'ov_start'}=$st1;
		$data{$fam2}{$fam1}{$bits2}{'ov_end'}=$en1;

	    }       
	}
    }
    else {
        die "Unrecognisd line in $file1\n[$_]\n";
    }
}
close FH;
print STDERR "parsed $file1\n";



my $count=0;
open(RESOLVED, ">resolved.$$") or die "Couldn't open resolved.$$ for writing\n";


		
my %already;
open(FH2, $file2) or die "$!";
while(<FH2>) {
    if(/^(PF\d\d\d\d\d)\s+\S+\s+\S+\s+\S+\s+\S+\s+\S+\s+\S+\s+\S+\s+\S+\s+\S+\s+(\S+)\s+(\S+)/) {
       my ($fam1, $total_overlaps, $fams2) = ($1, $2, $3);
       #$total_overlaps is total number of overlaps for a family

       next unless($total_overlaps <= $num_overlaps);
       next unless($total_overlaps > 0);

       my @fams2 = split(/,/, $fams2);
  
       foreach my $fam2 (@fams2) { 
           if($fam2 =~ /(PF\d\d\d\d\d)/) {
               $fam2 = $1;
	   }
           else {
               die "Unrecognised family format in [$fam2] line [$_]\n";
	   }
           my ($status_fam1, $status_fam2);


	   #See if raising threshold of fam1 will work
	   if(exists($data{$fam1}{$fam2})) { 

	       foreach my $bits (sort { $b <=> $a } keys(%{$data{$fam1}{$fam2}}) ) {

		   my $seq = $data{$fam1}{$fam2}{$bits}{'seq'};   
            	                  
		   my $a1 = check_align($fam1, $seq, $data{$fam1}{$fam2}{$bits}{'start'}, $data{$fam1}{$fam2}{$bits}{'end'});
                   my $a2 = check_align($fam2, $seq, $data{$fam1}{$fam2}{$bits}{'ov_start'}, $data{$fam1}{$fam2}{$bits}{'ov_end'});


                   unless($a1 and $a2) { #Yay - overlap has already been resolved                                      
                       print "$fam1:$fam2 [$seq] ($bits bits) already resolved\n" unless(exists($already{"$fam1:$fam2"}));
                       $already{"$fam1:$fam2"}=1;
                       $already{"$fam2:$fam1"}=1;
		       last;
		   }

                   #$fam_ov is number of overlaps between the two familes
		   my $fam_ov = $fam_overlaps{"$fam1:$fam2"};

                   my $allowed_plus = $fam_ov + $allowed_loss;
		   if($bits > $bits_threshold) {
		       next;
		   }

		   print "Going to try raise threshold on $fam1 to resolve $fam1:$fam2 overlap [$seq] ($bits bits)\n";           

		   $status_fam1 = resolve($fam1, $ga{$fam1}, $bits, $allowed_plus, $fam2, \$count, $seq, $fam_ov, $total_overlaps{$fam1} );
		   last if($status_fam1);
	       }
	   }

           #Then see if raising threshold of fam2 will work
	   if(exists($data{$fam2}{$fam1})) { 

	       foreach my $bits (sort { $b <=> $a } keys(%{$data{$fam2}{$fam1}}) ) {

		   my $seq = $data{$fam2}{$fam1}{$bits}{'seq'};                  

		   my $a1 = check_align($fam2, $seq, $data{$fam2}{$fam1}{$bits}{'start'}, $data{$fam2}{$fam1}{$bits}{'end'});
		   my $a2 = check_align($fam1, $seq, $data{$fam2}{$fam1}{$bits}{'ov_start'}, $data{$fam2}{$fam1}{$bits}{'ov_end'});


                   unless($a1 and $a2) { #Yay - overlap has already been resolved              
                       print "$fam2:$fam1 [$seq] ($bits bits) already resolved\n" unless(exists($already{"$fam1:$fam2"})) ;
                       $already{"$fam1:$fam2"}=1;
                       $already{"$fam2:$fam1"}=1; 
		       last;
		   }

		   my $fam_ov = $fam_overlaps{"$fam1:$fam2"};
                   my $allowed_plus = $fam_ov + $allowed_loss;

		   if($bits > $bits_threshold) {
		       next;
		   } 

		   print "Going to try raise threshold on $fam2 to resolve $fam2:$fam1 overlap [$seq] ($bits bits)\n";           

		   $status_fam2 = resolve($fam2, $ga{$fam2}, $bits, $allowed_plus, $fam1, \$count, $seq, $fam_ov, $total_overlaps{$fam2});
		   last if($status_fam2);
	       }
	   }
           #If neither worked then lets tell the user we couldn't resolve any of the overlaps
           unless($status_fam1 or $status_fam2) { 
	       print "Can't resolve $fam1:$fam2 overlap(s)\n";		  
	   }
       }
   }
}
close FH2;
close RESOLVED;
print "Resolved [$count] overlaps\n";
      
sub check_desc {
    #subroutine to see if seq and domain ga are diff
    my ($fam, $clan, $ignore) = @_;
    my $same_ga="0";

    open(DESC, "$families_dir/$fam/DESC") or die "$!";
    while(<DESC>) {
	if(/^GA\s+(\S+)\s+(\S+)\;/) {
	    if($1 == $2) {                          
		$same_ga=$1;
	    }
	}
	elsif(/^CL\s+(\S+)/) {
	    $$clan{$fam}=$1;
	    last;
	}
    }  			        
    close DESC;

    unless($same_ga) {
	$$ignore{$fam}=1;
	print STDERR "$fam has different seq and dom thresholds so will ignore the overlap(s) in this family\n";
    }
    return($same_ga);
}


sub check_align {
    #subroutine checks to see if a sequence still belongs to a family
    my ($fam, $seq, $st, $en) = @_;
    my $align="";

    open(ALIGN, "$families_dir/$fam/ALIGN") or print STDERR "Couldn't open $families_dir/$fam/ALIGN $!";
    while(<ALIGN>){
	if(/^$seq\/(\d+)\-(\d+)/) {
	    if( ($1 <= $st and $2 >= $en) ) {
                $align=1;
                last;
	    }
	}
    }
    close ALIGN;
    return($align);
}

 

sub resolve {
    #subroutine tries to resolve an overlap by raising the threshold
    my ($fam, $ga, $ov_bits, $allowed_loss, $overlap_fam, $count, $seq, $fam_ov, $total_ov) = @_;

    my $new_thresh = $ov_bits + 0.1;

    my $HMMResultsIO = Bio::Pfam::HMM::HMMResultsIO->new;
    my $HMMResults = $HMMResultsIO->parsePFAMOUT("$families_dir/$fam/PFAMOUT") or die "Couldn't open $families_dir/$fam/PFAMOUT $!";

    my ($new, $old);

    foreach my $seqId ( keys %{ $HMMResults->seqs } ) {

	#Count number of domains with current threshold
	if ( $HMMResults->seqs->{$seqId}->bits >= $ga ) {
	    foreach my $unit ( @{ $HMMResults->seqs->{$seqId}->hmmUnits } ) {
		if ( $unit->bits >= $ga ) {
		    $old++;
		}
	    }

            #Count number of domains if threshold is raised
	    if ( $HMMResults->seqs->{$seqId}->bits >= $new_thresh ) {
		foreach my $unit ( @{ $HMMResults->seqs->{$seqId}->hmmUnits } ) {
		    if ( $unit->bits >= $new_thresh ) {
			$new++;
		    }
		}
	    }
	}
    }
  
    my $diff = $old-$new;

    if($diff <= $allowed_loss and $diff > 0) {

	print RESOLVED "Resolving $fam:$overlap_fam [$seq] overlap by raising threshold from $ga to $new_thresh and losing $diff domain(s)\n";
	print STDOUT "Resolving $fam:$overlap_fam [$seq] overlap by raising threshold from $ga to $new_thresh and losing $diff domain(s)\n";
	print STDERR "Resolving $fam:$overlap_fam [$seq] overlap by raising threshold from $ga to $new_thresh and losing $diff domain(s)\n";

        chdir "$families_dir/$fam" or die "Couldn't chdir into $families_dir/$fam $!\n";
        system("pfmake.pl -t $new_thresh") and die "\'pfmake.pl -t $new_thresh\' failed to run\n";
 
	$$count++;
        return ("1");
    }
    else {
	print "Ignoring $fam\:$overlap_fam overlap [$seq]    bitscore_$fam:$ov_bits   total\_$fam\:$total_ov   $fam\/$overlap_fam\:$fam_ov   Loss:$diff\n";
        return ("");
    }
}



sub help {

  print STDERR <<EOF;

This script is used at release time to resolve overlaps by raising thresholds.

Usage: $0 -status_dir <status dir> -families_dir <family dir> > outfile

Addional options:

  -loss <n>           : Allow a maximum of ('loss' + number of overlaps 
                        between the two families) to be lost (default 25)
  -bits_threshold <n> : Work on sequence overlaps where the bit score is 
                        less that 'bits_threshold' (default 30)
  -num_overlaps <n>   : Work on families with less than 'num_overlaps' 
                        overlaps (default 200)

EOF
  exit;



}
