#! /usr/bin/env perl 

use strict;
use warnings;
use Bio::Pfam::HMM::HMMResultsIO;
use Date::Object;
use Date::Calc qw[Add_Delta_Days Today];
use Getopt::Long;
use File::Copy;

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
my $perc_thresh;    #Only allow threshold changes which result in keeping $perc_thresh % of the original members in ALIGN (calculated by doing new_align/(old_align - overlaps_resolved)*100)

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
$allowed_loss = "50" unless($allowed_loss);
$bits_threshold = "30" unless($bits_threshold);
$num_overlaps = "20" unless($num_overlaps);
$perc_thresh = 90; 

print STDERR "Allowing (number of overlap between two families + $allowed_loss) to be lost from a family\n";
print STDERR "Only working on families with less than $num_overlaps overlaps\n";
print STDERR "Only working on sequence overlaps where the bit score is less than $bits_threshold\n";
print STDERR "Ensuring $perc_thresh % of orignal members in ALIGN are kept after raising threshold\n\n";

print "#Allowing (number of overlap between two families + $allowed_loss) to be lost from a family\n";
print "#Only working on families with less than $num_overlaps overlaps\n";
print "#Only working on sequence overlaps where the bit score is less than $bits_threshold\n\n";
print "#Ensuring $perc_thresh % of orignal members in ALIGN are kept after raising threshold\n";


#Find the newest overlap file
my ($year, $month, $day) = Today();
$day = "0" . $day if($day !~ /\d\d/);  #Add 0 in front of day if single digit
$month = "0" . $month if($month !~ /\d\d/);  #Add 0 in front of month if single digit

#First look for a file with todays data
my $date_of_file = $year.$month.$day;
my $file1 = "$status_dir/$date_of_file" . "overlaps.overlaps.filtered";
my $file2 = "$status_dir/$date_of_file" . "overlaps.familyOverlaps";

#If overlaps file with todays date doesn't exist, look for most recent file
my $c;
my $day_counter=0;
until(-s $file1 and -s $file2) {
  $day_counter--;
  ($year, $month, $day) = Add_Delta_Days(Today(), $day_counter);
 
  $day = "0" . $day if($day !~ /\d\d/);  #Add 0 in front of day if single digit
  $month = "0" . $month if($month !~ /\d\d/);  #Add 0 in front of month if single digit

  my $date_of_file = $year.$month.$day;
  
  $file1 = "$status_dir/$date_of_file" . "overlaps.overlaps.filtered";
  $file2 = "$status_dir/$date_of_file" . "overlaps.familyOverlaps";

  $c++;
  die "Can't locate overlaps.filtered file and familyOverlaps file, no overlaps file has been created in the last 10 days" if($c eq 10);
}
print "Using these files:\n$file1\n$file2\n";


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
		push(@{$data{$fam1}{$fam2}{$bits1}}, { start => $st1, end => $en1, seq => $seq, ov_start => $st2, ov_end => $en2 });                  
	    }
	}
     
        
	unless($ignore{$fam2}) { #Ignore fams with diff seq and dom ga
	    next if($clan{$fam1} and $clan{$fam2} and $clan{$fam1} eq $clan{$fam2}); #If they're in the same clan it's not an overlap

	    if($type2 eq "ALIGN") { #Don't store any SEED family overlaps
		push(@{$data{$fam2}{$fam1}{$bits2}}, { start => $st2, end => $en2, seq => $seq, ov_start => $st1, ov_end => $en1 });                  
	    }       
	}
    }
    else {
        print STDERR "Unrecognised line in $file1\n[$_]\n";
    }
}
close FH;
print STDERR "parsed $file1\n";



my $count=0;
open(RESOLVED, ">resolved.$$") or die "Couldn't open resolved.$$ for writing\n";


		
my %already;
open(FH2, $file2) or die "$!";
while(<FH2>) {
  if(/^PF\d{5}\s+\S+\s+\S+\s+\S+\s+\S+\s+-/ or /^acc/) {
    next; #Family has no overlaps or its the header line
  }
  elsif(/^(PF\d\d\d\d\d)\s+\S+\s+\S+\s+\S+\s+(\S+)\s+(\S+)/) { #acc	id	NoSeed	NoFull 	NoOverlaps	OverlapFams
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
               print STDERR "Unrecognised family format in $file2 [$fam2] line [$_]\n";
	       next;
	   }
           my ($status_fam1, $status_fam2);


	   #See if raising threshold of fam1 will work
	   if(exists($data{$fam1}{$fam2})) { 
	     foreach my $bits (sort { $b <=> $a } keys(%{$data{$fam1}{$fam2}}) ) {
	       
	       if($bits > $bits_threshold) {
		 next;
	       }
	       
	       foreach my $bitsOverlap (@{$data{$fam1}{$fam2}{$bits}}) {
		 
		 my $seq = $bitsOverlap->{'seq'};   
		 
		 
		 my $a1 = check_align($fam1, $seq, $bitsOverlap->{'start'}, $bitsOverlap->{'end'});
		 my $a2 = check_align($fam2, $seq, $bitsOverlap->{'ov_start'}, $bitsOverlap->{'ov_end'});
		 
		 
		 unless($a1 and $a2) { #Yay - overlap has already been resolved                                      
		   print "$fam1:$fam2 [$seq] ($bits bits) already resolved\n" unless(exists($already{"$fam1:$fam2"}));
		   $already{"$fam1:$fam2"}=1;
		   $already{"$fam2:$fam1"}=1;
		   last;
		 }
		 
		 #$fam_ov is number of overlaps between the two familes
		 my $fam_ov = $fam_overlaps{"$fam1:$fam2"};
		 
		 my $allowed_plus = $fam_ov + $allowed_loss;
		 
		 print "Going to try raise threshold on $fam1 to resolve $fam1:$fam2 overlap [$seq] ($bits bits)\n";           
		 
		 #Count how many overlaps would be resolved if threshold raised to 0.1 above $bits
		 my $o_resolved;
		 foreach my $b (sort { $a <=> $b } keys(%{$data{$fam1}{$fam2}}) ) {
		   foreach my $overlapping (@{$data{$fam1}{$fam2}{$bits}}) {
		     if($b <= $bits) {
		       $o_resolved++;
		     }
		     else {
		       last;
		     }
		   }
		 }
	 
		 $status_fam1 = resolve($fam1, $ga{$fam1}, $bits, $allowed_plus, $fam2, \$count, $seq, $fam_ov, $total_overlaps{$fam1}, $o_resolved );
		 last if($status_fam1);
	       }
	     }
	   }

           #Then see if raising threshold of fam2 will work
	   if(exists($data{$fam2}{$fam1})) { 
	     foreach my $bits (sort { $b <=> $a } keys(%{$data{$fam2}{$fam1}}) ) {
	       
	       if($bits > $bits_threshold) {
		 next;
	       } 
	       
	       foreach my $bitsOverlap (@{$data{$fam2}{$fam1}{$bits}}) {
		 my $seq = $bitsOverlap->{'seq'};                  
		 
		 my $a1 = check_align($fam2, $seq, $bitsOverlap->{'start'}, $bitsOverlap->{'end'});
		 my $a2 = check_align($fam1, $seq, $bitsOverlap->{'ov_start'}, $bitsOverlap->{'ov_end'});
		 
		 
		 unless($a1 and $a2) { #Yay - overlap has already been resolved              
		   print "$fam2:$fam1 [$seq] ($bits bits) already resolved\n" unless(exists($already{"$fam1:$fam2"})) ;
		   $already{"$fam1:$fam2"}=1;
		   $already{"$fam2:$fam1"}=1; 
		   last;
		 }
		 
		 my $fam_ov = $fam_overlaps{"$fam1:$fam2"};
		 my $allowed_plus = $fam_ov + $allowed_loss;
		 
		 print "Going to try raise threshold on $fam2 to resolve $fam2:$fam1 overlap [$seq] ($bits bits)\n";           
		 
		 
		 #Count how many overlaps would be resolved if threshold raised to 0.1 above $bits
		 my $o_resolved;
		 foreach my $b (sort { $a <=> $b } keys(%{$data{$fam2}{$fam1}}) ) {
		   foreach my $overlapping (@{$data{$fam2}{$fam1}{$bits}}) {
		     if($b <= $bits) {
		       $o_resolved++;
		     }
		     else {
		       last;
		     }
		   }
		 }
		 
		 $status_fam2 = resolve($fam2, $ga{$fam2}, $bits, $allowed_plus, $fam1, \$count, $seq, $fam_ov, $total_overlaps{$fam2}, $o_resolved);
		 last if($status_fam2);
	       }
	     }
	   }
           #If neither worked then lets tell the user we couldn't resolve any of the overlaps
           unless($status_fam1 or $status_fam2) { 
	       print "Can't resolve $fam1:$fam2 overlap(s)\n";		  
	   }
       }
   }
  else {
    print STDERR "Unrecognised line in $file2: $_";
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
  my ($fam, $ga, $ov_bits, $allowed_loss, $overlap_fam, $count, $seq, $fam_ov, $total_ov, $o_resolved) = @_;
  
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
  
  #Only allow the overlap to be resolved if we keep 90% of members of the original align file
  my $prop = ($new/($old - $o_resolved)) *100;
  
  if($diff <= $allowed_loss and $diff > 0 and $prop >= $perc_thresh) {
    
    $prop = sprintf("%.1f", $prop);
    print RESOLVED "Resolving $fam:$overlap_fam [$seq] overlap by raising threshold from $ga to $new_thresh and losing $diff/$old domain(s) (keeping $prop % of orignal ALIGN)\n";
    print STDOUT "Resolving $fam:$overlap_fam [$seq] overlap by raising threshold from $ga to $new_thresh and losing $diff/$old domain(s) (keeping $prop % of orignal ALIGN)\n";
    print STDERR "Resolving $fam:$overlap_fam [$seq] overlap by raising threshold from $ga to $new_thresh and losing $diff/$old domain(s) (keeping $prop % of orignal ALIGN)\n";
    
    chdir "$families_dir/$fam" or die "Couldn't chdir into $families_dir/$fam $!\n";
#copy DESC file before pfmake just in case of problems
    copy ("DESC", "DESC_b4_raise") or die "Cannot copy DESC in $fam\n";
    system("pfmake.pl -t $new_thresh") and die "\'pfmake.pl -t $new_thresh\' failed to run\n";
    
    $$count++;
    return ("1");
  }
  else {
    $prop = sprintf("%.1f", $prop);
    print "Ignoring $fam\:$overlap_fam overlap [$seq]    bitscore_$fam:$ov_bits   totalOverlap\_$fam\:$total_ov   $fam\/$overlap_fam" . " Overlap\:$fam_ov   Loss:$diff (loses $prop % of original ALIGN)\n";
    return ("");
  }
}



sub help {

  print STDERR <<EOF;

This script is used at release time to resolve overlaps by raising thresholds.

Usage: $0 -status_dir <status dir> -family_dir <family dir> > outfile

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
