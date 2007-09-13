#!/software/bin/perl -w

use strict;
use Rfam;
use Rfam::RfamAlign;
use Getopt::Long;

my( $quiet,
    $nolog,
    $local_fams,
    @ignore);


&GetOptions("i=s@" => \@ignore,
	    "q"    => \$quiet,
	    "l"    => \$local_fams,
	    "n!"   => \$nolog);


if( $#ARGV == -1 ) {
    print "rqc-overlap-rdb - finds the overlap between a Rfam family and the current Rfamlive database\n";
    print "USAGE: rqc-overlap-rdb <model-directory> <optional - families to compare against>\n";
    print "     If no families given, assummes all\n";
    print "OPTIONS\n";
    print "  -i <family> ignore this family (-i can occur multiple times)\n";
    print "  -q quiet running\n";
    print "  -n  no log file\n";
    print "  -l  look in current directory for target families\n";
    exit(1);
}

#family to check against db
my $family_dir = shift; 
$family_dir=~ s/\///g;

if( ! defined $nolog ) {
    open(LOG,">$family_dir/overlap") || die "Could not open log file $family_dir/overlap - can use -n option $!";
}

# -l option not used at moment but will be added.

my %ignore;
push (@ignore, $family_dir); # add self to ignore list
foreach my $ignorefam ( @ignore ) {
    #print STDERR "ignoring $ignorefam\n";
    $ignore{$ignorefam} = 1;
}


my @overlap;
if( defined $quiet ) {
    @overlap = &get_overlaps( $family_dir );
} else {
    @overlap = &get_overlaps($family_dir, \*STDERR );
}

#checks for internal overlaps in seed only as infernal wont predict overlapping regions for same seq.
#so wont generate overlaps in the ALIGN file if not in seed.

open( SEED, "$family_dir/SEED" ) or die "can't find $family_dir/SEED\n";
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
my @list = $seed->each_seq();

for( my $seq = shift(@list); defined $seq; $seq = shift(@list) ) {
   foreach my $other ( @list ) {
	if( $seq->id ne $other->id ) {
	    next;
	}
	my $or1 = my $or2 = 1;
	my ($seq1, $seq2)= ($seq->id, $other->id);
	my ($start1, $end1, $start2, $end2) = ($other->start, $other->end,$seq->start, $seq->end); 
	my ($s1, $e1, $s2, $e2) = ($start1, $end1, $start2, $end2);
	if ($s1 > $e1) {
		    ($s1, $e1) = ($e1, $s1);
		    $or1= -1;
		}
		if ($s2 > $e2) {
		    ($s2, $e2) = ($e2, $s2);
		    $or2= -1;
		}
	if( &overlap($s1, $e1, $s2, $e2)) {
	    my $ov_len=&ov_length($s1, $e1, $s2, $e2);
	   printf LOG ("Internal overlap of $seq1/%d-%d with $seq2/%d-%d by $ov_len\n", $start1, $end1, $start2, $end2);
       }
    }
}

close(LOG) if( ! defined $nolog );
 
###SUBROUTINES#######
    
sub get_overlaps {
    my $dir = shift;
    my $report = shift;

    #read in this family align and seed seqs
    open( FULL, "$dir/ALIGN" ) or die "can't find $dir/ALIGN\n";
    my $full = new Rfam::RfamAlign;
    $full -> read_stockholm( \*FULL );
    close(FULL);
    open( SEED, "$dir/SEED" ) or die "can't find $dir/SEED\n";
    my $seed = new Rfam::RfamAlign;
    $seed -> read_stockholm( \*SEED );
    close(SEED);

    my %hash;
    # keys: sequence acc, values: array of starts and stops.
    foreach my $seq ( $full->each_seq(), $seed->each_seq() ) {
       	push( @{ $hash{$seq->id()} }, { 'start' => $seq->start(),
					'end'   => $seq->end() } );
    }

    my %sshash;  # nr list of ss OL 
    my %ophash;  # nr list of op OL

    #nr list of new family seq accs
    my @keys=keys(%hash);

    my $rdb = Rfam::live_rdb();  #Rfam::DB::DB_RDB

    #get all the exisiting annotations for each of the new family seqs in the rdb 
    #store array of ordered annotated regions for each seq
    #compare each in turn against new family regions

    my $sscount= my $opcount=0;

    foreach my $seq ( $rdb->get_AnnotSeqs(\@keys, ['seed', 'full']) ) {
	foreach my $current_reg (sort { $a->from <=> $b->from} ($seq->eachAnnotatedRegion)) {

	    if( $ignore{$current_reg->accession} ) {next;} #skip self

	    foreach my $startstop ( @{ $hash{$current_reg->rfamseq_id()} } ) {
		
                # create local values instead of having to access object all the time as suggested by code review.
		my ($seqacc, $reg_acc)= ($current_reg->rfamseq_id(),$current_reg->accession()); 
		my ( $fam_start, $fam_stop, $reg_start, $reg_stop) = ( $startstop->{'start'}, $startstop->{'end'}, $current_reg->from, $current_reg->to );
		my( $s1, $e1, $s2, $e2 )= ($fam_start, $fam_stop, $reg_start, $reg_stop);
                my $or1 = my $or2 = 1;
 
               #deal with orientations
		if ($s1 > $e1) {
		    ($s1, $e1) = ($e1, $s1);
		    $or1= -1;
		}
		if ($s2 > $e2) {
		    ($s2, $e2) = ($e2, $s2);
		    $or2= -1;
		}

 		#if overlap add to hash in format seqid.family1.family2
		if (($or1 == $or2) &&  ( &overlap($s1,$e1,$s2,$e2)) ) {
		    unless ( $sshash{$seqacc}{$dir}{$reg_acc} )  {
			my $ovlen=&ov_length($s1, $e1, $s2, $e2);
			my $string=sprintf ("Sequence [$seqacc] overlaps $dir/%d-%d with $reg_acc/%d-%d by $ovlen" , $fam_start,$fam_stop,$reg_start,$reg_stop);
			$sshash{$seqacc}{$dir}{$reg_acc}= $string;
   			++$sscount;
		    }
		}
		#op strand overlap
		if (($or1 != $or2) &&  ( &overlap($s1,$e1,$s2,$e2) ) ) {
		    unless ( $ophash{$seqacc}{$dir}{$reg_acc} )  {
			my $ovlen=&ov_length($s1, $e1, $s2, $e2);
			my $string=sprintf ("Sequence [$seqacc] overlaps $dir/%d-%d with $reg_acc/%d-%d by $ovlen" , $fam_start,$fam_stop,$reg_start,$reg_stop);
			$ophash{$seqacc}{$dir}{$reg_acc}= $string ; 
			++$opcount;
                    }
		}
	    }
	}
    }
    
    if( $report ) {
	if ($sscount ==0 && $opcount==0){
	    print sprintf("Done Model %-25s\nNo overlaps found\n",$dir);
	}else {
	    print sprintf("Done Model %-25s\nFound %d same strand and %d opposite strand overlaps\n",$dir, $sscount, $opcount);
	}
    }
   #print out report for same strand and opp strand overlaps
    foreach my $seqid (keys (%sshash )){
	foreach my $fam1 (keys ( %{$sshash{$seqid}} )){
	    foreach my $fam2 (keys ( % {$sshash{$seqid}{$fam1}} )){
		print LOG "SS_OL $sshash{$seqid}{$fam1}{$fam2}\n";
	    }
	}
    }

    foreach my $seqid (keys (%ophash )){
	foreach my $fam1 (keys ( %{$ophash{$seqid}} )){
	    foreach my $fam2 (keys ( % {$ophash{$seqid}{$fam1}} )){
		print LOG "OP_OL $ophash{$seqid}{$fam1}{$fam2}\n";
	    }
	}
    }
}

#check if regions overlap
sub overlap {
    my ($s1,$e1,$s2, $e2)=@_;
    if( ( $s1 >= $s2 and $e1 <= $e2 ) or  #full ol.
	( $s1 <= $e2 and $e1 >= $e2 ) or  #region 1 right extended or fully nested region 2
	( $s1 <= $s2 and $e1 >= $s2 ) ) { #region 1 left extended or fully nested region 2
	return 1;
    }
    return 0;
}


#length of overlap
sub ov_length {
    #all will be orientation sorted already
    my ($s1,$e1,$s2, $e2)=@_;
    my $len=0;
   
    if ( $s1 <= $e2 and $e1 >= $e2 ) {
	$len=$e2-$s1;
    }elsif($s1 <= $s2 and $e1 >= $s2  ){
	$len=$e1-$s2;
    }else {
	return 'fullOL';
    }

    return $len;
}

