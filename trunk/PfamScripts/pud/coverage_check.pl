#! /software/bin/perl -w

#Script to compare coverage between last release of pfam and pfamlive
#Queries rdb for sequence lengths, and region data and dumps to file (need to remove scp bit)
#Calculates coverage for both sequence db, and the intersection of the two seq db
#Calculates family by family coverage for last release, and percentage change in pfamllive
#Writes two files to cwd: overallCoverage.txt, familyCoverage.txt 

use strict;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Getopt::Long;
use Net::SCP;

my $help;
GetOptions('help'    => \$help);


&help if($help);

my $outfile = "overallCoverage.txt";
my $outfile2 = "familyCoverage.txt";

if(-s $outfile or -s $outfile2) {
    die "$outfile and/or $outfile2 already exists\n";
}

my $config = Bio::Pfam::Config->new;

#Set up databases
my $pfamDB1 = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamOldRelAdmin } );
my $dbh_old = $pfamDB1->getSchema->storage->dbh;
my $old_rel = $pfamDB1->{database};

my $pfamDB2 = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh_new = $pfamDB2->getSchema->storage->dbh;
my $new_rel = $pfamDB2->{database};



#Get auto_pfamA to acc and id mappings and find out which fams are now dead
print STDERR "querying $old_rel pfamA\n";
my %pfamA_old;
my $st_pfamA_old = $dbh_old->prepare("select auto_pfamA, pfamA_acc, pfamA_id from pfamA") or die "Failed to to prepare statement:".$dbh_old->errstr."\n";
$st_pfamA_old->execute() or die "Couldn't execute statement ".$st_pfamA_old->errstr."\n";

my $array_ref_old = $st_pfamA_old->fetchall_arrayref();
foreach my $element (@$array_ref_old) {
    $pfamA_old{$$element[0]}{'acc'} = $$element[1];
    $pfamA_old{$$element[0]}{'id'} = $$element[2];
}


print STDERR "querying $new_rel dead_families\n";
my %pfamA_dead;
my $st_pfamA_new = $dbh_new->prepare("select pfamA_acc from dead_families") or die "Failed to to prepare statement:".$dbh_new->errstr."\n";
$st_pfamA_new->execute() or die "Couldn't execute statement ".$st_pfamA_new->errstr."\n";

my $array_ref_new = $st_pfamA_new->fetchall_arrayref();
foreach my $element (@$array_ref_new) {
    $pfamA_dead{$$element[0]} = 1;
}





#Execute queries for getting coverage data
#Need length data from pfamseq and start ends from pfamA_reg_full_significant
#Do old data first

my $old_len_file = "old_lengths.dat.$$";
my $old_reg_file = "old_regions.dat.$$";
my $tmp = "/tmp";

print STDERR "querying $old_rel pfamseq\n";
my $st_old1 = $dbh_old->prepare("select auto_pfamseq, length into outfile \"$tmp/$old_len_file\" from pfamseq") or die "Failed to prepare statement:".$dbh_old->errstr."\n";
$st_old1->execute() or die "Couldn't execute statement ".$st_old1->errstr."\n";

print STDERR "querying $old_rel pfamA_reg_full_significant\n";
my $st_old2 = $dbh_old->prepare("select auto_pfamseq, seq_start, seq_end, auto_pfamA into outfile \"$tmp/$old_reg_file\" from pfamA_reg_full_significant where in_full =1") or die "Failed to prepare statement:".$dbh_old->errstr."\n";
$st_old2->execute() or die "Couldn't execute statement ".$st_old2->errstr."\n";
$dbh_old->disconnect;


#Copy data locally
my $scp_old = Net::SCP->new( { "host"=> $pfamDB1->{host} } );
$scp_old->get("$tmp/$old_len_file") or die "Couldn't scp $tmp/$old_len_file to cwd " . $scp_old->{errstr} . "\n";;
$scp_old->get("$tmp/$old_reg_file") or die "Couldn't scp $tmp/$old_reg_file to cwd " . $scp_old->{errstr} . "\n";;



#Now do new data
my $new_len_file = "new_lengths.dat.$$";
my $new_reg_file = "new_regions.dat.$$";

print STDERR "querying $new_rel pfamseq\n";
my $st_new1 = $dbh_new->prepare("select auto_pfamseq, length into outfile \"$tmp/$new_len_file\" from pfamseq") or die "Failed to prepare statement:".$dbh_new->errstr."\n";
$st_new1->execute() or die "Couldn't execute statement ".$st_new1->errstr."\n";


print STDERR "querying $new_rel pfamA_reg_full_significant\n";
my $st_new2 = $dbh_new->prepare("select auto_pfamseq, seq_start, seq_end, auto_pfamA into outfile \"$tmp/$new_reg_file\" from pfamA_reg_full_significant where in_full =1") or die "Failed to prepare statement:".$dbh_new->errstr."\n";
$st_new2->execute() or die "Couldn't execute statement ".$st_new2->errstr."\n";
$dbh_new->disconnect;

my $scp_new = Net::SCP->new( { "host"=> $pfamDB2->{host} } );
$scp_new->get("$tmp/$new_len_file") or die "Couldn't scp $tmp/$new_len_file to cwd " . $scp_new->{errstr} . "\n";
$scp_new->get("$tmp/$new_reg_file") or die "Couldn't scp $tmp/$new_reg_file to cwd " . $scp_new->{errstr} . "\n";




#Store the accessions for the old, new and intersection of the db, and the total number of amino acids in each
my (%old_db, %new_db, %intersection_db);
my ($old_total_aa, $new_total_aa, $intersection_total_aa) = total_length($old_len_file, $new_len_file, \%old_db, \%new_db, \%intersection_db);


#Store regions for each db
my (%old_reg, %new_reg, %old_int_fam_aa, %old_int_fam_seq, %new_int_fam_aa, %new_int_fam_seq);
my $old_aa_covered = parse_regions($old_reg_file, \%old_reg, \%intersection_db, \%old_int_fam_aa, \%old_int_fam_seq);
my $new_aa_covered = parse_regions($new_reg_file, \%new_reg, \%intersection_db, \%new_int_fam_aa, \%new_int_fam_seq);


unlink $old_len_file;
unlink $new_len_file;
unlink $old_reg_file;
unlink $new_reg_file;


open(COV, ">$outfile") or die "Couldn't open $outfile for writing $!";


#Overall coverage for old db
my ($seq_coverage_old, $aa_coverage_old) = coverage($old_aa_covered, \%old_reg, $old_total_aa, \%old_db, $old_rel,  \*COV);

#Overall coverage for new db
my ($seq_coverage_new, $aa_coverage_new) = coverage($new_aa_covered, \%new_reg, $new_total_aa, \%new_db, $new_rel,  \*COV);


#Now store regions for intersection
my (%old_intersection, %new_intersection, $old_aa_intersection, $new_aa_intersection);
foreach my $id (keys %intersection_db) {
    if(exists($old_reg{$id})){ 
	$old_intersection{$id}=1;
	$old_aa_intersection += $old_reg{$id};
    }
    if(exists($new_reg{$id})) {
	$new_intersection{$id}=1;
	$new_aa_intersection += $new_reg{$id};
    }
}

#Intersection coverage for old seq db
coverage($old_aa_intersection, \%old_intersection, $intersection_total_aa, \%intersection_db, "Intersection, $old_rel", \*COV);

#Intersection coverage for new seq db
coverage($new_aa_intersection, \%new_intersection, $intersection_total_aa, \%intersection_db, "Intersection, $new_rel", \*COV);


close COV;

#Breakdown of intersection coverage by family
intersection($outfile2, \%old_int_fam_aa, \%old_int_fam_seq, \%new_int_fam_aa, \%new_int_fam_seq, \%pfamA_old, \%pfamA_dead);











###############
# Subroutines #
###############


sub total_length {

    my ($file1, $file2, $old, $new, $intersection) = @_;

    my $length1=0;
    my $length2=0;
    my $length3=0;
    open(LEN, $file1) or die "C1ouldn't open $file1 $!";
    while(<LEN>) {
	if(/^(\S+)\s+(\S+)/) {
	    $length1+=$2;
	    $$old{$1}=1;
	}
    }
    close LEN;


    open(LEN2, $file2) or die "Couldn't open $file2 $!";
    while(<LEN2>) {
	if(/^(\S+)\s+(\S+)/) {
	    my ($id, $len)  = ($1, $2);
	    $length2+=$len;
	    $$new{$id}=1;

	    if(exists($$old{$id})) {
		$$intersection{$id}=1;
		$length3+= $len;
	    }
	}
    }
    close LEN2;

    return($length1, $length2, $length3);
}



sub parse_regions {

    my ($file, $hash, $intersection, $family_aa, $family_seq) = @_;
	
    my $covered_aa=0;
    
    open(REG, $file) or die "Couldn't open $file $!";
    while(<REG>) {
	if(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
	    my ($id, $start, $end, $auto_pfamA) = ($1, $2, $3, $4);
	    my $aa = $end - $start +1;
	    $$hash{$id}+=$aa;
	    $covered_aa += $aa;

	    if(exists($$intersection{$id})) {
		$$family_aa{$auto_pfamA}+=$aa;
		$$family_seq{$auto_pfamA}{$id}=1;
	    }
	}
	else {
	    warn "This line was not parsed from $file\n$_";
	}
    }
    close REG;

    return $covered_aa;
}


sub coverage {
    my ($aa_covered, $seq_hash, $total_aa, $total_seq_hash, $desc, $fh) = @_;


    my $seq_covered = keys %$seq_hash;  #Keys are seq id
    my $total_seq = keys %$total_seq_hash;

    my $seq_coverage = ($seq_covered / $total_seq)*100;

    my $aa_coverage = ($aa_covered/$total_aa)*100;

    print $fh "$desc ($total_seq sequences)\nSequence coverage: $seq_coverage %\nAmino acid coverage: $aa_coverage %\n\n";
}



sub intersection {

    my ($outfile, $old_fam_aa, $old_fam_seq, $new_fam_aa, $new_fam_seq, $pfamA, $dead) = @_;
    

    open(OUT, ">$outfile") or die "Couldn't open fh to $outfile $!";

    print OUT "#<pfamA_acc> <pfamA_id> <aa_change> <aa_percentage> <seq_change> <seq_percentage>\n";
    foreach my $fam (keys %$old_fam_seq) {
	my $old_seqs_covered = keys %{$$old_fam_seq{$fam}};

        my $old_aa_covered = $$old_fam_aa{$fam};

	next unless($fam);
	my $acc = $pfamA->{$fam}->{acc};

	if(exists($$dead{$acc})) {
	    print OUT sprintf("%7s %-16s %8s\n", $pfamA->{$fam}->{acc},  $pfamA->{$fam}->{id}, "killed"); 
	}
	else {
	    my $new_seqs_covered = keys %{$$new_fam_seq{$fam}};
	    $new_seqs_covered=0 unless($new_seqs_covered);
	    
	    my $new_aa_covered = $$new_fam_aa{$fam};
	    $new_aa_covered=0 unless($new_aa_covered);
	    
	    
#        print "new_seqs_covered:$new_seqs_covered  new_aa_covered: $new_aa_covered\n";
	    
	    my $aa_change = $new_aa_covered - $old_aa_covered;
	    my $seq_change = $new_seqs_covered - $old_seqs_covered;
	    
	    #print "aa_change: $aa_change  seq_change: $seq_change\n";
	    
	    my $aa_percentage = ($aa_change/$old_aa_covered)*100;
	    $aa_percentage = sprintf("%.3f", $aa_percentage) . " %";
	    my $seq_percentage = ($seq_change/$old_seqs_covered)*100;
	    $seq_percentage = sprintf("%.3f", $seq_percentage) . " %";
	    
	    
	    print OUT sprintf("%7s %-16s %8s   %10s  %8s   %10s\n", $pfamA->{$fam}->{acc}, $pfamA->{$fam}->{id}, $aa_change, $aa_percentage, $seq_change, $seq_percentage);
	    
	}	
    }
    close OUT;
}



sub help {

   print STDERR <<EOF; 
This is a qc script which calculates the coverage (both amino acid and
sequence) of the intersection of sequences between the last Pfam
release and pfamlive.  

The script writes the coverage of the last release, pfamlive and the
intersection coverage for each database to a file called
overallCoverage.txt in the current working directory.  

The script writes the coverage of each family for the last release,
along with the percentage increase/decrease in pfamlive to
a file called familyCoverage.txt.  The family by family breakdown only
includes those sequences that are present in both databases (old release
and pfamlive), and those families that are present in the old release.

Usage:
       $0


Options:
       -h or -help :displays this help
EOF
  exit;

}

