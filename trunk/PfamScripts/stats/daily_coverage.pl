#! /software/bin/perl -w

#Script to print out each family and cumulative sequence and residue coverage
#Prints the following to STDOUT:
#pfamA_acc, pfamA_id,seq, seq_coverage(%), residues, residue_coverage(%)


use strict;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Net::SCP;



my $config = Bio::Pfam::Config->new;

#Set up database
my $total_seq = $config->{pfamseq}->{dbsize};
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Find total number of residues
my $st_len = $dbh->prepare("select length from pfamseq") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_len->execute() or die "Couldn't execute statement ".$st_len->errstr."\n";
my $array_ref_len = $st_len->fetchall_arrayref();

my $total_aa;
foreach my $element (@$array_ref_len) {
    $total_aa += $$element[0];
}

#Get region data from rdb
my $reg_file = "tmp.regions.dat.$$";
my $sorted_reg ="regions.dat.$$";


my $st_reg = $dbh->prepare("select auto_pfamseq, pfamA_acc, pfamA_id, seq_start, seq_end into outfile \"/tmp/$reg_file\" from pfamA as a, pfamA_reg_full_significant as b where a.auto_pfamA = b.auto_pfamA and in_full=1") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_reg->execute() or die "Couldn't execute statement ".$st_reg->errstr."\n";


#Copy regions file to cwd
my $host = $pfamDB->{host};
system("scp $host:/tmp/$reg_file .") and die "Couldn't scp /tmp/$reg_file to cwd \n";


#Sort file by pfamA_acc
system("sort $reg_file -k2 > $sorted_reg") and die "Couldn't sort $reg_file $!"; 
unlink($reg_file);


#Get clan mapping
my $st_clan = $dbh->prepare("select pfamA_acc, clan_acc from pfamA as a, clan_membership as b, clans as c where a.auto_pfamA = b.auto_pfamA and b.auto_clan = c.auto_clan") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_clan->execute or die "Couldn't execute statement ".$st_clan->errstr."\n";
my $array_ref_clan = $st_clan->fetchall_arrayref();

my %clan;
foreach my $element (@$array_ref_clan) {
    $clan{$$element[0]}=$$element[1];
}

$dbh->disconnect;



#Go though each family and calculate cumulative amino acid and sequence coverage
my $aa_count=0;
my (%seq, %total_seq);
my ($acc, $id);
my ($auto_pfamseq, $pfamA_acc, $pfamA_id, $start, $end);

print STDOUT "#pfamA_acc, pfamA_id, clan, seq, seq_coverage(%), residues, residue_coverage(%)\n";
open(FH, $sorted_reg) or die "Couldn't open fh to $sorted_reg, $!";
while(<FH>) {
    if(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
	($auto_pfamseq, $pfamA_acc, $pfamA_id, $start, $end) = ($1, $2, $3, $4, $5);

	unless($acc) {
	    $acc=$pfamA_acc;
	    $id=$pfamA_id;
	}

	if($acc ne $pfamA_acc) {
	    my $clan = $clan{$pfamA_acc};
	    $clan = "No_clan" unless($clan);

            calculate($aa_count, \%seq, \%total_seq, $acc, $id, $clan, $total_seq, $total_aa);

	    $acc = $pfamA_acc;
	    $id = $pfamA_id;
	    %seq = ();
	}

	$aa_count += $end - $start +1;
	$seq{$auto_pfamseq}=1;
	$total_seq{$auto_pfamseq}=1;
    }
    else {
	chomp $_;
	print STDERR "Ignoring this line:[$_]\n";
    }
}

unlink($sorted_reg);

#Do last family
my $clan = $clan{$pfamA_acc};
$clan = "No_clan" unless($clan);

calculate($aa_count, \%seq, \%total_seq, $pfamA_acc, $pfamA_id, $clan, $total_seq, $total_aa);

close FH;




sub calculate { #Subroutine to calculate seq and aa coverage for each family
    my ($aa_count, $seq_hash, $total_seq_hash, $pfamA_acc, $pfamA_id, $clan, $total_seq, $total_aa) = @_;

    my $seq_in_fam = keys %$seq_hash; 

    my $num_seq = keys %$total_seq_hash;
    my $seq_cov = ($num_seq/$total_seq)*100;
    
    my $aa_cov = ($aa_count/$total_aa)*100;
 
    print STDOUT sprintf ("%7s, %-17s %7s, %7s, %-21s, %10s, %-21s\n", $pfamA_acc, "$pfamA_id,", $clan, $seq_in_fam, "$seq_cov,", $aa_count, $aa_cov);
}

