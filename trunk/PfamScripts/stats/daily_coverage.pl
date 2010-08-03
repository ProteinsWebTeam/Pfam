#! /software/bin/perl -w

#Script to print out each family and its sequence and residue coverage
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


#Get length and region data from rdb
my $reg_file = "tmp.regions.dat.$$";
my $sorted_reg ="regions.dat.$$";


#Find total number of residues
my $st_len = $dbh->prepare("select length from pfamseq") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_len->execute() or die "Couldn't execute statement ".$st_len->errstr."\n";
my $array_ref_len = $st_len->fetchall_arrayref();

my $total_aa;
foreach my $element (@$array_ref_len) {
    $total_aa += $$element[0];
}


my $st_reg = $dbh->prepare("select auto_pfamseq, auto_pfamA, seq_start, seq_end into outfile \"/tmp/$reg_file\" from pfamA_reg_full_significant where in_full =1") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_reg->execute() or die "Couldn't execute statement ".$st_reg->errstr."\n";


#Copy regions file to cwd
my $scp = Net::SCP->new( { "host"=> $pfamDB->{host} } );
$scp->get("/tmp/$reg_file") or die "Couldn't scp /tmp/$reg_file to cwd " . $scp->{errstr} . "\n";;


#Sort file by auto_pfamA
system("sort $reg_file -k2n > $sorted_reg") and die "Couldn't sort $reg_file $!"; 
#unlink($reg_file);


#Get auto_pfamA to pfamA_id/pfamA_acc mapping
my $st_pfamA = $dbh->prepare("select auto_pfamA, pfamA_acc, pfamA_id from pfamA") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_pfamA->execute() or die "Couldn't execute statement ".$st_reg->errstr."\n"; 
my $array_ref_pfamA = $st_pfamA->fetchall_arrayref();

my %pfamA;
foreach my $element (@$array_ref_pfamA) {
    my ($auto, $acc, $id) = ($$element[0], $$element[1], $$element[2]);
    $pfamA{$auto}->{'acc'}=$acc;
    $pfamA{$auto}->{'id'}=$id;
}

$dbh->disconnect;



#Go though each family and calculate amino acid and sequence coverage
my $aa_count=0;
my %seq;
my $pfamA;

#Put results in temporary file so we can sort them for the user
open(TMP, ">tmp.$$") or die "Couldn't open file tmp.$$ for writing, $!";

print "pfamA_acc, pfamA_id, seq, seq_coverage(%), residues, residue_coverage(%)\n";
open(FH, $sorted_reg) or die "Couldn't open fh to $sorted_reg, $!";
while(<FH>) {
    if(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
	my ($auto_pfamseq, $auto_pfamA, $start, $end) = ($1, $2, $3, $4);

	$pfamA=$auto_pfamA unless($pfamA);

	if($pfamA ne $auto_pfamA) {
            calculate($aa_count, \%seq, \%pfamA, $pfamA, $total_seq, $total_aa);

	    $pfamA = $auto_pfamA;
	    %seq = ();
	    $aa_count=0;
	}

	$aa_count += $end - $start +1;
	$seq{$auto_pfamseq}=1;
    }
    else {
	chomp $_;
	print STDERR "Ignoring this line:[$_]\n";
    }
}

unlink($sorted_reg);

#Do last family
calculate($aa_count, \%seq, \%pfamA, $pfamA, $total_seq, $total_aa);

close FH;
close TMP;

#Sort file by pfamA_acc
system("sort tmp.$$") and die "Couldn't sort tmp.$$, $!";
unlink("tmp.$$");





sub calculate { #Subroutine to calculate seq and aa coverage for eacg family
    my ($aa_count, $seq_hash, $pfamA, $auto_pfamA, $total_seq, $total_aa) = @_;

    my $pfamA_id = $$pfamA{$auto_pfamA}->{'id'};
    my $pfamA_acc = $$pfamA{$auto_pfamA}->{'acc'};

    my $num_seq = keys %$seq_hash;
    my $seq_cov = ($num_seq/$total_seq)*100;
 
    my $aa_cov = ($aa_count/$total_aa)*100;
 
    print TMP sprintf ("%7s %-16s %7s  %-21s %10s  %-21s\n", $pfamA_acc, $pfamA_id, $num_seq, $seq_cov, $aa_count, $aa_cov);
}

