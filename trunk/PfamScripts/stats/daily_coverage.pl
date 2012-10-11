#! /usr/bin/env perl 

#Script to print out amino acid coverage, and cumulative sequence coverage by family
#Prints the following to STDOUT:
#Total amino acid coverage
#pfamA_acc, pfamA_id, number_seq, new_seq, seq_coverage(%)


use strict;
use warnings;
use Net::SCP;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


my $config = Bio::Pfam::Config->new;

#Set up database
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Get total number of residues and sequences in pfamseq
my $total_seq = $config->{pfamseq}->{dbsize};
my $total_aa = $config->{pfamseq}->{totalRes};

print STDERR "Getting pfamA\n";
#Get pfamA family ids and accessions
my $st_pfamA = $dbh->prepare("select auto_pfamA, pfamA_acc, pfamA_id from pfamA") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_pfamA->execute() or die "Couldn't execute statement ".$st_pfamA->errstr."\n";
my $array_ref_pfamA = $st_pfamA->fetchall_arrayref();

my (%pfamA_acc, %pfamA_id);
foreach my $element (@$array_ref_pfamA) {
    my ($auto_pfamA, $pfamA_acc, $pfamA_id) = ($element->[0], $element->[1], $element->[2]);

    $pfamA_acc{$auto_pfamA}=$pfamA_acc;
    $pfamA_id{$auto_pfamA}=$pfamA_id;
}

print STDERR "Getting nested\n";
#Get nested families
my $st_nested = $dbh->prepare("select auto_pfamA, nests_auto_pfamA from nested_domains") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_nested->execute() or die "Couldn't execute statement ".$st_nested->errstr."\n";
my $array_ref_nested = $st_nested->fetchall_arrayref();

my (%nested);
foreach my $element (@$array_ref_nested) {
    my ($fam1, $fam2) = ($element->[0], $element->[1]);

    $nested{$fam1}{$fam2}=1;
    $nested{$fam2}{$fam1}=1;
}

print STDERR "GETting clan\n";
#Get clan mapping
my $st_clan = $dbh->prepare("select b.auto_pfamA, clan_acc from pfamA as a, clan_membership as b, clans as c where a.auto_pfamA = b.auto_pfamA and b.auto_clan = c.auto_clan") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_clan->execute or die "Couldn't execute statement ".$st_clan->errstr."\n";
my $array_ref_clan = $st_clan->fetchall_arrayref();

my %clan;
foreach my $element (@$array_ref_clan) {
    $clan{$element->[0]}=$element->[1];
}

print STDERR "Getting region data\n";
#Get region data from rdb
my $reg_file = "tmp.regions.dat.$$";
my $sorted_reg ="pfamA.dat.$$";
my $sorted_seq ="seq.dat.$$";


my $st_reg = $dbh->prepare("select auto_pfamseq, auto_pfamA, seq_start, seq_end, ali_start, ali_end, domain_evalue_score into outfile \"/tmp/$reg_file\" from pfamA_reg_full_significant where in_full=1") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_reg->execute() or die "Couldn't execute statement ".$st_reg->errstr."\n";

print STDERR "Copying regions\n";
#Copy regions file to cwd
my $scp = Net::SCP->new( { "host"=> $pfamDB->{host} } );
$scp->get("/tmp/$reg_file") or die $scp->{errstr};


#All rdb queries done
$dbh->disconnect;



#Look at aa coverage first
#Sort regions file by sequence, then by evalue score
print STDERR "Sorted region file\n";
system("sort $reg_file -k1n -k7n > $sorted_seq") and die "Couldn't sort $reg_file by auto_pfamseq and evalue score, $!"; 
unlink($reg_file);

my (%clan_regions, %nested_regions);
my $aa_covered;
my $seq;
my ($auto_pfamseq, $auto_pfamA, $start, $end, $ali_start, $ali_end);

print STDERR "Calculating aa coverage\n";
open(FH, $sorted_seq) or die "Couldn't open fh to $sorted_seq, $!";
while(<FH>) {
    if(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
	($auto_pfamseq, $auto_pfamA, $start, $end, $ali_start, $ali_end) = ($1, $2, $3, $4, $5, $6);

        $seq = $auto_pfamseq unless($seq);

        if($seq ne $auto_pfamseq) { #New sequence, clear the clan_regions and nested_regions hashes
	    %clan_regions = ();
	    %nested_regions = ();
            $seq = $auto_pfamseq;
	}

	my $clan = $clan{$auto_pfamA};


        if($clan) {
	    my $overlap=0;
	    if(exists($clan_regions{$clan})) { #Don't double count things in clans
	   
		foreach my $reg (@{ $clan_regions{$clan}}) {

		    unless ($ali_end < $reg->{'ali_start'} or $reg->{'ali_end'} < $ali_start){ #Overlaps

			if(exists($nested{$auto_pfamA}{$reg->{'pfamA'}}))  { #Overlap is due to domain being nested in another
  
				if($start < $reg->{'start'} or $end > $reg->{'end'}) {  #If this isn't true then $reg is the outer domain and we don't need to count the inner one 
				    my $outer = $end - $start +1;
				    my $inner = $reg->{'end'} - $reg->{'start'} +1;
				    $aa_covered += $outer - $inner; 
				    last;
				}
			}
			$overlap=1;
			last;
		    }
		}
	    }
	    unless($overlap) {
		push (@{ $clan_regions{$clan} }, { start => $start, end => $end, pfamA => $auto_pfamA, ali_start => $ali_start, ali_end => $ali_end  });
		if(exists($nested{$auto_pfamA})) {
		    push (@{ $nested_regions{$auto_pfamA} }, { start => $start, end => $end });
		}
		$aa_covered += $end - $start +1;
	    }
	}
	else {
	    if(exists($nested{$auto_pfamA})) {
	        foreach my $fam (keys %{$nested{$auto_pfamA}}) {
		    foreach my $reg (@{$nested_regions{$fam}}) {
		        unless ($end < $reg->{'start'} or $reg->{'end'} < $start){ #Overlaps
			    if($start < $reg->{'start'} or $end > $reg->{'end'}) {  #If this isn't true then $reg is the outer domain and we don't need to count the inner one 
			        my $outer = $end - $start +1;
			        my $inner = $reg->{'end'} - $reg->{'start'} +1;
                                $aa_covered += $outer - $inner; 
				last;
			    }
			}
		    }
	        }
	    }
	    else {
		if(exists($nested{$auto_pfamA})) {
		    push (@{ $nested_regions{$auto_pfamA} }, { start => $start, end => $end });
		}
	        $aa_covered += $end - $start + 1;
	    }
	 
	}
    }
}
close FH;


my $aa_coverage = ($aa_covered/$total_aa)*100;
$aa_coverage = sprintf("%.3f", $aa_coverage);

print "Amino acid coverage is $aa_coverage" . "%\n\n";




print STDERR "Sorting region file\n";
#Now look at sequence coverage by family
#Sort file by auto_pfamA
system("sort $sorted_seq -k2n > $sorted_reg") and die "Couldn't sort $reg_file by auto_pfamA, $!"; 
unlink($sorted_seq);


print STDERR "Calculating seq coverage\n";
#Go though each family and calculate cumulative sequence coverage
my (%seq, %total_seq);
my ($acc, $id, $pfamA_id, $pfamA_acc, $auto);
my $total_seq_without_current_fam =0;

print STDOUT "#auto_pfamA, pfamA_acc, pfamA_id, clan, num_seq, new_seq, seq_coverage(%)\n";
open(FH2, $sorted_reg) or die "Couldn't open fh to $sorted_reg, $!";
while(<FH2>) {
    if(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
	($auto_pfamseq, $auto_pfamA, $start, $end) = ($1, $2, $3, $4);


	unless($acc) {
	    $acc=$pfamA_acc{$auto_pfamA};
	    $id=$pfamA_id{$auto_pfamA};
	    $auto=$auto_pfamA;
	}

	if($auto ne $auto_pfamA) {
	    my $clan = $clan{$auto};
	    $clan = "No_clan" unless($clan);

            calculate(\%seq, \%total_seq, $total_seq_without_current_fam, $acc, $id, $clan, $total_seq, $auto);

	    $total_seq_without_current_fam = keys %total_seq;

	    $acc = $pfamA_acc{$auto_pfamA};
	    $id = $pfamA_id{$auto_pfamA};
	    $auto=$auto_pfamA;
	    %seq = ();
	}

	$seq{$auto_pfamseq}=1;
	$total_seq{$auto_pfamseq}=1;
    }
    else {
	chomp $_;
	print STDERR "Ignoring this line:[$_]\n";
    }
}



#Do last family
my $clan = $clan{$auto};
$clan = "No_clan" unless($clan);

calculate(\%seq, \%total_seq, $total_seq_without_current_fam, $acc, $id, $clan, $total_seq, $auto);

close FH2;
unlink($sorted_reg);



sub calculate { #Subroutine to calculate seq coverage for each family
    my ($seq_hash, $total_seq_hash, $total_seq_without_current_fam, $pfamA_acc, $pfamA_id, $clan, $total_seq, $auto_pfamA) = @_;

    my $seq_in_fam = keys %$seq_hash; 

    my $num_seq = keys %$total_seq_hash;

    my $num_seq_added = $num_seq - $total_seq_without_current_fam;

    my $seq_cov = ($num_seq/$total_seq)*100;
    
   
    print STDOUT sprintf ("%5s, %7s, %17s, %7s, %7s, %7s,  %-21s\n", $auto_pfamA, $pfamA_acc, $pfamA_id, $clan, $seq_in_fam, $num_seq_added, $seq_cov); 
    
}

