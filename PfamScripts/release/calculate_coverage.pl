#! /software/bin/perl -w

use strict;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Script for calculating amino acid coverage for a sequence database
#Script queries the database for the length of each sequence, and for the start/ends of each region that matches a Pfam-A

my ($ncbi, $meta);
GetOptions( 'meta' => \$meta,
	    'ncbi'  => \$ncbi);


if( ($meta and $ncbi) or (!$meta and !$ncbi)) {
    help();
}


my ($seq_tbl, $reg_full_tbl, $auto);
if($meta) {
    $seq_tbl = "metaseq";
    $reg_full_tbl = "meta_pfamA_reg";
    $auto = "auto_metaseq";
}
else {
    $seq_tbl = "ncbi_seq";
    $reg_full_tbl = "ncbi_pfamA_reg";
    $auto = "gi";
}


my $lengths_file = "lengths.$$";
my $regions_file = "regions.$$";
my $tmp_regions_file = "tmp.regions.$$";

#Set up database
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Run queries
print STDERR "querying $seq_tbl\n";
my $st1 = $dbh->prepare("select $auto, length into outfile \"/tmp/$lengths_file\" from $seq_tbl") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st1->execute() or die "Couldn't execute statement ".$st1->errstr."\n";

print STDERR "querying $reg_full_tbl\n";
my $st2 = $dbh->prepare("select $auto, seq_start, seq_end into outfile \"/tmp/$tmp_regions_file\" from $reg_full_tbl") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st2->execute() or die "Couldn't execute statement ".$st2->errstr."\n";
$dbh->disconnect;


#Copy data locally
system("scp " . $config->pfamliveAdmin->{host}. ":/tmp/$lengths_file ." ) and die("Could not scp $lengths_file from database host $!");
system("scp " . $config->pfamliveAdmin->{host}. ":/tmp/$tmp_regions_file ." ) and die("Could not scp $tmp_regions_file from database host $!");

system("sort -n $tmp_regions_file > $regions_file") and die "Failed to sort regions file $!";
unlink($tmp_regions_file);



print STDERR "Parsing $lengths_file...";

my $length=0;
open(LEN, $lengths_file) or die "Couldn't open $lengths_file $!";
while(<LEN>) {
    if(/^\S+\s+(\S+)/) { 
	$length+=$1;
    }
}
close LEN;

print STDERR "done\n";


print STDERR "Parsing $regions_file...";

my %reg;
my $aa=0;
open(REG, $regions_file) or die "Couldn't open $regions_file $!";

my $a;
while(<REG>) {
    if(/^(\S+)\s+(\S+)\s+(\S+)/) {
        my ($auto, $start, $end) = ($1, $2, $3);

	if($a) {
	    if($a ne $auto) {
		$aa+=calculate(\%reg, $a);
		$a=$auto;
	    }
	}
	else {
	    $a=$auto;
	}

        for(my $i=$start; $i <= $end; $i++) {
	    $reg{$auto}[$i-1]=1;
          
	}
    }
}
close REG;
$aa+=calculate(\%reg, $a);

close REG;

print STDERR "done\n";

unlink $lengths_file;
unlink $regions_file;

print "Number of residues covered by Pfam-A:$aa\nTotal number of residues:$length\n";
my $coverage = ($aa/$length)*100;
print "Amino acid coverage is $coverage\%\n";

sub calculate {
    my ($reg, $auto)= @_;

    my $aa;

    foreach my $element (@{$reg{$auto}}) {
	if($element) {
	    $aa++;
	}
    }

    delete $$reg{$auto};

    return($aa);   
}


sub help {

   print STDERR <<EOF;

A script for calculating the amino acid coverage for the ncbi_seq or
metaseq dataset in pfamlive.  You need to specify which dataset you
wish to run it on.  The results are printed to STDOUT;

Usage:

       $0 -meta or $0 -ncbi

EOF
  exit;

}
