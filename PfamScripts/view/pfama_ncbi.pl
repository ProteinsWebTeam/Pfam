#!/usr/bin/env perl

#populate pfamA_ncbi table
#required input - name of a file containing a dump of pfamA_acc and pfamseq_acc from pfamA_reg_full_significant
#and pfamA accession

use strict;
use warnings;
use DDP;
use File::Slurp;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my ($file, $acc);
&GetOptions(
  "acc=s"              => \$acc,
  "file=s"            => \$file,
	    );

unless ($file && $acc){
    die "Both -file and -acc must be specified\n";
}        

#for a pfamA accession:
#read file to find pfamseqs

my $st1 = $dbh->prepare("select ncbi_taxid from pfamseq where pfamseq_acc = ?") or die "Can't prepare statement: $dbh->errstr";
my $st2 = $dbh->prepare("select pfamA_id from pfamA where pfamA_acc = ?") or die "Can't prepare statement: $dbh->errstr";
my $st3 = $dbh->prepare("insert into pfamA_ncbi (pfamA_acc, pfamA_id, ncbi_taxid) values (?, ?, ?)") or die "Can't prepare statement: $dbh->errstr";


#query for taxid and add to hash
my %taxids;
my %seen;
open (DUMP, $file) or die "Can't open $file to read\n";
while (<DUMP>){
    my $line = $_;
    my $pfamseq;
    if($line =~ /^$acc/){
        my @data = split(/\s+/, $line);
        $pfamseq = $data[1];

        unless ($seen{$pfamseq}){
            $st1->execute($pfamseq);
            my $arrayref1 = $st1->fetchall_arrayref();
            foreach my $row1 (@$arrayref1){
                $taxids{$row1->[0]}=1;
            }

            $seen{$pfamseq}=1;
        }
    }    
}

close DUMP;

#query for pfamid
my $pfamid;
$st2->execute($acc);
my $arrayref2 = $st2->fetchall_arrayref();
foreach my $row2 (@$arrayref2){
    $pfamid=$row2->[0];
}

#go through the taxid hash and populate the table

foreach my $tax (keys %taxids){
    $st3->execute($acc, $pfamid, $tax);
}


