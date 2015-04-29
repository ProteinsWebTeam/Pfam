#!/usr/bin/env perl

#create pfamA-regions.tsv file

use strict;
use warnings;
use DDP;
use Log::Log4perl qw(:easy);
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use File::Slurp;
use Cwd;
use Getopt::Long;

#set up db stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my $num;
&GetOptions(
    'num=s' => \$num,
);

my $file = "regions_" . $num;

#query for version, crc, md5
my $st = $dbh->prepare("select seq_version, crc64, md5 from pfamseq where pfamseq_acc = ?") or die "Cannot prepare statement\n";

#set up outfile
my $outfile = "regionsout_" . $num;
open (OUTFILE, ">$outfile") or die "Can't open file to write";
#read in file
my @data = read_file($file);

foreach my $line (@data){
    if ($line =~ /^pfamseq_acc/){
        next;
    } elsif ($line =~ /(\w{6,10})\s+(PF\d{5})\s+(\d+)\s+(\d+)/){
        my $seq = $1;
        my $pfam = $2;
        my $start = $3;
        my $end = $4;
        
        $st->execute($seq);
        my $arrayref = $st->fetchall_arrayref();
        my $version = $arrayref->[0]->[0];
        my $crc = $arrayref->[0]->[1];
        my $md5 = $arrayref->[0]->[2];
        
        print OUTFILE "$seq\t$version\t$crc\t$md5\t$pfam\t$start\t$end\n";

    }
}

close (OUTFILE);
