#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Getopt::Long;

#Script to generate files from the database that are needed by Google to make Pfam-N
#They need sequences from pfamseq, and regions from pfamA_reg_full_signficiant and pfamA_reg_full_insignificant
#These jobs are submitted to the farm
#Also need pfamA and clan data, these queries are run locally (the tables are small, so these queries are quick)
#The following files will be written to the cwd:
#pfamseq.tsv.gz
#significant.tsv.gz
#insignificant.tsv.gz
#clans.tsv
#pfamA.tsv


my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my $queue = $config->{farm}->{lsf}->{queue};

my $command = "mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." --quick --max_allowed_packet=1024M ".$pfamDB->{database}." -e ";
my $bsub = "bsub -q $queue -R \"rusage[mem=4000]\" -M 4000 ";

#pfamseq
unless(-s "pfamseq.tsv.gz") {
    my $command_pfamseq = $command . "\"select pfamseq_acc, seq_version, pfamseq_id, sequence from pfamseq\" > pfamseq.tsv";
    my $bsub_pfamseq = $bsub . "-Jpfamseq -o pfamseq.log '". $command_pfamseq . "; gzip pfamseq.tsv'";
    print STDERR "Submitting pfamseq job to farm\n";
    system("$bsub_pfamseq\n") and die "Couldn't run '$bsub_pfamseq', $!";
}

#Sig
unless(-s "significant.tsv.gz") {
    my $command_sig = $command . "\"select pfamseq_acc, seq_start, seq_end, pfamA_acc, domain_evalue_score from pfamA_reg_full_significant\" > significant.tsv";
    my $bsub_sig = $bsub . "-Jsig -o significant.log '" . $command_sig ."; gzip significant.tsv'";
    print STDERR "Submitting significant regions job to farm\n";
    system("$bsub_sig\n") and die "Couldn't run '$bsub_sig', $!";
}

#Insig
unless(-s "insignificant.tsv.gz") {
    my $command_insig = $command . "\"select pfamseq_acc, seq_start, seq_end, pfamA_acc, domain_evalue_score from pfamA_reg_full_insignificant\" > insignificant.tsv";
    my $bsub_insig = $bsub . "-Jinsig -o insignificant.log '".$command_insig ."; gzip insignificant.tsv'";
    print STDERR "Submitting insignificant regions job to farm\n";
    system("$bsub_insig\n") and die "Couldn't run '$bsub_insig', $!";
}

#Clan
unless(-s "clans.tsv") {
    my $command_clan = $command . "\"select a.pfamA_acc, m.clan_acc, clan_id, pfamA_id, description from pfamA a left join clan_membership m on a.pfamA_acc=m.pfamA_acc left join clan c on m.clan_acc=c.clan_acc\" > clans.tsv";
    print STDERR "Creating clan file\n";
    system("$command_clan\n") and die "Couldn't run [$command_clan], $!";
}

#PfamA
unless(-s "pfamA.tsv") {
    my $command_pfamA = $command . "\"select pfamA_acc, pfamA_id, type, description from pfamA\" > pfamA.tsv";
    print STDERR "Creating pfamA file\n";
    system("$command_pfamA\n") and die "Couldn't run [$command_pfamA], $!";
}
