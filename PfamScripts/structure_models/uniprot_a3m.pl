#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Compress::Zlib;
use Getopt::Long;
use File::Copy;

#Need to pass in pfamA on the command line
#The script will get the uniprot alignment from the release database
#(need to point the pfam config at a release database before running this script)
#The script will generate an a3m alignment for the family, and will
#include the first sequence of the seed alignment at the top of the a3m alignment
#The a3m alignment will be written to a direcory in the cwd called Alignments (the script
#will create this directory if it doesn't already exist).




my ($pfamA_acc);
GetOptions('pfamA=s'  => \$pfamA_acc);

unless($pfamA_acc and $pfamA_acc =~ /^PF\d{5}$/) {
    die "Need to pass pfamA accession on the command line\nE.g. $0 -pfamA PF00001\n";
}


my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

if($pfamDB->{database} eq "pfam_live") {
    die "Config points to the pfam_live database, but you need to point it at a release database (eg pfam_34_0)\n";
}

make_a3m_aln($pfamA_acc, $pfamDB);

sub make_a3m_aln {

    my ($pfamA_acc, $pfamDB) = @_;

    #Get first line of seed alignment
    my $seed_seq="$pfamA_acc.seed";
    my $rs_seed = $pfamDB->getSchema->resultset('AlignmentAndTree')->find({ pfama_acc => $pfamA_acc, type => 'seed' } );
    my $seed_alignment = Compress::Zlib::memGunzip($rs_seed->alignment) ;
    my @seed_alignment=split(/\n/, $seed_alignment);

    my ($seed_seq_id, $seed_seq_st, $seed_seq_en, $seed_sequence);
    open(S, ">$seed_seq") or die "Couldn't open fh to $seed_seq, $!";
    foreach my $line (@seed_alignment) {  #One sequence per line (ie alignment is not interleaved)
        next if($line =~ /^#/ or $line =~ /^\/\//);

        if($line =~ /^(\S+)\/(\d+)-(\d+)\s+(.+)/) {
            ($seed_seq_id, $seed_seq_st, $seed_seq_en, $seed_sequence) = ($1, $2, $3, $4);
            if($seed_sequence =~ /X/) {  #Need a sequence without an X
                next;
            }
            else {
                print S "$line\n";
                last;
            }
        }
    }
    close S;

    unless($seed_seq_id and $seed_seq_st and $seed_seq_en) {
        die "Couldn't parse sequence from $pfamA_acc seed alignment\n";
    }

    #Convert to fasta format
    my $fasta = "$pfamA_acc.fa";
    system("esl-reformat fasta $seed_seq > $fasta") and die "Couldn't run 'esl-reformat fasta $seed_seq > $fasta', $!";

    #Get the sequence accession for the seed seqeunce from the db
    #We will remove this sequence from the uniprot alignment
    my $rs_uniprot = $pfamDB->getSchema->resultset('Uniprot')->find({ uniprot_id => $seed_seq_id } );
    my $seed_seq_acc = $rs_uniprot->uniprot_acc.".".$rs_uniprot->seq_version;

    #Get uniprot alignment
    my $rs_aln = $pfamDB->getSchema->resultset('AlignmentAndTree')->find({ pfama_acc => $pfamA_acc, type => 'uniprot' } );
    my $alignment = Compress::Zlib::memGunzip($rs_aln->alignment) ;
    my @alignment=split(/\n/, $alignment);

    #Print uniprot alignment
    my $uniprot_aln="$pfamA_acc.uniprot";
    my $uniprot_count=0;
    open(ALN, ">$uniprot_aln") or die "Couldn't open $uniprot_aln, $!";
    foreach my $line (@alignment) {  #One sequence per line (ie alignment is not interleaved)
        if($line =~ /^#/ or $line =~ /^\/\//) {
            next;
        }
        elsif($line =~ /^$seed_seq_acc\/(\d+)-(\d+)/) {  #Don't print the seed sequence that we've added to the top of the fasta file
            my ($start, $end) = ($1, $2);
            if($seed_seq_st > $end or $seed_seq_en < $start) { #If it doesn't overlap with the seed sequence, then print it
                print ALN "$line\n";
                $uniprot_count++;
            }
        }
        else {
            print ALN "$line\n";
            $uniprot_count++;
        }
    }
    close ALN;

    #There is a small handful of families (4 for Pfam 35.0) where there is only one sequence in the uniprot alignment
    #Won't be able to do structure prediction for these, so just exit
    if($uniprot_count < 2) {
        print STDERR "Less than 2 sequences in the uniprot alignment, will not make a3m alignment for this family\n";
        unlink($seed_seq, $fasta, $uniprot_aln);
        exit;
    }

    #Convert uniprot alignment to fasta format and add to fasta file
    system("esl-reformat fasta $uniprot_aln >> $fasta") and die "Couldn't run 'esl-reformat fasta $uniprot_aln >> $fasta', $!";

    #Print HMM to file
    my $hmm_file = "$pfamA_acc.hmm";
    my $rs_hmm = $pfamDB->getSchema->resultset('PfamAHmm')->find({ pfama_acc => $pfamA_acc});
    open(HMM, ">$hmm_file") or die "Coudln't open fh to $hmm_file, $!";
    print HMM $rs_hmm->hmm;
    close HMM;

    #Align fasta to HMM
    my $hmmalign_sto = $pfamA_acc.".sto";
    system("hmmalign $hmm_file $fasta > $hmmalign_sto") and die "Couldn't run 'hmmalign $hmm_file $fasta > $hmmalign_sto', $!";

    my $alns_dir = "Alignments";
    unless(-d $alns_dir) {
        mkdir($alns_dir, 0755);
    }

    #Reformat alignment to A3M
    my $hmmalign_a3m = $pfamA_acc.".a3m";
    system("reformat.pl $hmmalign_sto $alns_dir/$hmmalign_a3m") and die "Couldn't run 'reformat.pl $hmmalign_sto $alns_dir/$hmmalign_a3m', $!";

    #Tidy up
    unlink($seed_seq, $fasta, $uniprot_aln, $hmm_file, $hmmalign_sto);
}
