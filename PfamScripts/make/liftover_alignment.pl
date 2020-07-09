#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use POSIX qw(ceil);
use File::Temp qw(tempfile);
use Bio::Pfam::Config;

my ($alignment, $hmm_file, $scores_file);
my $evalue_threshold = 1e-10; #Default evalue

&GetOptions("align=s" => \$alignment,
  "hmm=s"   => \$hmm_file,
  "scores=s" => \$scores_file, 
  "evalue=f" => \$evalue_threshold);

unless($alignment and -s $alignment) {
  print STDERR "Need to supply alignment on the command line.\nE.g. $0 -align SEED\n";
  exit;
}


#Check E-value is sensible, or set to default value if not defined
unless($evalue_threshold >= 0 and $evalue_threshold <= 0.01) {
  die "The E-value threshold must be a value between 0 and 0.01\n";
}
print STDERR "Using E-value threshold of $evalue_threshold\n";


#Create HMM from alignment file if not supplied on command line
if($hmm_file and !-s $hmm_file) {
    print STDERR "The HMM [$hmm_file] does not exist\n";
    exit;
}
else {
  print STDERR "Making HMM from $alignment\n";
  $hmm_file = "$alignment.hmm";
  system("hmmbuild -o /dev/null $hmm_file $alignment") and die "Couldn't run 'hmmbuild -o /dev/null $hmm_file $alignment', $!";
}


#Define some parameters
my $config = Bio::Pfam::Config->new;
my $pfamseq_db = $config->pfamseqLoc."/pfamseq";
my $mini_db = "miniDB.fasta"; #Name of mini database that phmmer searches will be run against


#Make mini database using either the sequences in the scores file, or by running a hmmsearch against pfamseq
if($scores_file) {
  unless(-s $scores_file) {
    print STDERR "The scores file [$scores_file] does not exist";
    exit;
  }
  
  #Make a temporary file containing the accessions in the scores file
  my ($tmp_fh, $tmp_file ) = tempfile();
  my $c=0;
 
  my %added; #Use to prevent same sequence being added twice
  open(SCORES, $scores_file) or die "Couldn't open fh to $scores_file, $!";
  while(<SCORES>) {
    if(/^\S+\s+(\S+\.\d+)\/\S+\s+\S+\s+(\S+)/) {  #124.2 Q9J3E9.1/3921-4009 3921-4009 6.1e-33
      my ($acc, $evalue) = ($1, $2);
      if($evalue <= $evalue_threshold) {
        next if(exists($added{$acc}));

        $c++;
        print $tmp_fh "$acc\n"; 
        $added{$acc}=1;
      }
    }
  }
  close SCORES;
  close $tmp_fh;

  unless($c > 0) {
    warn "There were no sequences in the $scores_file file that satisfied the E-value theshold of $evalue_threshold\n"; 
    exit;
  }
  print STDERR "Found $c sequences in the $scores_file file that satified the E-value threshold of $evalue_threshold\n";

  #Make a mini db using accessions
  print STDERR "Creating mini database containing $c sequences\n";
  make_fasta($pfamseq_db, $tmp_file, $mini_db);
}
else {
  #Search HMM against pfamseq
  print STDERR "Running hmmsearch against pfamseq\n";
  my $cpu = 4;
  my $db_size = $config->dbsize;
  my $hmmsearch_results = "hmmsearch.tbl";
  
  system("hmmsearch -o /dev/null --noali --tblout $hmmsearch_results --incE $evalue_threshold --cpu $cpu -Z $db_size $hmm_file $pfamseq_db") and die "Couldn't run 'hmmsearch -o /dev/null --noali --tblout $hmmsearch_results --incE $evalue_threshold --cpu $cpu -Z $db_size $hmm_file $pfamseq_db', $!";

  #Make a temporary file containing the accessions in the hmmsearch results file
  my ($tmp_fh, $tmp_file ) = tempfile();
  my $c=0;
  my %added; #Use to prevent same sequence being added twice
  open(HMMSEARCH, $hmmsearch_results) or die "Couldn't open fh to $hmmsearch_results, $!";
  while(<HMMSEARCH>) { 
    next if (/^#/);
    if(/^(\S+\.\d+)\s+/) {
      my $acc = $1;
      next if (exists($added{$acc}));

      print $tmp_fh "$1\n"; #print sequence accessions to file
      $c++;
      $added{$acc}=1;
    }
  }
  close HMMSEARCH;
  close $tmp_fh;

  unless($c > 0) {
    warn "There were no sequences in the hmmsearch results file ($hmmsearch_results)\n";
    exit;
  }
  print STDERR "Found $c sequences in the hmmsearch results\n";

  #Make a mini db using accessions
  print STDERR "Creating mini database containing $c sequences\n";
  make_fasta($pfamseq_db, $tmp_file, $mini_db);
}


#Run a phmmer against mini database for each sequence in the input alignment
my $best_hit_fasta = "phmmer_best_hit.fa";
my $change_log = "change.log";
print STDERR "Running phmmer on each sequence in SEED alignment against mini database\n";
open(PF, ">$best_hit_fasta") or die "Couldn't open fh to $best_hit_fasta, $!";
open(CL, ">$change_log") or die "Couldn't open fh to $change_log, $!";
open(SEED, $alignment) or die "Couldn't opn fh to $alignment, $!";
my ($total_seq, $count) = (0, 0);
my %added; #Use to prevent same sequence being added twice
while(<SEED>) {
  if(/^(\S+\/\d+-\d+)\s+(\S+)/ or /^(\S+)\s+(\S+)/) {  #May or may not have co-ordinates
    my ($acc, $seq) = ($1, $2);

    my $acc_se = $acc;
    if($acc =~ /(\S+)\/\d+-\d+/) { #Strip off co-ordinates if present
      $acc = $1;
    }
    $total_seq++;

    $seq =~ s/[-.]//g; #Remove gaps
    $seq = uc($seq); #Make uppercase

    #Create fasta file for sequence
    open(F, ">$acc.fa") or die "Couldn't open fh to $acc.fa, $!";
    print F ">$acc\n$seq\n";
    close F;

    #phmmer against mini database
    system("phmmer -A $acc.aln --incE $evalue_threshold -o /dev/null $acc.fa $mini_db") and die "Couldn't run 'phmmer -A $acc.aln --incE $evalue_threshold -o /dev/null $acc.fa $mini_db', $!";
    open(PHMMER, "$acc.aln") or die "Couldn't open fh to $acc.aln, $!";

    #Add best hit to fasta file
    my ($best_seq_acc, $best_seq) = ("", "");
    while(<PHMMER>) {
      next if(/^#/ or /^\s+/);
      if(/(\S+)\s+(\S+)/) {
        ($best_seq_acc, $best_seq) = ($1, $2);
        print CL "$acc_se => $best_seq_acc\n";  #Keep a log of what was changed

        last if(exists($added{$best_seq_acc})); #Don't add things more than once
        
        $added{$best_seq_acc}=1;
        $best_seq =~ s/[-.]//g; #Remove gaps
        $best_seq = uc($best_seq); #Make uppercase

        print PF ">$best_seq_acc\n$best_seq\n";
        $count++;
        last;
      }
    }
    close PHMMER;

    unless($best_seq) {
      print CL "$acc_se => no phmmer match to mini database\n";
    }
    unlink("$acc.aln");
    unlink("$acc.fa");
  }
}
close SEED;
close PF;
close CL;


#Align sequences to HMM using hmmalign
if($count > 0) {
  my $new_alignment = "$alignment.phmmer";
  system("hmmalign $hmm_file $best_hit_fasta > $alignment.tmp") and die "Couldn't run 'hmmalign $hmm_file $best_hit_fasta > $alignment.tmp', $!";

  #Reformat to Pfam style alignment (strip out # lines from stockholm format)
  pfam_format("$alignment.tmp", $new_alignment);
  print STDERR "Found a replacement sequence for $count/$total_seq sequences in $alignment alignment from the mini database (see $new_alignment and $change_log)\n";
}


#############################


sub make_fasta {
  #Takes input of database (in fasta format), file of accessions, outfile name
  #Creates a fasta file containing the sequences in argument 2

  my ($db, $accs_file, $outfile) = @_;

  system("esl-sfetch -f $db $accs_file > $outfile") and die "Couldn't run 'esl-sfetch -f $db $accs_file > $outfile', $!\n";
}

sub pfam_format {

  #Modified from pfmake.pl
  #Reformats stockholm format to Pfam style aligment (ie one line per sequence)
  #and deletes orginal alignment

  my ($original_alignment, $new_alignment) = @_;
  
  my %reformat;
  my $maxlength = 0;
  open(A1, $original_alignment) or die "Couldn't open $original_alignment, $!";
  open(A2, ">$new_alignment") or die "Couldn't open $new_alignment, $!";

  while(<A1>){
    if(/^(\S+)\s+(\S+)$/){
      $maxlength = length($1) if ($maxlength < length($1));
      $reformat{$1} .= $2;
    }elsif(/^#/){
      next;
    }elsif(/\/\//){
      next;
    }elsif(/^$/){
      next;
    }else{
      warn "Did not parse $_ from the stockholm format\n";
    }
  }
  close(A1);

  $maxlength += 2;
  foreach my $nse (keys %reformat){
    print A2 sprintf("%-".$maxlength."s", $nse);
    print A2 $reformat{$nse}."\n";
  }
  close(A2);

  if(!-s $new_alignment) {
    warn "$new_alignment file has zero size, this is probably not good\n";
  }
  else {
    unlink($original_alignment);
  }
  return;
}
