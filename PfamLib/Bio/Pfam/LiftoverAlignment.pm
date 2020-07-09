=head1 NAME

LiftoverAlignment

=head1 SYNOPSIS

Contains routines used by liftover_alignment.pl script

=cut

package Bio::Pfam::LiftoverAlignment;

use strict;
use warnings;
use Getopt::Long;
use File::Temp qw(tempfile);

=head2 make_hmm

 Title    : make_hmm
 Usage    : Bio::Pfam::LiftoverAlignment::make_hmm($alignment, $hmm_outfile_name)
 Function : Make a HMM from input alignment, write it to $hmm_outfile_name
 Returns  : Nothing
 Args     : Alignment, HMM outfile name

=cut

sub make_hmm {
  my ($alignment, $hmm_outfile) = @_;
  system("hmmbuild -o /dev/null $hmm_outfile $alignment") and die "Couldn't run 'hmmbuild -o /dev/null $hmm_outfile $alignment', $!";
}

=head2 make_database_from_scores

 Title    : make_database_from_scores
 Usage    : Bio::Pfam::LiftoverAlignment::make_database_from_scores($scores_file, $evalue_threshold, $mini_db_name) 
 Function : Makes a fasta file of all sequences that are present in the scores file and satisfy the 
            E-value threshold supplied
 Returns  : Nothing
 Args     : Scores file, E-value threshold, outfile name for fasta file

=cut

sub make_database_from_scores {
  my ($scores_file, $evalue_threshold, $database, $mini_db) = @_;

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

  #Make a mini db using accessions
  print STDERR "Creating mini database containing $c sequences\n";
  _make_fasta($database, $tmp_file, $mini_db);
}

=head2 make_database_from_hmmsearch

 Title    : make_database_from_hmmsearch
 Usage    : Bio::Pfam::LiftoverAlignment::make_database_from_hmmsearch
 Function : Runs hmmsearch with HMM against database using E-value threshold supplied. Makes a fasta file 
            of all sequences in the hmmsearch outfile.
 Returns  : Nothing
 Args     : HMM file, E-value threshold, outfile name for fasta file, database to run hmmsearch against, database size

=cut

sub make_database_from_hmmsearch {

  my ($hmm_file, $evalue_threshold, $mini_db, $database, $db_size) = @_;

  #Search HMM against pfamseq
  print STDERR "Running hmmsearch against $database\n";
  my $cpu = 4;
  my $hmmsearch_results = "hmmsearch.tbl";
  
  system("hmmsearch -o /dev/null --noali --tblout $hmmsearch_results --incE $evalue_threshold --cpu $cpu -Z $db_size $hmm_file $database") and die "Couldn't run 'hmmsearch -o /dev/null --noali --tblout $hmmsearch_results --incE $evalue_threshold --cpu $cpu -Z $db_size $hmm_file $database', $!";

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

  #Make a mini db using accessions
  print STDERR "Creating mini database containing $c sequences\n";
  _make_fasta($database, $tmp_file, $mini_db);
}

=head2 make_alignment_from_phmmer

 Title    : make_alignment_from_phmmer
 Usage    : Bio::Pfam::LiftoverAlignment::make_alignment_from_phmmer($alignment, $hmm_file, $evalue_threshold, $database_to_phmmer_against)
 Function : Runs a phmmer search of each sequence in the alignment againt the database using the E-value 
            suppplied. The top hit for each phmmer is aligned to the HMM supplied.
 Returns  : Nothing
 Args     : Alignment, HMM, E-value threshold, database to phmmer against

=cut

sub make_alignment_from_phmmer {

  my ($alignment, $hmm_file, $evalue_threshold, $mini_db) = @_;

  my $best_hit_fasta = "phmmer_best_hit.fa";
  my $change_log = "change.log";

  print STDERR "Running phmmer on each sequence in $alignment alignment against mini database\n";

  open(PF, ">$best_hit_fasta") or die "Couldn't open fh to $best_hit_fasta, $!";
  open(CL, ">$change_log") or die "Couldn't open fh to $change_log, $!";
  open(ALN, $alignment) or die "Couldn't opn fh to $alignment, $!";

  my ($total_seq, $count) = (0, 0);
  my %added; #Use to prevent same sequence being added twice

  while(<ALN>) {
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
  close ALN;
  close PF;
  close CL;

  if($count > 0) {
    my $outfile = "$alignment.phmmer";
    _run_hmmalign($hmm_file, $best_hit_fasta, $outfile);
    print STDERR "Found a replacement sequence for $count/$total_seq sequences in $alignment alignment from the mini database\n";
  }
  else {
    warn "0 replacement sequences were found in the mini database\n";
    exit;
  }
}

=head2 

 Title    : _run_hmm_align
 Usage    : Bio::Pfam::LiftoverAlignment::_run_hmm_align
 Function : Aligns sequences to HMM
 Returns  : Nothing
 Args     : HMM file, fasta file containing sequences to align, name of output alignment

=cut

sub _run_hmmalign {
  my ($hmm_file, $fasta, $outfile) = @_;

  #Align sequences to HMM using hmmalign
  my $tmp_file = "tmp.$$";
  
  system("hmmalign $hmm_file $fasta > $tmp_file") and die "Couldn't run 'hmmalign $hmm_file $fasta > $tmp_file', $!";
  
  #Reformat to Pfam style alignment (strip out # lines from stockholm format)
  _convert_to_pfam_format($tmp_file, $outfile);
  
  unlink($tmp_file);
}

=head2 _make_fasta

 Title    : _make_fasta
 Usage    : Bio::Pfam::_make_fasta($database, $file_of_seq_accessions. $outfile_name)
 Function : Creates a fasta file from a list of sequence accessions 
 Returns  : Nothing
 Args     : Database to retrived sequences from, list of accessions, name of outfile to write to

=cut

sub _make_fasta {
  #Takes input of database (in fasta format), file of accessions, outfile name
  #Creates a fasta file containing the sequences in argument 2

  my ($db, $accs_file, $outfile) = @_;
  
  system("esl-sfetch -f $db $accs_file > $outfile") and die "Couldn't run 'esl-sfetch -f $db $accs_file > $outfile', $!\n";
}

=head _convert_to_pfam_format

 Title    : _convert_to_pfam_format
 Usage    : Bio::Pfam::AlignmentLiftover::_convert_to_pfam_format
 Function : Reformats stockholm format to Pfam style alignment (ie one line per sequence)
 Returns  : Nothing
 Args     : Alignment, name of output alignment

=cut

sub _convert_to_pfam_format {

  #Modified from pfmake.pl

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
  
  return;
}

1;
