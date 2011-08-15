#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use File::Basename;
use IPC::Open3;
use IO::Handle;
use String::Diff;
use Data::Dumper;
use Bio::Pfam::Config;
use Algorithm::Diff qw(sdiff);


my $config = Bio::Pfam::Config->new();


my $SEQDB = $config->pfamseqLoc() . '/pfamseq';
my $DEFAULT_LABEL_LEN = 15;

$|++;

main();

1;


sub main {
  # get input options
  my $opt = get_options();
  if (!$opt->{'file'} || $opt->{'help'}) {
    usage();
  }

  if (!-e $opt->{'file'}) {
    warn "Input file doesn't seem to exist!\n";
    usage();
  }

  my($fname, $dir, $suffix) = fileparse($opt->{'file'}, qr/\.[^.]*/);

  my $fa = convert_to_fasta($opt->{'file'});

  # get nearest match with hmmsearch and phmmer
  my $matches = get_matches_with_hmmer($fname, $dir, $suffix, $opt);

  strip_out_duplicates($matches);

  # add alignment spacing back into the new sequences and output as .seed
  open my $seedfile, '>', "$dir$fname.seed"
    or die "Couldn't open seed file for writing [$dir$fname.seed]: $!\n";

  open my $pfamfile, '>', "$dir$fname.pfam"
    or die "Couldn't open seed file for writing [$dir$fname.pfam]: $!\n";

  open my $errors, '>', "$dir$fname.errors"
    or die "Couldn't open errors file for writing [$dir$fname.errors]: $!\n";

  open my $in_align, '<', $opt->{'file'}
    or die "Couldn't open original alignment for reading [@{[$opt->{'file'}]}]: $! \n";

  # loop through the matches and find the longest id so we can set the length
  # of the labels in the output seed file
  set_label_length($matches);

  print $seedfile "# STOCKHOLM 1.0\n";

  my @realigned = ();
  my $insertions = {};

  foreach my $match (@$matches) {
    if ($match->[0]->{'fail'}) {
      $match->[0]->{'seed'}  = sprintf "#=GF CC  SEARCH FAILURE %-${DEFAULT_LABEL_LEN}s\n", $match->[0]->{query};
      $match->[0]->{'error'} = sprintf "SEARCH FAILED TO MATCH %s\n", $match->[0]->{query};
    }
    elsif ($match->[0]->{'duplicate'}) {
      $match->[0]->{'fail'} = 1;
      $match->[0]->{'error'} = sprintf "REMOVED QUERY SEQUENCE [%s] AS IT RESULTED IN DUPLICATE SEQUENCE [%s]\n", $match->[0]->{tname}, $match->[0]->{qname};
      $match->[0]->{'seed'} = sprintf "#=GF CC DUPLICATE %-${DEFAULT_LABEL_LEN}s\n", $match->[0]->{label};
    }
    else {
      rebuild_alignment($match->[0], $in_align, $insertions);
    }
    push @realigned, $match;
  }

  strip_removed_seq_inserts(\@realigned, $insertions);
  pad_alignments_for_inserts(\@realigned, $insertions);

  foreach my $result (@realigned) {
    if (exists $result->[0]->{'fail'}) {
      print $errors $result->[0]->{error};
      print $seedfile $result->[0]->{seed};
    }
    else {
      printf $seedfile "%-${DEFAULT_LABEL_LEN}s\t%s\n", $result->[0]->{label}, $result->[0]->{seq};
      printf $pfamfile "%-${DEFAULT_LABEL_LEN}s\t%s\n", $result->[0]->{label}, $result->[0]->{seq};
    }
  }

  print $seedfile "\n//";
  close $seedfile;
  close $errors;
  close $pfamfile;
  close $in_align;
  if (!$opt->{'skipclean'}) {
    cleanup("$dir$fname", $suffix);
  }
}

sub strip_removed_seq_inserts {
  my ($realigned, $insertions) = @_;
  # go through insertions and remove sequences that failed
  my %realigned_lookup = ();
  for my $seq (@$realigned) {
    if (!exists $seq->[0]->{fail}) {
      $realigned_lookup{$seq->[0]->{label}}++;
    }
  }

  for my $ins (keys %$insertions) {
    for my $ins_seq (keys %{$insertions->{$ins}}) {
      if (!exists $realigned_lookup{$ins_seq}) {
        delete $insertions->{$ins}->{$ins_seq};
      }
    }
    if (!keys %{$insertions->{$ins}}) {
     delete $insertions->{$ins};
    }
  }

  return;
}

sub pad_alignments_for_inserts {
  my ($realigned, $insertions) = @_;

  # get insertion points in reverse order
  for my $insert (sort { $b <=> $a } keys %$insertions) {
    for my $seq (@$realigned) {
      next if exists $seq->[0]->{fail};
      next if exists $insertions->{$insert}->{$seq->[0]->{label}};
      # add a . at insert position to all sequences not in
      # the list of insertions at that point
      substr($seq->[0]->{seq}, $insert, 0) = '.';
    }
  }

  return;
}

sub strip_out_duplicates {
  my ($matches) = @_;
  my $unique = [];
  my %seen   = ();
  for my $match (@$matches) {
    if ($match->[0]->{tname}) {
      if ($seen{$match->[0]->{tname}}) {
        warn "$match->[0]->{tname} was seen multiple times. Removing duplicates.\n";
        $match->[0]->{duplicate}++;
      }
      $seen{$match->[0]->{tname}}++;
    }
  }

  return;
}

sub cleanup {
  my ($file, $suffix) = @_;
  unlink "$file$suffix.fa";
  unlink "$file$suffix.fa.ssi";
  unlink "$file.hmm";
  unlink "$file.hmmsearch";
  unlink "$file.phmmerdb";
  unlink "$file.phmmerdb.ssi";
  unlink "$file.acclist";
  return;
}

sub set_label_length {
  my ($matches) = @_;
  foreach my $match (@$matches) {
    my $m = $match->[0];
    if ($m->{label} && (length $m->{label} > $DEFAULT_LABEL_LEN)) {
      $DEFAULT_LABEL_LEN = length $m->{label};
    }
  }
  return;
}

sub rebuild_alignment {
  my ($match, $infile, $insertions) = @_;

  my $original = undef;
  # get original seq out of infile
  my $qid = $match->{qname};
  if ($qid) {
    while(<$infile>){
      if ($_ =~ /^$qid\s+(.*)$/) {
        $original = $1;
        last;
      }
    }

    # convert all dashes (-) to dots (.) in the original sequence
    $original =~ s/-/\./g;

    # grab the new sequence out of pfamseq
    my $cmd = $config->hmmer3bin . "/esl-sfetch $SEQDB $match->{tname}";
    my $new_seq = `$cmd`;

    $new_seq =~ s/^>.*//;
    $new_seq =~ s/\n//gs;

    $new_seq = substr($new_seq, $match->{ali_from} - 1, $match->{ali_to} - $match->{ali_from} + 1);

    my $diffcount = 0;

    # merge the new and original sequence
    ($new_seq, $diffcount) = merge_seqs(uc $original, uc $new_seq, $match, $insertions);

    if (uc($original) ne uc($new_seq)) {
      my $diff = String::Diff::diff uc($original), uc($new_seq);

      # if more than 3 percent of residues differ, then fail and show diff"
      my $delta_percent = int(($diffcount * 100) / $match->{qlen});
      if ($delta_percent > 3) {
        my $qstring = sprintf "%-${DEFAULT_LABEL_LEN}s\t%s", $match->{qname}, $diff->[0];
        my $tstring = sprintf "%-${DEFAULT_LABEL_LEN}s\t%s", $match->{tname}, $diff->[1];
        my $message = qq(The differenecs between the original [$match->{qname}] and proposed sequence [$match->{tname}] were too great. It wont be added to the .seed file.:
$qstring
$tstring
\n);
        warn $message;
        $match->{'fail'} = 1;
        $match->{'seed'}  = sprintf "#=GF CC BAD MATCH %-${DEFAULT_LABEL_LEN}s\n", $match->{label};
        $match->{'error'} = sprintf $message;
      }
      else {
        # else add it to the seedfile
        my $qstring = sprintf "%-${DEFAULT_LABEL_LEN}s\t%s", $match->{qname}, $diff->[0];
        my $tstring = sprintf "%-${DEFAULT_LABEL_LEN}s\t%s", $match->{tname}, $diff->[1];
        warn qq(The original [$match->{qname}] and proposed sequence [$match->{tname}] differed by only $diffcount. It has been added to the .seed file.:\n);
        warn "$qstring\n";
        warn "$tstring\n";
        $match->{seq} = $new_seq;
      }
    }
    else {
      # print to .seed file
      $match->{seq} = $new_seq;
    }
  }
  return $match;
}

sub get_matches_with_hmmer {
  my ($fname, $dir, $suffix, $opt) = @_;
  # use hmmbuild to generate an hmm from the input
  warn "Generating HMM\n";
  my $cmd = $config->hmmer3bin . "/hmmbuild $dir$fname.hmm $dir$fname$suffix >/dev/null";
  system $cmd;

  # use hmmsearch to get a small database of potential matches
  if (exists $opt->{debug}) {
    warn "Skipping hmmsearch [debug]\n";
  }
  else {
    warn "Performing hmmsearch\n";
    $cmd = $config->hmmer3bin . "/hmmsearch --nobias --tblout $dir$fname.hmmsearch -E 1 -o /dev/null $dir$fname.hmm $SEQDB";
    system $cmd;
  }

  # parse the .hmmsearch file to get a list of sequences and use esl-sfetch to get them out of pfamseq
  parse_hmmsearch("$dir$fname");

  # for each input seq use phmmer to search against the new sub database
  my $matches = find_matches_with_phmmer($dir,$fname,$suffix);

  # return the best matches
  return $matches;
}

sub find_matches_with_phmmer {
  my ($dir,$fname,$suffix) = @_;
  my $matches = [];
  open my $in_fa, '<', "$dir$fname$suffix.fa"
    or die "Couldn't open generated fasta file for reading [$dir$fname$suffix.fa]: $!\n";
  local $/ = "\n>";
  while (<$in_fa>) {
    $_ =~ s/>//g;
    my $query = ">$_";
    my $match = run_phmmer_search($query,"$dir$fname");
    if (!$match->[0]) {
      my ($query_name) = $query =~ /^>(\w+)/;
      print STDERR "Couldn't find quick match for $query_name, running full search...";
      $match = run_phmmer_search($query,"$dir$fname",'full');
      if (!$match->[0]) {
        print STDERR "FAILED.\n";
        $match = [{fail => 'search', query => $query_name}];
      }
      else {
        print STDERR "SUCCESS.\n";
      }
    }
    push @$matches, $match;
  }
  return $matches;
}

sub run_phmmer_search {
  my ($query,$fname, $type) = @_;
  my ($reader, $writer, $err) = (IO::Handle->new, IO::Handle->new, IO::Handle->new);
  my $cmd = $config->hmmer3bin . "/phmmer --incdomE 10e-5 --domE 10e-5 --domtblout /dev/stderr - $fname.phmmerdb 1>/dev/null";
  if ($type && $type eq 'full') {
    $cmd = $config->hmmer3bin . "/phmmer --incdomE 10e-5 --domE 10e-5 --domtblout /dev/stderr - $SEQDB 1>/dev/null";
  }
  my $pid = undef;
  eval {
    $pid = open3($writer, $reader, $err, $cmd);
  };
  if ($@) {
    die "open3 failed: $!\n$@\n";
  }

  print $writer $query;
  close $writer;
  close $reader;

  my $match = [];
  local $/ = "\n";
  while (<$err>) {
    next if $_ =~ /^#/;

    my ($tname, $tacc, $tlen, $qname, $qacc, $qlen,
        $evalue, $score, $full_bias, $full_num, $full_of,
        $c_evalue, $i_evalue, $dom_score, $dom_bias,
        $hmm_from, $hmm_to, $ali_from, $ali_to, $env_from,
        $env_to, $acc, $description) = $_ =~ /^([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+([^\s]+)\s+(.*)/;
    if ($hmm_from == 1 && $hmm_to == $qlen && $acc >= 0.99) {
      push @$match, {
          qname    => $qname,
          qlen     => $qlen,
          tname    => $tname,
          ali_from => $ali_from,
          ali_to   => $ali_to,
          accuracy => $acc,
          label    => $tname . '/' . $ali_from . '-' . $ali_to,
        };
      last;
    }
  }

  return $match;
}

sub parse_hmmsearch {
  my ($file) = @_;
  warn "creating database for phmmer search\n";

  # create acc list to pipe into sfetch
  open my $results, '<', "$file.hmmsearch"
    or die "Couldn't open hmmsearch results file for reading [$file.hmmsearch]: $!\n";

  open my $acclist ,'>', "$file.acclist"
    or die "Couldn't open acclist for writing [$file.acclist]: $!\n";

  while(<$results>) {
    next if $_ =~ /^#/;
    my ($acc) = $_ =~ /^([^\s]+)/;
    print $acclist "$acc\n";
  }
  close $results;
  close $acclist;

  my $cmd = $config->hmmer3bin . "/esl-sfetch -o $file.phmmerdb -f $SEQDB $file.acclist >/dev/null";

  system $cmd;

  $cmd = $config->hmmer3bin . "/esl-sfetch --index $file.phmmerdb 1>/dev/null";
  system $cmd;
  return;
}

sub merge_seqs {
  my ($original, $new, $match, $ins) = @_;
  my @diffs = sdiff [split(//, $original)], [split(//, $new)];
  my $index = 0;
  my $dcount = 0;
  my $result = '';
  for my $position (@diffs) {
    if ($position->[0] eq 'u') {
      $result .= $position->[1];
    }
    elsif ($position->[1] eq '.' && $position->[2] eq '') {
      $result .= '.';
    }
    elsif ($position->[0] eq 'c' && $position->[1] eq '.') {
      $result .= $position->[2] . '.';
      $ins->{$index}->{$match->{label}}++;
      $dcount++;
    }
    elsif ($position->[0] eq 'c') {
      $result .= $position->[2];
      $dcount++;
    }
    elsif ($position->[0] eq '+') {
      $result .= $position->[2];
      $ins->{$index}->{$match->{label}}++;
      $dcount++;
    }
    elsif ($position->[0] eq '-') {
      $result .= '.';
      $dcount++;
    }
    $index++;
  }
  return $result, $dcount;
}

sub convert_to_fasta {
  # convert to fasta sequences with esl-reformat
  my ($input) = @_;
  warn "Creating fasta datadase from input file\n";
  my $cmd = $config->hmmer3bin . "/esl-reformat -o $input.fa  --informat stockholm fasta $input";
  system $cmd;

  if (-z "$input.fa") {
    warn "Failed to generate a fasta file. Possible duplicate sequence ids.\n";
    unlink "$input.fa";
    exit;
  }

  # index the fasta file for later
  $cmd = $config->hmmer3bin . "/esl-sfetch --index $input.fa 1>/dev/null";
  system $cmd;
  return "$input.fa";
}

sub usage {
  print qq($0 [options] -f <filename>
  options:
    -h|--help     - print this help message and exit.
    -s|skipclean  - don't cleanup the build files.\n\n);
  exit;
}

sub get_options {
  my $opt = {};
  GetOptions($opt, 'file=s', 'help', 'skipclean', 'debug');
  return $opt;
}
