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
  my $matches = get_matches_with_hmmer($fname, $dir, $suffix);

  strip_out_duplicates($matches);

  # add alignment spacing back into the new sequences and output as .seed
  open my $seedfile, '>', "$dir$fname.seed"
    or die "Couldn't open seed file for writing [$dir$fname.seed]: $!\n";

  open my $in_align, '<', $opt->{'file'}
    or die "Couldn't open original alignment for reading [@{[$opt->{'file'}]}]: $! \n";

  # loop through the matches and find the longest id so we can set the length
  # of the labels in the output seed file
  set_label_length($matches);

  print $seedfile "# STOCKHOLM 1.0\n";
  foreach my $match (@$matches) {
    rebuild_alignment($match, $in_align, $seedfile);
  }
  print $seedfile "\n//";
  close $seedfile;
  close $in_align;
  if (!$opt->{'skipclean'}) {
    cleanup("$dir$fname", $suffix);
  }
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
  my ($match, $infile, $seedfile) = @_;

  if ($match->[0]->{'fail'}) {
    printf $seedfile "#=GF CC  SEARCH FAILURE %-${DEFAULT_LABEL_LEN}s\n", $match->[0]->{query};
  }
  elsif ($match->[0]->{duplicate}) {
    printf $seedfile "#=GF CC DUPLICATE %-${DEFAULT_LABEL_LEN}s\n", $match->[0]->{label};
  }
  else {

    my $original = undef;
    # get original seq out of infile
    my $qid = $match->[0]->{qname};
    if ($qid) {
      while(<$infile>){
        if ($_ =~ /^$qid\s+(.*)$/) {
          $original = $1;
          last;
        }
      }

      # convert all dashes (-) to dots (.) in the original sequence
      $original =~ s/-/\./g;

      # grab spacings from the original
      my @spacings = ();
      while($original =~ /\.+/g) {
        push @spacings, [@-, @+];
      }

      # grab the new sequence out of pfamseq
      my $cmd = $config->hmmer3bin . "/esl-sfetch -c $match->[0]->{ali_from}..$match->[0]->{ali_to} $SEQDB $match->[0]->{tname}";
      my $new_seq = `$cmd`;

      $new_seq =~ s/^>.*//;
      $new_seq =~ s/\n//gs;

      # apply spacings to the original sequence in order;
      foreach my $space (@spacings) {
        substr($new_seq, int($space->[0]), 0) = '.' x ($space->[1] - $space->[0]);
      }

      if ($original ne $new_seq) {
        my $full_diff  = String::Diff::diff_fully($original, $new_seq);
        my $diff       = String::Diff::diff $original, $new_seq;
        my $diff_count = 0;

        for my $line (@$full_diff) {
          for my $rmatch (@$line) {
            if ($rmatch->[0] =~ /(-|\+)/) {
              $diff_count += length $rmatch->[1];
            }
          }
        }

        # if more than 2 residues differ, then fail and show diff"
        my $delta_percent = int(($diff_count * 100) / $match->[0]->{qlen});
        if ($delta_percent > 6) {
          warn qq(The differenecs between the original [$match->[0]->{qname}] and proposed sequence [$match->[0]->{tname}] were too great. It wont be added to the .seed file.:
  $match->[0]->{qname}\t$diff->[0]
  $match->[0]->{tname}\t$diff->[1]
  \n);
          printf $seedfile "#=GF CC BAD MATCH %-${DEFAULT_LABEL_LEN}s\n", $match->[0]->{label};
        }
        else {
          # else add it to the seedfile
          warn qq(The original [$match->[0]->{qname}] and proposed sequence [$match->[0]->{tname}] differed by only $diff_count. It has been added to the .seed file.:
  $match->[0]->{qname}\t$diff->[0]
  $match->[0]->{tname}\t$diff->[1]
  \n);
          printf $seedfile "%-${DEFAULT_LABEL_LEN}s\t%s\n", $match->[0]->{label}, $new_seq;
        }
      }
      else {
        # print to .seed file
        printf $seedfile "%-${DEFAULT_LABEL_LEN}s\t%s\n", $match->[0]->{label}, $new_seq;
      }
    }
  }
  return;
}

sub get_matches_with_hmmer {
  my ($fname, $dir, $suffix) = @_;
  # use hmmbuild to generate an hmm from the input
  if (! -e "$dir$fname.hmm") {
    warn "Generating HMM\n";
    my $cmd = $config->hmmer3bin . "/hmmbuild $dir$fname.hmm $dir$fname$suffix >/dev/null";
    system $cmd;
  }
  else {
    warn "using HMM found in: $dir$fname.hmm\n";
  }

  # use hmmsearch to get a small database of potential matches
  if (! -e "$dir$fname.hmmsearch") {
    warn "Performing hmmsearch\n";
    my $cmd = $config->hmmer3bin . "/hmmsearch --nobias --tblout $dir$fname.hmmsearch -E 1 -o /dev/null $dir$fname.hmm $SEQDB";
    system $cmd;
  }
  else {
    warn "skipping hmmsearch, found results in: $dir$fname.hmmsearch\n";
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
  if (! -e "$file.phmmerdb") {
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
  }
  else {
    warn "Using phmmer database found in: $file.phmmerdb\n";
  }
  return;
}

sub convert_to_fasta {
  # convert to fasta sequences with esl-reformat
  my ($input) = @_;
  if (! -e "$input.fa") {
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
  }
  else {
    warn "Using input fasta database found in: $input.fa\n";
  }
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
  GetOptions($opt, 'file=s', 'help', 'skipclean' );
  return $opt;
}
