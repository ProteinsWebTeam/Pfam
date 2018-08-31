#!/usr/bin/env perl

#Script to calculate coverge stats for uniprot, ncbi and metaseq using the flatfiles
#This script is a wrapper around flatfile_stats.pl
#It is run via make_release_from_rdb.pl

use strict;
use warnings;
use Bio::Pfam::Config;
use Getopt::Long;


my $flatfile_dir;
GetOptions("flatfile_dir=s" => \$flatfile_dir);


die "Need to specifiy flatfile directory on the commandline\n$0 -flatfile_dir <dir>\n" unless(-d $flatfile_dir);

my $config = Bio::Pfam::Config->new;


my $stats_dir="stats";

#Set the location of the fasta files
my %db;
$db{'metaseq'} = $config->{metaseq}->{location}."/metaseq";
$db{'ncbi'} = $config->{ncbi}->{location}."/ncbi";
$db{'uniprot'} = $config->{uniprot}->{location}."/uniprot";

#Create the stats directory
unless(-d $stats_dir) {
  mkdir($stats_dir, 0755) or die "Couldn't make $stats_dir, $!";
}
chdir($stats_dir) or die "Couldn't chdir into $stats_dir, $!";

#Run esl_seqstat, then run the flatfile_stats.pl script on each db
foreach my $db_name (keys %db) {
  my $esl_stats = $db_name ."_stats"; #metaseq_stats
  if(-s $esl_stats) {
    print STDERR "Already made $esl_stats\n";
  }
  else {
    esl_seqstat($esl_stats, $db{$db_name});
  }

  my $stats_file = $db_name.".txt"; #metaseq.txt
  my $flatfile;
  if($db_name eq 'metaseq') {
    $flatfile=$flatfile_dir."/Pfam-A.full.metagenomics";
  }
  else {
    $flatfile = $flatfile_dir."/Pfam-A.full.".$db_name;
  }
  if(-s $stats_file) {
    print STDERR "Already made $stats_file\n";
  }
  else {
    flatfile_stats($esl_stats, $stats_file, $flatfile);
  }
}

sub esl_seqstat {
  my ($filename, $db_location) = @_;

  print STDERR "Making $filename\n";
  system("esl-seqstat $db_location > $filename") and die "Error running 'esl-seqstat $db_location > $filename', $!";
}

sub flatfile_stats {  
  my ($esl_file, $outfile, $flatfile) = @_;

  unless(-s $flatfile) {
    die "$flatfile does not exist\n"
  }
  
  print STDERR "Making $outfile\n";
  open(ESL, $esl_file) or die "Couldn't open fh to $esl_file, $!";

  my ($no_seqs, $no_res) = (0, 0);
  while(<ESL>) {
    if(/Number of sequences:\s+(\d+)/) {
      $no_seqs=$1;
    }
    elsif(/Total # residues:\s+(\d+)/) {
      $no_res=$1;
    }
  }
  close ESL;

  unless($no_seqs and $no_res) {
    die "Failed to retrieve number of sequences and/or residues from $esl_file\n";
  }

  system("flatfile_stats.pl $flatfile $no_seqs $no_res > $outfile") and die "Error running 'flatfile_stats.pl $flatfile $no_seqs $no_res > $outfile', $!";
}
