#!/usr/bin/env perl

#Script to generate md5 checksums for all files in a particular dir
#md5 checksums are written to a file in the cwd called md5_checksums
#Need to pass dir on the command line
#Run this on the ftp dir, the database_files and the proteomes dir


use strict;
use warnings;
use Getopt::Long;
use File::Slurp;



my ($dir);
GetOptions('dir=s'  => \$dir);

unless(-d $dir) {
  print STDERR "Need to pass the directory containing all the files on the command line\n";
  print STDERR "E.g. $0 -dir <dir_location>\n";
  exit;
}

my @files = read_dir($dir);
my @sorted_files = sort @files;
my $outfile = "md5_checksums";
if(-s $outfile) {
  unlink($outfile);
}

foreach my $file (@sorted_files) {
  next if (-d $file);
  next if ($file eq 'userman.txt');
  next if ($file eq 'relnotes.txt');
  print STDERR "Doing $file\n";
  system("md5sum $file >> $outfile") and die "Problem running 'md5sum $file >> $outfile', $!";
}

