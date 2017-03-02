#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;

my ($dir, $help);

  &GetOptions(
        "help"   => \$help,
        "dir=s"  => \$dir);  

if($help) {
  help();
}


unless(-d $dir) {
  help();
}

chdir($dir) or die "Couldn't chdir into $dir, $!";

#Get list of all log files
my @logs = glob("*/*log");
my %log_files;

#Put log files in a hash, with dir as key
#This will help organise the log files and deal with multiple log files being in the same dir
foreach my $file (@logs) {
  if($file =~ /(\S+)\/(\S+)/) {
    my ($directory, $log_file) = ($1, $2);
    $log_files{$directory}{$log_file}=1;
  }
}

#Go through the files and re-run any that have failed
my $count=0;
my $total=0;
foreach my $directory (sort keys %log_files) {
  print STDERR "Checking $directory - ";
  $total++;

  my $success;
  foreach my $log_file (sort keys %{$log_files{$directory}}) {
    open(LOG, "$directory/$log_file") or die "Couldn't open fh to $directory/$log_file, $!";
    while(<LOG>) {
      if(/^Success/) {
        $success=1;
        last;
      }   
    }   
    close LOG;
    if($success) {
      last;
    }
 
  }

  if($success) {
    print STDERR "ran successfully\n";
  }
  else { #Doesn't look to have run successfully, so re-run it, this time asking for more memory
    print STDERR "failed\n";
    chdir($directory) or die "Couldn't chdir into $directory, $!";
    print STDERR "Re-running $directory\n";
    system("pfjbuild -N 3 -fa seq.fa -gzip -M 8");
    $count++;
    chdir("../") or die "Couldn't chdir up from $directory, $!";
  }
}

print STDERR "\n\n$count/$total families have been set to re-run\n";


sub help {
  print <<EOF;

This script should be used to re-run any pfjbuilds that have
failed to run on the farm. You need to pass in a directory on
the command line that contains all the pfjbuild directories. 
The script will read the log files in each pfjbuild dir, and 
re-run any that have failed, this time requesting more memory.

Usage:

$0 -dir <dir>";

EOF

  exit(1);

}
