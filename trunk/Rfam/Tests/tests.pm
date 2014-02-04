#! /usr/bin/perl
#
# tests.pm
# Eric Nawrocki
# EPN, Wed Jan 22 13:38:29 2014
#
# Perl module with subroutines used for testing Rfam scripts.
# All subroutine names are prefixed with T.
# 
# List of subroutines in this file:
#
# TcleanUpAndDie:           unlink files and die with provided error message
# TcleanUp:                 unlink files listed in a passed in array
# TrunCommand:              run a command with a 'system' call. 
# TcopyFiles:               copy a set of files to the current dir
# TrunScript:               run a Rfam script, e.g. rfmake.pl
# TparseLogForOutputFiles:  parse a Rfam script log file for a list of output files it created
# TnumLinesInFile:          determine number of lines in a file
#

# All of these subroutines (except TcleanUpAndDie(), see below) return
# 1 or more values. The first value is always either "" or a non-empty
# string. If it is "" this means the subroutine completed without
# error. If it is a non-empty string the subroutine did not complete
# properly and the string serves as the error message. The caller
# should handle an error by unlinking all temporary files and then
# die'ing with the error message, this is usually done with a call
# to TcleanUpAndDie.
#
# TcleanUpAndDie removes all files in a passed in array and 
# then dies with the provided error message, it is the only subroutine
# in this module that does not return either "" or an error message to
# the caller.
#
use strict;
use warnings;

#####################################################################
###############
# SUBROUTINES #
###############
sub TcleanUpAndDie {
  if(scalar(@_) != 2) { return "ERROR TcleanUpAndDie: entered with wrong number of input args"; }
  my ($errmsg, $unlinkAR) = (@_);
  foreach my $file (@{$unlinkAR}) { 
    if(-e $file) { unlink $file; }
  }
  die $errmsg;
}
###############
sub TrunCommand {
  if(scalar(@_) != 1) { return "ERROR TrunCommand: entered with wrong number of input args"; }
  my ($cmd) = (@_);
  system($cmd);
  if($? != 0) { return "ERROR TrunCommand: command $cmd failed"; }
  return ""; # success
}
###############
sub TcleanUp {
  if(scalar(@_) != 1) { return "ERROR TcleanUp: entered with wrong number of input args"; }
  my ($unlinkAR) = (@_);
  foreach my $file (@{$unlinkAR}) { 
    if(-e $file) { unlink $file; }
    if(-e $file) { return "ERROR TcleanUp: unable to unlink $file"; }
  }
  return ""; # success
}
###############
sub TcopyFiles {
  if(scalar(@_) != 2) { return "ERROR TcopyFiles: entered with wrong number of input args"; }
  my ($datadir, $fileAR) = (@_);
  foreach my $file (@{$fileAR}) { 
    if(! -e $datadir . "/" . $file) { return "ERROR TcopyFiles: unable to copy required file $file from $datadir"; }
    my $status = TrunCommand("cp $datadir/$file ."); 
    if($status ne "") { return $status; } # return error code if TrunCommand didn't finish properly 
  }
  return ""; # success
}
###############
sub TrunScript { 
  if(scalar(@_) != 5) { return "ERROR TrunScript: entered with wrong number of input args"; }
  my ($script, $options, $logfile, $unlinkAR, $testctr) = (@_);
  my $status = TrunCommand("$script $options");
  if($status ne "") { return $status; } # return error code if TrunCommand didn't finish properly 

  # parse output, find files we output and add them to unlinkAR
  my @newfilesA = TparseLogForOutputFiles($logfile);
  push(@{$unlinkAR}, @newfilesA);

  return ""; # success
}
###############
sub TparseLogForOutputFiles {
  if(scalar(@_) != 1) { return "ERROR TparseLogForOutputFiles: entered with wrong number of input args"; }
  my ($logfile) = (@_);
  
  my @newfileA = ();

  open(LOG, $logfile) || return "ERROR TparseLogForOutputFiles: unable to open $logfile";
  my $found_outfiles = 0;
  my $line;
  while($line = <LOG>) { 
    if($line =~ m/^\# file name\s+description\s*\n$/) { 
      $found_outfiles = 1;
      $line = <LOG>; # next line is just ====== ======= line
      $line = <LOG>;
      while($line !~ m/^\#/) { 
        chomp $line;
        $line =~ s/^\s+//;
        $line =~ s/\s+.*$//;
        push(@newfileA, $line);
        $line = <LOG>;
      }
    }
  }
  if(! $found_outfiles) { return "ERROR TparseLogForOutputFiles: unable to find output file list in $logfile"; }

  return ("", @newfileA); # "" indicates success
}
###############
sub TnumLinesInFile {
  if(scalar(@_) != 1) { return "ERROR TnumLinesInFile: entered with wrong number of input args"; }
  my ($infile) = (@_);

  open(IN, $infile) || return "ERROR TnumLinesInFile: unable to open $infile";
  my $cnt = 0;
  while(<IN>) { $cnt++; }
  close(IN);
  
  return ("", $cnt);
}
###############
sub TverifyNonEmptyFileExists {
  if(scalar(@_) != 1) { return "ERROR TverifyNonEmptyFileExists: entered with wrong number of input args"; }
  my ($file) = (@_);

  if(! -e $file) { return "ERROR TverifyNonEmptyFileExists: expected file $file does not exist"; }
  if(! -s $file) { return "ERROR TverifyNonEmptyFileExists: expected file $file exists but is empty"; }

  return (""); # success
}
###############

return 1;

