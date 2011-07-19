#!/usr/local/bin/perl -w

# A script to look for files in each directory
# and report directories that are missing the file

# Usage: pud-checkfile <dir> <filename>
#
# Options:  -size will report files that have no size or don't exist
#
# e.g. pud-checkfile.pl /lustre/pfam/pfam/Production/ALL_FAMILIES HMM_ls
# Will check all directories in <dir> for an HMM_ls file.

use Getopt::Long;
use strict;

my ($size,@dirs);

&GetOptions('size!'      => \$size);

my $dir  = shift;
my $file = shift;

opendir(DIR,$dir) || die "No directory $dir $!";
@dirs = readdir(DIR);
shift(@dirs);
shift(@dirs);
closedir(DIR);

foreach my $fam ( @dirs ) {
  if (! -d "$dir/$fam"){ # Ignore files in directory
    next;
  }
  if ($size){
    if (! -s "$dir/$fam/$file"){
      print "No $dir/$fam/$file\n";
    }
  } else {
    if (! -e "$dir/$fam/$file"){
      print "No $dir/$fam/$file\n";
    }
  }
}
