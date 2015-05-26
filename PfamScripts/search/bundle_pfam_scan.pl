#! /usr/bin/env perl 

#Script to make a tar ball of pfam_scan.pl and its associated modules for the ftp site

use strict;
use warnings;
use File::Path;
use File::Copy;
use Getopt::Long;

#Get input dir
my ($pfam_scan_root, $modules_root);
GetOptions('script=s' => \$pfam_scan_root,
              'lib=s' => \$modules_root);

#Check dirs/files exist 
my $script = "pfam_scan.pl";
unless($modules_root and -d $modules_root and -s "$pfam_scan_root/$script") {
  die "Need to specify directory location of pfam_scan.pl, and directory location of Pfam modules\nE.g. $0 -script /nfs/production/xfam/pfam/software/Scripts/PfamScripts/search/ -lib /nfs/production/xfam/pfam/software/Modules/PfamLib/\n";
}

my $directory = "PfamScan";

#Make PfamScan directory
unless(-d $directory) {
    mkdir($directory, 0775) or die "Couldn't create directory '$directory', $!";
}
chdir($directory) or die "Couldn't change directory into '$directory', $!";


#Copy the script
my $file = "$pfam_scan_root/$script";
copy($file, $script) or die "Copying $file to $script failed, $!";
chmod(0775, $script) or die "Couldn't change permissions on $file, $!\n";  #Need to do this as 'copy' changes file permissions


#Copy active site modules
my @active_site = qw(as_search.pm);
copy_files($modules_root, "Bio/Pfam/Active_site", \@active_site);


#Copy scan modules
my @scan = qw (PfamScan.pm Seq.pm);
copy_files($modules_root, "Bio/Pfam/Scan", \@scan);


#Copy HMM modules
my @hmm = qw (HMM.pm HMMIO.pm HMMMatch.pm HMMResults.pm HMMResultsIO.pm HMMSequence.pm HMMUnit.pm);
copy_files($modules_root, "Bio/Pfam/HMM", \@hmm);


chdir("../") or die "Couldn't change up a directory, $!";
print STDERR "Copied all the files, going to make tar ball\n";


#Create tar ball and gzip
system("tar -cvf PfamScan.tar PfamScan") and die "Couldn't make tar ball, $!";
system("gzip PfamScan.tar") and die "Couldn't gzip PfamScan.tar, $!";


#Remove PfamScan directory
rmtree($directory) or die "Couldn't remove direcotory '$directory', $!";


sub copy_files {

    my ($root, $dir, $files) = @_;

    unless(-d $dir) {
      mkpath([$dir, '.'], 1, 0777); 
    } 
    
    foreach my $file (@$files) {
      my $file1 = "$root/$dir/$file";
      my $file2 = "$dir/$file";

      copy($file1, $file2) or die "Copying $file1 to $file2 failed, $!";
    }
}



