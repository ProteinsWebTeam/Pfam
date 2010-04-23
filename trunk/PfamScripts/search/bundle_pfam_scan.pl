#! /software/bin/perl -w

#Script to make a tar ball of pfam_scan.pl and its associated modules for the ftp site

use strict;
use File::Path;
use File::Copy;

my $pfam_scan_root = "/software/pfam/Scripts/PfamScripts/search";
my $modules_root = "/software/pfam/Modules/PfamLib/";
my $directory = "PfamScan";
my $script = "pfam_scan.pl";

print STDERR "pfam_scan.pl will be copied from $pfam_scan_root\n";
print STDERR "Modules will be copied from $modules_root\n\n\n"; 


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



