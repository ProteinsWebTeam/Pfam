#! /software/bin/perl -w

use strict;
use Bio::Pfam::Config;
use Getopt::Long;
use Log::Log4perl qw(:easy);

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


my $dir;
GetOptions( "dir=s" => \$dir);


unless($dir and -e $dir) {
    $logger->logdie("You need to specify the directory containing all the families on the command line.\ne.g. $0 -dir <directory>");
}

opendir(DIR, $dir) or $logger->logdie("Cannot open directory [$dir] $!");

my $config = Bio::Pfam::Config->new;

my $fail=0;

foreach (readdir(DIR))  {
    next unless(-d "$dir/$_");
    next if(/^\./);

    my %check;

    foreach my $file ( @{ $config->mandatoryFamilyFiles() } ) {
	if(-s "$_/$file") {
	    $check{$file}=1;
	}
    } 

    unless($check{"SEED"}) {
	$logger->logdie("$_ has no SEED file - serious error!");
	$fail++;
	next;
    }

    unless($check{"DESC"}) {
	$logger->logdie("$_ has no DESC file - serious error!");
	$fail++;
	next;
    }

    unless($check{"HMM"} and $check{"PFAMOUT"}) {
	$logger->info("$_ is missing the HMM and/or PFAMOUT file, running pfbuild -withpfmake on family");
        chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
        system("pfbuild -withpfmake") and $logger->logdie("pbuild -withpfmake on $_ failed:[$!]");
	chdir("../");
	$fail++;
	next;
    }

    if(-M "$_/SEED" < -M "$_/HMM") {
	$logger->info("$_ has a HMM that is older than SEED, running pfbuild -withpfmake on family");
        chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
        system("pfbuild -withpfmake") and $logger->logdie("pfbuild -withpfmake on $_ failed:[$!]");
	chdir("../");
	$fail++;
	next;
    }
    
    if(-M "$_/HMM" < -M "$_/PFAMOUT") {
	$logger->info("$_ has a HMM that is older than PFAMOUT, running pfbuild -withpfmake on family"); 
        chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
        system("pfbuild -withpfmake") and $logger->logdie("pbuild -withpfmake on $_ failed:[$!]");
	chdir("../");
	$fail++;
	next;
    }


    unless($check{"ALIGN"}) {	
	$logger->info("$_ is missing the ALIGN file, running pfmake on family"); 
        chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
        system("pfmake") and $logger->logdie("pfmake on $_ failed:[$!]");
	chdir("../");
	$fail++;
	next; 
    }

    if(-M "$_/PFAMOUT" < -M "$_/ALIGN") {
	$logger->info("$_ has a PFAMOUT file that is older than ALIGN, running pfmake on family"); 
        chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
        system("pfmake") and $logger->logdie("pfmake on $_ failed:[$!]");
	chdir("../");
	$fail++;
	next; 
    }

    unless($check{"scores"}) {
	$logger->info("$_ has no scores file, running pfmake on family"); 
        chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
        system("pfmake") and $logger->logdie("pfmake on $_ failed:[$!]");
	chdir("../");
	$fail++;
	next; 
    }

}

print STDERR "$fail families failed to build/search correctly\n";
