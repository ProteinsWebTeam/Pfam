#! /usr/bin/env perl 

use strict;
use warnings;
use Bio::Pfam::Config;
use Getopt::Long;
use Log::Log4perl qw(:easy);

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


my ($help, $dir, $memory_gb);
GetOptions( "dir=s" => \$dir,
            "help"  => \$help,
            "M=i"   => \$memory_gb);


if($help) {
  help();
}
unless($dir and -e $dir) {
  $logger->logdie("You need to specify the directory containing all the families on the command line.\ne.g. $0 -dir <directory>");
}

opendir(DIR, $dir) or $logger->logdie("Cannot open directory [$dir] $!");

my $config = Bio::Pfam::Config->new;
my $farm_queue=$config->{farm}->{lsf}->{queue};

my $fail=0;
chdir($dir) or $logger->logdie("Couldn't chdir into $dir, $!");

foreach (readdir(DIR))  {
  next unless(-d "$_");
  next if(/^\./);
  
  my %check;

  foreach my $file ( @{ $config->mandatoryFamilyFiles() } ) { #scores DESC ALIGN SEED PFAMOUT HMM
    if(-s "$_/$file") {
      $check{$file}=1;
    }
    elsif($file eq 'ALIGN' and -e "$_/ALIGN") {
      $check{$file}=1;
    }
    elsif($file eq 'scores' and -e "$_/scores") {
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
    if($memory_gb) {
      system("pfbuild -withpfmake -M $memory_gb") and $logger->logdie("pfbuild -withpfmake -M $memory_gb on $_ failed:[$!]");
    }
    else {
      system("pfbuild -withpfmake") and $logger->logdie("pfbuild -withpfmake on $_ failed:[$!]");
    }
    chdir("../");
    $fail++;
    next;
  }

  if(-M "$_/SEED" < -M "$_/HMM") {
    $logger->info("$_ has a HMM that is younger than SEED, running pfbuild -withpfmake on family");
    chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
    if($memory_gb) {
      system("pfbuild -withpfmake -M $memory_gb") and $logger->logdie("pfbuild -withpfmake -M $memory_gb on $_ failed:[$!]");
    }   
    else {
      system("pfbuild -withpfmake") and $logger->logdie("pfbuild -withpfmake on $_ failed:[$!]");
    }
    chdir("../");
    $fail++;
    next;
  }

  if(-M "$_/HMM" < -M "$_/PFAMOUT") {
    $logger->info("$_ has a PFAMOUT that is younger than HMM, running pfbuild -withpfmake on family"); 
    chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
    if($memory_gb) {
      system("pfbuild -withpfmake -M $memory_gb") and $logger->logdie("pfbuild -withpfmake -M $memory_gb on $_ failed:[$!]");
    }   
    else {
      system("pfbuild -withpfmake") and $logger->logdie("pfbuild -withpfmake on $_ failed:[$!]");
    }
    chdir("../");
    $fail++;
    next;
  }


  unless($check{"ALIGN"}) {	
    $logger->info("$_ is missing the ALIGN file, running pfmake on family"); 
    chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
    pfmake($_, $memory_gb, $farm_queue);
    chdir("../");
    $fail++;
    next; 
  }

  if(-M "$_/PFAMOUT" < -M "$_/ALIGN") {
    $logger->info("$_ has an ALIGN file that is younger than PFAMOUT, running pfmake on family"); 
    chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
    pfmake($_, $memory_gb, $farm_queue);
    chdir("../");
    $fail++;
    next; 
  }

  unless($check{"scores"}) {
    $logger->info("$_ has no scores file, running pfmake on family"); 
    chdir($_) or $logger->logdie("Couldn't change directory into $_ $!");
    pfmake($_, $memory_gb, $farm_queue);
    chdir("../");
    $fail++;
    next; 
  }

}
chdir("../") or $logger->logdie("Couldn't chdir up from $dir, $!");
print STDERR "$fail families failed to build/search correctly\n";



sub help {
  print <<EOF;
        
This script loops through all the families contained within the 
directory specified on the command line, and checks to see that 
all the family files are present. If any of the family files 
are missing, or are not in the correct timestamp order, 
it will run a pfbuild -withpfmake. If the PFAMOUT file is younger
than the ALIGN file, it will run a pfmake.

Usage: $0 -dir <dir>

Options:  
        -M   :amount of memory in gb to request on the farm for pfbuilds 
        
EOF
  exit(1);

}


sub pfmake {
  
  my ($fam, $memory_gb, $queue) = @_;

  my $memory_mb;
  if($memory_gb) {
      $memory_mb=$memory_gb*1000;
  }
  else {
      $memory_mb=4000;
  }
  system("bsub -q $queue -o pfmake.log -J$fam -M $memory_mb -R \"rusage[mem=$memory_mb]\" pfmake");

}
