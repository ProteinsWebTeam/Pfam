#!/usr/bin/env perl

#Script to annotate ID, DE and AU lines in mgnifam DESC files
#The next MGDUF number is calculated and used to populate the DE and ID lines
#The author is specified on the command line using the -author flag
#Either a directory containing the families can be supplied using the -directory flag, or the cwd is used
#The script works on all directories that start MGYP

use strict;
use warnings;
use File::Copy;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::MGnifam;
use Bio::Pfam::FamilyIO;


my ($directory, $author);

GetOptions('directory=s' => \$directory,
             'author=s' => \$author);


#Check inputs           
unless($directory) {
  print STDERR "No directory specified, will look for families in current working directory\n";
  $directory = ".";
}
unless($author) {
  print STDERR "Please specify author(s) to add to AU line, eg $0 -author 'Mistry J, Finn RD'\n";
  exit;
}

#Set up db connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new(%{$config->pfamliveAdmin});

unless($pfamDB and $pfamDB->{database} eq "mgnifam") {
  die "Need to use the config for mgnifam\n";
}

#Read in MGYP families
opendir(DIR, $directory) or die "Can't open dir $directory, $!";
my @families = grep { /^MGYP/ } readdir DIR;
closedir DIR;

#Get next MGDUF number
my $next_MGDUF_number=Bio::Pfam::MGnifam::next_MGDUF_number($pfamDB);

my $n=0;
my $list = "annotated_list";
open(LIST, ">$list") or die "Couldn't open fh to $list, $!";
foreach my $family (@families) {
  print STDERR "Working on $family\n";
  $n++;
  copy("$family/DESC", "$family/DESC.old.$$") or die "Failed to copy $family/DESC to $family/DESC.old.$$, $!";

  #Add MGDUF number
  my $ID_line = "MGDUF".$next_MGDUF_number;
  my $DE_line = "Protein of unknown function (MGDUF". $next_MGDUF_number.")";

  open(NEWDESC, ">$family/DESC") or die "Couldn't open fh to $family/DESC. $!";
  open(OLDDESC, "$family/DESC.old.$$") or die "Couldn't open DESC.old.$$, $!";
  while(<OLDDESC>) {
    if(/^ID/) {
      print NEWDESC "ID   $ID_line\n";
    }
    elsif(/^DE/) {
      print NEWDESC "DE   $DE_line\n";
    }
    elsif(/AU/) {
      print NEWDESC "AU   $author\n";
    }
    else {
      print NEWDESC $_;
    }
  }
  close OLDDESC;
  close NEWDESC;

  $next_MGDUF_number++;
  print LIST "$family\n";
}
close LIST;

if($n) {
  print STDERR "Annotated $n DESC files\n";
}
else {
  print STDERR "No directories were found that start with /MGYP/\n";
}
