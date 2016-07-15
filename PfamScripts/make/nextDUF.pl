#!/usr/bin/env perl

use strict;
use warnings;
use File::Slurp;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless($pfamDB){
  die "Could not connect to pfam database\n";  
}

#Mapping between types and DE lines
my %desc = ( 'Family' => 'Family',
             'Domain' => 'Domain',
             'Motif'  => 'Motif',
             'Repeat' => 'Repeat',
             'Disordered' => 'Disordered region',
             'Coiled-coil' => 'Coiled-coil region');

#Need to get a list of all pfam ids;
my @dufNos;
my $pfams    = $pfamDB->getAllPfamFamilyDataLike('DUF%');
my $deadFams = $pfamDB->getAllDeadFamilyData; 
foreach my $fam (@{$pfams}, @{$deadFams}){
  if($fam->pfama_id =~ /^DUF(\d+)$/){
    push(@dufNos, $1); 
  }
}

my @dufNosSort = sort {$b <=> $a} @dufNos;

my $no = $dufNosSort[0] + 1;
my $id="DUF$no";

print "\n The next available DUF number is: $id\n\n";

# Now check if in family dir overwrite DESC info
if (! -e "DESC"){
  #This is bad
  warn "Could not find DESC file, expected it in the current working directory\n";
  exit 0;}


#Read in the desc file
my @desc = read_file("DESC");

#Now determine the type of the family.
my $type = 'Domain';
foreach my $line (@desc){
  if($line =~ /^TP\s+(\S+)/){
    $type = $1;
  }
}


#Open up a new DESC file
open (DESCNEW, "> DESCNEW") or die "Cannot write to file DESCNEW";
print DESCNEW "ID   $id\nDE   ".$desc{$type}." of unknown function ($id)\n";

#Now transfer everything across from the existing DESC file!
foreach (@desc){
    if (/^ID/){
    #We have just written this above
	  next;
    }
    if (/^DE/){
      #We have just written this above
	next;
    }
    if (/^WK/){
      #Defaults to the DUF wikipedia line  
	    next;
    }

    print DESCNEW;

    if (/^TP/){
        print DESCNEW "WK   Domain_of_unknown_function\n";
    }
}
close DESCNEW;

# Do file moves, such that we can fall back.
system("cp DESC DESCOLD");
system("cp DESCNEW DESC");
