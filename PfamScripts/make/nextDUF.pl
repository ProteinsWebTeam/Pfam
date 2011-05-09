#!/software/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless($pfamDB){
  die "Could not connect to pfam database\n";  
}

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
if (! -e "DESC"){exit 0;}

open (DESC, "DESC") or die "Cannot open DESC";
open (DESCNEW, "> DESCNEW") or die "Cannot write to file DESCNEW";

print DESCNEW "ID   $id\nDE   Domain of unknown function ($id)\n";

while(<DESC>){
    if (/^ID/){
	next;
    }
    if (/^DE/){
	next;
    }
    if (/^WK/){
	next;
    }

    print DESCNEW;

    if (/^TP/){
        print DESCNEW "WK   Domain_of_unknown_function\n";
    }
}
close DESC;
close DESCNEW;

# Do file moves
system("cp DESC DESCOLD");
system("cp DESCNEW DESC");
