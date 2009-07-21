#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::SVN::Client;

#-------------------------------------------------------------------------------
#Get the family name
unless($ARGV[0]){
  help();  
}

my $family = $ARGV[0];
chomp($family);
#-------------------------------------------------------------------------------
#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
$client->checkFamilyExists($family);

#-------------------------------------------------------------------------------
#Get local and SVN based copies of the family.
my $pwd          = getcwd();
my $familyIO     = Bio::Pfam::FamilyIO->new;
my $localFamObj  = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
my $remoteFamObj = $familyIO->loadPfamAFromSVN($family, $client);

my $success = Bio::Pfam::PfamQC::noMissing($localFamObj, $remoteFamObj, $family);

if($success){
  exit(0);
}else{
  warn "You are missing sequences compared to the SVN copy of the family\n Consult $family/missing\n";
  exit(1); 
}


sub help {
 
print <<EOF;

  $0 <FAMILY ACCESSION>
  
This will take the SEED and FULL alignments from the local copy of the family and
compare it to the SVN version of the family.

EOF

exit(1);
}
