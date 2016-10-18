#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Cwd;
use File::Copy;
use File::Touch;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::SeqDB;
use Bio::Rfam::SVN::Client;

my $config = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;
my $rfamseqObj = $config->rfamseqObj;
my $client = Bio::Rfam::SVN::Client->new({config => $config});

if($#ARGV == -1) {
  help();
}
my $pwd = getcwd;

my ($full, @ignore, $family, $help, $nospell, $nocoding);
&GetOptions(
      '-fam=s'=> \$family,
      '-i=s@' => \@ignore,
      '-h|help' => \$help,
      'nospell' => \$nospell,
      'nocoding' => \$nocoding,
      ) or die "Unknown option passed, try running $0 -h\n";
      
#This is coded like this for legacy reasons. The family can be passed in
#either way, an option or command line argument.
$family = shift;


help() if($help);

if(!$family){
  help();  
}

#rqc-passed file
my $out="$pwd/$family/qcpassed";

if (-e "$out"){
  copy($out, $out.'old') ;
  unlink("$out");
}

#load the family.
my $familyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );

#Build up the ignore hash.
my %ignore;
if (@ignore){
    print STDERR "A list of families to ignore in the overlap check has been given\n";
   %ignore = map{$_ => 1}@ignore;
}
$ignore{ $familyObj->DESC->AC }++ if($familyObj->DESC->AC);

open(my $L, '>', "$pwd/$family/allqc.log") 
  or die "Could not open $pwd/$family/allqc.log: [$!]\n";

#------------------------------------------------------------------------------
print STDERR "\n(1) CM check\n";
print $L "\n** CM check **\n";
my $error = 0;
my $masterError = 0;
eval{
  $error = Bio::Rfam::QC::checkCMFormat($familyObj);
};
print $L $@ if($@);
if ($error){
  $masterError++;
  print STDERR "\t--errors" 
} else { 
  print STDERR "\t--CM check completed with no major errors";
}
#------------------------------------------------------------------------------

print STDERR "\n(2) FORMAT CHECK\n";
print $L "\n** FORMAT check **\n";

$error = 0;
eval{
  $error = Bio::Rfam::QC::checkTimestamps("$pwd/$family", $config);
  $error = Bio::Rfam::QC::checkFamilyFormat( $familyObj ) if(!$error);
  #Spell check
  if(!$nospell){
    $error = Bio::Rfam::QC::checkSpell("$pwd/$family", $config->dictionary) if(!$error);
  }
};
print $L $@ if($@);
if ($error){ 
  $masterError++;
  print STDERR "\t--errors" 
} else { 
  print STDERR "\t--FORMAT check completed with no major errors";
}

#------------------------------------------------------------------------------

print STDERR "\n(3) OVERLAP CHECK\n";
print $L "\n** OVERLAP check **\n";

$error = 0;
eval{
  $error = Bio::Rfam::QC::checkOverlaps($familyObj, $config, \%ignore, "$pwd/$family");
};
print $L $@ if($@);
if ($error){ 
  $masterError++;
  print STDERR "\t--errors" 
} else { 
  print STDERR "\t--OVERLAP check completed with no major errors";
}

#------------------------------------------------------------------------------

print STDERR "\n(4) STRUCTURE CHECK\n";
print $L "\n** STRUCTURE check **\n";

$error = 0;
eval{
  $error = Bio::Rfam::QC::ssStats($familyObj, "$pwd/$family");
};
print $L $@ if($@);
if ($error){ 
  $masterError++;
  print STDERR "\t--errors" 
} else { 
  print STDERR "\t--STRUCTURE check completed with no major errors";
}

#------------------------------------------------------------------------------
print STDERR "\n(5) MISSING CHECK\n";
print $L "\n** MISSING check **\n";

$error = 0;

#Check the SEED
eval{
  $error = Bio::Rfam::QC::compareSeedAndScores($familyObj);
};
print $L $@ if($@);
if($error){
  $masterError++;
  print STDERR "\t--errors" 
}

#Check the old and new....but it may be a new family.
$error = 0;
my ($oldFamilyObj);
eval{
  if($familyObj->DESC->AC){
    #$client->checkFamilyExists($familyObj->DESC->AC);
    #$oldFamilyObj = $familyIO->loadRfamFromSVN( $familyObj->DESC->AC, $client );
    $oldFamilyObj = $familyIO->loadRfamFromLocalFile( $familyObj->DESC->AC, $pwd );
  }
};
print $L $@ if($@);
if ($error){ 
  $masterError++;
  print STDERR "\t--errors" 
}

if(defined($oldFamilyObj)){
my($found, $missing);
eval {
 ($found, $missing) = 
   Bio::Rfam::QC::compareOldAndNew($oldFamilyObj, $familyObj);
};
if(scalar(@$missing)){
  print $L $@ if($@);
  print STDERR "\t--errors (but non-fatal)" 
} else { 
  print STDERR "\t--MISSING check completed with no major errors";
}
}
#------------------------------------------------------------------------------
print STDERR "\n(6) SEQUENCE CHECK\n";
print $L "\n** SEQUENCE check **\n";

$error = 0;
eval{
  $error = $error = Bio::Rfam::QC::checkSEEDSeqs($familyObj, $rfamseqObj );
  $error = Bio::Rfam::QC::checkScoresSeqs($familyObj, $rfamseqObj) if(!$error);
};
print $L $@ if($@);
if ($error){ 
  $masterError++;
  print STDERR "\t--errors" 
} else { 
  print STDERR "\t--SEQUENCE check completed with no major errors";
}
#------------------------------------------------------------------------------
if($nocoding){ 
  print STDERR "\n(7) SKIPPING NON-CODING CHECK (due to -nocoding option)\n";
}
else { # -nocoding not enabled
  print STDERR "\n(7) NON-CODING CHECK\n";
  print $L "\n** NON-CODING check **\n";

  $error = 0;
  my ($coding);
  eval{
    ($error, $coding) = Bio::Rfam::QC::codingSeqs($familyObj, $config);
  };
  print $L $@ if($@);
  if ($error){ 
    print $L $coding if($coding);
    $masterError++;
    print STDERR "\t--errors" 
  } else { 
    print STDERR "\t--NON-CODING check completed with no major errors";
  }
}
#------------------------------------------------------------------------------

#And in summary!

if ($masterError){
    print STDERR "\n\n****Family failed rqc-all checks:YOU CANNOT CHECK IT IN****\n";
}else{
    print $L "\nFamily passed with no serious errors\n\n";
    print STDERR "\n\nFamily passed with no serious errors\n\n";
    touch($out);
}
close($L);

sub help {
  
  print<<EOF;
  
USAGE: $0 <options> <family>

Performs all QC steps against the family.

OPTIONS:

 fam             : family to be QC, instead of passing as a parameter via ARGV.
 i <family>      : List of accessions to ignore during the overlap check.
 h|help          : prints thiss help message.
 nospell         : Does not run the spelling QC check, thereby permitting running as
                 : as non-interactive process, e.g. for f in `ls`; do rqc-all.pl \$f; done;
 nocoding        : Does not run the coding QC check

EOF

exit;

}

