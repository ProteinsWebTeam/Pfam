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

my ($full, @ignore, $family);
&GetOptions(
      '-fam=s'=> \$family,
      '-i=s@' => \@ignore
      );
      
#This is coded like this for legacy reasons. The family can be passed in
#either way, an option or command line argument.
$family = shift;

#rqc-passed file
my $out="$pwd/$family/qcpassed";

if (-e "$out"){
  copy($out, $out.'old') ;
  unlink("$out");
}

my $ignore_string;
if (@ignore){
    print STDERR "A list of families to ignore in the overlap check has been given\n";
    $ignore_string="-i ";
    $ignore_string.=join( " -i ", @ignore);
    print STDERR $ignore_string, "\n"; 
}

my $familyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );

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
};
print $L $@ if($@);
if ($error){ 
  $masterError++;
  print STDERR "\t--errors" 
} else { 
  print STDERR "\t--FORMAT check completed with no major errors";
}

=head


system ("echo '\n**OVERLAP ERRS**\n' >> $errlog");
my $overlap;
if($ignore_string){    
    print STDERR "\n(3) OVERLAP CHECK - ignoring $family and $ignore_string\n";
    $overlap=system ("rqc-overlap-rdb.pl $family -i $family $ignore_string 1>> $outlog 2>> $errlog");
    }
else{
    print STDERR "\n(3) OVERLAP CHECK - ignoring $family\n";
    $overlap=system ("rqc-overlap-rdb.pl $family -i $family 1>> $outlog 2>> $errlog");
}
if ($overlap){ $error=1; print STDERR "\t--errors"} else{ print STDERR "\t--overlap check completed with no major errors";}

=cut
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

#Check the old and new....
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
#And in summary!

if ($masterError){
    print STDERR "\n\n****Family failed rqc-all checks:YOU CANNOT CHECK IT IN****\n";
}else{
    print $L "\nFamily passed with no serious errors\n\n";
    print STDERR "\n\nFamily passed with no serious errors\n\n";
    touch($out);
}
close($L);