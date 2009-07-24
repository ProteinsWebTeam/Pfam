#!/software/bin/perl

use strict;
use warnings;
use File::Copy;
use Getopt::Long;


use Bio::Pfam::Config;
use Bio::Pfam::FamilyIO;


my( $seq, $version, $start, $end, $family, $help );

&GetOptions (
		"seq=s"     => \$seq,
		"s=s"       => \$start,
		"e=s"       => \$end,
		"version=s" => \$version,
		"family=s"  => \$family,
		"h"         => \$help
);


my $config = Bio::Pfam::Config->new;
my $familyIO = Bio::Pfam::FamilyIO->new;

unless(-s "SEED"){
  die "Can not find the SEED file in the current working directory\n";
}

unless(-s "DESC"){
  warn "Could not find DESC file in working directoy, going to write generic DESC file\n"; 
  $familyIO->writeEmptyDESC;  
}

my $descObj = Bio::Pfam::FamilyIO->parseDESC("DESC");

#Now add the new data to the DESC object (if we have any).
if($seq and $version and $end and $start and $family){
  #We have all of the information to add to the nested location hash.
  my $nest;
  $nest = $descObj->NESTS if($descObj->NESTS);
  push(@$nest, { dom => $family, seq => "$seq.$version", from => $start, to => $end });   
  $descObj->NESTS($nest);
}

#Now check that te sequences in the nested locations hash are present in the seed alignment.
open(S, "SEED") or die "Could not open SEED alignment:[$!]\n";
my $seed = Bio::Pfam::AlignPfam->new;
$seed->read_selex(\*S);
close(S);

foreach my $nestLoc (@{ $descObj->NESTS }){
  my ( $thisSeq, $thisVersion ) = $nestLoc->{seq} =~ /(\S+)\.(\d+)/; 
  my $seen = 0;
  foreach my $seqObj ($seed->each_seq_with_id($thisSeq)){
    if($nestLoc->{from} < $seqObj->end and  $nestLoc->{from} > $seqObj->start and
       $nestLoc->{to} < $seqObj->end and  $nestLoc->{to} > $seqObj->start){
      unless($thisVersion eq $seqObj->version){
        die "Sequence version mis-match for $thisSeq.  Alignment has [".
            $seqObj->version."], nested location has [$thisVersion]\n";       
      }      
      $seen++;
      last;       
    }
  }
  unless($seen){
    die "Could not find sequence corresponding to ".
      $nestLoc->{seq}."/".$nestLoc->{from}."-".$nestLoc->{to}." in the SEED alignment\n";  
  }
}

# Start the process of getting the inital HMMER determined match states
system("sreformat stockholm SEED > _SEED.sto") and 
  die "Failed to run sreformat on SEED:[$!]\n";
system($config->hmmer3bin."/hmmbuild -o /dev/null -O _SEED.rf _HMM _SEED.sto") and
  die;
system('sreformat --pfam -u stockholm _SEED.rf > _SEED.rf.sto') and die;



#Now read the alignment in to an object!
open(S, "_SEED.rf.sto") or die "Could not open _SEED.rf.sto alignment:[$!]\n";
my $seedRF = Bio::Pfam::AlignPfam->new;
$seedRF->read_stockholm(\*S);
close(S);

#Munge that object.
my $RFline = $seedRF->match_states_string->display;

foreach my $nestLoc (@{ $descObj->NESTS }){
  my ( $thisSeq, $thisVersion ) = $nestLoc->{seq} =~ /(\S+)\.(\d+)/;
  # $pos = $aln->column_from_residue_number('1433_LYCES', 14); # = 6;
  my $startPos = $seedRF->column_from_residue_number($thisSeq, $nestLoc->{from});
  my $endPos   = $seedRF->column_from_residue_number($thisSeq, $nestLoc->{to});
  my $length   = $endPos -$startPos + 1;
  my $dots     = '.' x $length;
  substr($RFline, $startPos - 1, $length, $dots); 
}
$seedRF->match_states_string->display($RFline);

foreach my $seq ($seedRF->each_seq){
  my $seqStr = $seq->seq;
  #$seqStr = uc($seq);
  $seqStr =~ tr/-/\./;
  $seq->seq($seqStr);
}

copy("SEED", "SEED.beforeRF");
open(S, ">SEED");
$seedRF->write_Pfam(\*S);

foreach my $f (qw(_SEED.sto _SEED.rf _HMM _SEED.slx)){
  unlink($f);
}

copy("DESC", "DESC.beforeRF");
$familyIO->writeDESC($descObj, ".");  

warn "Please varify that the SEED alignment is okay as it may have been altered by HMMER!!\n";
        

sub help {  
  
print <<EOF;
$0 will add HMMER readable match states to your SEED
alignment and mask out the sequence information for all sequences between 
the start and end sequence co-ordinates of a designated sequence.   

Useage:
$0 -seq [sed_id] -version [seq version] -s [start coos] -e [end coos] -family [nested family acceesion]

The command line parameter are stored in the DESC file.

EOF

exit;
  
}
