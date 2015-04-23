#!/usr/bin/env perl

use strict;
use warnings;
use DBI;
use Bio::LocatableSeq;
use Getopt::Long;

my $family;
GetOptions("family=s" => \$family);

my @families;
if($family) {
  push(@families, $family);
}
else {
  my $dir = ".";
  opendir(DIR, $dir) or die "Couldn't directory open '$dir', $!";
  @families = sort grep { ! /^\./ } readdir(DIR);
}


#Get a db connection
my $dbh = DBI->connect("dbi:mysql:database=merops;host=mysql-merops-curation:port=4408", "admin", "gCox9MH5");

#Set up queries for family and subfamily levels
my $st_family = $dbh->prepare("SELECT Name, MERNO, PU, Features.AC, Features.AT, Features.ML, Features.MT, TXMerno FROM Features LEFT JOIN Families ON Features.MERNUM = TXMerno WHERE Features.Family = ? and SeqID=?");   
my $st_subfamily = $dbh->prepare("SELECT Name, MERNO, PU, Features.AC, Features.AT, Features.ML, Features.MT, TXMerno FROM Features LEFT JOIN Subfamilies ON Features.MERNUM = TXMerno WHERE Features.Subfamily = ? and SeqID=?");

#hmmer bin directory
my $hmmerBin="/nfs/production/xfam/users/jaina/merops/bin/"; 

#Go through each family and filter based match length and presence of active sites
foreach my $fam (@families)  {
  print STDERR "Doing $fam\n";

  my $hmmLength=hmmLength("$fam/HMM");
  
  #Now store active site data for holotype sequences, and store sequences from hmmer search in a hash for later
  my (%sequences, %alignment, %seed, %uniquePattern, %asPattern, %acc_se, %alignment2);
  my $align="$fam/ALIGN";
  open(ALIGN, $align) or die "Couldn't open fh to $align, $!";
  while(<ALIGN>) {
    if(/^((\S+)\/(\d+)-(\d+))\s+(\S+)/) {
      my ($acc_se, $acc, $start, $end, $seq)=($1, $2, $3, $4, $5);

      #Store a shorter version of sprot and trembl accession
      #sp|Q9XEC4|APA3_ARATH/70-508
      #tr|A0A067GI16|A0A067GI16_CITSI/72-514
      #Use the shortened version for printing alignments later
      my $short_acc;
      if($acc_se =~ /[tr|sp]\|(\S+)\|\S+/) {
	$short_acc="$1/$start-$end"
      }
      else {
	$short_acc="$acc/$start-$end";
      }
      $short_acc=sprintf('%-30s', $short_acc); #Uniprot acc can be 10 char, so 30 characters for the acc/start-ends should give plenty of room
      
      if($acc =~ /^H\|/) { #It's a holotype seq
	#Extract holotypes active sites for holotype seq from db
	#Populate hash %uniquePattern - the keys are unique active site patterns in the form '67C:308[S|T]:311H:' 
	#Populate hash %asPatterns - in the form $asPattern{$acc_se}->{$colNumber}=$residue
	
	my $seqObj = Bio::LocatableSeq->new('-id'=>$acc, '-start'=>$start, '-end'=>$end, 'seq' => $seq);
	if($fam =~ /[A-Z]\d+[A-Z]/) {  #It's a subfamily
	  getActSites($seqObj, $st_subfamily, $fam, $acc, $acc_se, \%uniquePattern, \%asPattern, $start, $end);
	}
	else { #It's a family
	  getActSites($seqObj, $st_family, $fam, $acc, $acc_se, \%uniquePattern, \%asPattern, $start, $end);
	}
	$seed{$acc_se}=$short_acc."$seq\n";
      }
      elsif($acc =~ /^MER\d+/) { #It's from the original clustal phylum alignment from Neil
	$seed{$acc_se}=$short_acc."$seq\n";
      }
      else { #It's from the hmmer3 search against uniprot, store for later
	$sequences{$acc_se}=$seq;
	$alignment{$acc_se}=$short_acc."$seq\n";
      }
    }
    elsif(/^\/\//) {
    }
    else {
      die "Unrecognised line in $align: $_";
    }
  }
  close ALIGN;

  printActSites(\%asPattern, $fam, "$fam/activeSite.dat");

  #All sequences in alignment ('ALIGN') have an evalue of <=0.01
  #Filter sequences based on match length
  #If active site data - filter on whether seq contains active site pattern
  #Sequences that do not pass filters go into a file called additionalHits
  #Sequences that pass filter go into a file called homologues
  open(ADDITIONAL, ">$fam/additionalHits") or die "Couldn't open fh to $fam/additionalHits, $!";
  open(HOMOLOGUES, ">$fam/homologues.aln") or die "Couldn't open fh to $fam/homologues.aln, $!";

  #Print seed sequences to homologues file
  foreach my $acc_se (keys %seed) {
    print HOMOLOGUES $seed{$acc_se};
  }

  my %checkEvalue;
  foreach my $acc_se (keys %sequences) { 
    my ($acc, $start, $end);
    if($acc_se =~ /^(\S+)\/(\d+)-(\d+)$/) {
      ($acc, $start, $end) = ($1, $2, $3);
    }
    else {
      die "Couldn't parse $acc_se";
    }
    
    #See if length is >50% of length, if not print it to additional file
    my $len = $end-$start+1;
    my $prop=$len/$hmmLength*100;
    
    unless($prop>50) {
      print ADDITIONAL $alignment{$acc_se};
      next;
    }
    
    
    if(-s "$fam/activeSite.dat") { #There is active site data, so look for the presence of the active site pattern
      
      my $match=lookForActSite($sequences{$acc_se}, \%asPattern, $fam);
      if($match) {
	print HOMOLOGUES $alignment{$acc_se};
      }
      else {
	print ADDITIONAL $alignment{$acc_se};
      }
    }
    else {
      #It's passed the length filter, and there is no active site data
      print HOMOLOGUES $alignment{$acc_se}
    }
  }
  close ADDITIONAL;
  close HOMOLOGUES;

  #Reformat homologue alignment into aligned fasta format
  system("$hmmerBin/esl-reformat -o $fam/homologues.afa --gapsym=- afa $fam/homologues.aln"); #Change gap symbol to '-' (raxml requires this)

}

sub hmmLength{
  my $hmm = shift;
  
  my $hmmLength;
  open(HMM, $hmm) or die "Couldn't open fh to $hmm, $!";
  while(<HMM>) {
    if(/^LENG\s+(\d+)$/) {
      $hmmLength=$1;
      last;
    }
  }
  close HMM;

  if($hmmLength) {
    return($hmmLength);
  }
  else {
    die "Couldn't get length of HMM from $hmm" unless($hmmLength);
  }
}
sub getActSites {
  my ($seqObj, $sth, $fam, $acc, $acc_se, $uniquePattern, $asPattern, $start, $end) = @_;

  if($acc =~ /^H\|(\S+)$/) { #Remove leading 'H|' from acc
    $acc=$1;
  }

  $sth->execute($fam, $acc) or die "Couldn't execute statement ".$sth->errstr."\n";
  
  my ($name, $merNo, $pepUnit, $as, $acceptAs, $metLig, $acceptMetLig, $merAcc);
  $sth->bind_columns(\$name, \$merNo, \$pepUnit, \$as, \$acceptAs, \$metLig, \$acceptMetLig, \$merAcc);

  #Get the active site pattern for the sequence
  while ($sth->fetch()) {
    my ($pattern, %tmpHash);
    
    if($as and $acceptAs) {
      my @asPos=split(/\, /, $as); #S270, S332
      my @asRes=split(/, /, $acceptAs); #C, S/T
      
 
      for(my $i=0; $i<@asPos; $i++) {
	next unless($asPos[$i] and $asRes[$i]); #Ignore if there is residue position ($asPos) but no residue ($asRes)
	my $resNum;
	if($asPos[$i] =~ /\S(\d+)/) {
	  $resNum=$1;
	}
	else {
	  die "Couldn't parse residue number for $acc_se (@asPos), array position $i ($asPos[$i])\n" unless($resNum);
	}
	
	unless($resNum == 0 or ($resNum < $start or $resNum > $end)) { #Unless the residue falls outside the alignment
	  my $colNumber = $seqObj->column_from_residue_number($resNum);
	  #What to do if position is 0? Ignore all active sites on holotype?
	  
	  my $residue;
	  if($asRes[$i] =~ /\//) {
	    $residue = "[".$asRes[$i]."]";
	    $residue =~ s/\//|/g;  #S/T becomes [S|T];
	  }
	  else {
	    $residue = $asRes[$i];
	  }
	  
	  $pattern.=$colNumber.$residue.":";    
	  $tmpHash{$colNumber}=$residue;
	}
      }
    }
    
    if($fam =~ /^M/) { #It's a metallopeptidase, so need to look for metal binding sites too
      if($metLig and $acceptMetLig) {
	my @metPos=split(/\, /, $metLig); #S270, S332
	my @metRes=split(/, /, $acceptMetLig); #C, S/T
	
	for(my $i=0; $i<@metPos; $i++) {
	  my $metResNum;
	  if($metPos[$i] =~ /\S(\d+)/) {
	    $metResNum=$1;
	  }
	  else {
	    die "Couldn't parse residue number for $acc_se (@metPos), array position $i ($metPos[$i])\n" unless($metResNum);
	  }
	  
	  unless($metResNum == 0 or ($metResNum < $start or $metResNum > $end) ) {
	    my $colNumber = $seqObj->column_from_residue_number($metResNum);
	    
	    my $residue;
	    if($metRes[$i] =~ /\//) {
	      $residue = "[".$metRes[$i]."]";
	      $residue =~ s/\//|/g;  #S/T becomes [S|T];
	    }
	    else {
	      $residue = $metRes[$i];
	    }
	    $pattern.=$colNumber.$residue.":";
	    $tmpHash{$colNumber}=$residue;  
	  }
	}
      }
    }
    
    #Only keep patterns we haven't seen before
    if($pattern) {
      unless(exists($uniquePattern->{$pattern})) {
	#print STDERR "$pattern\n";
	$uniquePattern->{$pattern}=1;
	$asPattern->{$acc_se}=\%tmpHash; 
      }
    }
  }
}

sub printActSites {

  my ($asPattern, $fam, $outfile)=@_;
  
  open(AS, ">$outfile") or die "Couldn't open fh to $outfile, $!";
  if($fam eq "M69" or $fam eq "M76" or $fam eq "M98") { #These families have an HEXXH metal binding site
    print AS "Metal binding site: HEXXH\n";
  }
  foreach my $asSeq (keys %$asPattern) {
    print AS "$asSeq\t";
    foreach my $asCol (sort { $a <=> $b } keys %{$asPattern->{$asSeq}}) {
      print AS "$asCol:".$asPattern->{$asSeq}->{$asCol}." ";
    }
    print AS "\n";
  }
  close AS;
}

sub lookForActSite {
  my ($sequence, $asPattern, $fam) = @_;
  
  my $homologue;
   
  #Look for active site residues
  my $match;
  foreach my $asSeq (keys %$asPattern) {
    $match=1;
    foreach my $asCol (sort { $a <=> $b } keys %{$asPattern->{$asSeq}}) { #asCol is the column position of the active site residue
      my $residue = substr($sequence, $asCol-1, 1); #Extract residue that is at the active site position
      if(!$residue or uc($residue) !~ /$asPattern->{$asSeq}->{$asCol}/) {  #eg /[S|T]/, see if residue is same as the allowed active site residue
	$match=0;
	last;
      }
    }
    
    
    #M69, M76, and M98 have an HEXXH metal binding site
    if($match and ($fam eq "M69" or $fam eq "M76" or $fam eq "M98") ) { #Match is true if the seq matched all active sites from a seq with known active sites
      my $sequenceNoGaps = $sequence;
      $sequenceNoGaps =~ s/\-//g;
      $sequenceNoGaps =~ s/\.//g;
      unless($sequenceNoGaps =~ /HE\w\wH/) {
	$match=0;
      }
    }
    if($match) {
      return $match;
    }
  }

  unless(keys %$asPattern) {  #Catch cases where there is no active site data, and it is one of the families that has HEXXH metal binding motif
    if($fam eq "M69" or $fam eq "M76" or $fam eq "M98") {
      $match=1;
      my $sequenceNoGaps = $sequence;
      $sequenceNoGaps =~ s/\-//g;
      $sequenceNoGaps =~ s/\.//g;
      unless($sequenceNoGaps =~ /HE\w\wH/) {
	$match=0;
      }
    }
  }
  return $match;
}
