
=head1 NAME

Bio::Rfam::QC - a module that performs QC on Rfam data.

=cut

package Bio::Rfam::QC;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: QC.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 25, 2013 8:43:56 AM

=cut

use strict;
use warnings;
use File::Temp qw(tempfile);
use File::Copy;
use Data::Printer;
#-------------------------------------------------------------------------------

=head1 METHODS

=cut

#Templating this off the old code......
sub checkFamilyFiles {
  my ( $family, $upFamilyObj ) = @_;

  &checkQCPerformed();
  &checkTimestamps();
  #return 1 on failure.
}

sub checkQCPerformed {
  my ( $dir, $acc, $config);

 #need to add in this for checking the qc has been done-dont allow ci otherwise.
  if ( !-e "$dir/$acc/qcpassed" ) {
    die
"rfci: [$acc] has not been passed by qc checks so not ready to check in - run rqc-all.pl\n";
  }

  foreach my $f ( @{ $config->mandatoryFiles } ) {
    if( -M "$dir/$acc/$f" < -M "$dir/$acc/qcpassed" ){
    die
  "You need to rerun the rqc-all.pl as $f has changed since you ran it last\n";
    }
  }
  
}

sub checkFamily {
  my ($familyObj) = @_;
  
  my $error = 0;
  $error = checkDESC($familyObj);
  if($error){
    return $error;
  }
  $error = checkSEED($familyObj);
  if($error){
    return $error;
  }
  
  checkCM($familyObj);
  
  return $error;
}

sub checkSEED {
  my($familyObj) = @_;
  
  my $error = 0;
  #Check that the SEED is in stockholm format
  if($familyObj->SEED->format ne 'Stockholm'){
    warn "FATAL: SEED is not in Stockholm format, ".$familyObj->SEED->format."\n";
    $error++;
  }
  
  #Check that the are no all gap columns
  if($familyObj->SEED->any_allgap_columns) { 
    warn "FATAL: SEED has all gap columns"; 
    $error++;
  }
  
  #Check that there are more than 2 sequences in the SEED alignment
  if($familyObj->SEED < 2) { 
    warn "FATAL: SEED has less than 2 sequences"; 
    $error++;
  }

  #Check that the SEED has a RF annotation line.
  if(!$familyObj->SEED->has_rf){
    warn "FATAL: SEED does not have an RF line\n";
    $error++;
  }
  
  #Check that the SEED has a SS_cons annotation line.
  if(!$familyObj->SEED->has_ss_cons){
    warn "FATAL: SEED does not have an SS_cons line\n";
    $error++;
  }
  
  return $error;
}

sub checkCM {
  my ($familyObj) = @_;

  my $error = 0;

  #Check that the CM and internal HMM agree.
  if ( $familyObj->CM->cmHeader->{nSeq} != $familyObj->CM->hmmHeader->{nSeq} ) {
    $error = 1;
    print STDERR
"Somehow the number of sequences in the CM does not agree with its own internal HMM\n";
    return $error;
  }

  # Cross reference the number of SEED sequences with the number in the CM.
  if ( $familyObj->CM->cmHeader->{nSeq} != $familyObj->SEED->nseq ) {
    $error = 1;
    print STDERR
"The number of sequences in the CM does not agree with the number in the SEED.\n";
    return $error;
  }
  my $acc = $familyObj->DESC->AC;
  print STDERR "SEED:CM:$acc: No serious format errors found\n";
  return $error;
}

sub checkDESC {
  my ($familyObj) = @_;
  
  my $error = 0;
  #Get the required fields and check that they are present.
  #Also check that SO and GO terms are set in the xrefs.
  $error = checkRequiredFields($familyObj);
  return $error if($error);
  
  #The DESC can have SQ tags, these should not be standard
  if($familyObj->DESC->SQ){
    warn "FATAL: your DESC file has SQ tags, please remove.\n";
    $error++;
  }
  return $error if($error);
  
  #Now check the type field.
  $error = checkTPField($familyObj);
  
  return $error;
}

sub checkScores {
  #need to check all seed sequences are present.
}


sub checkRequiredFields {
  my ($familyObj, $config) = @_;
  
  if(!$config){
    $config = Bio::Rfam::Config->new;
  }
  
  my $error = 0;
  foreach my $f (@{$familyObj->DESC->requiredFields}){
    if(!defined($familyObj->DESC->$f)){
      warn "Required DESC field $f not defined.\n";
      $error++;
    }elsif(ref($familyObj->DESC->$f) eq 'ARRAY'){
      if(!scalar(@{$familyObj->DESC->$f})){
        warn "Required DESC field $f not defined [ARRAY].\n";
        $error++;
      }
    }
  }
  
  #There are also two specical cases......
  my($SOseen, $GOseen, $SOsuggestions, $GOsuggestions);
  
  $SOsuggestions = $config->SOsuggestions;
  $GOsuggestions = $config->GOsuggestions;
  if($familyObj->DESC->DBREFS){
    foreach my $xref (@{$familyObj->DESC->DBREFS}){
      if($xref->{db_id} eq 'SO'){
        $SOseen++;
      }elsif($xref->{db_id} eq 'GO'){
        $GOseen++;
      }
    }
  }
  if(!$SOseen){  
    warn "\nFATAL: There are no SO mappings for this family\nLook up:\n$SOsuggestions\n\n";
    $error=1;
  }
  if (!$GOseen ){
    warn "\nNo GO mappings for this family-have you tried to add any?\nLook up:\n$GOsuggestions\n\n";
  }
  
  return ($error);
}



sub checkTPField {
  my ($familyObj, $config) = @_;
  
  if(!$config){
    $config = Bio::Rfam::Config->new;
  }
  
  my $error =0;

  if(!$familyObj->DESC->TP){
    warn "The DESC file has no TP line.\n";
    return $error;
  }
  
  my @TPline = split( /\; /, $familyObj->DESC->TP);
  $TPline[-1] =~ s/\;//;
  my $tpHashRef = $config->descTypes;

  #For each element/TP we find, descend down the list.
  for (my $i = 0; $i< scalar(@TPline); $i++){
     if($tpHashRef->{$TPline[$i]}){
        $tpHashRef = $tpHashRef->{$TPline[$i]};
     }else{
       warn "\nFATAL: Invalid TP line: ".$familyObj->DESC->TP.", because $TPline[$i] not found in hash\n";
       $i = scalar(@TPline); #Break out as nothing will work
       $error = 1;
    }
  }
  return($error);
}

sub compareSeedAndScores {
  my ( $familyObj ) = @_;
  
  my %seed;
  for(my $i = 0; $i < $familyObj->SEED->nseq; $i++){
    my $s = $familyObj->SEED->get_sqname($i);
    my ($seq) = $s =~ /^(\S+)\/\d+\-\d+$/;
    $seed{$seq} = 1;
  }
  foreach my $r (@{$familyObj->SCORES->regions}){
    if(exists($seed{$r->[3]}) ){
      delete($seed{$r->[3]});
    }
    last if(!%seed);
  }
  
  if(%seed){
    foreach my $seq (keys %seed){
      warn "SERIOUS ERROR: $seq in SEED in not in SCORES list!\n";
    }
    return 1;
  }else{
    return 0;
  }
}

sub compareOldAndNew {
  my($oldFamObj, $newFamObj, $path) = @_;
  
  #Find the things that are in the old, but not the new. Generate a hash of the
  #new things the pull out the unique sequence accessions (fourth elemenet)
  my %e = map{ $_->[3] => undef } @{$newFamObj->SCORES->regions};
  my @missing = keys %{{ map{ $_->[3] => 1 }
                         sort{ $a->[3] cmp $b->[3] } 
                         grep( ! exists( $e{$_->[3]} ), @{$oldFamObj->SCORES->regions} ) }}; 
  
  %e = map{ $_->[3] => undef } @{$oldFamObj->SCORES->regions};
  my @found = keys %{{ map{ $_->[3] => 1 }
                         sort{ $a->[3] cmp $b->[3] } 
                         grep( ! exists( $e{$_->[3]} ), @{$newFamObj->SCORES->regions} ) }}; 
  
  if($path and -d $path){
    #Remove the files if they are there.
    unlink( $path.'/missing' ) if(-e ($path.'/missing'));
    unlink( $path.'/found' ) if(-e ($path.'/found'));
    
    if(@missing){
      open(M, '>', $path.'/missing') 
        or die "Failed to open missing ($path/missing) file:[$!]\n";
      foreach (@missing){
        print M "$_ not found\n";
      }
      close(M);
    }
    if(@found){
      open(M, '>', $path.'/found') 
        or die "Failed to open found ($path/found) file:[$!]\n";
      foreach (@found){
        print M "$_ found\n";
      }
      close(M);
    }
  }
  
  
  if(!scalar(@found) and !scalar(@missing)){
    print STDERR "No change in SEED and ALIGN members.\n";
  }else{
    print STDERR "Lost ".scalar(@missing).". Found ".scalar(@found).". See the missing and found files for details\n";
  }

  return(\@found, \@missing);
} 


sub checkTimestamps {
  my ( $fam, $config ) = @_;

  my $error = 0;

  #First check all files are present.
  foreach my $f ( @{ $config->mandatoryFiles } ) {
    if ( !-e "$fam/$f" ) {
      warn "$f is missing from $fam\n";
      $error++;
    }
  }
  return $error if ($error);

  #Now check the timestamps.
  if ( -M "$fam/SEED" <= -M "$fam/CM" ) {
    warn
"\nFATAL ERROR: $fam: Your SEED [$fam/SEED] is younger than your CM file [$fam/CM].\n";
    $error = 1;
  }
  if ( -M "$fam/CM" <= -M "$fam/TBLOUT" ) {
    warn
"\nFATAL ERROR: $fam: Your CM [$fam/CM] is younger than your TBLOUT file [$fam/TBLOUT].\n";
    $error = 1;
  }
  if ( -M "$fam/TBLOUT" <= -M "$fam/SCORES" ) {
    warn
"\nFATAL ERROR: $fam: Your TBLOUT [$fam/TBLOUT] is younger than your scores [$fam/scores].\n";
    $error = 1;
  }
  
  return $error;
}


sub valid_sequences {

}

#------------------------------------------------------------------------------

=head2 

  Title    : checkSpell
  Incept   : finnr, Jul 17, 2013 1:29:27 PM
  Usage    : Bio::Rfam::QC::checkSpell($dir, $dictPath, $familyIOObj)
  Function : This takes the DESC file and grabs out the free text fields, removes
           : our tags and writes a temporary file. It then runs ispell over the
           : contents of the file, interactively with the users. Finally, the 
           : data is written back into the file.
  Args     : Path containting the family, Path to dictionary file used by ispell,
           : a familyIO object (optional)
  Returns  : An error flag if encountered.
  
=cut

sub checkSpell {
  my ( $fam, $dictionary, $familyIO ) = @_;

  my $error = 0;
  #
  unless ($familyIO) {
    $familyIO = Bio::Pfam::FamilyIO->new;
  }

  #Make sure that the DESC file is vaild to start off with
  eval { $familyIO->parseDESC("$fam/DESC"); };
  if ($@) {
    print STDERR $@;
    $error = 1;
  }

  my (%line);
  my ($lineNo) = 0;

  open( DESC, "$fam/DESC" )
    || die "Can't open DESC file for family $fam:[$!]\n";

  while (<DESC>) {

    # If a free text line add to %lines
    if (/^RT   (\.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^CC   (.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^RC   (.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^DC   (.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^DE   (.*)$/) {
      $line{$lineNo} = $1;
    }
    $lineNo++;
  }
  close(DESC);

  my @lineNos = sort { $a <=> $b; } keys %line;

  my ( $bit, @line_number_array );

  #Now make temporary file and write free text
  my ( $tfh, $tfilename ) = tempfile();

  foreach (@lineNos) {

    #Print all free text lines.
    print $tfh $line{$_}, "\n";
  }
  close $tfh;

  # Start ispell session on file
  system("ispell -W 0 -w 0123456789 -p$dictionary $tfilename");

  # Now need to put changes back into DESC file
  my ( %editedline, $line_number );
  open( TMP, '<', $tfilename ) || die "Can't open temp file $tfilename:[$!]\n";

  while (<TMP>) {
    if (/^(.*)$/) {
      $line_number = shift @lineNos;
      $editedline{"$line_number"} = $1;
    }
    else {
      die "unrecognised line [$_]\n Serious error!\n";
    }
  }
  close(TMP);

  # Write out new DESC file
  open( TEMPDESC, ">$fam/DESC.$$" )
    || die "Can't write to temp DESC file for family $fam\n";

  open( DESC, "$fam/DESC" )
    || die "Can't open DESC file for fam $fam\n";

  my ($prefix);
  $lineNo = 0;

  while (<DESC>) {
    if ( $editedline{$lineNo} ) {

      # Find if DE, RT or CC line
      if ( $_ =~ /^(\S+)/ ) {
        $prefix = $1;
      }
      else {
        die "unrecognised line [$_]\n";
      }

      # Write out line
      print TEMPDESC "$prefix   $editedline{$lineNo}\n";
    }
    else {
      print TEMPDESC;
    }
    $lineNo++;
  }
  close(DESC);
  close(TEMPDESC);

  # Move DESC across
  copy( "$fam/DESC.$$", "$fam/DESC" );

  # Clean up
  unlink("$fam/DESC.$$");

  #Now make sure that I have not screwed anyting up!  It is possible that the
  #line lengths could overflow.
  $familyIO->parseDESC("$fam/DESC");
  return($error);
}
=head1
  Title    : ssStats
  Incept   : EPN, Thu Jul 18 15:34:22 2013
  Usage    : Bio::Rfam::QC::ssStats($familyObj, $outputDir)
  Function : Calculates and outputs per-family, per-sequence and per-basepair
           : statistics to 'ss-stats-perfamily', 'ss-stats-persequence' and 
           : 'ss-stats-perbasepair' files.
  Args     : A Family object, output directory (optional, default '.').
  Returns  : 1 on error, 0 if no errors were found.
  
=cut

sub ssStats {
  my ( $famObj , $outdir) = @_;

  $outdir = '.' if(!$outdir);
  my $seed = $famObj->SEED;
  eval {
    if($seed->any_allgap_columns) { die "ERROR, SEED has all gap columns"; }
    if($seed->nseq < 2)           { die "ERROR, SEED has less than 2 sequences"; }
    $seed->set_name($famObj->DESC->ID);
    $seed->set_accession($famObj->DESC->AC);
    $seed->weight_GSC();
    $seed->rfam_qc_stats("$outdir/ss-stats-perfamily", 
                       "$outdir/ss-stats-persequence", 
                       "$outdir/ss-stats-perbasepair");
  };
  if($@){
    warn "FATAL: experienced an error generating SS stats.\n $@\n";
    return 1; #There is an error;
  }else{
    return 0; #No error;
  }
}




1;
