
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
use Data::Dump qw(dump);

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
  my ( $dir, $acc, $config );

 #need to add in this for checking the qc has been done-dont allow ci otherwise.
  if ( !-e "$dir/$acc/qcpassed" ) {
    die
"rfci: [$acc] has not been passed by qc checks so not ready to check in - run rqc-all.pl\n";
  }

  foreach my $f ( @{ $config->mandatoryFiles } ) {

#    if( -M "$dir/$acc/$f" < -M "$dir/$acc/qcpassed" ){
#    die
#  "You need to rerun the rqc-all.pl as $f has changed since you ran it last\n";
#    }
  }

}

#Ended templating...I feel dirty.

#------------------------------------------------------------------------------

=head2 checkFamilyFormat

  Title    : checkFamilyFormat
  Incept   : finnr, Jul 24, 2013 2:29:22 PM
  Usage    : Bio::Rfam::QC::checkFamily($familyObj)
  Function : Performs series of format checks
  Args     : A Bio::Rfam::Family object
  Returns  : 1 if an error is found, 
  
=cut

sub checkFamilyFormat {
  my ($familyObj) = @_;

  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  my $error = 0;
  $error = checkDESCFormat($familyObj);
  if ($error) {
    return $error;
  }
  $error = checkSEEDFormat($familyObj);
  if ($error) {
    return $error;
  }
  $error = checkCMFormat($familyObj);
  if ($error) {
    return $error;
  }
  checkScoresFormat($familyObj);
  return $error;
}

#------------------------------------------------------------------------------

=head2 checkSEEDFormat

  Title    : checkSEEDFormat
  Incept   : finnr, Jul 24, 2013 2:36:13 PM
  Usage    : Bio::Rfam::QC::checkSEEDFormat($familyObj)
  Function : Performs format QC steps on the SEED, via the object.
  Args     : A Bio::Rfam::Family object
  Returns  : 1 on error, 0 on passing checks.
  
=cut

sub checkSEEDFormat {
  my ($familyObj) = @_;

  #
  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  my $error = 0;

  #Check that the SEED is in stockholm format
  if ( $familyObj->SEED->format ne 'Stockholm' ) {
    warn "FATAL: SEED is not in Stockholm format, "
      . $familyObj->SEED->format . "\n";
    $error++;
  }

  #Check that the are no all gap columns
  if ( $familyObj->SEED->any_allgap_columns ) {
    warn "FATAL: SEED has all gap columns";
    $error++;
  }

  #Check that there are more than 2 sequences in the SEED alignment
  if ( $familyObj->SEED < 2 ) {
    warn "FATAL: SEED has less than 2 sequences";
    $error++;
  }

  #Check that the SEED has a RF annotation line.
  if ( !$familyObj->SEED->has_rf ) {
    warn "FATAL: SEED does not have an RF line\n";
    $error++;
  }

  #Check that the SEED has a SS_cons annotation line.
  if ( !$familyObj->SEED->has_ss_cons ) {
    warn "FATAL: SEED does not have an SS_cons line\n";
    $error++;
  }

  return $error;
}

#------------------------------------------------------------------------------

=head2 checkCMFormat

  Title    : checkCMFormat
  Incept   : finnr, Jul 24, 2013 2:36:13 PM
  Usage    : Bio::Rfam::QC::checkCMFormat($familyObj)
  Function : Performs format QC steps on the CM, via the object.
  Args     : A Bio::Rfam::Family object
  Returns  : 1 on error, 0 on passing checks.
  
=cut

sub checkCMFormat {
  my ($familyObj) = @_;

  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }
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
  return $error;
}

#------------------------------------------------------------------------------

=head2 checkDESCFormat

  Title    : checkDESCFormat
  Incept   : finnr, Jul 24, 2013 2:36:13 PM
  Usage    : Bio::Rfam::QC::checkDESCFormat($familyObj)
  Function : Performs format QC steps on the DESC, via the object.
  Args     : A Bio::Rfam::Family object
  Returns  : 1 on error, 0 on passing checks.
  
=cut

sub checkDESCFormat {
  my ($familyObj) = @_;

  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  my $error = 0;

  #Get the required fields and check that they are present.
  #Also check that SO and GO terms are set in the xrefs.
  $error = checkRequiredFields($familyObj);
  return $error if ($error);

  #The DESC can have SQ tags, these should not be standard
  if ( $familyObj->DESC->SQ ) {
    warn "FATAL: your DESC file has SQ tags, please remove.\n";
    $error++;
  }
  return $error if ($error);

  #Now check the type field.
  $error = checkTPField($familyObj);

  return $error;
}

#------------------------------------------------------------------------------

=head2 checkScoresFormat

  Title    : checkSEEDFormat
  Incept   : finnr, Jul 24, 2013 2:36:13 PM
  Usage    : Bio::Rfam::QC::checkScoresFormat($familyObj)
  Function : Performs format QC steps on the scores, via the object.
  Args     : A Bio::Rfam::Family object
  Returns  : 1 on error, 0 on passing checks.
  
=cut

sub checkScoresFormat {

  #TODO - check scores
  #need to check all seed sequences are present.
  #Should check that no regions exceed threshold?
}

#------------------------------------------------------------------------------

=head2 checkRequiredFields

  Title    : checkRequiredFields
  Incept   : finnr, Jul 24, 2013 2:47:01 PM
  Usage    : Bio::Rfam::QC::checkRequiredFields($familyObj, $config)
  Function : Ensures that all of the required fields and database cross references
           : are present in the DESC file.
  Args     : A Bio::Rfam::Family object, a Bio::Rfam::Config object (optional).
  Returns  :  1 on error, 0 on passing checks.
  
=cut

sub checkRequiredFields {
  my ( $familyObj, $config ) = @_;

  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  if ( !$config ) {
    $config = Bio::Rfam::Config->new;
  }

  if ( !$config->isa('Bio::Rfam::Config') ) {
    die "Expeceted an Bio::Rfam::Config object\n";
  }
  my $error = 0;
  foreach my $f ( @{ $familyObj->DESC->requiredFields } ) {
    if ( !defined( $familyObj->DESC->$f ) ) {
      warn "Required DESC field $f not defined.\n";
      $error++;
    }
    elsif ( ref( $familyObj->DESC->$f ) eq 'ARRAY' ) {
      if ( !scalar( @{ $familyObj->DESC->$f } ) ) {
        warn "Required DESC field $f not defined [ARRAY].\n";
        $error++;
      }
    }
  }

  #There are also two specical cases......
  my ( $SOseen, $GOseen, $SOsuggestions, $GOsuggestions );

  $SOsuggestions = $config->SOsuggestions;
  $GOsuggestions = $config->GOsuggestions;
  if ( $familyObj->DESC->DBREFS ) {
    foreach my $xref ( @{ $familyObj->DESC->DBREFS } ) {
      if ( $xref->{db_id} eq 'SO' ) {
        $SOseen++;
      }
      elsif ( $xref->{db_id} eq 'GO' ) {
        $GOseen++;
      }
    }
  }
  if ( !$SOseen ) {
    warn
"\nFATAL: There are no SO mappings for this family\nLook up:\n$SOsuggestions\n\n";
    $error = 1;
  }
  if ( !$GOseen ) {
    warn
"\nNo GO mappings for this family-have you tried to add any?\nLook up:\n$GOsuggestions\n\n";
  }

  return ($error);
}

#------------------------------------------------------------------------------

=head2 checkTPField

  Title    : checkTPField
  Incept   : finnr, Jul 24, 2013 2:22:18 PM
  Usage    : Bio::Rfam::QC::checkTPField( $familyObj, $config)
  Function : Takes the DESC file and checks that the TP field conforms to the
           : the expected data structure that is stored in the config.
  Args     : A Bio::Rfam::Family object, a Bio::Rfam::Config object (optional)
  Returns  : 0 if everything is okay, 1 if there is an error detected.
  
=cut

sub checkTPField {
  my ( $familyObj, $config ) = @_;

  if ( !$config ) {
    $config = Bio::Rfam::Config->new;
  }

  if ( !$config->isa('Bio::Rfam::Config') ) {
    die "Expeceted an Bio::Rfam::Config object\n";
  }

  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  my $error = 0;

  #There should always be a TP line when the DESC has been written.
  if ( !$familyObj->DESC->TP ) {
    warn "The DESC file has no TP line.\n";
    $error = 1;
    return $error;
  }

  #Process the DESC line, semi-colon separated list.
  my @TPline = split( /\; /, $familyObj->DESC->TP );
  $TPline[-1] =~ s/\;//;

  #Get the predefined, okay data structure
  my $tpHashRef = $config->descTypes;

  #For each element/TP we find, descend down the hash.
  for ( my $i = 0 ; $i < scalar(@TPline) ; $i++ ) {
    if ( $tpHashRef->{ $TPline[$i] } ) {
      $tpHashRef = $tpHashRef->{ $TPline[$i] };
    }
    else {
      warn "\nFATAL: Invalid TP line: "
        . $familyObj->DESC->TP
        . ", because $TPline[$i] not found in hash\n";
      $i     = scalar(@TPline);    #Break out as nothing will work
      $error = 1;
    }
  }
  return ($error);
}

#------------------------------------------------------------------------------

=head2 compareSeedAndScores

  Title    : compareSeedAndScores
  Incept   : finnr, Jul 24, 2013 1:03:53 PM
  Usage    : Bio::Rfam::QC::compareSeedAndScores($familyObj);
  Function : Takes a family object and compares the SEED to the scores file to
           : ensure that all sequences are found. It does not look at co-ordinates
           : just sequence accessions.
  Args     : A Bio::Rfam::Family obkect
  Returns  : 0 when all sequences found, otherwise 1
  
=cut

sub compareSeedAndScores {
  my ($familyObj) = @_;

  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  #Hash os SEED sequnece accessions
  my %seed;
  for ( my $i = 0 ; $i < $familyObj->SEED->nseq ; $i++ ) {
    my $s = $familyObj->SEED->get_sqname($i);
    my ($seq) = $s =~ /^(\S+)\/\d+\-\d+$/;
    $seed{$seq} = 1;
  }

  #Now loop over the scores files and delete keys when we find accessions
  #present, with the hope we have an empty seed hash
  foreach my $r ( @{ $familyObj->SCORES->regions } ) {
    if ( exists( $seed{ $r->[3] } ) ) {
      delete( $seed{ $r->[3] } );
    }
    last if ( !%seed );    #If we empty, break the loop
  }

  #Report as necessary.
  if (%seed) {
    foreach my $seq ( keys %seed ) {
      warn "SERIOUS ERROR: $seq in SEED in not in SCORES list!\n";
    }
    return 1;
  }
  else {
    return 0;
  }
}

#------------------------------------------------------------------------------

=head2 compareOldAndNew

  Title    : compareOldAndNew
  Incept   : finnr, Jul 24, 2013 1:07:21 PM
  Usage    : Bio::Rfam::QC::compareOldAndNew($oldFamily, $updatedFamily, $path)
  Function : Comapres the SCORES files from the old and update family to identify
           : new and missing sequences. If a path is supplied, it will write files
           : containing the found/missing sequence accessions 
  Args     : Two family objects, first the old, second the updated family.  Third
           : argument is an optional path of the directory where missng and found
           : files should be written
  Returns  : array referneces to the arrays containing the found or missing 
           : sequence accessions.
  
=cut

sub compareOldAndNew {
  my ( $oldFamObj, $newFamObj, $path ) = @_;

  if ( !$oldFamObj or !$oldFamObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object as first argument.\n";
  }
  if ( !$newFamObj or !$newFamObj->isa('Bio::Rfam::Family') ) {
    die
      "Did not get passed in a Bio::Rfam::Family object as second argument.\n";
  }
  if ($path) {
    if ( !-d $path ) {
      die "You passed in a path, but is does not appear to be a directory.\n";
    }
  }

  #Find the things that are in the old, but not the new. Generate a hash of the
  #new things the pull out the unique sequence accessions (fourth elemenet)
  my %e = map { $_->[3] => undef } @{ $newFamObj->SCORES->regions };
  my @missing = keys %{
    {
      map { $_->[3] => 1 }
      sort { $a->[3] cmp $b->[3] }
      grep( !exists( $e{ $_->[3] } ), @{ $oldFamObj->SCORES->regions } )
    }
  };

  %e = map { $_->[3] => undef } @{ $oldFamObj->SCORES->regions };
  my @found = keys %{
    {
      map { $_->[3] => 1 }
      sort { $a->[3] cmp $b->[3] }
      grep( !exists( $e{ $_->[3] } ), @{ $newFamObj->SCORES->regions } )
    }
  };

  if ( $path and -d $path ) {

    #Remove the files if they are there.
    unlink( $path . '/missing' ) if ( -e ( $path . '/missing' ) );
    unlink( $path . '/found' )   if ( -e ( $path . '/found' ) );

    if (@missing) {
      open( M, '>', $path . '/missing' )
        or die "Failed to open missing ($path/missing) file:[$!]\n";
      foreach (@missing) {
        print M "$_ not found\n";
      }
      close(M);
    }
    if (@found) {
      open( M, '>', $path . '/found' )
        or die "Failed to open found ($path/found) file:[$!]\n";
      foreach (@found) {
        print M "$_ found\n";
      }
      close(M);
    }
  }

  if ( !scalar(@found) and !scalar(@missing) ) {
    print STDERR "No change in SEED and ALIGN members.\n";
  }
  else {
    print STDERR "Lost " . scalar(@missing) . ". Found " . scalar(@found);
    if ($path) {
      print STDERR "See the missing and found files for details";
    }
    print STDERR "\n";
  }

  return ( \@found, \@missing );
}

#------------------------------------------------------------------------------

=head2 checkTimestamps

  Title    : checkTimestamps
  Incept   : finnr, Jul 24, 2013 1:31:37 PM
  Usage    : Bio::Rfam::QC::checkTimestamps($famDir, $config)
  Function : Checks that all mandatory files are present in the directory and
           : that have been built in the same way.
  Args     : A path to family directory, a Bio::Rfam::Config object (optional).
  Returns  : 0 on passing, 1 on issue.
  
=cut

sub checkTimestamps {
  my ( $fam, $config ) = @_;

  if ( !$fam or !-d $fam ) {
    die "Did not get passed in a path to a directory\n";
  }

  if ( !$config ) {
    $config = Bio::Rfam::Config->new;
  }
  if ( !$config->isa('Bio::Rfam::Config') ) {
    die "Expeceted an Bio::Rfam::Config object\n";
  }

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

  if ( !$familyIO->isa('Bio::Rfam::FamilyIO') ) {
    die "Did not get passed in a Bio::Rfam::FamilyIO object\n";
  }

  #Make sure that the DESC file is vaild to start off with
  eval { $familyIO->parseDESC("$fam/DESC"); };
  if ($@) {
    print STDERR $@;
    $error = 1;
    return $error;
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
  return ($error);
}

#------------------------------------------------------------------------------

=head2 ssStats

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
  my ( $famObj, $outdir ) = @_;

  if ( !$famObj or !$famObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  $outdir = '.' if ( !$outdir );
  my $seed = $famObj->SEED;
  eval {
    if ( $seed->any_allgap_columns ) { die "ERROR, SEED has all gap columns"; }
    if ( $seed->nseq < 2 ) { die "ERROR, SEED has less than 2 sequences"; }
    $seed->set_name( $famObj->DESC->ID );
    $seed->set_accession( $famObj->DESC->AC );
    $seed->weight_GSC();
    $seed->rfam_qc_stats(
      "$outdir/ss-stats-perfamily",
      "$outdir/ss-stats-persequence",
      "$outdir/ss-stats-perbasepair"
    );
  };
  if ($@) {
    warn "FATAL: experienced an error generating SS stats.\n $@\n";
    return 1;    #There is an error;
  }
  else {
    return 0;    #No error;
  }
}

sub checkSEEDSeqs {
  my ( $familyObj, $seqDBObj ) = @_;

  #Check we have the correct family object.
  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  my $error = 0;

  my @seedSeqs;
  for ( my $i = 0 ; $i < $familyObj->SEED->nseq ; $i++ ) {
    my $nse     = $familyObj->SEED->get_sqname($i);
    my $seedSeq = $familyObj->SEED->get_sqstring_unaligned($i);
    my ( $is_nse, $name, $start, $end ) = Bio::Rfam::Utils::nse_breakdown($nse);
    push( @seedSeqs, [ $nse, $start, $end, $name, $seedSeq ] );
  }

  #This next bit is a little inefficient
  my $seqDBSeqs = $seqDBObj->fetch_subseqs( \@seedSeqs, -1 );

  #Make RNA
  $seqDBSeqs =~ s/T/U/g;

  #now make array of alternatice head/sequence - Eric???? Can we avoid this.
  my @s = split( /\n/, $seqDBSeqs );

  for ( my $i = 0 ; $i < $familyObj->SEED->nseq ; $i++ ) {
    if ( $s[ ( $i * 2 ) + 1 ] ne $seedSeqs[$i]->[4] ) {
      $error = 1;
      warn "The sequence in the SEED, "
        . $seedSeqs[$i]->[0]
        . " does not match the database.\n";
      warn "SEED:" . $seedSeqs[$i]->[4] . "\n";
      warn "DB  :" . $s[ ( $i * 2 ) + 1 ] . "\n\n";
    }
  }
}

sub checkScoresSeqs {
  my ( $familyObj, $seqDBObj ) = @_;

  #Check we have the correct family object.
  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    die "Did not get passed in a Bio::Rfam::Family object\n";
  }

  #Check we have the correct seqDBobj object.
  if ( !$seqDBObj or !$seqDBObj->isa('Bio::Rfam::SeqDB') ) {
    die "Did not get passed in a Bio::Rfam::SeqDB object\n";
  }

  my $error = 0;
  eval { $seqDBObj->fetch_subseqs( $familyObj->SCORES->regions ); };
  if ($@) {
    $error = 1;
    warn "ERROR: $@\n";
  }
  return $error;
}

sub overlap {
  my ( $familyObj, $config, $ignore, $type ) = @_;

  my $rfamdb = $config->rfamlive;

  findExternalOverlaps($familyObj, $rfamdb, $ignore);
  findInternalOverlaps($familyObj);

}

sub findExternalOverlaps{
  my ($familyObj, $rfamdb, $ignore) = @_;
  
  my $currentAcc = '';
  my $regions;
  foreach my $r (sort{$a->[3] cmp $b->[3]} @{$familyObj->SCORES->regions}){
    my ($s1, $e1, $or1) = 
        $r->[1] <= $r->[2] ? ($r->[1], $r->[2], 1) : ($r->[2], $r->[1], -1);

    if($currentAcc ne $r->[3]){
      $regions = $rfamdb->resultset('FullRegion')->regionsByRfamseqAcc($r->[3]);
      $currentAcc=  $r->[3];
    }
    foreach my $dbReg (@$regions){
      #Does it belong to a family we want to ignore?
      next if(exists($ignore->{$dbReg->[1]}));
      my ($s2, $e2) = 
        $dbReg->[4] == 1 ? ($dbReg->[2], $dbReg->[3]) : ($dbReg->[3], $dbReg->[2]);
      my $overlap = 0;
      $overlap = _overlapCoos( $s1, $e1, $s2, $e2);
      if($overlap != 0){
          $overlap = 'fullOL' if ( $overlap == -1 );
          my $overlapType =  $dbReg->[4] eq $or1 ? 'SS' : 'OS';
          #TODO Fix reporting when I have information.
          printf STDERR "External overlap [%s] of %s with %s by %s\n",
            $overlapType,
            $r->[0],
            $dbReg->[1].":".$dbReg->[0]."/".$dbReg->[2]."-".$dbReg->[3],
            $overlap;
      }
    }
  }
}

sub findInternalOverlaps {
  my ($familyObj) = @_;

  my @atomizedNSE;
  for ( my $i = 0 ; $i < $familyObj->SEED->nseq - 1 ; $i++ ) {
     $atomizedNSE[$i]  = _atomizeNSE($familyObj->SEED->get_sqname($i)) 
        if ( !$atomizedNSE[$i] );
    for ( my $j = $i + 1 ; $j < $familyObj->SEED->nseq ; $j++ ) {
      $atomizedNSE[$j]  = _atomizeNSE($familyObj->SEED->get_sqname($j)) 
        if ( !$atomizedNSE[$j] );

      #Name, start, end corresponds to
      if (  ( $atomizedNSE[$i]->[1] eq $atomizedNSE[$j]->[1] )
        and ( $atomizedNSE[$i]->[4] eq $atomizedNSE[$j]->[4] ) )
      {
        #Same sequence, same orientation, now see if they overlap.
        my $overlap = 0;
        if ( $atomizedNSE[$i]->[4] == 1 ) {
          $overlap = _overlapCoos(
            $atomizedNSE[$i]->[2], $atomizedNSE[$i]->[3],
            $atomizedNSE[$j]->[2], $atomizedNSE[$j]->[3]
          );
        }
        else {
          $overlap = _overlapCoos(
            $atomizedNSE[$i]->[3], $atomizedNSE[$i]->[2],
            $atomizedNSE[$j]->[3], $atomizedNSE[$j]->[2]
          );
        }
        if ( $overlap != 0 ) {
          $overlap = 'fullOL' if ( $overlap == -1 );
          #TODO Fix reporting when I have information.
          printf STDERR "Internal overlap of %s with %s by %s\n",
            $familyObj->SEED->get_sqname($i),
            $familyObj->SEED->get_sqname($j),
            $overlap;
        }
      }
    }
  }
}

sub _overlapCoos {
  my ( $s1, $e1, $s2, $e2 ) = @_;

  my $len = 0;
  if ( $s1 >= $s2 and $e1 <= $e2 ) {    #full ol.
    $len = -1;
  }
  elsif ( $s1 <= $e2 and $e1 >= $e2 )
  {    #region 1 right extended or fully nested region 2
    $len = $e2 - $s1;
  }
  elsif ( $s1 <= $s2 and $e1 >= $s2 )
  {    #region 1 left extended or fully nested region 2
    $len = $e1 - $s2;
  }
  return $len;
}

sub _atomizeNSE {
  my ( $nse ) = @_;
  #Set orientation
  my @nse =
        Bio::Rfam::Utils::nse_breakdown( $nse );
      if ( $nse[2] > $nse[3] ) {
        $nse[4] = -1;
      }
      else {
        $nse[4] = 1;
      }
  return \@nse;
}

1;
