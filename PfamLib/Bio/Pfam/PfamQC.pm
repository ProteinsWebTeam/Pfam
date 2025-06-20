 
# PfamQC - an attempt to bring all the pfam quality control
# measures into one place
#
# sgj
#

=head1 NAME

PfamQC

=head1 SYNOPSIS

Contains quality check routines typically to check local family prior to rcs check-in.

=cut

package Bio::Pfam::PfamQC;

use strict;
use warnings;

use File::Copy;

use Bio::Pfam::Config;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::SeqFetch;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Range;
use Carp;
use Data::Dumper;
use Cwd;
use File::Basename;

my $CONFIG = Bio::Pfam::Config->new;

=head2 passesAllFormatChecks

 Title    : passesAllFormatChecks
 Usage    : &PfamQC::passesAllFormatChecks(familyObject, directoryName, isNewFlag, ignoreTime, pfamDB)
 Function : Runs all format check routines from this module,
            and warns about any errors
 Returns  : 1 if all is OK, 0 if not
 Args     : Bio::Pfam::Family object, directory containing the family, true flag if family is new

=cut

sub passesAllFormatChecks {
  my ( $famObj, $family, $isNew, $ignoreTime, $pfamDB ) = @_;

  # check directory exists
  if ( !-d "$family" ) {
    confess("$family: directory does not exist locally\n");
  }

  my $error = 0;

  # Check format of family name
  unless ( &nameFormatIsOK( $famObj->DESC->ID ) ) {
    $error = 1;
    warn "|$family|: your family identifier contains disallowed characters\n";
  }

  unless( &onlyASCII( $famObj->DESC, $family )) {
    $error = 1;
    warn "|$family|: desc file contains illegal characters\n";
  }


  unless($ignoreTime){
    unless ( &checkFamilyFiles( $family, $famObj ) ) {
      $error = 1;
    }
  }

  unless( &checkRefs( $famObj->DESC, $family )) {
    warn "|$family|: Please check references are correct, this is a warning not an error!\n";
    sleep 3;
  }

  if ( !&checkSearchMethod( $family, $famObj ) ) {
    warn "$family: Bad search method (SM), databases are different sizes\n";
    $error = 1;
  }

  if ( !&verifyEdits( $family, $famObj ) ) {
    warn "$family: Bad ED lines\n";
    $error = 1;
  }

  if ( !&checkNestings( $family, $famObj ) ) {
    warn "$family: Bad nested format\n";
    $error = 1;
  }

  # check seed alignment
  foreach my $aln (qw(SEED ALIGN)) {
    next if($aln eq "ALIGN" and -z "$family/$aln"); #ALIGN0 ALIGN can be empty for families that do not have any reference proteome matches
    if ( !&mulFormatIsOK( $aln, $family ) ) {
      warn "$family: Bad format for $aln!\n";
      $error = 1;
    }
  }

  unless(-z "$family/ALIGN") { #ALIGN0 Skip these checks if ALIGN is empty
    if ( !&moreInSEEDthanALIGN($famObj, $pfamDB) ) {
      warn "$family: You have more sequences in SEED than ALIGN\n";
      $error = 1;
    }
    if ( !&compareAlignToScores($famObj) ) {
      warn
      "$family: You have a different number of matches in scores and ALIGN files\n";
      $error = 1;
    }
  }

  if ($error) {
    warn "$family: Family $family contains errors\n";
    return 0;    # failure
  }
  else {
    warn "$family: Family $family passes checks \n";
    return 1;    # success
  }
}

#=head2 local_dirs_overlap
#
# Title    : local_dirs_overlap
# Usage    : &PfamQC::local_dirs_overlap( \@dirs )
# Function : Check for overlaps in the supplied list of directories
# Returns  : 0 if all is OK, number of overlaps
# Args     : reference to list of directories (full paths safest)
#
#=cut
#
#sub local_dirs_overlap {
#  my ( $dir, $fams ) = @_;
#
#  my $io         = Bio::Pfam::FamilyIO->new;
#  my $noFullRegs = 0;
#  my ( %scores, %regionsAll, %overlaps, $allowed, %clans );
#  open( OVERLAPS, ">$dir/overlaps" )
#    || die "Could not open $dir/overlap file: $!";
#  foreach my $fam (@$fams) {
#
#    # check is a directory
#    if ( !-d "$dir/$fam" ) {
#      warn "Cannot find directory $dir/$fam\n";
#      next;
#    }
#
#    print "$fam ";
#    eval {
#      open( D, "$dir/$fam/DESC" )
#        || die "Could not open $dir/$fam/DESC:[$!]\n";
#      my $descObj = $io->parseDESC( \*D );
#
#      my $acc = $descObj->AC;
#
#      if ( $descObj->NESTS ) {
#        foreach my $n ( @{ $descObj->NESTS } ) {
#          push( @{ $allowed->{$fam} }, $n->{'dom'} );
#        }
#      }
#
#      #Now open the scores file and get the list of start/end points
#      open( AL, "$dir/$fam/scores" )
#        || die "Could not open $dir/$fam/scores $!";
#      my $scoresObj = $io->parseScores( \*AL );
#      close(AL);
#      my %regions = %{ $scoresObj->regions };
#      foreach my $seqId ( keys %regions ) {
#        foreach my $r ( @{ $regions{$seqId} } ) {
#          $r->{fam}   = $fam;
#          $r->{acc}   = $acc;
#          $r->{ali}   = 'FULL';
#          $r->{start} = $r->{aliStart};
#          $r->{end}   = $r->{aliEnd};
#          push( @{ $regionsAll{$seqId} }, $r );
#          $noFullRegs++;
#        }
#      }
#      print "$fam, $noFullRegs\n";
#
#      open( SEED, "$dir/$fam/SEED" )
#        || die "Could not open $dir/$fam/SEED $!";
#
#      # read names into hash, concating as $fam/start/end:
#      #$fam = "SEED!$fam";
#      my $aln = new Bio::Pfam::AlignPfam;
#      $aln->read_selex( \*SEED );
#      foreach my $seq ( $aln->each_seq() ) {
#        my $key = $seq->id . "." . $seq->seq_version;
#        push(
#          @{ $regionsAll{$key} },
#          {
#            fam   => $fam,
#            acc   => $acc,
#            start => $seq->start,
#            end   => $seq->end,
#            score => "**",
#            ali   => 'SEED'
#          }
#        );
#
#      }
#    };
#    if ($@) {
#      print "Error dealing with $fam\n";
#    }
#  }
#
#  my $error = 0;
#
#  #print Dumper %regionsAll;
#  #exit;
#
#  # Loop over each protein to find overlaps
#  foreach my $protein ( keys %regionsAll ) {
#    for ( my $i = 0 ; $i <= $#{ $regionsAll{$protein} } ; $i++ ) {
#    REGION:
#      for ( my $j = $i + 1 ; $j <= $#{ $regionsAll{$protein} } ; $j++ ) {
#        next REGION
#          if ( $regionsAll{$protein}->[$i]->{fam} eq
#          $regionsAll{$protein}->[$j]->{fam} );
#        if ( $allowed->{ $regionsAll{$protein}->[$i]->{acc} } ) {
#          foreach
#            my $aFam ( @{ $allowed->{ $regionsAll{$protein}->[$i]->{acc} } } )
#          {
#            next REGION if ( $aFam eq $regionsAll{$protein}->[$j]->{acc} );
#          }
#        }
#        if ( $allowed->{ $regionsAll{$protein}->[$j]->{acc} } ) {
#          foreach
#            my $aFam ( @{ $allowed->{ $regionsAll{$protein}->[$j]->{acc} } } )
#          {
#            next REGION if ( $aFam eq $regionsAll{$protein}->[$i]->{acc} );
#          }
#        }
#
#        if ( $regionsAll{$protein}->[$i]->{start} <=
#             $regionsAll{$protein}->[$j]->{start}
#          && $regionsAll{$protein}->[$i]->{end} >=
#          $regionsAll{$protein}->[$j]->{start} )
#        {
#
#          my $string =
#              "(1) In "
#            . $regionsAll{$protein}->[$j]->{fam} . " "
#            . $regionsAll{$protein}->[$j]->{ali} . " " . ": "
#            . $protein . "/"
#            . $regionsAll{$protein}->[$j]->{start} . "-"
#            . $regionsAll{$protein}->[$j]->{end} . " ("
#            . $regionsAll{$protein}->[$j]->{score}
#            . " bits) overlaps with "
#            . $regionsAll{$protein}->[$i]->{fam} . " "
#            . $regionsAll{$protein}->[$i]->{ali} . " "
#            . $protein . "/"
#            . $regionsAll{$protein}->[$i]->{start} . "-"
#            . $regionsAll{$protein}->[$i]->{end} . " ("
#            . $regionsAll{$protein}->[$i]->{score}
#            . " bits)\n";
#          $error++;
#          print $string;
#          print OVERLAPS $string;
#
#        }
#        elsif ( $regionsAll{$protein}->[$i]->{start} <=
#             $regionsAll{$protein}->[$j]->{end}
#          && $regionsAll{$protein}->[$i]->{end} >=
#          $regionsAll{$protein}->[$j]->{end} )
#        {
#
#          my $string =
#              "(2) In "
#            . $regionsAll{$protein}->[$j]->{fam} . " "
#            . $regionsAll{$protein}->[$j]->{ali} . " " . ": "
#            . $protein . "/"
#            . $regionsAll{$protein}->[$j]->{start} . "-"
#            . $regionsAll{$protein}->[$j]->{end} . " ("
#            . $regionsAll{$protein}->[$j]->{score}
#            . " bits) overlaps with "
#            . $regionsAll{$protein}->[$i]->{fam} . " "
#            . $regionsAll{$protein}->[$i]->{ali} . " "
#            . $protein . "/"
#            . $regionsAll{$protein}->[$i]->{start} . "-"
#            . $regionsAll{$protein}->[$i]->{end} . " ("
#            . $regionsAll{$protein}->[$i]->{score}
#            . " bits)\n";
#          print $string;
#          print OVERLAPS $string;
#          $error++;
#        }
#        elsif ( $regionsAll{$protein}->[$i]->{start} >=
#             $regionsAll{$protein}->[$j]->{start}
#          && $regionsAll{$protein}->[$i]->{end} <=
#          $regionsAll{$protein}->[$j]->{end} )
#        {
#
#          my $string =
#              "(3) In "
#            . $regionsAll{$protein}->[$j]->{fam} . " "
#            . $regionsAll{$protein}->[$j]->{ali} . " "
#            . $regionsAll{$protein}->[$j]->{acc} . ": "
#            . $protein . "/"
#            . $regionsAll{$protein}->[$j]->{start} . "-"
#            . $regionsAll{$protein}->[$j]->{end} . " ("
#            . $regionsAll{$protein}->[$j]->{score}
#            . " bits) overlaps with "
#            . $regionsAll{$protein}->[$i]->{fam} . " "
#            . $regionsAll{$protein}->[$i]->{ali} . " "
#            . $protein . "/"
#            . $regionsAll{$protein}->[$i]->{start} . "-"
#            . $regionsAll{$protein}->[$i]->{end} . " ("
#            . $regionsAll{$protein}->[$i]->{score}
#            . " bits)\n";
#          print $string;
#          print OVERLAPS $string;
#
#          $error++;
#
#        }
#      }
#    }
#  }
#  close(OVERLAPS) || die "Could not close $dir/overlap file: $!\n";
#  return $error;
#}

#=head2 _get_DESC_overlaps
#
# Title    : _get_allowed_overlaps
# Usage    : $self->_get_DESC_overlaps("family_id")
# Function : Gets a list of families that are allowed to overlap with this family
# Returns  : @_
# Args     : family id
#
#=cut
#
#sub _get_DESC_overlaps {
#  my $family = shift;
#  my @overlaps;
#  if ( -e "$family/DESC" ) {
#    open( DESC, "$family/DESC" ) || die "can not open DESC file\n";
#    while (<DESC>) {
#      if (/^NE\s+(.*)$/) {
#        my @overlaps_allowed = split( /\;/, $1 );
#        my $db = Bio::Pfam->default_db;
#        foreach (@overlaps_allowed) {
#          my $id = $db->acc2id($_);
#          push( @overlaps, $id );
#        }
#      }
#    }
#    close(DESC);
#  }
#
#  return @overlaps;
#}

#=head2 _get_flat_overlaps_all
# Title    : _get_flat_overlaps_all
# Usage    : $self->_get_flat_overlpas("$current_dir")
# Function : Gets a list of all families that are allowed to overlap
# Returns  : @_
# Args     : family id
#=cut
#
#sub _get_flat_overlaps_all {
#  my $fams = shift;
#  my %nested;
#  foreach my $path (@$fams) {
#    my ( $dir, $fam );
#    if ( $path =~ /\// ) {
#      ( $dir, $fam ) = $path =~ (/^(\S+)\/(\S+)$/);    # get the last name
#    }
#    else {
#      $fam = $path;
#    }
#
#    # check is a directory
#    if ( !-e "$dir/$fam/DESC" ) {
#      warn "Cannot find directory $dir/$fam\n";
#      next;
#    }
#    my ( $outer, $inner, @ids );
#    open( DESC, "$dir/$fam/DESC" ) || die "Can not open $fam/DESC file";
#    while (<DESC>) {
#      if ( $_ =~ /^AC   (PF\d+)$/ ) {
#        $outer = $1;
#        my $db = Bio::Pfam::default_db();
#        $outer = $db->acc2id($outer);
#      }
#      elsif ( $_ =~ /^NE   (.*)$/ ) {
#        $inner = $1;
#        my @accs = split( /;/, $inner );
#        my $db = Bio::Pfam::default_db();
#        foreach (@accs) {
#          my $id = $db->acc2id($_);
#          push( @ids, $id );
#        }
#      }
#    }
#    close(DESC) || die "Can not close $fam/DESC file";
#    if (@ids) {
#      $nested{$outer} = [@ids];
#      $outer          = undef;
#      @ids            = undef;
#    }
#  }
#  return %nested;
#}
#
#=head2  check_acc_vs_name
#
# Title    : check_acc_vs_name
# Usage    : check_acc_vs_name($family)
# Function : Checks that the accesion in the DESC file and family matches that stored in RCS
# Returns  : 1 if there are errors, 0 if the SEED is fine
# Args     : family id
#
#=cut
#
#sub check_acc_vs_name {
#  my $family = shift;
#  my $error  = 0;
#  my $db     = Bio::Pfam->default_db();
#  open( DESC, "$family/DESC" ) || die "Could not open DESC file:[$!]\n";
#  my $desc_acc;
#  while (<DESC>) {
#    if (/^AC\s+(PF\d+)/) {
#      $desc_acc = $1;
#      last;
#    }
#  }
#  close(DESC);
#  if ($desc_acc) {
#    my $rcs_acc    = $db->id2acc($family);
#    my $rcs_family = $db->acc2id($desc_acc);
#    if ( $rcs_acc ne $desc_acc ) {
#      warn "\n$family acc:$desc_acc does not match rcs acc:$rcs_acc\n\n";
#      $error = 1;
#    }
#    elsif ( $rcs_family ne $family ) {
#      warn
#        "\n$family does not match rcs family name [expected $rcs_family]\n\n";
#      $error = 1;
#    }
#  }
#  else {
#    warn "Assuming this family is new and does not have an accession\n";
#  }
#  return $error;
#}

#
# REVIEWED SUBS
#

#-------------------------------------------------------------------------------

=head2 compareAlignToScores

 Title    : compare_align_to_scores
 Usage    : &PfamQC::compare_align_to_scores("family_id")
 Function : Checks that the number in ALIGN is the same as in scores
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub compareAlignToScores {
  my $famObj = shift;

  if ( $famObj->scores->numRegions != $famObj->ALIGN->num_sequences ) {

    #TODO - would be good to actually print these out!
    return 0;
  }
  else {
    return 1;
  }
}

#-------------------------------------------------------------------------------

=head2 more_in_align_than_seed

 Title    : more_in_align_than_seed
 Usage    : &PfamQC::more_in_align_than_seed("family_id")
 Function : Checks that the ALIGN file has more sequence
          : than the SEED alignment
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub moreInSEEDthanALIGN {
  my ($famObj, $pfamDB) = @_;

  if ( $famObj->SEED->num_sequences > $famObj->ALIGN->num_sequences ) {

    if($pfamDB) {#Compare number of refprot seq in SEED to ALIGN
      my $dbh = $pfamDB->getSchema->storage->dbh;
      my $sth=$dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc=?");
      my $numRPSeqSeed=0;
      foreach my $seq ($famObj->SEED->each_seq) { 
        $sth->execute($seq->id) or die "Couldn't execute statement ".$sth->errstr."\n";
        my $inPfamseq=$sth->fetchrow;
        if($inPfamseq) {
          $numRPSeqSeed++;
        }
      }
      if($numRPSeqSeed > $famObj->ALIGN->num_sequences) {
        # PF15403 has historically broken this rule...
        return 0 unless $famObj->DESC->AC eq 'PF15403';
      }
      else {
        return 1;
      }
    }
    else {
      return 0;
    }
  }
  else {
    return 1;
  }
}

#-------------------------------------------------------------------------------

=head2 mulFormatIsOK

 Title    : mul_format_is_OK
 Usage    : &PfamQC::mul_format_is_OK("file","family_id")
 Function : Checks a mul format alignment for errors
 Returns  : 1 if all is OK, 0 if there are problems
 Args     : mul file, family id

=cut

sub mulFormatIsOK {

  # can we replace this routine with loading into a simplealign object?
  my $file      = shift @_;
  my $family    = shift @_;
  my $error     = 0;
  my $seen      = 0;
  my $length    = 0;
  my $rf_length = 0;
  my $s_length  = 0;
  my ( $start, $end, $nse, $expected_length, $sequence, $name );

  open( MUL, "$family/$file" ) or die "Can't open $family/$file\n";
  while (<MUL>) {
    chop;
    $seen = 1;

    /^([A-Z0-9_]+\.\d+)\/(\d+)\-(\d+)\s+([A-Za-z\.\-]+)\s*$/
    || do {
      if ( $_ !~ /\#=RF/ ) {
        warn "$family: [$_] looks like a bad mul format line\n";
        $error = 1;
        next;
      }
      else {
        if (/#=RF\s+(\S+)/) {
          $rf_length = length($1);
        }

        next;
      }
    };
    $name     = $1;
    $start    = $2;
    $end      = $3;
    $sequence = $4;
    $s_length = $4;

    if ( $start >= $end or $start < 1 or $end < 1 ) {
      warn "$family: [$_] has bad start/end points";
      $error = 1;
    }

    # Check that length of sequence is end-start+1.
    $expected_length = $end - $start + 1;
    $sequence =~ s/\.//g;
    $sequence =~ s/\-//g;
    if ( $expected_length != length($sequence) ) {
      warn
      "$family: Your start-ends $name/$start-$end is out of sync with your sequence length\n ";
      $error = 1;
    }

    # Checking that all the mul alignment data is lined up correctly
    /^([A-Z0-9_]+\.\d+\/\d+\-\d+\s+)/
      or die "Mmmm - you shouldn't ever get this error!\n";
    $nse = $1;

    if ( !$length ) {
      $length = length($nse);
    }
    else {
      if ( length($nse) != $length ) {
        warn "$family: looks like mul format is out of sync on this file\n";
        $error = 1;
      }
    }
  }
  if ($rf_length) {
    if ( $rf_length != length($s_length) ) {
      warn "$family: RF line length does not match alignment length $rf_length,"
      . length($s_length) . "\n";
      $error = 1;
    }
  }

  if ( !$seen ) { 
    warn "$family: empty alignment\n";
    $error = 1;
  }

  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}

#-------------------------------------------------------------------------------

=head2 nameFormatIsOK

 Title    : name_format_is_OK
 Usage    : &PfamQC::name_format_is_OK("family_id")
 Function : Checks the format of the family name supplied
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub nameFormatIsOK {
  my $family = shift;

  if ( $family =~ /^[A-Za-z0-9_\-]{1,30}$/ ) {
    return 1;
  }
  else {
    warn "Your Pfam name contains invalid characters and/or is too long\n";
    return 0;
  }
}

#-------------------------------------------------------------------------------

=head2 onlyASCII

 Title    : onlyASCII
 Usage    : &PfamQC::onlyASCII($desc)
 Function : Checks the format of the DESC object supplied
 Returns  : 1 if all is OK, 0 if not
 Args     : DESC object

=cut

sub onlyASCII {
  my ($desc, $family_dir) = @_;

  my $chars = 0;

  foreach my $line ($desc->ID, $desc->DE, $desc->CC){
    next unless($line); #Sometimes $desc->CC is undefined;
    if($line =~ /[^[:ascii:]]/){ 
      warn "ERROR: The following line(s) in the DESC file match non-ASCII characters. If the non-ASCII characters are not visible below, type 'cat -v $family_dir/DESC' into the terminal to see them.\n";
      system("grep --color='auto' -P -n \"[^[:ascii:]]\" $family_dir/DESC");
      print "\n";
      $chars=1;
      last;
    }
  }

  if ( $chars == 0 ) {
    return 1;
  }
  else {
    warn "Your Pfam DESC file contains invalid characters\n";
    return 0;
  }
}

#-------------------------------------------------------------------------------

=head2 checkRefs

 Title    : checkRefs
 Usage    : &PfamQC::checkRefs($desc)
 Function : Checks if highest reference in CC exists in REFS list
 Returns  : 1 if all is OK, 0 if not
 Args     : DESC object

=cut

sub checkRefs {
  my ($desc, $family_dir) = @_;

  my $max_ref = 0;
  if ($desc->REFS) {
    $max_ref = $desc->REFS->[-1]->{'RN'};
  }

  my $cc = $desc->CC // '';
  my $max_ccref = 0;
  while ( $cc =~ /\[[\d,-]*(\d+)\]/g ) {
    my $cc_ref = $1;
    if ($cc_ref > $max_ccref) {
      $max_ccref = $cc_ref;
    }
  }

  if ($max_ccref > $max_ref) {
    warn "Reference [$max_ccref] seems to be referred in the CC, but the corresponding RN could not be found.\n";
    return 0;
  }

  return 1;
}

#-------------------------------------------------------------------------------

=head2 nonRaggedSeed

 Title    : nonRaggedSeed
 Usage    : PfamQC::raggedSeed($family)
 Function : Checks the SEED to make sure that it is not too ragged at the ends
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : family id

=cut

sub nonRaggedSeed {
  my ( $family, $famObj ) = @_;

  my $error  = 0;
  my $no_seq = 0;
  my $no_lhd = 0;
  my $no_rhd = 0;
  foreach my $seq ( $famObj->SEED->each_seq() ) {
    my $sequence = $seq->seq();
    if ( $sequence =~ /^(\.*)\S+/ ) {
      $no_lhd += length($1);
    }
    if ( $sequence =~ /(\.*)$/ ) {
      $no_rhd += length($1);
    }
    $no_seq++;
  }

  confess("No sequences found in the seed. This is very bad\n")
  if ( !$no_seq );
  my $bad_n = $no_lhd / $no_seq;
  my $bad_c = $no_rhd / $no_seq;

  if ( ( $bad_n > 0.5 ) || ( $bad_c > 0.5 ) ) {
    $error = 1;
    print STDERR "\n*** WARNING: SEED alignment is ragged ***\n\n";
    printf STDERR "%7s\t%7s\n", "N-term", "C-term";
    printf STDERR "%5.2f\t%5.2f\n", $bad_n, $bad_c;
  }
  else {
    print STDERR "\n--- SEED alignment is not ragged ---\n\n";
  }

  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}

#-------------------------------------------------------------------------------

=head2 sequenceChecker

 Title    : sequenceChecker
 Usage    : PfamQC::sequenceChecher($family, $familyObject, $pfamDB)
 Function : Checks the sequences in the SEED and ALIGN are valid.
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : A Bio::Pfam::Family::PfamA object 

=cut

sub sequenceChecker {
  my ( $family, $famObj, $pfamDB, $fast ) = @_;

  unless ($famObj) {
    confess("Undefined family object passed");
  }

  unless ( $famObj->isa("Bio::Pfam::Family::PfamA") ) {
    confess("A Bio::Pfam::Family::PfamA object was not passed in");
  }

  my $error = 0;
  my %allseqs;
  my $count = 0;

  my $dbh = $pfamDB->getSchema->storage->dbh;
  my $sth_pfamseq=$dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc = ? and seq_version = ?") or die "Failed to prepare statement:".$dbh->errstr."\n";
  my $sth_uniprot=$dbh->prepare("select uniprot_acc from uniprot where uniprot_acc = ? and seq_version = ?") or die "Failed to prepare statement:".$dbh->errstr."\n"; 
  my $sth_rp_seed=$dbh->prepare("select rp_seed from pfamA where pfamA_acc='$family'");
  $sth_rp_seed->execute() or die "Couldn't execute statement ".$sth_rp_seed->errstr."\n";  
  my $rp_seed=$sth_rp_seed->fetchrow;
  
  my ($not_pfamseq_count, $uniprot_count, $not_uniprot_count) = (0, 0, 0);
  my ($not_pfamseq_log, $not_uniprot_log) = ('', '');
  foreach my $aln (qw(SEED ALIGN)) {
    if(ref($famObj->$aln) eq 'Bio::Pfam::AlignPfamLite'){

      foreach my $seq (@{$famObj->$aln->all_nse_with_seq}){
        my $not_pfamseq;
        #Check whether seed seq are in pfamseq
        #If seed seq is not in pfamseq set, it must be in the current uniprot set to be valid
        if($aln eq "SEED") {
          my ($seq_acc, $seq_version);
          if($seq->[0] =~ /(\S+)\.(\d+)/) {
            ($seq_acc, $seq_version) = ($1, $2);
          }
          else {
            die "Couldn't parse accession number from $seq->[0]";
          }

          $sth_pfamseq->execute($seq_acc, $seq_version) or die "Couldn't execute statement ".$sth_pfamseq->errstr."\n";
          my $pfamseq = $sth_pfamseq->fetchrow;
          unless($pfamseq) {
            $not_pfamseq=1;
            $not_pfamseq_count++;
            $sth_uniprot->execute($seq_acc, $seq_version) or die "Couldn't execute statement ".$sth_uniprot->errstr."\n";           
            my $uniprot = $sth_uniprot->fetchrow;
            if($uniprot) {
              $uniprot_count++;
              $not_pfamseq_log .= $seq->id . "\n";
              # print STDERR $seq->id . ".$seq_version is in SEED but not in the pfamseq set in the rdb\n";
            }
            else {
              $not_uniprot_count++;
              $not_uniprot_log .= $seq->id . "\n";
              # print STDERR $seq->id . ".$seq_version is in SEED but not in the pfamseq or uniprot set in the rdb\n";
            }
          }
        }

        unless($not_pfamseq) {
          Bio::Pfam::SeqFetch::addSeqToVerify( $seq->[0], $seq->[1], $seq->[2], $seq->[3], \%allseqs ); 
          $count++;
        }
      }
    }elsif(ref($famObj->$aln) eq 'Bio::Pfam::AlignPfam'){
      foreach my $seq ( $famObj->$aln->each_seq ) {
        my $not_pfamseq;
        if($aln eq "SEED") {
          $sth_pfamseq->execute($seq->id, $seq->version) or die "Couldn't execute statement ".$sth_pfamseq->errstr."\n";
          my $pfamseq = $sth_pfamseq->fetchrow;
          unless($pfamseq) {
            $not_pfamseq=1;
            $not_pfamseq_count++;
            $sth_uniprot->execute($seq->id, $seq->version) or die "Couldn't execute statement ".$sth_uniprot->errstr."\n";     
            my $uniprot = $sth_uniprot->fetchrow;
            if($uniprot) {
              $uniprot_count++;
              $not_pfamseq_log .= $seq->id . "." . $seq->version . "\n";
              # print STDERR $seq->id . "." . $seq->version . " is in SEED but not in the pfamseq set in the rdb\n";
            }    
            else {
              $not_uniprot_count++;
              $not_uniprot_log .= $seq->id . "." . $seq->version . "\n";
              # print STDERR $seq->id . "." . $seq->version . " is in SEED but not in the pfamseq or uniprot set in the rdb\n";
            }    
          }    
        }

        unless($not_pfamseq) {  
          $count++;
          my $str_ali = uc( $seq->seq() );
          $str_ali =~ s/[.-]//g;
          my $seqName = $seq->id;
          if($seq->version){
            $seqName .= ".".$seq->version;
          }
          Bio::Pfam::SeqFetch::addSeqToVerify( $seqName, $seq->start, $seq->end, $str_ali, \%allseqs );
        }
      }
    }
  }


  #Update seedcheck
  if($not_pfamseq_count) {
    if($not_pfamseq_count == $uniprot_count) { #All sequences not in pfamseq are in the uniprot table
      $famObj->seedcheck('pfamseqplus');
    } else {
      #Some sequences not in pfamseq are not in the uniprot table
      $error = 1;
    }
    print STDERR "\n--- " . ($not_pfamseq_count - $not_uniprot_count) . " seed sequences are not in pfamseq ---\n";
    print STDERR "$not_pfamseq_log\n";
  }
  else {
    $famObj->seedcheck('pfamseq');
  }

  if($not_uniprot_count) {
    print STDERR "\n--- $not_uniprot_count seed sequences are not in pfamseq or uniprot ---\n";
    print STDERR "$not_uniprot_log\n";
    $error = 1;
  }

  if ($error) {
    # if errors do not verify seqs
    return 0;
  }

  if ($famObj->seedcheck eq 'pfamseqplus') {
    print STDERR "\nWill continue, but sequences should be from pfamseq where possible.";
    print STDERR "\nPress cntrl-c now to abort operation and attempt to include only pfamseq seqs\n";
    sleep 5 unless $fast;
  }

  my $verified_seq = Bio::Pfam::SeqFetch::verifySeqs( \%allseqs, $CONFIG->pfamseqLoc . "/pfamseq" );

  if ( $verified_seq == $count and $not_pfamseq_count == $uniprot_count) { #... and all sequences not in pfamseq are in the uniprot table
    print STDERR "\n--- All sequences are in the database ---\n\n";
  }
  else {
    print STDERR "\n*** ERROR: mismatch of sequences in family ***\n\n";
    $error = 1;
  }

  if($rp_seed and $not_pfamseq_count) {
    print STDERR "\n*** ERROR: SEED alignment for $family used to be on reference proteomes, but now is not ***\n\n";
    $error = 1;
  }

  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}
#-------------------------------------------------------------------------------

=head2 family_overlaps_with_signal_peptide
Usage       : &PfamQC::family_overlaps_with_signal_peptide("family_id", $famObj, $pfamDB)
Function    : check if the seed and align contain signal peptides as determined by phobius
Returns     : hash of overlaps found (keys in hash are seed, align, total)
Args        : family_id, Bio::Pfam::Family::PfamA, Bio::Pfam::PfamLiveDBManager

=cut

sub family_overlaps_with_signal_peptide {

  my ($family, $famObj, $pfamDB) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("$family: Did not get a family object passed in.....\n");
  }

  open(LOG, ">$family/sig_p_overlap") or die "Can't open file $family/sig_p_overlap for writing log.\n";

  my ( %count);
  $count{seed} = $count{align} = 0;


  foreach my $aln (qw(SEED ALIGN)){
    if($aln eq "ALIGN" and !$famObj->ALIGN) { # ALIGN0 This is not populated for families that do not have any reference proteome matches
      next;
    }
    my(%regions);
    if(ref($famObj->$aln) eq 'Bio::Pfam::AlignPfam'){
      foreach my $seq ($famObj->$aln->each_seq) {
        next if($seq->start > 120);
        if(exists($regions{$seq->id})) {
          if($seq->start < $regions{$seq->id}{start}) { #Only store first region on the sequence
            $regions{$seq->id}{start}=$seq->start;
            $regions{$seq->id}{end}=$seq->end;
          }
        }
        else {
          $regions{$seq->id}{start}=$seq->start;
          $regions{$seq->id}{end}=$seq->end;
        }
      }
    }elsif(ref($famObj->$aln) eq 'Bio::Pfam::AlignPfamLite'){
      foreach my $seq (@{$famObj->$aln->all_nse}) {
        my $id;
        if($seq->[0] =~ /(\S+)\.\d+/) {
          $id = $1;
        }
        else {
          $id=$seq->[0];
        }
        next if($seq->[1] > 120);
        if(exists($regions{$id})) {
          if($seq->[1] < $regions{$id}{start}) { #Only store first region on the sequence
            $regions{$id}{start}=$seq->[1];
            $regions{$id}{end}=$seq->[2];
          }
        }else{
          $regions{$id}{start}=$seq->[1];
          $regions{$id}{end}=$seq->[2];
        }
      }
    }else{
      die "Unknown alignment type!\n";
    }
    my $overlap_hash = $pfamDB->getSignalPeptideRegion(\%regions);
    
    foreach my $pfamseq_acc (keys %{$overlap_hash}) {
      print LOG "Sequence $pfamseq_acc/" . 
      $regions{$pfamseq_acc}{start} . "-". $regions{$pfamseq_acc}{end} . " in ".$aln." overlaps with signal peptide ".$overlap_hash->{$pfamseq_acc}."\n";
      $count{lc($aln)}++;
    }
  }

  $count{total}=$count{seed}+$count{align};

  return \%count;
}

#-------------------------------------------------------------------------------

=head2 family_overlaps_with_db

 Title    : family_overlaps_with_db
 Usage    : &PfamQC::family_overlaps_with_db("family_id", \@ignore, $pfamDB, $famObj, $compete, $noFilter)
 Function : Checks that the alignment contains no overlaps to data in 
            the RDB, and prints any overlaps to STDERR
 Returns  : number of overlaps
 Args     : family_id, reference to an array of families to ignore, pfam db object, family object, compete flag, 
            no Filter flag (whether to filter overlaps according to long and short)

=cut

sub family_overlaps_with_db {
  my ( $family, $ignore_ref, $pfamDB, $famObj, $compete, $noFilter) = @_;
  my ( %ignore, @overlaps );
  
  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("$family: Did not get a family object passed in.....\n");
  }

  unless($famObj->scores->numRegions) { #If empty ALIGN file, no need to filter overaps as SEED overlaps cannot be filtered
     $noFilter=1;
   }
  if($noFilter) {
    warn "Overlaps will not be filtered\n";
  }

  #This could be nested in another domain, so we need to check!
  my $nestedRef = $pfamDB->getNestedDomain( $famObj->DESC->AC );

  if ($nestedRef) {
    foreach my $n (@$nestedRef) {
      $$ignore_ref{$n}++;

      my $clan = $pfamDB->getClanDataByPfam($n);
      if ( $clan and $clan->clan_acc->clan_acc ) {
        my $clanMem = $pfamDB->getClanMembership( $clan->clan_acc->clan_acc );
        foreach my $fam (@$clanMem) {
          $$ignore_ref{ $fam->pfama_acc->pfama_acc }++;
        }
      }
    }
  }

  if ( $famObj->DESC->NESTS ) {
    foreach my $nesting ( @{ $famObj->DESC->NESTS } ) {
      $$ignore_ref{ $nesting->{dom} }++;
      my $clan = $pfamDB->getClanDataByPfam( $nesting->{dom} );
      if ( $clan and $clan->clan_acc->clan_acc ) {
        my $clanMem = $pfamDB->getClanMembership( $clan->clan_acc->clan_acc );
        foreach my $fam (@$clanMem) {
          $$ignore_ref{ $fam->pfama_acc->pfama_acc }++;
        }
      }
    }
  }

  my %clanFam;
  if ( $famObj->DESC->CL ) {

    #Okay, we have a family that is part of a clan
    my $clanMem = $pfamDB->getClanMembership( $famObj->DESC->CL );
    foreach my $fam (@$clanMem) {
      $$ignore_ref{ $fam->pfama_acc->pfama_acc }++;
      $clanFam{$fam->pfama_acc->pfama_acc}=1;
      my $nestedRef =
      $pfamDB->getNestedDomain( $fam->pfama_acc->pfama_acc );

      if ($nestedRef) {
        foreach my $n (@$nestedRef) {
          $$ignore_ref{$n}++;

          my $clan = $pfamDB->getClanDataByPfam($n);
          if ( $clan and $clan->clan_acc->clan_acc ) {
            my $clanMem = $pfamDB->getClanMembership( $clan->clan_acc->clan_acc );
            foreach my $fam (@$clanMem) {
              $$ignore_ref{ $fam->pfama_acc->pfama_acc }++;
            }
          }
        }
      }
    }
  }

  # Test ignores are real families
  foreach my $ignore ( keys %{$ignore_ref} ) {
    warn "Ignoring family $ignore\n";

#    eval { $pfamDB->id2acc($ignore); };
#    $@ and die
#    "The family $ignore does not seem to exist. Have you mistyped the name?\n";
#    $ignore{$ignore} = 1;
  }

  #Overlaps will be checked for in all regions added to this hash
  my %regions;

  #Go through seed sequences
  foreach my $seq ( $famObj->SEED->each_seq ) {
    push @{ $regions{$seq->id} },
    {
      from      => $seq->start,
      to        => $seq->end,
      family    => ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
      ali       => 'SEED',
      family_id => ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" )
    };
  }


#Then for the full, use the scores file as this contains tha alignment co-ordinates.
#All sequences in ALIGN will be in pfamseq
#We now allow overlaps between envelopes.
  foreach my $seq ( keys %{ $famObj->scores->regions } ) {
    my $id;
    if ( $seq =~ /(\S+)\.\d+/ ) {
      $id = $1;
    }
    else {
      $id = $seq;
    }

    foreach my $fullReg ( @{ $famObj->scores->regions->{$seq} } ) {
      push @{ $regions{$id} },
      {
        ali_from  => $fullReg->{aliStart},
        ali_to    => $fullReg->{aliEnd},
        from      => $fullReg->{start},
        to        => $fullReg->{end},
        score     => $fullReg->{score},
        evalue    => $fullReg->{evalue}, 
        family    => ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
        family_id => ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" ),
        ali       => 'FULL'
      };
    }
  }

  my %overlaps;
  $pfamDB->getOverlapingFullPfamRegions( \%regions, \%overlaps );
  $pfamDB->getOverlapingSeedPfamRegions( \%regions, \%overlaps );


  my @overlapLines; # array for keeping printed overlap lines
  my $numOverlaps = 0;
  my %seen;

#Now print out any overlaps that should not be ignored
  my $LOG;
  if ( -d $family ) {
    open( $LOG, ">$family/overlap" ) or die "Can't open $family/overlap file\n";
  }
  foreach my $seqAcc ( keys %overlaps ) {
    foreach
    my $region ( sort { $a->{from} <=> $b->{from} } @{ $overlaps{$seqAcc} } )
    {

      REGION:
      foreach my $overRegion ( @{ $region->{overlap} } ) {
        
        #Skip if it's in the ignore hash, unless it's an overlap between two SEED alignments within a clan
        if ( $ignore_ref->{ $overRegion->{family} } ) {
          next unless (exists($clanFam{$overRegion->{family}}) and $region->{ali} eq "SEED" and $overRegion->{ali} eq "SEED");
        }

        if($region->{ali} eq 'FULL' and $compete){

          if(_compete($seqAcc, $region, $overRegion, $pfamDB, $famObj->DESC->CL)){
            next REGION;  
          }
        }

        my $line;
        if($region->{ali} eq 'SEED') {
          $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{from}."-".$region->{to}." ".$region->{ali}." with ";
        }
        else {
          $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{ali_from}."-".$region->{ali_to}." (".
          $region->{family}."/".$region->{from}."-".$region->{to}.", ".$region->{score}." bits) ".$region->{ali}." with ";
        }

        if($overRegion->{ali} eq 'SEED') {
          $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}." ".$overRegion->{ali}."\n";
        }
        else {
          $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{ali_from}."-".$overRegion->{ali_to}." (".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}
          .", ".$overRegion->{score}." bits) ".$overRegion->{ali}."\n";
        }

        next if ( $seen{$line} );
        $seen{$line}++;

        if (defined $noFilter) # if there is no filtering steps print the overlap lines now
        {
          $numOverlaps++;
          print STDERR $line;
          print $LOG $line if $LOG;
        }
        else # else keep printed line in the appropriate array for the filtering step
        {
          push (@overlapLines, $line); 
        }
      }
    }
  }

  unless (defined $noFilter)
  {
    warn "Filtering overlaps\n";
    $numOverlaps = filterOverlaps($family, $famObj->scores->numRegions, \@overlapLines, "1");
  }

  close $LOG if ($LOG);

  my $sOverlaps = seedIntOverlaps($famObj);
  $numOverlaps += $sOverlaps;

  if ($numOverlaps) {
    return $numOverlaps;
  }
  else {
    return 0;
  }
}

=head2 findOverlapsDb

 Title    : findOverlapsDb
 Usage    : &PfamQC::findOverlapsDb($regionObj, \@ignore, $pfamDB, $clan, $compete, $filter, $numFull)
 Function : Checks that the regions object contains no overlaps to data in 
            the RDB. Similar to the family_overlaps_with_db subroutine above, but takes regions object as an argument rather than 
            populating the regions object within the sub
 Returns  : number of overlaps, array of overlap lines
 Args     : regions object reference, reference to an array of families to ignore, pfamDB object, clan, compete flag, filter flag, numFull

=cut

sub findOverlapsDb {
  my ($regions, $ignore_ref, $pfamDB, $clan, $compete, $filter, $numFull) = (@_);

  my %overlaps;

  $pfamDB->getOverlapingFullPfamRegions( $regions, \%overlaps );
  $pfamDB->getOverlapingSeedPfamRegions( $regions, \%overlaps );

  my %clanFam;
  if($clan) {
    my $clanMem = $pfamDB->getClanMembership($clan);
    foreach my $fam (@$clanMem) {
      $clanFam{$fam->pfama_acc->pfama_acc}=1;
    }
  }


  my (@overlapLines, %seen);
  my $numOverlaps = 0;
  foreach my $seqAcc ( keys %overlaps ) {
    foreach my $region ( sort { $a->{ali_from} <=> $b->{ali_from} } @{ $overlaps{$seqAcc} } ) {

      REGION:
      foreach my $overRegion ( @{ $region->{overlap} } ) {
        #Skip if it's in the ignore hash, unless it's an overlap between two SEED alignments within a clan
        if ( $ignore_ref->{ $overRegion->{family} } ) {
          next unless (exists($clanFam{$overRegion->{family}}) and $region->{ali} eq "SEED" and $overRegion->{ali} eq "SEED");
        }

        if($region->{ali} eq 'FULL' and $compete){
          if(_compete($seqAcc, $region, $overRegion, $pfamDB, $clan)){
            next REGION;  
          }
        }

        my $line;
        if($region->{ali} eq 'SEED') {
          $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{from}."-".$region->{to}." ".$region->{ali}." with ";
        }
        elsif(!$region->{from}) { #For jackmmer searches, $region->{from}, $region->{to} and $region->{bits} are not populated
          $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{ali_from}."-".$region->{ali_to}." ".$region->{ali}." with ";
        }
        else {
          $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{ali_from}."-".$region->{ali_to}." (".
          $region->{family}."/".$region->{from}."-".$region->{to}.", ".$region->{score}." bits) ".$region->{ali}." with ";
        }

        if($overRegion->{ali} eq 'SEED') {
          $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}." ".$overRegion->{ali}."\n";
        }
        else {
          $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{ali_from}."-".$overRegion->{ali_to}." (".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}
          .", ".$overRegion->{score}." bits) ".$overRegion->{ali}."\n";
        }

        next if ( $seen{$line} );
        $seen{$line}++;

        $numOverlaps++;
        push (@overlapLines, $line); 
      }
    }
  }

  if($filter) {
    #Get family name
    my $family;
    foreach my $acc (keys %{$regions}) {
      foreach my $reg (@{$regions->{$acc}}) {
        $family = $reg->{family};
        last;
      }
    }
    die "Couldn't get family accession from regions object\n" unless($family);
    my ($numOverlaps, $filteredOverlapLines, $summary) = filterOverlaps($family, $numFull, \@overlapLines, "");
    return($numOverlaps, $filteredOverlapLines, $summary);
  }
  else {
    return($numOverlaps, \@overlapLines);
  }
}


#------------------------------------------------------------------------------
sub filterOverlaps {
  my ( $family, $familySize, $overlapArray, $print) = @_;

  # Get the Pfam Config
  my $config = Bio::Pfam::Config->new;

  # Filter the overlaps according to the auto-resolve paramaters found inside the config file
  my $lengthLimit = $config->sequenceOverlapRule; #Proportion of lowest scoring match length that is allowed to overlap
  my $numberLimit = $config->familyOverlapRule; #% of ALIGN regions allowed to overlap

  my $LOG;
  if ($print and -d $family ) {
    open( $LOG, ">$family/overlap" ) or die "Can't open $family/overlap file\n";
  }

  my @filteredOverlapLines;

  #Loop through the overlaps and sort into SEED overlaps, short overlaps and long overlaps
  my $seedOverlaps=0;
  my (%shortOverlaps, %longOverlaps); #These hashes will contain ALIGN region overlaps, apart from those where familyA has the higher bit score
  foreach my $overlapLine (@$overlapArray) {
    next unless($overlapLine =~ /.+/); #Ignore blank lines
    my $header = "SEED overlaps:\n";
    if($overlapLine =~ /SEED/) {
      if($print) {
        print STDERR $header unless($seedOverlaps);
        print STDERR "$overlapLine" if($print);
        print $LOG "$overlapLine" if($LOG);
      }
      else {
        push(@filteredOverlapLines, $header) unless($seedOverlaps);
        push(@filteredOverlapLines, $overlapLine);
      }
      $seedOverlaps++;
    }
    #Sequence [F1RZS8] overlap ShortName newFam/62-117 (newFam/60-117, 101.2 bits) FULL with Daxx PF03344/63-157 (PF03344/60-158, 159.80 bits) FULL
    elsif ($overlapLine =~ /^Sequence \[(\w+)\] overlap \S+\s(\S+)\/(\d+)\-(\d+) \(\S+\/\d+-\d+, (\S+) bits\) FULL with \S+ (\S+)\/(\d+)-(\d+) \(\S+\/\d+-\d+, (\S+) bits\) FULL/) {
      my ($seqAcc, $familyA, $ali_st1, $ali_en1, $scoreA, $familyB, $ali_st2, $ali_en2, $scoreB) = ($1, $2, $3, $4, $5, $6, $7, $8, $9);

      my $regionA = new Bio::Range( -start => $ali_st1, -end => $ali_en1, -strand => +1 );	
      my $regionB = new Bio::Range( -start => $ali_st2, -end => $ali_en2, -strand => +1 );

      my ( $s, $e, $d ) = $regionA->intersection($regionB);
      my $overlapLength = ( $e - $s ) + 1;

      # if the lowest scoring region doesn't belong to the local family skip to the next overlap line
      if ( $scoreA > $scoreB ) {#Potentially this could introduce lots of overlaps for familyB, but we don't care too much about this
        next;
      }

      my $overlapPerc = $overlapLength / $regionA->length * 100 ;

      if ( $overlapPerc < $lengthLimit ) { # Short overlaps that are allowed, because the overlap length is less than $lengthLimit residues
        $shortOverlaps{$overlapLine}=1;
      }
      else  {  # Long overlaps that should not be allowed (and have to be resolved manually)
        $longOverlaps{$overlapLine}=1;
      }
    }
    else {
      print STDERR "Couldn't parse this line: $overlapLine";
    }
  }

  my $shortOverlaps=scalar(keys(%shortOverlaps));
  my $longOverlaps=scalar(keys(%longOverlaps));
  my $percOverlap = ($shortOverlaps / $familySize) * 100;
  $percOverlap = sprintf("%.1f", $percOverlap);  #One decimal point should be enough to report

  #Count total overlaps
  my $numOverlaps=0;
  $numOverlaps+=$seedOverlaps;


  # See if the number of short overlaps is lower than 1% of the family size
  if($percOverlap >= $numberLimit ) { #If % $shortOverlaps >= than that allowed then need to report them
    my $header = "Short overlaps:\n";
    if($print) {
      print STDERR $header;
    }
    else {
      push(@filteredOverlapLines, $header);
    }

    foreach my $overlap (keys %shortOverlaps) {
      if($print) {
        print STDERR $overlap;
        print $LOG $overlap if $LOG;
      }
      else {
        push (@filteredOverlapLines, $overlap);
      }
    }
    $numOverlaps+=$shortOverlaps;
  }

  #Long overlaps are not permitted so need to report them
  if($longOverlaps) {
    my $header = "Long overlaps:\n";
    if($print) {
      print STDERR $header;
    }
    else {
      push(@filteredOverlapLines, $header);
    }

    foreach my $overlap (keys %longOverlaps) {
      if($print) {
        print STDERR $overlap;
        print $LOG $overlap if $LOG;
      }
      else {
        push (@filteredOverlapLines, $overlap);
      }
    }
    $numOverlaps+=$longOverlaps;
  }
  close $LOG if ($LOG);

  #Summarise the data
  my $summary;
  if($seedOverlaps) {
    my $line = "$family: there are $seedOverlaps seed overlaps\n";
    print STDERR $line if($print);
    $summary .= $line;
  }
  if($percOverlap ==0) {
    #Nothing to report
  }
  elsif($percOverlap >= $numberLimit ) {
    my $line ="$family: $percOverlap". "% ($shortOverlaps/$familySize) of regions in ALIGN have short overlaps, only <$numberLimit". "% are permitted (short means <$lengthLimit". "% of their length)\n";
    print STDERR $line if($print);
    $summary .= $line;
  }
  else {
    my $line = "$family: $percOverlap". "% ($shortOverlaps/$familySize) of regions in ALIGN have short overlaps, these are permitted (short means <$lengthLimit". "% of their length)\n";
    print STDERR $line if($print);
    $summary .= $line;
  }

  if($longOverlaps) {
    my $line = "$family: $longOverlaps/$familySize regions in ALIGN have long overlaps, these are not permitted (long means >=$lengthLimit" . "% of their length, long overlaps are not permitted).\n";
    print STDERR $line if($print);
    $summary .= $line;
  }

  if($print) {
    return ($numOverlaps);
  }
  else {
    return ($numOverlaps, \@filteredOverlapLines, $summary);
  }
}



#------------------------------------------------------------------------------
sub seedIntOverlaps{
  my( $famObj ) = shift;

  my $numOverlaps = 0;
  # Test for internal overlaps in the SEED.. With H3 we do get overlaps between hits!
  my @list = $famObj->SEED->each_seq();
  for ( my $seq = shift(@list) ; defined $seq ; $seq = shift(@list) ) {
    foreach my $other (@list) {
      if ( $seq->id ne $other->id ) {
        next;
      }
      if (
        ( $other->start() >= $seq->start() && $other->end() <= $seq->end() )
        || ( $other->start() <= $seq->end() && $other->end() >= $seq->end() )
        || ( $other->start() <= $seq->start
          && $other->end() >= $seq->start() )
      )
      {

        printf STDERR (
          "Internal SEED overlap of %s/%d-%d to %s/%d-%d\n",
          $seq->id,   $seq->start,   $seq->end,
          $other->id, $other->start, $other->end
        );
        $numOverlaps++;
      }
    }
  }  

  return $numOverlaps;
}



#-------------------------------------------------------------------------------

=head2 noMissing

  Title    : noMissing
  Usage    : Bio::Pfam::PfamQC::nomissing($oldFam, $newFam, $name) 
  Function : Checks that all of the sequences in the old family are present
           : in the updated version.
  Args     : Bio::Pfam::Family::PfamA objects representing the old and new families and
           : name of the family, which is assumed also to be the directory name of the 
           : family
  Returns  : 1/0, success or failure

=cut

sub noMissing {
  my ( $newFamObj, $oldFamObj, $family ) = @_;

  my (@allnewseqs, @alloldseqs);

  if(ref($newFamObj->ALIGN) eq 'Bio::Pfam::AlignPfamLite'){  
    push(@allnewseqs, @{$newFamObj->ALIGN->all_seq_accs});          
  }elsif(ref($newFamObj->ALIGN) eq 'Bio::Pfam::AlignPfam'){
    my $previous_id = '';
    foreach my $seq ( sort{$a cmp $b} $newFamObj->ALIGN->each_seq ) {
      push(@allnewseqs, $seq->id) if($seq->id ne $previous_id);
      $previous_id = $seq->id;
    }
  }
  elsif(!$newFamObj->ALIGN) {
    # ALIGN0 This is not populated for families that do not have any reference proteome matches
  }else{
    die "Did not get a Bio::Pfam::AlignPfamLite or Bio::Pfam::AlignPfam object\n";  
  }


  if(ref($oldFamObj->ALIGN) eq 'Bio::Pfam::AlignPfamLite'){  
    push(@alloldseqs, @{$oldFamObj->ALIGN->all_seq_accs});          
  }elsif(ref($oldFamObj->ALIGN) eq 'Bio::Pfam::AlignPfam'){
    my $previous_id = '';
    foreach my $seq ( sort{$a cmp $b} $oldFamObj->ALIGN->each_seq ) {
      push(@alloldseqs, $seq->id) if($seq->id ne $previous_id);
      $previous_id = $seq->id;
    }
  }
  elsif(!$newFamObj->ALIGN) {
    # ALIGN0 This is not populated for families that do not have any reference proteome matches
  } else{
    die "Did not get a Bio::Pfam::AlignPfamLite or Bio::Pfam::AlignPfam object\n";  
  }

  my (@found);
  NEW:
  for (my $i =0; $i<= $#allnewseqs; $i++){
    for(my $j = 0; $j <= $#alloldseqs; $j++){
      if($allnewseqs[$i] eq $alloldseqs[$j]){
        $alloldseqs[$j] = 0;
        next NEW;  
      }
    }  
    push(@found, $allnewseqs[$i]);    
  }


  ###########################################
  # Find missing sequences in edited family #
  ###########################################
  my $lost  = 0;
  my $extra = 0;

  # Put missing sequences into a missing file in directory
  open( MISSING, "> $family/missing" )
  || die "Can't write to file $family/missing\n";
  for( my $s =0; $s < scalar(@alloldseqs); $s++ ) {
    next if($alloldseqs[$s] eq '0');
    print MISSING $alloldseqs[$s]." not found\n";
    # Add element to missing hash
    $lost++;
  }
  close(MISSING);

  ##################################
  # Find how many sequences gained #
  ##################################
  open( FOUND, "> $family/found" )
  || die "Can't write to file $family/found\n";
  foreach my $acc ( @found ) {
    print FOUND "$acc found\n";
    $extra++;
  }
  close(FOUND);
  print "Lost $lost. Found $extra.\n";

  my $error;
  if ($lost) {
    print STDERR "\n*** ERROR: missing members compared to svn copy ***\n\n";
    $error = 1;
  }
  else {
    print STDERR "\n--- No missing sequences compared to svn copy ---\n\n";
  }

  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}

#-------------------------------------------------------------------------------

=head2 noFragsInSeed

 Title    : noFragsInSeed
 Usage    : Bio::Pfam::PfamQC::noFragsInSeed($family)
 Function : Does exactly what you expect.  It identifies if there are any fragments in the SEED
            file.  
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : family id

=cut

sub noFragsInSeed {
  my ( $family, $famObj ) = @_;
  my %bad_seq;

  foreach my $seq ( $famObj->SEED->each_seq() ) {
    my $sequence = $seq->seq();
    if ( $sequence =~ /^(\.*)\S+/ ) {
      if ( length($1) > 15 ) {
        $bad_seq{ $seq->id . "/" . $seq->start . "-" . $seq->end }++;
      }
    }
    if ( $sequence =~ /(\.*)$/ ) {
      if ( length($1) > 15 ) {
        $bad_seq{ $seq->id . "/" . $seq->start . "-" . $seq->end }++;
      }
    }
  }

  my $error = 0;

  open( FRAG, ">$family/seedFrag" )
    or confess("Could not open $family/seedFrag:[$!]");

  if ( keys %bad_seq ) {
    $error = 1;
    print STDERR "\n*** ERROR: your seed contains fragments! ***\n\n";
  }
  else {
    print STDERR "\n--- Your seed does not contain fragments! ---\n\n";
  }

  foreach my $seq ( keys %bad_seq ) {
    print FRAG "$seq\n";
    print STDERR "$seq look like a fragment in the seed\n";
  }

  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}

#-------------------------------------------------------------------------------

=head2 checkFamilyFiles

 Title    : checkFamikyfiles
 Usage    : &PfamQC::check_current_family_files("family_id")
 Function : Checks that the requisite files are present and correct in the
            local family directory
 Returns  : 1 is all is OK, 0 if there are problems
 Args     : family id

=cut

sub checkFamilyFiles {
  my ( $family, $famObj ) = @_;

  my $error = 0;

  foreach my $file ( @{ $CONFIG->mandatoryFamilyFiles() } ) {
    if ( !( -e "$family/$file" ) ) {
      warn "$family: $file does not exist\n";
      $error = 1;
    }
  }

  unless ( $famObj and $famObj->source and $famObj->source eq 'svn' ) {
    if ( -M "$family/SEED" < -M "$family/HMM" ) {
      warn
      "$family: Your seed alignment [$family/SEED] is younger than your HMM file [$family/HMM].\n";
      $error = 1;
    }
    if ( -M "$family/HMM" < -M "$family/PFAMOUT" ) {
      warn
      "$family: Your HMM [$family/HMM] is younger than your OUTPUT file [$family/PFAMOUT].\n";
      $error = 1;
    }
    if ( -e "$family/OUTPUT" ) {
      if ( -M "$family/HMM" < -M "$family/OUTPUT" ) {
        warn
        "$family: Your HMM [$family/HMM] is younger than your OUTPUT file [$family/OUTPUT].\n";
        $error = 1;
      }
      if ( -M "$family/OUTPUT" < -M "$family/PFAMOUT" ) {
        warn
        "$family: Your OUTPUT [$family/OUTPUT] is younger than your parsed output [$family/PFAMOUT].\n";
        $error = 1;
      }
    }
    if ( -M "$family/PFAMOUT" < -M "$family/ALIGN" ) {
      warn
      "$family: Your PFAMOUT [$family/PFAMOUT] is younger than your full alignment [$family/ALIGN].\n";
      $error = 1;
    }
    if ( -M "$family/PFAMOUT" < -M "$family/scores" ) {
      warn
      "$family: Your OUTPUT [$family/PFAMOUT] is younger than your scores [$family/scores].\n";
      $error = 1;
    }
    if ( -M "$family/scores" < -M "$family/ALIGN" ) {
      warn
      "$family: Your scores [$family/scores] is younger than your full alignment [$family/ALIGN].\n";
      $error = 1;
    }
  }
  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}

#-------------------------------------------------------------------------------

=head2 verifyEdits

  Title    : verifyEdits
  Usage    : Bio::Pfam::PfamQC::verifyEdits($family, $familyObj);
  Function : Chekst to see if the family has ED lines and cross-checks that they truely correspond
           : to regions that are present in the database.
  Args     : name of the family, a Bio::Pfam::Family::PfamA object
  Returns  : 1/0, success or failure


=cut

sub verifyEdits {
  my ( $family, $famObj ) = @_;

  my $error;
  open( BADED, ">$family/badEdits" )
    or die "Could not open badEdits line:[$!]\n";

  if ( $famObj->DESC->EDITS and scalar( @{ $famObj->DESC->EDITS } ) ) {

    foreach my $edit ( @{ $famObj->DESC->EDITS } ) {

#$edit is a hashref! {seq => $1, oldFrom => $2, oldTo => $3, newFrom => $5, newTo => $6 }
      if ( $edit->{newFrom} and $edit->{newTo} <= $edit->{newFrom} ) {
        warn "New end is less than or equal to new start\n";

        $error = 1;
      }

      #Now check that the new coos are withing the old coordinates
      unless ( $edit->{delete} ) {
        unless ( $edit->{newFrom} >= $edit->{oldFrom}
            and $edit->{newFrom} < $edit->{oldTo} )
        {
          warn $edit->{seq} . "/"
          . $edit->{oldFrom} . "-"
          . $edit->{oldTo}
          . ":Your new start co-ordinated is out of range\n";

          print BADED $edit->{seq} . "/"
          . $edit->{oldFrom} . "-"
          . $edit->{oldTo}
          . ":Your new start co-ordinated is out of range\n";

          $error = 1;
        }
        unless ( $edit->{newTo} > $edit->{oldFrom}
            and $edit->{newTo} <= $edit->{oldTo} )
        {
          warn $edit->{seq} . "/"
          . $edit->{oldFrom} . "-"
          . $edit->{oldTo}
          . "Your new end co-ordinated is out of range\n";
          print BADED $edit->{seq} . "/"
          . $edit->{oldFrom} . "-"
          . $edit->{oldTo}
          . "Your new end co-ordinated is out of range\n";
          $error = 1;
        }
      }

      #Now cross reference the edit against the matches;
      my $checked = 0;

      #print Dumper $famObj->PFAMOUT->seqs;
      if ( $famObj->PFAMOUT->seqs->{ $edit->{seq} } ) {
        foreach
        my $seq ( @{ $famObj->PFAMOUT->seqs->{ $edit->{seq} }->hmmUnits } )
        {
          if (  $seq->envFrom == $edit->{oldFrom}
              and $seq->envTo == $edit->{oldTo} )
          {
            $checked = 1;
            last;
          }
        }
      }
      unless ($checked) {
        warn "Could not find "
        . $edit->{seq} . "/"
        . $edit->{oldFrom} . "-"
        . $edit->{oldTo}
        . "in PFAMOUT\n";
        print BADED "Could not find "
        . $edit->{seq} . "/"
        . $edit->{oldFrom} . "-"
        . $edit->{oldTo}
        . " in PFAMOUT\n";
        $error = 1;
      }

      unless ( $edit->{delete} ) {
        my $checkedScores;
        if ( $famObj->scores->regions->{ $edit->{seq} } ) {
          foreach my $seq ( @{ $famObj->scores->regions->{ $edit->{seq} } } ) {
            if (  $seq->{start} == $edit->{newFrom}
                and $seq->{end} == $edit->{newTo} )
            {
              $checkedScores = 1;
              last;
            }
          }
        }
        unless ($checkedScores) {
          warn "Could not find "
          . $edit->{seq} . "/"
          . $edit->{newFrom} . "-"
          . $edit->{newTo}
          . " in scores file\n";
          print BADED "Could not find "
          . $edit->{seq} . "/"
          . $edit->{newFrom} . "-"
          . $edit->{newTo}
          . " in scores file\n";
          $error = 1;
        }
      }
    }
  }
  else {
    print STDERR "This family does not have any ED lines, no need to check\n";
  }
  close(BADED);

  if ($error) {
    return 0;    # failure
  }
  else {
    return 1;    # success
  }
}

#-------------------------------------------------------------------------------

=head2 checkNestings 

  Title    : checkNestings
  Usage    : Bio::Pfam::PfamQC::checkNestings($family, $familyObj);
  Function : Chekst to see if the family has NE/NL lines and cross-checks that there
           : is the corresponding RF line in the SEED alignment.  Note, it does not check
           : that either the NE line is correct or the RF lines is correct
  Args     : name of the family, a Bio::Pfam::Family::PfamA object
  Returns  : 1/0, success or failure


=cut

sub checkNestings {
  my ( $family, $famObj ) = @_;

  my $error = 0;
  if ( $famObj->DESC->NESTS and scalar( @{ $famObj->DESC->NESTS } ) ) {
    unless ( $famObj->SEED->match_states_string )

    #and $famObj->SEED->match_states_string->isa('Bio::Pfam::OtherReg') )
    {
      warn
      "Found nested location in $family, but no nested domain indicated by #=RF line!\n";
      $error = 1;
    }
  }
  else {
    if ( $famObj->SEED->match_states_string ) {
      warn
      "Found an #=RF line in SEED alignment, but no nested locations in the DESC file!\n";
      $error = 1;
    }
  }

  if ($error) {
    return 0;    # failure
  }
  else {
    return 1;    # success
  }
}

sub checkCLANDESCSpell {
  my ( $clan, $clanIO ) = @_;

  my $config     = Bio::Pfam::Config->new;
  my $dictionary = $config->dictionary;

  my (%line);

  #
  unless ($clanIO) {
    $clanIO = Bio::Pfam::ClanIO->new;
  }

  #Make sure that the DESC file is vaild to start off with
  $clanIO->parseCLANDESC("$clan/CLANDESC");

  my ($line) = 0;

  open( DESC, "$clan/CLANDESC" )
  || die "Can't open CLANDESC file for clan $clan:[$!]\n";
  while (<DESC>) {

    # If a free text line add to %lines
    if (/^RT   (\.*)$/) {
      $line{"$line"} = $1;
    }
    elsif (/^CC   (.*)$/) {
      $line{"$line"} = $1;
    }
    elsif (/^RC   (.*)$/) {
      $line{"$line"} = $1;
    }
    elsif (/^DC   (.*)$/) {
      $line{"$line"} = $1;
    }
    elsif (/^DE   (.*)$/) {
      $line{"$line"} = $1;
    }
    $line++;
  }
  close(DESC);

  my ( $bit, @line_number_array );

  # Now make temporary file
  open( TMP, "> tmp.$$" ) || die "Can't write to temp file\n";
  foreach $bit ( sort { $a <=> $b; } keys %line ) {

    # Make an array to store line numbers
    push( @line_number_array, $bit );
    print TMP $line{"$bit"}, "\n";
  }
  close TMP;

  # Start ispell session on file
  # system("ispell --dont-validate-words -W 0 -w 0123456789 -p$dictionary tmp.$$");
  my $cwd = cwd;
  my ( $dictionary_file, $path_to_dictionary, $suffix ) = fileparse( $dictionary );
  system("cd $path_to_dictionary && aspell --dont-validate-words -W 0 -p$dictionary check $cwd/tmp.$$") == 0
    or warn "WARNING: couldn't run spell checker: $!\n[cd $path_to_dictionary && aspell --dont-validate-words -W 0 -p$dictionary check $cwd/tmp.$$]";

  # Now need to put changes back into DESC file
  my ( %editedline, $line_number );
  open( TMP, "tmp.$$" ) || die "Can't open temp file tmp.$$\n";
  while (<TMP>) {
    if (/^(.*)$/) {
      $line_number = shift @line_number_array;
      $editedline{"$line_number"} = $1;
    }
    else {
      die "unrecognised line [$_]\n Serious error!\n";
    }
  }
  close(TMP);

  # Write out new DESC file
  open( TEMPDESC, ">$clan/CLANDESC.$$" )
  || die "Can't write to temp CLANDESC file for clan $clan\n";

  open( DESC, "$clan/CLANDESC" )
  || die "Can't open CLANDESC file for clan $clan\n";

  my ($prefix);
  $line = 0;

  while (<DESC>) {
    if ( $editedline{"$line"} ) {

      # Find if DE, RT or CC line
      if ( $_ =~ /^(\S+)/ ) {
        $prefix = $1;
      }
      else {
        die "unrecognised line [$_]\n";
      }

      # Write out line
      print TEMPDESC "$prefix   $editedline{$line}\n";
    }
    else {
      print TEMPDESC;
    }
    $line++;
  }
  close(DESC);
  close(TEMPDESC);

  # Move DESC across
  copy( "$clan/CLANDESC.$$", "$clan/CLANDESC" );

  # Add spell file to directory so programs can enforce spell check.
  open( S, ">touch $clan/spell" );
  close(S);

  # Clean up
  unlink("tmp.$$");

  #Now make sure that I have not screwed anyting up!
  $clanIO->parseCLANDESC("$clan/CLANDESC");
}

#-------------------------------------------------------------------------------

=head2 checkDESCSpell 

  Title    : 
  Usage    :  
  Function :
  Args     :
  Returns  :


=cut

sub checkDESCSpell {
  my ( $fam, $familyIO ) = @_;

  my $config     = Bio::Pfam::Config->new;
  my $dictionary = $config->dictionary;
  my (%line);

  unless ($familyIO) {
    $familyIO = Bio::Pfam::FamilyIO->new;
  }

  #Make sure that the DESC file is vaild to start off with
  $familyIO->parseDESC("$fam/DESC");

  my ($line) = 0;
  my $decc;

  open( DESC, "$fam/DESC" )
  || die "Can't open DESC file for family $fam:[$!]\n";
  while (<DESC>) {

    # If a free text line add to %lines
    if (/^RT   (\.*)$/) {
      $line{"$line"} = $1;
    }
    elsif (/^CC   (.*)$/) {
      my $cc_line = $1;
      $cc_line =~ s/ +/ /g;
      $cc_line =~ s/ +$//g;
      $line{"$line"} = $cc_line;
      $decc .= ' '. $cc_line;
    }
    elsif (/^RC   (.*)$/) {
      $line{"$line"} = $1;
    }
    elsif (/^DC   (.*)$/) {
      $line{"$line"} = $1;
    }
    elsif (/^ID   (.*)$/) {
      $decc .= "$1\n";
    }
    elsif (/^DE   (.*)$/) {
      my $de_line = $1;
      $de_line =~ s/ +/ /g;
      $de_line =~ s/ +$//g;
      $line{"$line"} = $de_line;
      $decc .= ' '. $de_line;
    }
    $line++;
  }
  close(DESC);

  my ( $bit, @line_number_array );

  # Now make temporary file
  open( TMP, "> tmp.$$" ) || die "Can't write to temp file\n";
  foreach $bit ( sort { $a <=> $b; } keys %line ) {

    # Make an array to store line numbers
    push( @line_number_array, $bit );
    print TMP $line{"$bit"}, "\n";
  }
  close TMP;

  # Start ispell session on file
  # system("ispell --dont-validate-words -W 0 -w 0123456789 -p$dictionary tmp.$$");
  my $cwd = cwd;
  my ( $dictionary_file, $path_to_dictionary, $suffix ) = fileparse( $dictionary );
  system("cd $path_to_dictionary && aspell --dont-validate-words -W 0 -p$dictionary check $cwd/tmp.$$") == 0
    or warn "WARNING: couldn't run spell checker: $!";

  # Now need to put changes back into DESC file
  my ( %editedline, $line_number );
  open( TMP, "tmp.$$" ) || die "Can't open temp file tmp.$$\n";
  while (<TMP>) {
    if (/^(.*)$/) {
      $line_number = shift @line_number_array;
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
  $line = 0;

  while (<DESC>) {
    if ( $editedline{"$line"} ) {

      # Find if DE, RT or CC line
      if ( $_ =~ /^(\S+)/ ) {
        $prefix = $1;
      }
      else {
        die "unrecognised line [$_]\n";
      }

      # Write out line
      print TEMPDESC "$prefix   $editedline{$line}\n";
    }
    else {
      print TEMPDESC;
    }
    $line++;
  }
  close(DESC);
  close(TEMPDESC);

  # Move DESC across
  copy( "$fam/DESC.$$", "$fam/DESC" );

  if ($decc =~ /(alcholol|analagous|aparrently|archael|assocaited|bacteriaa|batcer|betwwen|caracterized|chlorplasts|conatins|dependant|doamin|domainss|domian|enrty|fsmily|golbin|haeme|homolog |lenght|phague|portein|potein|protien|releated|repersents|represnts|reveales|siganture|specifacally|supress|variousely|This domains|neighbor)/) {
    my $mispell = $1;
    print "Common misspell word found.\n";
    print "The frequently misspelled '$1' is present within ID/DE/CC lines.\n";
    print "Please check spelling!\n";
    sleep 5;
  }

  my $open_rou = () = $decc =~ /\(/g;
  my $close_rou = () = $decc =~ /\)/g;
  my $open_squ = () = $decc =~ /\[/g;
  my $close_squ = () = $decc =~ /\]/g;
  my $open_cur = () = $decc =~ /\{/g;
  my $close_cur = () = $decc =~ /\}/g;

  if ($open_rou != $close_rou || $open_squ != $close_squ || $open_cur != $close_cur) {
    print "Unmatched brackets found.\n";
    if ($open_rou > $close_rou) {
      print "Opened round brackets '(' exists with no matching close round brackets.\n";
    }
    if ($open_rou < $close_rou) {
      print "Closed round brackets ')' exists with no matching open round brackets.\n";
    }
    if ($open_squ > $close_squ) {
      print "Opened square brackets '[' exists with no matching close square brackets.\n";
    }
    if ($open_squ < $close_squ) {
      print "Closed square brackets ']' exists with no matching open square brackets.\n";
    }
    if ($open_cur > $close_cur) {
      print "Opened curly brackets '{' exists with no matching close curly brackets.\n";
    }
    if ($open_cur < $close_cur) {
      print "Closed curly brackets '}' exists with no matching open curly brackets.\n";
    }
    print "Please check existing brackets!\n";
    sleep 5;
  }

  # Add spell file to directory so programs can enforce spell check.
  open( S, ">touch $fam/spell" );
  close(S);

  # Clean up
  unlink("tmp.$$");

  #Now make sure that I have not screwed anyting up!
  $familyIO->parseDESC("$fam/DESC");
}

sub checkClanMembership {
  my ( $memNew, $memOld ) = @_;

  my %count;
  foreach my $m ( @$memNew, @$memOld ) {
    $count{$m}++;
  }

  my ( @isect, @diff );
  foreach my $m ( keys %count ) {
    push @{ $count{$m} == 2 ? \@isect : \@diff }, $m;
  }

  my $error = 0;
  if ( scalar(@diff) ) {
    $error++;
    print STDERR "Detected the following differences between the memberships\n";
    my %newMem = map { $_ => 1 } @$memNew;
    foreach my $d (@diff) {
      print STDERR defined( $newMem{$d} )
      ? "$d is not in the old membership\n"
      : "$d is not in the new membership\n";
    }

  }

  if ($error) {
    return 0;    # failure
  }
  else {
    return 1;    # success
  }
}

sub checkSearchMethod {
  my ( $family, $famObj ) = @_;

  my $error = 0;
  my ($thisDBSize) = $famObj->DESC->SM =~ m/-Z\s+(\d+)/;
  if ( $thisDBSize != $CONFIG->dbsize ) {
    print STDERR
    "Search was performed with a dbsize of [$thisDBSize], expected ["
    . $CONFIG->dbsize . "]\n";
    $error = 1;
  }

  if ($error) {
    return 0;    # failure
  }
  else {
    return 1;    # success
  }
}


sub _compete {
  my( $seqAcc, $region, $overRegion, $pfamDB, $clanAcc) = @_; 

  my $skip = 0;

  #Is this family part of a clan? If it is, then get all the clan regions.
  if($clanAcc and $clanAcc =~ /CL\d{4}/){
    #Look up to see if there are any regions in the database with an E-value
    #less than this one!
    unless(defined($region->{evalue})) {
      die "No evalue for $seqAcc in scores file\n";
    }
    my $seqRegions = $pfamDB->findLowerEvalueRegion($seqAcc, $region, $clanAcc, $region->{evalue});  
    if($seqRegions > 0){
      $skip = 1;  
    }
  }

  unless( $skip == 1 ) {
    if($overRegion->{ali} eq 'FULL'){
      #Okay, now inspect the overlaping region.
      my $otherClanAcc;
      my $cRS = $pfamDB->getClanDataByPfam($overRegion->{family});
      if($cRS){
        $otherClanAcc = $cRS->clan_acc->clan_acc;
      }
      if($otherClanAcc and $otherClanAcc =~ /CL\d{4}/){
        #Need to get the E-value for the other sequence
        #Look up to see if there are any regions in the database with an E-value
        #less than this one!
        my $seqRegions = $pfamDB->findLowerEvalueRegion($seqAcc, $overRegion, $otherClanAcc);  
        if($seqRegions > 0){
          $skip = 1;  
        }
      }
    }
  }
  return($skip);
}

=head2 seedOnReferenceProteome

  Title    : seedOnReferenceProteome
  Usage    : seedOnReferenceProteome($famObj, $pfamDB) 
  Function : Checks whether all sequences in the SEED are in the current reference proteomes set
             in the database, populates $famObj->seedcheck
  Args     : Bio::Pfam::Family::PfamA, pfamDB object
  Returns  : nothing


=cut

sub seedOnReferenceProteome {

  my ($famObj, $pfamDB) = @_;

  unless ( $famObj->isa("Bio::Pfam::Family::PfamA") ) {
    confess("A Bio::Pfam::Family::PfamA object was not passed in");
  }

  #Query to check whether entry is in pfamseq
  my $dbh = $pfamDB->getSchema->storage->dbh;
  my $query = "select pfamseq_acc from pfamseq where pfamseq_acc = ? and seq_version=?";
  my $sth=$dbh->prepare($query);

  foreach my $seq ( $famObj->SEED->each_seq ) {
    $sth->execute($seq->id, $seq->version) or die "Couldn't execute statement ".$sth->errstr."\n";
    unless ($sth->fetchrow) {
      $famObj->seedcheck('pfamseqplus');
      return;
    }
  }
  
  #If we get to here then seed must be on pfamseq;
  $famObj->seedcheck('pfamseq');
}


=head2 checkReferencesAdded

  Title    : checkReferencesAdded
  Usage    : checkReferencesAdded($famObj) 
  Function : Checks whether all the references in the CC lines have been added to DESC file
  Args     : $famObj
  Returns  : 1 if there are errors, 0 if everthing is fine 


=cut

sub checkReferencesAdded {
  my $famObj = shift;
  
  my $error;
  my $cc = $famObj->DESC->CC;
  
  if ( $famObj->DESC->REFS and ref( $famObj->DESC->REFS ) eq 'ARRAY' ) {
    my %cc_refs;
    while ($cc =~ m{\[(\d+)\]}g) {
        $cc_refs{$1} = 1; 
    }    
    foreach my $r (map { $_->{RN} } @{ $famObj->DESC->REFS }) { 
        delete $cc_refs{$r};
    }    
    if(keys %cc_refs) {
      print STDERR "\n*** ERROR: found literature references in CC lines that have not been added to the DESC file: ".  join(",", keys(%cc_refs)) ." ***\n\n";    
      $error=1;
    }
  }
  else {
    my (@refs) = $cc =~ m{\[(\d+)\]}g;
    if (@refs) {
      print STDERR "\n*** ERROR: found literature references in CC lines that have not been added to the DESC file: " . join(",", @refs) . " ***\n\n";
      $error=1;
    }
  }

  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}


=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
