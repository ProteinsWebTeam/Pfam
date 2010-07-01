#
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
use Carp;
use Data::Dumper;

my $CONFIG = Bio::Pfam::Config->new;

=head2 passesAllFormatChecks

 Title    : passesAllFormatChecks
 Usage    : &PfamQC::passesAllFormatChecks(familyObject, directoryName, inNewFlag)
 Function : Runs all format check routines from this module,
            and warns about any errors
 Returns  : 1 if all is OK, 0 if not
 Args     : Bio::Pfam::Family object, directory containing the family, true flag if family is new

=cut

sub passesAllFormatChecks {
  my ( $famObj, $family, $isNew ) = @_;

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

  unless ( &checkFamilyFiles( $family, $famObj ) ) {
    $error = 1;
  }

  if ($isNew) {

    # Check name is unused
    my $clash = &name_clashes($family);
    if ($clash) {
      $error = 1;
      warn "$family: your family identifier clashes with existing id $clash\n";
    }
  }
  else {
    print STDERR "check acc vs name\n";

    #Check that the accession and name match with stored copy
    #if ( &check_acc_vs_name($family) ) {
    #   $error = 1;
    # }
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
    if ( !&mulFormatIsOK( $aln, $family ) ) {
      warn "$family: Bad format for $aln!\n";
      $error = 1;
    }
  }

  if ( !&moreInSEEDthanALIGN($famObj) ) {
    warn "$family: You have more sequences in SEED than ALIGN\n";
    $error = 1;
  }

  if ( !&compareAlignToScores($famObj) ) {
    warn
"$family: You have a different number of matches in scores and ALIGN files\n";
    $error = 1;
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

#=head2 name_clashes
#
# Title    : name_clashes
# Usage    : &PfamQC::name_clashes("family_id","ignore")
# Function : Checks to see if the family name has been used before
#            in case independant fashion
# Returns  : id of any clashing family name or 0 if no clashes
# Args     : family id, optional 2nd id to ignore (when we want to use
#	    pfmove to change the case of an id for example)
#
#=cut
#
#sub name_clashes {
#
#  # returns 0 if all is OK, id of clashing family if all is not OK
#  my $family    = shift;
#  my $ignore    = shift;
#  my $lc_family = $family;
#  $lc_family =~ tr/A-Z/a-z/;
#
#  my $db       = Bio::Pfam->default_db();
#  my @fam_list = $db->get_allacc();
#
#  foreach my $acc (@fam_list) {
#    my $id = $db->acc2id("$acc");
#    if ($ignore) {
#      next if ( $id eq $ignore );
#    }
#    my $lc_id = $id;
#    $lc_id =~ tr/A-Z/a-z/;
#    if ( $lc_family eq $lc_id ) {
#      return $id;
#    }
#  }
#  return 0;
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

  if ( $famObj->scores->numRegions != $famObj->ALIGN->no_sequences ) {

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
  my $famObj = shift;
  if ( $famObj->SEED->no_sequences > $famObj->ALIGN->no_sequences ) {
    return 0;
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

  if ( $family =~ /^[A-Za-z0-9_\-]{1,15}$/ ) {
    return 1;
  }
  else {
    warn "Your Pfam name contains invalid characters and/or is too long\n";
    return 0;
  }
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
    print STDERR "\n--- SEED alignment is ragged ---\n\n";
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
 Usage    : PfamQC::sequenceChecher($familyObject)
 Function : Checks the sequences in the SEED and ALIGN are valid.
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : A Bio::Pfam::Family::PfamA object 

=cut

sub sequenceChecker {
  my ( $family, $famObj ) = @_;

  unless ($famObj) {
    confess("Undefined family object passed");
  }

  unless ( $famObj->isa("Bio::Pfam::Family::PfamA") ) {
    confess("A Bio::Pfam::Family::PfamA object was not passed in");
  }

  my $error = 0;
  my %allseqs;
  my $count = 0;

  foreach my $aln (qw(SEED ALIGN)) {
    foreach my $seq ( $famObj->$aln->each_seq ) {
      $count++;

      my $str_ali = uc( $seq->seq() );
      $str_ali =~ s/[.-]//g;
      my $seqName = $seq->id;
      if($seq->version){
        $seqName .= ".".$seq->version;
      }
      Bio::Pfam::SeqFetch::addSeqToVerify( $seqName,
        $seq->start, $seq->end, $str_ali, \%allseqs );
    }
  }

  my $verified_seq = Bio::Pfam::SeqFetch::verifySeqs( \%allseqs,
    $CONFIG->pfamseqLoc . "/pfamseq" );

  if ( $verified_seq == $count ) {
    print STDERR "\n--- All sequences match the pfamseq database ---\n\n";
  }
  else {
    print STDERR "\n--- Mismatch of sequences in family --- \n\n";
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

=head2 family_overlaps_with_db

 Title    : family_overlaps_with_db
 Usage    : &PfamQC::family_overlaps_with_db("family_id", \@ignore, 1)
 Function : Checks that the alignment contains no overlaps to data in 
            the RDB, and prints any overlaps to STDERR
 Returns  : number of overlaps
 Args     : family_id, reference to an array of families to ignore, 
            optional flag to calculate end points

=cut

sub family_overlaps_with_db {
  my ( $family, $ignore_ref, $endpoints_opt, $pfamDB, $famObj, $compete ) = @_;
  my ( %ignore, @overlaps );

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("$family: Did not get a family object passed in.....\n");
  }

  #This could be nested in another domain, so we need to check!
  my $nestedRef = $pfamDB->getNestedDomain( $famObj->DESC->AC );

  if ($nestedRef) {
    foreach my $n (@$nestedRef) {
      $$ignore_ref{$n}++;

      my $clan = $pfamDB->getClanDataByPfam($n);
      if ( $clan and $clan->auto_clan->clan_acc ) {
        my $clanMem = $pfamDB->getClanMembership( $clan->auto_clan->clan_acc );
        foreach my $fam (@$clanMem) {
          $$ignore_ref{ $fam->auto_pfama->pfama_acc }++;
        }
      }
    }
  }

  if ( $famObj->DESC->NESTS ) {
    foreach my $nesting ( @{ $famObj->DESC->NESTS } ) {
      $$ignore_ref{ $nesting->{dom} }++;
      my $clan = $pfamDB->getClanDataByPfam( $nesting->{dom} );
      if ( $clan and $clan->auto_clan->clan_acc ) {
        my $clanMem = $pfamDB->getClanMembership( $clan->auto_clan->clan_acc );
        foreach my $fam (@$clanMem) {
          $$ignore_ref{ $fam->auto_pfama->pfama_acc }++;
        }
      }
    }

  }

  if ( $famObj->DESC->CL ) {

    #Okay, we have a family that is part of a clan
    my $clanMem = $pfamDB->getClanMembership( $famObj->DESC->CL );
    foreach my $fam (@$clanMem) {
      $$ignore_ref{ $fam->auto_pfama->pfama_acc }++;
      my $nestedRef =
        $pfamDB->getNestedDomain( $fam->auto_pfama->pfama_acc );

      if ($nestedRef) {
        foreach my $n (@$nestedRef) {
          $$ignore_ref{$n}++;

          my $clan = $pfamDB->getClanDataByPfam($n);
          if ( $clan and $clan->auto_clan->clan_acc ) {
            my $clanMem = $pfamDB->getClanMembership( $clan->auto_clan->clan_acc );
            foreach my $fam (@$clanMem) {
              $$ignore_ref{ $fam->auto_pfama->pfama_acc }++;
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

  #Now pull out all of the regions
  my %regions;

  #First from the SEED
  foreach my $seq ( $famObj->SEED->each_seq ) {
    my $id;
    if ( $seq->id =~ /(\S+)\.\d+/ ) {
      $id = $1;
    }
    else {
      $id = $seq->id;
    }

    #print $seq->id."\t".$id."\n";

    push @{ $regions{$id} },
      {
      from      => $seq->start,
      to        => $seq->end,
      family    => ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
      ali       => 'SEED',
      family_id => ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" )
      };
  }

#Then for the full, use the scores file as this contains tha alignment co-ordinates.
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
        from      => $fullReg->{aliStart},
        to        => $fullReg->{aliEnd},
        family    => ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
        family_id => ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" ),
        ali       => 'FULL'
        };
    }
  }

  my %overlaps;
  $pfamDB->getOverlapingFullPfamRegions( \%regions, \%overlaps );
  $pfamDB->getOverlapingSeedPfamRegions( \%regions, \%overlaps );


  my $pfamoutRegions;
  if($compete){
    $pfamoutRegions = $famObj->PFAMOUT->eachHMMSeq;  
  }

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
        next if ( $$ignore_ref{ $overRegion->{family} } );
        if($region->{ali} eq 'FULL' and $compete){
         if(_compete($seqAcc, $region, $overRegion, $pfamDB, $pfamoutRegions, $famObj->DESC->CL)){
          next REGION;  
         }
        }
        
        my $line =
            "Sequence [" 
          . $seqAcc
          . "] overlap "
          . $region->{family_id} . " "
          . $region->{family} . "/"
          . $region->{from} . "-"
          . $region->{to} . " "
          . $region->{ali}
          . " with "
          . $overRegion->{family_id} . " "
          . $overRegion->{family} . "/"
          . $overRegion->{from} . "-"
          . $overRegion->{to} . " "
          . $overRegion->{ali} . "\n";

        next if ( $seen{$line} );
        $seen{$line}++;
        $numOverlaps++;
        print STDERR $line;
        print $LOG $line if $LOG;
      }
    }
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
          "Internal overlap of %s/%d-%d to %s/%d-%d\n",
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

  my ( $oldSeqs, $newSeqs );
  foreach my $aln (qw(SEED ALIGN)) {
    foreach my $seq ( $newFamObj->$aln->each_seq ) {
      $newSeqs->{ $seq->id }++;
    }
    foreach my $seq ( $oldFamObj->$aln->each_seq ) {
      $oldSeqs->{ $seq->id }++;
    }
  }

  ###########################################
  # Find missing sequences in edited family #
  ###########################################
  my $lost  = 0;
  my $extra = 0;

  # Put missing sequences into a missing file in directory
  open( MISSING, "> $family/missing" )
    || die "Can't write to file $family/missing\n";
  foreach my $element ( keys %{$oldSeqs} ) {
    if ( !$newSeqs->{$element} ) {
      print MISSING "$element not found\n";

      # Add element to missing hash
      $lost++;
    }
  }
  close(MISSING);

  ##################################
  # Find how many sequences gained #
  ##################################
  open( FOUND, "> $family/found" )
    || die "Can't write to file $family/found\n";
  foreach my $element ( keys %{$newSeqs} ) {
    if ( !$oldSeqs->{$element} ) {
      print FOUND "$element found\n";
      $extra++;
    }
  }
  close(FOUND);
  print "Lost $lost. Found $extra.\n";

  my $error;
  if ($lost) {
    print STDERR "\n--- Missing members compared to svn copy --- \n\n";
    $error = 1;
  }
  else {
    print STDERR "\n--- No missing sequences compared to svn copy --- \n\n";
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
            file.  Also makes sure there is more than one sequence in the SEED.
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

  if ( $famObj->SEED->no_sequences <= 1 ) {
    print STDERR
"\n--- Only 1 sequence in the seed.  Your seed should contain more than one sequence! ---\n\n";
    $error = 1;
  }

  open( FRAG, ">$family/seedFrag" )
    or confess("Could not open $family/seedFrag:[$!]");

  if ( keys %bad_seq ) {
    $error = 1;
    print STDERR "\n--- Your seed contains fragments! ---\n\n";
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
    if ( -M "$family/PFAMOUT" < -M "$family/ALIGN" ) {
      warn
"$family: Your PFAMOUT [$family/PFAMOUT] is younger than your full alignment [$family/ALIGN].\n";
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
  system("ispell -W 0 -w 0123456789 -p$dictionary tmp.$$");

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

  #
  unless ($familyIO) {
    $familyIO = Bio::Pfam::FamilyIO->new;
  }

  #Make sure that the DESC file is vaild to start off with
  $familyIO->parseDESC("$fam/DESC");

  my ($line) = 0;

  open( DESC, "$fam/DESC" )
    || die "Can't open DESC file for family $fam:[$!]\n";
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
  system("ispell -W 0 -w 0123456789 -p$dictionary tmp.$$");

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
  my( $seqAcc, $region, $overRegion, $pfamDB, $pfamoutRegions, $clanAcc) = @_; 
  
  my $skip = 0;
  my $thisEvalue;
  foreach my $seq (@$pfamoutRegions){
    #Find the sequence
    
    my ($acc, $version) = $seq->name =~ /(\S+)\.(\d+)/;
    if($acc eq $seqAcc){
      print STDERR "Comparing $seqAcc to ".$seq->name."\n";
      #Now find the region - we are screwed if it has ED lines
      foreach my $u ( @{ $seq->hmmUnits } ) {
          print STDERR  $u->seqFrom." ".$region->{from}." ".$u->seqTo." ".$region->{to}."\n";
          if($u->seqFrom == $region->{from} and $u->seqTo == $region->{to}){
            #Right, we have found the overlaping regions  
            $thisEvalue = $u->evalue;
            print STDERR $thisEvalue;
            last;
          }
      }
      last;
    }      
  }
  
  unless($thisEvalue){
    die "Could not find sequence region!\n";  
  }
  
  #Is this family part of a clan? If it is, then get all the clan regions.
  if($clanAcc and $clanAcc =~ /CL\d{4}/){
    #Look up to see if there are any regions in the database with an E-value
    #less than this one!
    my $seqRegions = $pfamDB->findLowerEvalueRegion($seqAcc, $region, $clanAcc, $thisEvalue);  
    if($seqRegions > 0){
      $skip = 1;  
    }
  }
  
  unless( $skip == 1 ) {
    if($overRegion->{align} eq 'FULL'){
      #Okay, now inspect the overlaping region.
    my $otherClanAcc;
    my $cRS = $pfamDB->getClanDataByPfam($overRegion->{family});
    if($cRS){
      $otherClanAcc = $cRS->auto_clan->clan_acc;
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

