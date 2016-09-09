=head1 NAME

AcitveSite

=head1 SYNOPSIS

Contains subroutines required for active site prediction during the view process
This module is not required for active site prediction in pfam_scan.pl
The active site prediction code for pfam_scan.pl is in a subroutine in PfamScan.pm

=cut

package Bio::Pfam::ViewProcess::ActiveSite;

use strict;
use warnings;
use Carp;

=head2 new

 Title    : new
 Usage    : my $asp = new( -pfama_acc =>'PF00001', -database => 'PfamLiveDBManager object');
 Function : constructor for object
 Returns  : Object
 Args     : -pfamA_acc => PF00001, -database => 'PfamLiveDBManager object'

=cut

sub new {
  my ( $class, @args ) = @_; 

  my $self = {}; 
  bless ($self, $class);

  # accept both a hash and a hash ref
  my $args = ( ref $args[0] eq 'HASH' ) ? shift @args : { @args };


  croak "FATAL - no pfama_acc passed\n" unless( $args->{-pfama_acc} || $args->{-PFAMA_ACC});
  croak "FATAL - no database object passed\n" unless( $args->{-database} || $args->{-DATABASE});

  $self->{pfama_acc} = $args->{-pfama_acc} || $args->{-PFAMA_ACC};
  $self->{database} = $args->{-database} || $args->{-DATABASE};

  return $self;
}



=head2 active_site_prediction

 Title    : active_site_prediction
 Usage    : $asp->active_site_prediction($alignment)
 Function : Finds hmm locations of active site residues in the alignnment
            Uplodad active site patterns to _active_site_hmm_postions table in db
            Uploads predicted active sites to pfamseq_markup table in db
            Adds display line to aligment to indicate the presence of active site residues
 Returns  : Alignment marked up with active site residues
 Args     : Bio::Pfam::AlignPfam object

=cut

sub active_site_prediction {
  my ($self, $alignment) = @_; 

  die "No alignment passed" unless($alignment);
  $self->{alignment}=$alignment;


  #Delete old data, if any
  $self->{database}->getSchema->resultset('ActiveSiteHmmPosition')->search( { pfamA_acc => $self->{pfama_acc}} )->delete;
  my $dbh = $self->{database}->getSchema->storage->dbh;
  $dbh->do("delete from pfamseq_markup where pfamA_acc='".$self->{pfama_acc}."';");

  #Query db for active sites in the family
  $self->_get_active_sites_from_db();

  if($self->{_as_data}) {

    #Extract hmm positions of active site residues
    $self->_extract_act_site_positions();

    if(keys %{$self->{_all_patterns}}) {  #%_all_patterns may be empty for families that contain a nested family that has active site residues

      #Remove subpatterns
      $self->_remove_subpatterns();

      #Upload patterns to db
      $self->_upload_patterns();

      #Predict active sites in alignment
      #    $self->_pred_act_sites();

      #Upload Pfam predicted active sites to db
      #$self->_upload_pred_act_sites();

      #Add display line to alignment
      #$alignment=$self->active_site_display_line();
    }
  }

  return($alignment);
}  

=head2 _get_active_sites_from_db

 Title    : _get_active_sites_from_db
 Usage    : $self->_get_active_sites_from_db()
 Function : Extracts experimentally determined and UniProt predicted active sites for family from pfamseq_markup table
            Populates $self->{_experimental_as}, $self->{_uniprot_predicted_as}, $self->{_as_positions}, $self->{_swissprot}
 Returns  : Nothing
 Args     : None

=cut

sub _get_active_sites_from_db {

  my ($self) = @_;

  #Query to pull out sequences in the family that contains active site residues
  my @as_data = $self->{database}->getSchema->resultset('Pfamseq')
  ->search( { 'pfam_a_reg_full_significants.pfama_acc' => $self->{pfama_acc}, auto_markup => [qw(1 3)], in_full => 1 }, #auto_markup = 1 corresponds to experimentally determined active sites, auto_markup =3 is UniProt predicted active sites
    { select    => [ qw (pfamseq_acc pfamseq_markups.residue pfamseq_markups.auto_markup pfam_a_reg_full_significants.seq_start pfam_a_reg_full_significants.seq_end swissprot ) ],
      as        => [qw(pfamseq_acc residue auto_markup seq_start seq_end swissprot)],
      join      => [ qw ( pfam_a_reg_full_significants pfamseq_markups )] }); 

  foreach my $as (@as_data) {
    my ($pfamseq_acc, $residue_number, $start, $end, $auto_markup, $swissprot) = ($as->pfamseq_acc, $as->get_column('residue'), $as->get_column('seq_start'), $as->get_column('seq_end'), $as->get_column('auto_markup'), $as->swissprot);

    if($residue_number >= $start and $residue_number <= $end) {  #If residue falls in the family
      $self->{_as_data}=1;
      if($auto_markup ==3) { #Uniprot predicted active site
        $self->{_uniprot_predicted_as}->{$pfamseq_acc}->{$residue_number}=1;
      }   
      else {  #Experimental
        $self->{_experimental_as}->{$pfamseq_acc}->{$residue_number}=1;
        $self->{_as_positions}->{$pfamseq_acc}->{$residue_number}=1; #This hash is used later when extracting the active site hmm positions

        #Store which sequences are in swissprot - use this later to choose swissprot seq over non swissprot ones
        $self->{_swissprot}->{$pfamseq_acc}=1 if($swissprot);

      }
    }
  }
}


=head2 _extract_act_site_poistions

 Title    : _extract_act_site_poistions
 Usage    : $self->_extract_act_site_poistions
 Function : Extracts active site model positions and residues from alignment in $self->{alignment}
            Populates $self->{_all_patterns}
 Returns  : Nothing
 Args     : None

=cut


sub _extract_act_site_positions {

  my ($self) = @_;

  my @as_sequences;


  foreach my $seq ( $self->{alignment}->each_seq() ) {
    my @sequence=split(//, $seq->seq);
    my $acc=$seq->id;

    my $match_state=0;
    my $residue_number = $seq->start;
    my $pattern;

    #Loop through the residues in the sequence, and store patterns
    #Active site residues in insert states will not be captured
    foreach my $aa (@sequence) {
      if ($aa =~ /[A-Z]/) {  #Uppercase residues are match states
        $match_state++;
        if(exists($self->{_as_positions}->{$acc}->{$residue_number})) {
          $pattern.=" " if($pattern);
          $pattern.="$aa"."$match_state";
          delete $self->{_as_positions}->{$acc}->{$residue_number};
        }
        unless(keys %{$self->{_as_positions}->{$acc}}) {
          last;
        }
        $residue_number++;
      }
      elsif($aa eq "-") { #Deletion in a match state position
        $match_state++;
      }
      elsif ($aa =~ /[a-z]/) { #Lowercase residues are not match states
        $residue_number++;
      }
      elsif($aa eq ".") { #Residue is not a match state
      }
      else {
        die "Unrecognised character [$aa] in ".$seq->seq."\n";
      }
    }
    if($pattern) { #This will be empty if the active site residue(s) are in a nested domain, or active site residue(s) are in insert state
      $self->{_all_patterns}->{$pattern}->{$acc}=$seq->seq; 
    }
  }
}


=head2 _remove_subpatterns

Title    : _remove_subpatterns
Usage    : $self->_remove_subpatterns
Function : Looks for subpatterns in the patterns in _all_patterns hash
           Populates $self->{_patterns} hash with all non-subpattern patterns from $self->{_all_patterns} hash
Returns  : Nothing
Args     : None

=cut

sub _remove_subpatterns {

  my ($self) = shift;

  foreach my $as_pattern (sort { length($b) <=> length($a) } keys %{$self->{_all_patterns}}) { #Sort patterns by decreasing length
    foreach my $asp (keys %{$self->{_patterns}}) { #_patterns contains patterns from _all_patterns_hash that have already been checked for subpatterns

      #Put active site positions and residues into a hash
      my %asp;
      pos($asp)=0;
      while($asp  =~ m/(\S)(\d+)/g) { #$1=residue $2=hmm_position
        $asp{$2}=$1;
      }   
      
      #Check this isn't a subpattern of a bigger pattern already found on the sequence
      my $different;
      while($as_pattern  =~ m/(\S)(\d+)/g) {
        unless(exists($asp{$2})) {
          $different=1;
          last;
        }    
      }   
      if($different) {  #It's not a subpattern 
        next;
      }   

      #If we get here, the pattern is potentially a subpattern as hmm positions of active site residues are the same as in a bigger patter, need to check residues are the same as in the bigger pattern 
      foreach my $acc (keys %{$self->{_all_patterns}->{$as_pattern}}) { #Go through every sequence with this pattern and check if it contains all residues from bigger pattern
        my %asp2 = %asp;

        my @sequence = split(//, $self->{_all_patterns}->{$as_pattern}->{$acc});
        my $match_state=0;
        my $matched;
        for(my $i=0; $i<@sequence; $i++) {
          if ($sequence[$i] =~ /[A-Z]/) {  #Uppercase residues are match states
            $match_state++;
            if(exists($asp2{$match_state})) {
              if($asp2{$match_state} eq $sequence[$i]) {
                delete $asp2{$match_state};
                unless(keys %asp2) {
                  $matched=1; #It's matched every residue in pattern
                  last;
                }   
              }   
              else { 
                last;
              }   
            }   
          }   
          elsif($sequence[$i] eq "-") { #Deletion in a match state position
            $match_state++;
            if(exists($asp2{$match_state})) { #Doesn't match this residue therefore not a subpattern
              last;
            }   
          }   
          elsif ($sequence[$i] =~ /[a-z]/) { #Lowercase residues are not match states
          }   
          elsif($sequence[$i] eq ".") { #Residue is not a match state
          }   
          else {
            die "Unrecognised character $sequence[$i] in @sequence";
          }   
        }   
        if($matched) {
          delete $self->{_all_patterns}->{$as_pattern}->{$acc}; #Delete because it's a sub pattern
        }   
      }   
    }


    #Choose a swissprot seq as the example seq for this patttern
    my $swissprot;
    foreach my $acc (keys %{$self->{_all_patterns}->{$as_pattern}}) {
      if(exists($self->{_swissprot}->{$acc})) {
        $self->{_patterns}->{$as_pattern}=$acc;
        $swissprot=1;
        last;
      }
    }

    #If there isn't a swissprot seq with this pattern, a take non-swissprot seq as the example seq
    unless($swissprot) {
      foreach my $acc (keys %{$self->{_all_patterns}->{$as_pattern}}) {
        $self->{_patterns}->{$as_pattern}=$acc;
        last;
      }
    }
  }
}

=head2 _upload_patterns

Title    : _upload_patterns
Usage    : $self->_upload_patterns
Function : Uploads active site model positions and residues to _active_site_hmm_positions table in db
           Populates $self->{_as_hmm_positions} with patterns in decreasing length order
Returns  : Nothing
Args     : None

=cut

sub _upload_patterns {
  my ($self) = shift;

  foreach my $pattern (sort { length($b) <=> length($a) } keys %{$self->{_patterns}}) { #Sort the patterns according to length
    push(@{$self->{_as_hmm_positions}}, $pattern);
    while($pattern  =~ m/(\S)(\d+)/g) { #$1=residue $2=hmm_position
      my ($residue, $hmm_position) = ($1, $2);

      $self->{database}->getSchema->resultset('ActiveSiteHmmPosition')->create( {
          pfama_acc       => $self->{pfama_acc},
          pfamseq_acc     => $self->{_patterns}->{$pattern},
          hmm_position    => $hmm_position,
          residue         => $residue
        }); 
    }   
  }
}


=head2 _pred_act_sites

Title    : _pred_act_sites
Usage    : $self->_pred_act_sites
Function : Predict active site residues in the alignment and populate  $self->{_pfam_predicted_as} hash
Returns  : Nothing
Args     : None

=cut

sub _pred_act_sites {
  my ($self) = shift;

  foreach my $seq ( $self->{alignment}->each_seq() ) {
    my @seq=split(//, $seq->seq);
    my $acc=$seq->id;

    my $subpattern;
    my %matched_patterns; #Store matched hmm positions in this hash. Use to see if future patterns are subpatterns 

    foreach my $as_pattern (@{$self->{_as_hmm_positions}}) { #$as_pattern contains active site residues from a single seq, eg 'S23 T34 S56'

      #Check this isn't a subpattern of a bigger pattern already found on the sequence
      #Patterns were added to @{$self->{_as_hmm_positions}} in order of size (longest pattern first)
      foreach my $asp (keys %matched_patterns) {
        my $different=0;
        while($as_pattern  =~ m/(\S)(\d+)/g) {  #eg 'S23 T34 S56'
          my ($residue, $hmm_position) = ($1, $2);
          if(exists($matched_patterns{$asp}{$hmm_position})) {
          }
          else {
            $different=1;
            last;
          }
        }
        unless($different) { #Unless it contains a different pattern to those already added, it's a subpattern
          $subpattern=1;
          last;
        }
      }
      if($subpattern) {
        next;
      }

      my $match;

      #Set up the counters so we know which position we are at
      my $hmm_counter=0;
      my $residue_counter=$seq->start();


      #Now check to see if pattern is in this sequence 
      #Put pattern into a hash
      my %as_positions;
      pos($as_pattern) = 0; #Have to reset to 0 as already performed reg ex on it, otherwise it may start looking from the position of the last match
      while($as_pattern  =~ m/(\S)(\d+)/g) {  #$as_pattern contains active site residues from a single seq, eg 'S23 T34 S56'
        my ($residue, $hmm_position) = ($1, $2);
        $as_positions{$hmm_position}=$residue;
      }

      #Look for the active site pattern in the sequence
      #Active site residues will be in match postions only
      my %pred_as;
      foreach my $aa (@seq) {
        if ($aa =~ /[A-Z]/) {  #Uppercase residues are match states
          $hmm_counter++;
          if(exists($as_positions{$hmm_counter})) { #It's an active site residue position
            if($aa eq $as_positions{$hmm_counter}) { #Does it have the correct aa at that position
              $pred_as{$residue_counter}=1;
              $matched_patterns{$as_pattern}{$hmm_counter}=1; 
              delete $as_positions{$hmm_counter};
              last unless(keys %as_positions);
            }
            else {
              last;
            }
          }
          $residue_counter++;
        }
        elsif($aa eq "-") { #Deletion in a match state position
          $hmm_counter++;
          if(exists($as_positions{$hmm_counter})) {
            last;
          }
        }
        elsif ($aa =~ /[a-z]/)  { #Lowercase residues are not match state positions
          if(exists($as_positions{$hmm_counter})) {
            last;
          } 
          $residue_counter++;
        }
        elsif($aa eq ".") { #'.' is not a match state position
          if(exists($as_positions{$hmm_counter})) {
            last;
          } 
        }     
        else {
          die "Unrecognised character [$aa] in $seq\n";
        }
      }

      #If it matched all residues in the pattern, add it, if not delete the pattern from the matched patterns hash
      if(keys %as_positions) {  #This hash will be empty if all positions were matched
        delete $matched_patterns{$as_pattern};
      }
      else {
        foreach my $position (keys %pred_as) {
          unless(exists($self->{_experimental_as}->{$acc}->{$position})) {  #No need to predict if it has a known active site residue
            $self->{_pfam_predicted_as}->{$acc}->{$position}=$self->{_patterns}->{$as_pattern}; #$self->{_patterns}->{$as_pattern} contains the accession of the sequence from which the active site residues were transferred
          }
        }
      }
    }
  }  
}

=head2 _upload_pred_act_sites

Title    : _upload_pred_act_sites
Usage    : $self->_upload_pred_act_sites
Function : Uploads predicted active site residue poistions to pfamseq_markup table
Returns  : Nothing
Args     : None

=cut

sub _upload_pred_act_sites {
  my ($self) = shift;

  foreach my $acc (keys %{$self->{_pfam_predicted_as}}) {
    foreach my $residue_position (keys %{$self->{_pfam_predicted_as}->{$acc}}) {
      my $annotation="Similarity to ".$self->{_pfam_predicted_as}->{$acc}->{$residue_position}; #$self->{_pfam_predicted_as}->{$acc}->{$residue_position} contains acc from which the accession of the sequence from which the active site residues were transferred

      $self->{database}->getSchema->resultset('PfamseqMarkup')->create( {
          pfamseq_acc => $acc,
          auto_markup => "2",   #auto_markup=2 is for pfam predicted active site residues
          residue => $residue_position,
          annotation => $annotation,
          pfama_acc => $self->{pfama_acc} });

    }   
  }
}


=head2 active_site_display_line

Title    : active_site_display_line
Usage    : $self->active_site_display_line
Function : Adds 'display line' (e.g. .....*....) to show where active site residues are located
           by adding a Bio::Pfam::OtherRegion object to the sequence
Returns  : Alignment with display lines containing active site residues
Args     : If no alignment is passed, $self->{alignment} is marked up with active site residues
           If alignment is passed, this is marked up with active site residues

=cut

sub active_site_display_line {
  my ($self, $aln) = @_;

  unless($aln) {
    $aln=$self->{alignment};
  }

  foreach my $seq ( $aln->each_seq ) {

    my $expAS_string = "." x length($seq->seq);
    my $pfamAS_string = $expAS_string;
    my $uniprotAS_string = $expAS_string;

    my ($expAS, $pfamAS, $uniprotAS);

    #Display lines for experimental active site residues
    foreach my $residue_number (keys %{$self->{_experimental_as}->{$seq->id}}) {
      if($residue_number >= $seq->start and $residue_number <= $seq->end) {
        my $col = $aln->column_from_residue_number($seq->id, $residue_number);
        substr($expAS_string, $col-1, 1, "*");
        $expAS = 1;
      }
    }

    #Display lines for pfam predicted active site residues
    foreach my $residue_number (keys %{$self->{_pfam_predicted_as}->{$seq->id}}) {
      if($residue_number >= $seq->start and $residue_number <= $seq->end) {
        my $col = $aln->column_from_residue_number($seq->id, $residue_number);
        substr($pfamAS_string, $col-1, 1, "*");
        $pfamAS = 1;
      }
    }

    #Display lines for uniprot predicted active site residues
    foreach my $residue_number (keys %{$self->{_uniprot_predicted_as}->{$seq->id}}) {
      if($residue_number >= $seq->start and $residue_number <= $seq->end) {
        next if(exists($self->{_pfam_predicted_as}->{$seq->id}->{$residue_number}));
        my $col = $aln->column_from_residue_number($seq->id, $residue_number);
        substr($uniprotAS_string, $col-1, 1, "*");
        $uniprotAS = 1;
      }  
    }

    #Add display lines to the sequence object
    if($expAS) {
      my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
        '-from'   => 1,
        '-to'     => length($seq->seq),
        '-type'   => "active_site",
        '-display'=> $expAS_string,
        '-source' => 'Pfam');
      $seq->active_site($asObj);
    }
    if($pfamAS) {
      my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
        '-from'   => 1,
        '-to'     => length($seq->seq),
        '-type'   => "pfam_pred_active_site",
        '-display'=> $pfamAS_string,
        '-source' => 'Pfam');
      $seq->pfam_pred_active_site($asObj);
    }
    if($uniprotAS) {
      my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
        '-from'   => 1,
        '-to'     => length($seq->seq),
        '-type'   => "sprot_pred_active_site",
        '-display'=> $uniprotAS_string,
        '-source' => 'Pfam');
      $seq->sprot_pred_active_site($asObj);
    }
  }
  return($aln);
}

1;
