
=head1 NAME

Bio::Rfam::FamilyIO - a module that enables access to the Rfam SVN

=cut

package Bio::Rfam::FamilyIO;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: FamilyIO.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 24, 2013 9:33:51 PM

=cut

#-------------------------------------------------------------------------------

use strict;
use warnings;
use File::Temp;
use Text::Wrap;
use Carp;
use Data::Printer;
use Cwd 'abs_path';
use Moose;

use Bio::Rfam::Config;
use Bio::Rfam::Family;
use Bio::Rfam::Family::DESC;
use Bio::Rfam::Family::CM;
use Bio::Rfam::Family::Scores;

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

sub new {
  my ($caller) = @_;

  my $class = ref($caller) || $caller;
  my $self;

  $self->{config} = Bio::Rfam::Config->new;

  return bless( $self, $class );
}

sub loadRfamFromLocalFile {
  my ( $self, $family, $dir, $source ) = @_;

  unless ( -d "$dir/$family" ) {
    confess("Could not find family directory $dir/$family");
  }

  my %params;
  foreach my $f ( @{ $self->{config}->mandatoryFiles } ) {
    unless ( -e "$dir/$family/$f" ) {
      confess("Could not find $dir/$family/$f\n");
    }
    my $fh;

    #print "Opening $dir/$family/$f\n";
    open( $fh, "$dir/$family/$f" )
      or confess("Could not open $dir/$family/$f:[$!]");
    $params{$f} = $fh;
  }

  if ($source) {
    $params{'source'} = $source;
  }
  else {
    $params{'source'} = 'file';
  }

  my $params = {
    'SEED' => {
      fileLocation => "$dir/$family/SEED",
      aliType      => 'seed'
    },
    'DESC'   => $self->parseDESC("$dir/$family/DESC"),
    'CM'     => $self->parseCM("$dir/$family/CM"),
    'SCORES' => $self->parseScores("$dir/$family/SCORES"),
    'TBLOUT' => { fileLocation => "$dir/$family/TBLOUT" }
  };

  #Use Moose to coerce these through!
  my $famObj = Bio::Rfam::Family->new($params);

  return ($famObj);
}

sub loadRfamFromRDB {
  my ( $self, $family ) = @_;
  
  my $rfamdb = $self->{config}->rfamlive;
  my $row = $rfamdb->resultset('Family')->find( { rfam_acc => $family }, 
                                                { prefetch => 'family_files' });
  
  my $files = $row->family_files->first;
  my $regions = $rfamdb->resultset('FullRegion')->allRegions($family);
  #Do you have specific storage.
  my $dir = '/tmp';
  
  #File::Temp->newdir( 'CLEANUP' => 1 );
  if(! -d "$dir/$family"){
    mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");
  }
  open(S, '>', "$dir/$family/SEED") 
    or croak("Could not open $dir/$family/SEED:[$!]\n");
  print S $files->unzipped_seed;
  close S;
  
  open(C, '>', "$dir/$family/CM") 
    or croak("Could not open $dir/$family/CM:[$!]\n");
  print C $files->unzipped_cm;
  close C;
  
  open(T, '>', "$dir/$family/TBLOUT") 
    or croak("Could not open $dir/$family/TBLOUT:[$!]\n");
  print T $files->unzipped_seed;
  close T;
  
  
  
  my $params = {
    source => 'database',
    'SEED' => {
      fileLocation => "$dir/$family/SEED",
      aliType      => 'seed'
    },
    'DESC'   => $self->parseDESC("$dir/$family/DESC"),
    'CM'     => $self->parseCM("$dir/$family/CM"),
    'SCORES' => { numRegions => scalar(@$regions),
                  regions    => $regions },
    'TBLOUT' => { fileLocation => "$dir/$family/TBLOUT" }
  };

  #Use Moose to coerce these through!
  my $famObj = Bio::Rfam::Family->new($params);
  
  return $famObj;
}


sub loadRfamFromSVN {
  my ( $self, $family, $client ) = @_;

  my $dir = File::Temp->newdir( 'CLEANUP' => 1 );
  mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");

  foreach my $f ( @{ $self->{config}->mandatoryFiles } ) {
    my $fh;
    open( $fh, ">$dir/$family/$f" ) or die "Could not open $dir/$f";
    $client->catFile( $family, $f, $fh );
    close($fh);
  }
  my $famObj = $self->loadRfamFromLocalFile( $family, $dir, 'svn' );
  return $famObj;
}

sub parseCM {
  my ( $self, $file ) = @_;

  my @file;
  if ( ref($file) eq "GLOB" ) {
      @file = <$file>;
  }
  elsif ( ref($file) eq "ARRAY" ) {
    @file = @{$file};
  }
  else {
    open( my $fh, "$file" ) or die "Could not open $file:[$!]\n";
    @file = <$fh>;
  }

  my $i = 0;
  my $cm = { 'rawcm' => \@file };
  $self->_parseCMHeader( $cm, \$i );
  $self->_parseCMBody( $cm, \$i );
  $self->_parseCMHMMHeader( $cm, \$i );
  $self->_parseHMMBody($cm, \$i);
  my $cmObj = 'Bio::Rfam::Family::CM'->new($cm);
  return ($cmObj);
}

sub _parseCMHeader {
  my ($self, $cm, $iRef ) = @_;
#INFERNAL1/a [1.1rc2 | December 2012]
#NAME     SEED
#STATES   269
#NODES    66
#CLEN     85
#W        103
#ALPH     RNA
#RF       no
#CONS     yes
#MAP      yes
#DATE     Fri Jan 25 17:21:03 2013
#COM      [1] /Users/finnr/Work/Projects/Rfam/Infernal/infernal-1.1rc2/src/cmbuild test.cm.justbuild RF00014/SEED
#PBEGIN   0.05
#PEND     0.05
#WBETA    1e-07
#QDBBETA1 1e-07
#QDBBETA2 1e-15
#N2OMEGA  1.52588e-05
#N3OMEGA  1.52588e-05
#ELSELF   -0.08926734
#NSEQ     5
#EFFN     1.245117
#CKSUM    3944183696
#NULL     0.000  0.000  0.000  0.000 
#EFP7GF   -7.2971 0.71888
#CM

  #Store the CM header in here initally.
  my $objHash = {};

  my $i = 0;
  for ( $i = $$iRef; $i <= scalar(@{$cm->{rawcm}}); ){  
    $_ = $cm->{rawcm}->[$i];
    if(my ($version) = $_ =~ /(INFERNAL1.*)/){
      $objHash->{version} = $version;
    }elsif(/NAME\s+(\S+)/){ 
      $objHash->{name} =  $1 ;
    }elsif(/STATES\s+(\d+)/){
      $objHash->{states} = $1;
    }elsif(/NODES\s+(\d+)/){
       $objHash->{nodes} = $1;
    }elsif(my ($length) = $_ =~ /^CLEN\s+(\d+)/){
      $objHash->{clen} = $length;
    }elsif(/^W\s+(\d+)$/){
      $objHash->{w} = $1;
    }elsif( my ($alpha) = $_ =~ /^ALPH\s+(\S+)/){
      $objHash->{alpha} = $alpha;
    }elsif( my ($rf) = $_ =~ /^RF\s+(no|yes)/){
      $objHash->{rf} = ($rf eq "no") ? 0 : 1; 
    }elsif( my ($cons) = $_ =~ /^CONS\s+(no|yes)/){
      $objHash->{cons} = ($cons eq "no") ? 0 : 1;
    }elsif(my ($map) = $_ =~ /^MAP\s+(no|yes)/){
      $objHash->{map} = ($map eq "no") ? 0 : 1; 
    }elsif(my ($date) = $_ =~ /^DATE\s+(.*)/){
      $objHash->{date} =  $date; 
    }elsif(my ($index, $line) = $_ =~ /^COM\s+\[(\d+)\]\s+(.*)$/){
      $index--;
      if(!exists($objHash->{com})){
        $objHash->{com} = [];
      }
      $objHash->{com}->[$index] = $line;
    }
    
    #PBEGIN   0.05
#PEND     0.05
#WBETA    1e-07
#QDBBETA1 1e-07
#QDBBETA2 1e-15
#N2OMEGA  1.52588e-05
#N3OMEGA  1.52588e-05
#ELSELF   -0.08926734
#NSEQ     5
#EFFN     1.245117
#CKSUM    3944183696
#NULL     0.000  0.000  0.000  0.000 
#EFP7GF   -7.2971 0.71888
#CM
    elsif( my $pbegin = $_ =~ /^PBEGIN\s+(\d+\.\d+)/){
       $objHash->{pbegin} = $pbegin;
    }elsif( my $pend = $_ =~ /^PEND\s+(\d+\.\d+)/){
       $objHash->{pend} = $pend;
    }elsif( my $wbeta = $_ =~ /^WBETA\s+(\S+)/){
       $objHash->{wbeta} = $wbeta;
    }elsif( my $qdbbeta1 = $_ =~ /^QDBBETA1\s+(\S+)/){
       $objHash->{qdbbeta1} = $qdbbeta1;
    }elsif( my $qdbbeta2 = $_ =~ /^QDBBETA2\s+(\S+)/){
       $objHash->{qdbbeta2} = $qdbbeta2;
    }elsif( my $n2omega = $_ =~ /^N2OMEGA\s+(\S+)/){
       $objHash->{n2omega} = $n2omega;
    }elsif( my $n3omega = $_ =~ /^N3OMEGA\s+(\S+)/){
       $objHash->{n3omega} = $n3omega;
    }elsif( my $elself = $_ =~ /^ELSELF\s+(\S+)/){
       $objHash->{elself} = $elself;
    }elsif(my($noSeqs) = $_ =~ /^NSEQ\s+(\d+)/){
      $objHash->{nSeq} = $noSeqs;
    }elsif( my($effn) = $_ =~ /^EFFN\s+(\d+\.\d+)/){
       #EFFN  4.966292
      $objHash->{effn} =  $effn ;
    }elsif( my ( $cksum ) = $_ =~ /^CKSUM\s+(\d+)/){
      $objHash->{cksum} = $cksum ;
    }elsif( my $null = $_ =~ /^NULL\s+(.*)/){
       $objHash->{null} = $null;
    }elsif( my $efp7gf = $_ =~ /^EFP7GF\s+(\S+)/){
       $objHash->{efp7gf} = $efp7gf;
    }
    
#If the model is calibrated, parse     
#ECMLC    0.62369    -8.95393     0.81613     1600000      531557  0.002258
#ECMGC    0.42792   -14.49103    -3.20105     1600000       50144  0.007977
#ECMLI    0.53383    -8.38474     2.25076     1600000      350673  0.003422
#ECMGI    0.47628    -9.31019     0.57693     1600000       44378  0.009013    
    elsif( $_ =~ /^(ECM\S{2})\s+(.*)/){
      my $rowLabel = lc($1);
      my @values   = split(/\s+/, $2);
      if(scalar(@values) != 6){
        die "Expected 6 values on $rowLabel.\n";
      } 
      $objHash->{$rowLabel} = \@values;
    }
    
#If the CM has had the thresholds entered.    
    elsif(/GA\s+(\S+)/){ 
      $objHash->{hitGA} = $1;
    }elsif(/TC\s+(\S+)/){ 
      $objHash->{hitTC} = $1;
    }elsif(/NC\s+(\S+)/){ 
      $objHash->{hitNC} = $1;
    }elsif( $_ =~ /^CM/){
      #Reached the end of the CM header
      $$iRef = $i;
      last;
    }else{
      confess("Got a bad CM header line:$_\n"); 
    }
    $i++;
  }
  $cm->{cmHeader} = $objHash;
  #See if the CM has a field indicating that it has been calibrated.
  if(exists($cm->{cmHeader}->{ecmli})){
    $cm->{is_calibrated}=1;
  }
}

sub _parseCMBody {
  my ( $self, $cm, $iRef ) = @_;
  
  #To determine if the CM has secondary structure we need to see if there are
  #positions in the CM marked as match pair nodes.
  #e.g. [ MATP   52 ]     63     84 C G - -

  $cm->{match_pair_node} = 0;
  for (
    my $i = ( defined($iRef) ? $$iRef : 0 ) ;
    $i < scalar( @{ $cm->{rawcm} } ) ;
    $i++
    )
  {
    push(@{ $cm->{cmBody} }, $cm->{rawcm}->[$i]);
    if ( $cm->{rawcm}->[$i] =~ /\s+\[\s+MATP\s+\d+\s+\]/ ) {
      $cm->{'match_pair_node'} = 1;
    }elsif( $cm->{rawcm}->[$i] =~ /\/\//){
      #Should have reached the end of the CM body, so set the reference counter
      #to be equal to our counter.
      $$iRef = ++$i;
      last;
    }
  }
}

sub _parseCMHMMHeader {
  my ( $self, $cm, $iRef ) = @_;
    #Parse the header section of the HMM!

#HMMER3/f [i1.1rc2 | December 2012]
#NAME  SEED
#LENG  85
#MAXL  177
#ALPH  RNA
#RF    no
#MM    no
#CONS  yes
#CS    yes
#MAP   yes
#DATE  Fri Jan 25 17:21:03 2013
#COM   [1] /Users/finnr/Work/Projects/Rfam/Infernal/infernal-1.1rc2/src/cmbuild test.cm.justbuild RF00014/SEED
#NSEQ  5
#EFFN  2.250977
#CKSUM 3944183696
#STATS LOCAL MSV       -9.1751  0.71888
#STATS LOCAL VITERBI   -9.6795  0.71888
#STATS LOCAL FORWARD   -3.3562  0.71888

  #To add GA, TC, NC, CKSUM, DESC
  my($objHash);
  my $i;
  for ( $i = $$iRef; $i <= scalar(@{$cm->{rawcm}}); ){  
    $_ = $cm->{rawcm}->[$i];
    if(my ($version) = $_ =~ /(HMMER3.*)/){
      $objHash->{version} = $version;
    }elsif(/NAME\s+(\S+)/){ 
      $objHash->{name} =  $1 ;
    }elsif(my ($length) = $_ =~ /^LENG\s+(\d+)/){
      $objHash->{length} = $length;
    }elsif(/^MAXL\s+(\d+)$/){
      $objHash->{maxl} = $1;
    }elsif( my ($alpha) = $_ =~ /^ALPH\s+(\S+)/){
      $objHash->{alpha} = $alpha;
    }elsif( my ($rf) = $_ =~ /^RF\s+(no|yes)/){
      $objHash->{rf} = ($rf eq "no") ? 0 : 1; 
    }elsif( my ($mm) = $_ =~ /^MM\s+(no|yes)/){
      $objHash->{mm} = ($mm eq "no") ? 0 : 1;
    }elsif( my ($cons) = $_ =~ /^CONS\s+(no|yes)/){
      $objHash->{cons} = ($cons eq "no") ? 0 : 1;
    }elsif(my ($cs) = $_ =~ /^CS\s+(no|yes)/ ){
      $objHash->{cs} =  ($cs eq "no") ? 0 : 1; 
    }elsif(my ($map) = $_ =~ /^MAP\s+(no|yes)/){
      $objHash->{map} = ($map eq "no") ? 0 : 1; 
    }elsif(my ($date) = $_ =~ /^DATE\s+(.*)/){
      $objHash->{date} =  $date; 
    }elsif(my ($index, $line) = $_ =~ /^COM\s+\[(\d+)\]\s+(.*)$/){
      $index--;
      $objHash->{com}->[$index] = $line;
    }elsif(my($noSeqs) = $_ =~ /^NSEQ\s+(\d+)/){
      $objHash->{nSeq} = $noSeqs;
    }elsif( my($effn) = $_ =~ /^EFFN\s+(\d+\.\d+)/){
       #EFFN  4.966292
      $objHash->{effn} =  $effn ;
    }elsif( my ( $cksum ) = $_ =~ /^CKSUM (\d+)/){
      $objHash->{cksum} = $cksum ;
    }elsif(/GA\s+(\S+)\;/){ 
      $objHash->{hitGA} = $1;
    }elsif(/TC\s+(\S+)\;/){ 
      $objHash->{hitTC} = $1;
    }elsif(/NC\s+(\S+)\;/){ 
      $objHash->{hitNC} = $1;
    }elsif( my ($msv_mu, $msv_lambda ) = $_ =~ /^STATS LOCAL MSV\s+(\S+)\s+(0\.\d+)/){
      $objHash->{msvStats} = { mu => $msv_mu, lambda => $msv_lambda};
    }elsif( my ($viterbi_mu, $viterbi_lambda ) = $_ =~ /^STATS LOCAL VITERBI\s+(\S+)\s+(0\.\d+)/){
      $objHash->{viterbiStats} = { mu => $viterbi_mu, lambda => $viterbi_lambda };
    }elsif( my ($forward_tau, $forward_lambda ) = $_ =~ /^STATS LOCAL FORWARD\s+(\S+)\s+(0\.\d+)/){
      $objHash->{forwardStats} = {tau => $forward_tau, lambda => $forward_lambda};
    }elsif( $_ =~ /^HMM\s+A/){
      $$iRef = $i;
      last;
    }else{
      confess("Got a bad HMM header line:$_\n"); 
    }
    $i++;
  }
  $cm->{hmmHeader} = $objHash;

}


sub _parseHMMBody {
  my ( $self, $cm, $iRef ) = @_;

  #Should just be a read to the end of the file
  for (
    my $i = ( defined($iRef) ? $$iRef : 0 ) ;
    $i < scalar( @{ $cm->{rawcm} } ) ;
    $i++
    )
  {
    push(@{ $cm->{hmmBody} }, $cm->{rawcm}->[$i]);
  }
}

sub parseScores {
  my ( $self, $file ) = @_;

  my @file;
  if ( ref($file) eq "GLOB" ) {
    @file = <$file>;
  }
  elsif ( ref($file) eq "ARRAY" ) {
    @file = @{$file};
  }
  else {
    open( my $fh, "$file" ) or die "Could not open $file:[$!]\n";
    @file = <$fh>;
  }

  my $noHits = 0;
  my @regions;
  foreach my $line (@file) {

    #How many scores file data elements
    next if ( $line =~ /^#/ );
    my @row = split( /\s+/, $line );

    if ( $row[0] !~ m|^\S+/\d+\-\d+$| ) {
      die "First element in $line does not look like a name/start-end\n";
    }

    foreach my $p (qw(1 2 6 7)) {
      if ( $row[$p] !~ m|^\d+$| ) {
        die "$row[$p] in $line does not look like an integer\n";
      }
    }

    if ( $row[0] ne $row[3] . '/' . $row[1] . '-' . $row[2] ) {
      die "Expected $row[0] to match "
        . $row[3] . '/'
        . $row[1] . '-'
        . $row[2] . "\n";
    }

    foreach my $p (qw(4 5)) {
      local $^W = 0;
      if ( $row[$p] == 0 && $row[$p] ne '0' ) {
        print "$row[$p] does not look like a number number\n";
      }
    }
    if ( $row[8] !~ /^(53|0|3|5)$/ ) {
      die "Incorrect truncation type, $row[8] in $line\n";
    }

    if ( defined( $row[9] ) ) {
      if ( $row[9] ne 'seed' and $row[9] ne 'full' ) {
        die "Incorrect type, $row[8] in $line\n";
      }
    }
    push( @regions, \@row );
    $noHits++;
  }
  my $scoresObj = Bio::Rfam::Family::Scores->new(
    {
      numRegions => $noHits,  
      regions    => \@regions
    }
  );
  return ($scoresObj);
}

sub parseDESC {
  my ( $self, $file ) = @_;

  my @file;
  if ( ref($file) eq "GLOB" ) {
    @file = <$file>;
  }
  else {
    open( my $fh, "$file" ) or die "Could not open $file:[$!]\n";
    @file = <$fh>;
    close($file);
  }

  my %params;
  my $expLen = 80;

  my $refTags = {
    RC => {
      RC => 1,
      RN => 1
    },
    RN => { RM => 1 },
    RM => { RT => 1 },
    RT => {
      RT => 1,
      RA => 1
    },
    RA => {
      RA => 1,
      RL => 1
    },
    RL => { RL => 1 },
  };

  for ( my $i = 0 ; $i <= $#file ; $i++ ) {
    my $l = $file[$i];
    chomp($l);
    if ( length($l) > $expLen ) {
      croak("\nGot a DESC line that was longer than $expLen, $file[$i]\n\n"
          . "-" x 80
          . "\n" );
    }

    if ( $file[$i] =~ /^(AC|ID|DE|PI|AU|SE|SS|BM|SM|CB|TP|TX|SQ|CL|FR|SN)\s{3}(.*)$/ ) {
      if ( exists( $params{$1} ) ) {
        croak("Found second $1 line, only expecting one\n");
      }
      $params{$1} = $2;
      next;
    } elsif ( $file[$i] =~ /^(TC|NC|GA)\s{3}(\S+)$/ ) {
      $params{ "CUT" . $1 } = $2;
    }
    elsif ( $file[$i] =~ /^\*\*\s{3}(.*)$/ ) {
      $params{private} .= " " if ( $params{private} );
      $params{private} .= $1;
    }
    elsif ( $file[$i] =~ /^WK\s{3}(.*)$/ ) {
      my $page = $1;
      if ( $page =~ /^http.*\/(\S+)/ ) {

        #TODO - supress warn, if we can remove URL. Page title sufficient.
        carp( "$page going to be set to $1\n" );
        $page = $1;
      }
      if ( defined( $params{"WIKI"} ) ) {
        $params{"WIKI"}->{$page}++;
      }
      else {
        $params{"WIKI"} = { $page => 1 };
      }
    }
    elsif ( $file[$i] =~ /^SM\s{3}(.*)$/ ) {
      my $sm = $1;
      if ( $params{SM} ) {
        $params{SM} .= " ";
      }
      $params{SM} .= $sm;
      next;
    }
    elsif ( $file[$i] =~ /^CC\s{3}(.*)$/ ) {
      my $cc = $1;
      while ( $cc =~ /(\w+):(\S+)/g ) {
        my $db  = $1;
        my $acc = $2;

        #TODO - check that there are no CC xrefs like this!
        if ( $db eq 'Swiss' ) {
          $acc =~ s/\W+//g;
          unless ( $acc =~ /^\S{6}$/ ) {
            confess(
"\nDESC file format for link to Swiss-Prot is wrong $acc is not a valid accession\n"
                . "-" x 80
                . "\n" );
          }
        }

        if ( $db eq 'Dfam' ) {
          $acc =~ s/\.|\,//g;
          unless ( $acc =~ /^DF\d{7}/ ) {
            warn(
"\nDESC file format for link to Dfam is wrong $acc is not a valid accession\n"
                . "-" x 80
                . "\n" );
          }
        }

        if ( $db eq 'EC' ) {
          unless ( $acc =~ /^\(?(\d+)(\.(\d+|\-)){1,3}(\.|\,|\)){0,2}$/ ) {
            confess(
"DESC file format for link to EC is wrong $acc is not a valid accession\n"
            );
          }
        }
      }

      if ( $params{CC} ) {
        $params{CC} .= " ";
      }
      $params{CC} .= $cc;
      next;
    }
    elsif ( $file[$i] =~ /^R(N|C)\s{3}/ ) {
      my $ref;
    REFLINE:
      foreach ( my $j = $i ; $j <= $#file ; $j++ ) {
        if ( $file[$j] =~ /^(\w{2})\s{3}(.*)/ ) {
          my $thisTag = $1;
          if ( $ref->{$1} ) {
            $ref->{$1} .= " $2";
          }
          else {
            $ref->{$1} = $2;
          }
          if ( $j == $#file ) {
            $i = $j;
            last REFLINE;
          }
          my ($nextTag) = $file[ $j + 1 ] =~ /^(\w{2})\s{3}/;

          #Now lets check that the next field is allowed
          if ( $refTags->{$thisTag}->{$nextTag} ) {
            next REFLINE;
          }
          elsif (
            (
              !$refTags->{$nextTag}
              or ( $nextTag eq "RN" or $nextTag eq "RC" )
            )
            and ( $thisTag eq "RL" )
            )
          {
            $i = $j;
            last REFLINE;
          }
          else {
            confess("Bad references fromat. Got $thisTag then $nextTag ");
          }
        }
      }
      $ref->{RN} =~ s/\[|\]//g;
      push( @{ $params{REFS} }, $ref );
    }
    elsif ( $file[$i] =~ /^NE\s{3}(PF\d{5})\;$/ ) {
      my $nestAcc = $1;
      if ( $file[ $i + 1 ] =~ /NL\s{3}(\S+)\/(\d+)\-(\d+)\;/ ) {
        $i++;
        push(
          @{ $params{NESTS} },
          { dom => $nestAcc, seq => $1, from => $2, to => $3 }
        );
      }
      else {
        confess("NE lines must be followed by an NL line");
      }
    }
    elsif (
      $file[$i] =~ /^ED\s{3}(\S+)\/(\d+)\-(\d+)\;\s+(\S+)\/(\d+)\-(\d+)\;\s+$/ )
    {
      push(
        @{ $params{EDITS} },
        { seq => $1, oldFrom => $2, oldTo => $3, newFrom => $5, newTo => $6 }
      );
      next;
    }
    elsif ( $file[$i] =~ /^ED\s{3}(\S+)\/(\d+)\-(\d+)\;\s+$/ ) {
      push(
        @{ $params{EDITS} },
        { seq => $1, oldFrom => $2, oldTo => $3, delete => 1 }
      );
      next;
    }

 #------------------------------------------------------------------------------
 #Database cross references
    elsif ( $file[$i] =~ /^D[C|R]\s{3}/ ) {
      for ( ; $i <= $#file ; $i++ ) {
        my $com;
        for ( ; $i <= $#file ; $i++ ) {
          if ( $file[$i] =~ /^DC\s{3}(.*)/ ) {
            $com .= " " if ($com);
            $com = $1;
          }
          else {
            last;
          }
        }

        if ( !$file[$i] ) {
          confess("Found a orphan DC line\n");
        }

        #TODO - Not this is not a comment, but an additional parameter!
        if ( $file[$i] =~ /^DR   SO:(\d+) SO:(.*)$/ ) { #old style

          #GO:0010628 GO:positive regulation of gene expression
          push(
            @{ $params{DBREFS} },
            { db_id => 'SO', db_link => $1, other_params => $2 }
          );
        }elsif ( $file[$i] =~ /^DR   SO; (\d+); (.*);$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
            @{ $params{DBREFS} },
            { db_id => 'SO', db_link => $1, other_params => $2 }
          );
        }
        
        elsif ( $file[$i] =~ /^DR   GO:(\d+) GO:(.*)$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
            @{ $params{DBREFS} },
            { db_id => 'GO', db_link => $1, other_params => $2 }
          );
        }elsif ( $file[$i] =~ /^DR   GO; (\d+); (.*);$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
            @{ $params{DBREFS} },
            { db_id => 'GO', db_link => $1, other_params => $2 }
          );
        }
        
        elsif ( $file[$i] =~ /^DR   (MIPF); (MIPF\d+)$/ ) {

          #MIPF; MIPF0000879
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (snornadb);\s(.*?\d+)$/ ) {

          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );

          #TODO - check that these are really different.
        }
        elsif ( $file[$i] =~ /^DR   (snoRNABase);\s(.*?\d+)$/ ) {

          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );

        }
        elsif ( $file[$i] =~ /^DR   (PKBASE); (PKB\d+)$/ ) {

          #PKBASE; PKB00277
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (snoopy); (.*)$/ ) {

          #snoopy;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (MIR); (.*)$/ ) {

          #MIR; MI0001007;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (URL); (.*)$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR/ ) {
          confess( "Bad reference line: unknown database |$file[$i]|.\n"
              . "This may be fine, but we need to know the URL of the xref."
              . "Talk to someone who knows about these things!\n" );
        }
        else {

          #We are now on to no DR lines, break out and go back on position
          $i--;
          last;
        }
        if ($com) {
          $params{DBREFS}->[ $#{ $params{DBREFS} } ]->{db_comment} = $com;
        }
      }
    }
    elsif ( $file[$i] =~ /^(AM|AL)/ ) {

      #old desc lines
      next;
    }
    else {
      chomp( $file[$i] );
      my $msg = "Failed to parse the DESC line (enclosed by |):|$file[$i]|\n\n"
        . "-" x 80 . "\n";

      #croak($msg);
      die $msg;
    }
  }

  my $desc = 'Bio::Rfam::Family::DESC'->new(%params);
  return $desc;

  #End of uber for loop
}

sub writeEmptyDESC {
  my ($self) = @_;

  my %desc = (
    ID    => 'ShortName',
    DE    => 'Family description',
    AU    => 'Who RU',
    SE    => 'Where did the seed come from',
    CUTGA => '27.00',
    CUTNC => '27.00',
    CUTTC => '27.00',
  );

  my $desc = 'Bio::Rfam::Family::DESC'->new( \%desc );
  $self->writeDESC($desc);
}

sub writeDESC {
  my ( $self, $desc, $path ) = @_;

  unless ( $desc->isa('Bio::Rfam::Family::DESC') ) {
    confess("You did not pass in a  Bio::Rfam::Family::DESC object");
  }

  my $descfile;
  if ($path) {
    $descfile = $path . "/DESC";
  }
  else {
    $descfile = "DESC";
  }
  open( D, ">$descfile" )
    or die "Could not open $descfile file for writing to\n";

  $Text::Wrap::columns = 80;
  foreach my $tagOrder ( @{ $desc->order } ) {
    if ( length($tagOrder) == 2 ) {
      if ( $desc->$tagOrder and $desc->$tagOrder =~ /\S+/ ) {
        print D wrap( "$tagOrder   ", "$tagOrder   ", $desc->$tagOrder );
        print D "\n";
      }
    }
    else {
      next unless ( $desc->$tagOrder );
      if ( $tagOrder eq 'CUTTC' ) {
        printf D "TC   %.2f\n", $desc->$tagOrder;
      }
      elsif ( $tagOrder eq 'CUTGA' ) {
        printf D "GA   %.2f\n", $desc->$tagOrder;
      }
      elsif ( $tagOrder eq 'CUTNC' ) {
        printf D "NC   %.2f\n", $desc->$tagOrder;
      }
      elsif ( $tagOrder eq 'NESTS' ) {
        foreach my $n ( @{ $desc->$tagOrder } ) {
          print D wrap( "NE   ", "NE   ", $n->{dom} . ";" );
          print D "\n";
          print D wrap( "NL   ", "NL   ",
            $n->{seq} . "/" . $n->{from} . "-" . $n->{to} . ";" );
          print D "\n";
        }
      }
      elsif ( $tagOrder eq 'CLASS' ) {
        foreach my $key (qw(Type Class Superfamily Comment)) {
          next if ( !exists( $desc->$tagOrder->{$key} ) );
          my $value = $desc->$tagOrder->{$key};
          if ( $key eq 'Comment' ) {
            print D wrap( "CN   ", "CN   ", $value );
            print D "\n";
          }
          else {
            print D "CT   " . $key . "; " . $value . ";\n";
          }
        }

      }
      elsif ( $tagOrder eq 'WIKI' ) {
        if ( ref( $desc->$tagOrder ) eq 'HASH' ) {
          my @pages = keys( %{ $desc->$tagOrder } );
          foreach my $p (@pages) {
            print D wrap( "WK   ", "WK   ", $p );
            print D "\n";
          }
        }
      }
      elsif ( $tagOrder eq 'CLASS' ) {
        foreach my $key (qw(Type Class Superfamily Comment)) {
          next if ( !exists( $desc->$tagOrder->{$key} ) );
          my $value = $desc->$tagOrder->{$key};
          if ( $key eq 'Comment' ) {
            print D wrap( "CN   ", "CN   ", $value );
            print D "\n";
          }
          else {
            print D "CT   " . $key . "; " . $value . ";\n";
          }
        }
      }
      elsif ( $tagOrder eq 'REFS' ) {
        foreach my $ref ( @{ $desc->$tagOrder } ) {
          if ( $ref->{RC} ) {
            print D wrap( "RC   ", "RC   ", $ref->{RC} );
            print D "\n";
          }
          print D "RN   [" . $ref->{RN} . "]\n";
          print D "RM   " . $ref->{RM} . "\n";
          print D wrap( "RT   ", "RT   ", $ref->{RT} );
          print D "\n";
          print D wrap( "RA   ", "RA   ", $ref->{RA} );
          print D "\n";
          print D "RL   " . $ref->{RL} . "\n";

        }
      }
      elsif ( $tagOrder eq 'DBREFS' ) {
        foreach my $xref ( @{ $desc->$tagOrder } ) {

          #Print out any comment
          if ( $xref->{db_comment} ) {
            print D wrap "DC   ", "DC   ", $xref->{db_comment};
            print D "\n";
          }

          if ( $xref->{other_params} ) {
            #TODO - go back and remove this is a really nasty hack!!!
            
            
              print D "DR   "
              . $xref->{db_id} . "; "
              . $xref->{db_link} . "; "
              . $xref->{other_params} . ";\n";
          }
          else {
            print D "DR   " . $xref->{db_id} . "; " . $xref->{db_link} . ";\n";
          }
        }
      }
      elsif ( $tagOrder eq 'EDITS' ) {
        foreach my $e ( @{ $desc->$tagOrder } ) {
          if ( $e->{newTo} ) {
            print D "ED   "
              . $e->{seq} . "/"
              . $e->{oldFrom} . "-"
              . $e->{oldTo} . "; "
              . $e->{seq} . "/"
              . $e->{newFrom} . "-"
              . $e->{newTo} . ";\n";
          }
          else {
            print D "ED   "
              . $e->{seq} . "/"
              . $e->{oldFrom} . "-"
              . $e->{oldTo} . ";\n";
          }
        }
      }
      elsif ( $tagOrder eq 'private' ) {
        print D wrap "**   ", "**   ", $desc->{$tagOrder};
        print D "\n";
      }
    }
  }
  close(D);
}

sub updateRfamInRDB {
  my ( $self, $famObj, $isNew, $depositor ) = @_;

  #This essential updates all of the content contained in the desc file
  unless ( $famObj and $famObj->isa('Bio::Rfam::Family') ) {
    confess("Did not get a Bio::Rfam::Family object");
  }

  my $rfamDB = $self->{config}->rfamlive;
  if ($isNew) {
    $rfamDB->resultset('Family')->createFamily( $famObj, $depositor );
  }
  else {
    $rfamDB->resultset('Family')->updateFamily($famObj);
  }
}

sub updateFamilyRfamseqRegions {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Rfam::Family') ) {
    confess("Did not get a Bio::Rfam::Family object");
  }

  my $rfamDB = $self->{config}->rfamlive;

  $rfamDB->resultset('SeedRegion')->updateSeedReg($famObj);
  $rfamDB->resultset('FullRegion')->updateFullReg($famObj);

}

sub moveFamilyInRDB {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Rfam::Family') ) {
    confess("Did not get a Bio::Rfam::Family object");
  }
  my $rfamDB = $self->{config}->rfamlive;
  $rfamDB->resultset('Family')->updateFamily($famObj);

}

sub deleteFamilyInRDB {
  my ( $self, $family, $comment, $forward, $user ) = @_;
    
  unless ( $family and $family =~ /RF\d{5}/ ) {
    confess("Did not get a Dfam accession\n");
  }
  
  unless ($comment) {
    confess('Did not get a comment as to why this family is being killed');
  }

  my $rfamDB = $self->{config}->rfamlive;
  my $entry = $rfamDB->resultset('Family')->find('rfam_acc' => $family);
  
  $rfamDB->resultset('Family')->delete('rfam_acc' => $family);
  $rfamDB->resultset('DeadFamily')->createFromFamilyRow($entry, $comment, $forward, $user);
}

# make scores 2D array from outlist, then write it by calling writeScores()
sub makeAndWriteScores {
    my ($self, $famObj, $outlistLocation) = @_;

    my $n    = 0; # number of hits
    my $nres = 0; # total number of residues in all hits
    my $name;     # sequence name (name/start-end)
    my $tcode;    # truncation code ("no", "5'", "3'", "5'&3'")
    my @scoresAA = (); # 2D array of scores
    my $threshold = $famObj->DESC->CUTGA; # threshold
    my $stype;    # lowercase version of type ('seed' or 'full')

    if(! defined $threshold)    { croak "ERROR GA not set"; }
    open(OL, $outlistLocation) || croak "ERROR unable to open $outlistLocation for reading";

    # process the outlist
    while (<OL>){
	if(! m/^\#/) { 
	    my @elA = split(/\s+/);
	    # example line
	    # 105.50	4.9e-16	FULL	CP000970.1	   1234057	   1234142	     1	    85	no (SPECIES DATA REMOVED)
	    # 0         1       2                3          4              5                 6       7   8  
	    my ($bits, $evalue, $type, $id, $start, $end, $qstart, $qend, $tstr) = ($elA[0], $elA[1], $elA[2], $elA[3], $elA[4], $elA[5], $elA[6], $elA[7], $elA[8]);
	    if($bits < $threshold) { last; }
	    
	    $name  = "$id/$start-$end";
	    if($tstr eq "no")       { $tcode = 0;  }
	    elsif($tstr eq "5'")    { $tcode = 5;  }
	    elsif($tstr eq "3'")    { $tcode = 3;  }
	    elsif($tstr eq "5'&3'") { $tcode = 53; }
	    else { croak "ERROR invalid truncation string $tstr"; }

	    if   ($type eq "SEED") { $stype = "seed"; }
	    elsif($type eq "FULL") { $stype = "full"; }
	    else { croak "ERROR invalid type $type"; }

	    push(@{$scoresAA[$n]}, ($name, $start, $end, $id, $bits, $evalue, $qstart, $qend, $tcode, $stype));
	    if($start <= $end) { $nres += ($end - $start + 1); }
	    else               { $nres += ($start - $end + 1); }
	    $n++;
	}
    }
    close(OL);

    my $scoresObj = Bio::Rfam::Family::Scores->new(
	{
	    numRegions => $n,
	    regions    => \@scoresAA,
	    nres       => $nres,
	}
    );
    
    $famObj->SCORES($scoresObj);
    $self->writeScores($famObj);

    return;
}

# given a TBLOUT file, write four files:
# 'outlist', 'species', 'rin.dat', 'rinc.dat'
sub writeTbloutDependentFiles {
    my ($self, $rfdbh, $seedmsa, $ga, $RPlotScriptPath) = @_;

    if(! defined $self->TBLOUT->fileLocation) { croak "TBLOUT's fileLocation not set"; }
    my $tblI = $self->TBLOUT->fileLocation;

    # output files
    my $outlistO = "outlist";
    my $speciesO = "species";
    my $rinO     = "rin.dat";
    my $rincO    = "rinc.dat";

    # misc variables
    my $prv_bits   = 99999.00;     # previous bit score seen
    my $prv_evalue = 0.;           # previous E-value seen
    my %kingdomCounts;             # counts of hits in each kingdom
    my $printed_thresh;            # TRUE if threshold has already been printed
    my $outline;                   # dividing line

    # create nse HAA in msa for overlap checking
    $seedmsa->nse_createHAA();
    my $idx;
    my %seedseq_foundH = ();
    for($idx = 0; $idx < $seedmsa->nseq; $idx++) { 
	$seedseq_foundH{$seedmsa->get_sqname($idx)} = 0;
    }

    # db queries:
    # query to search for the description of embl entries with the embl id
    my $queryDesc = qq( 
           select description
                   from rfamseq where rfamseq_acc=?;
    );
    # query to fetch the species name, full taxonomy string and ncbi id for a given embl id:
    my $queryTax = qq(
           select t.species, t.tax_string, t.ncbi_id 
           from taxonomy as t, rfamseq as r 
           where t.ncbi_id=r.ncbi_id and r.rfamseq_acc=?;
    );

    # Prepare the queries for execution.
    my $sthDesc = $rfdbh->prepare($queryDesc);
    my $sthTax  = $rfdbh->prepare($queryTax);

    # open OUTPUT files
    open(OUT, "> $outlistO") || die "FATAL: failed to open $outlistO)\n[$!]";
    printf OUT "# %0.4s\t%0.12s\t%0.6s\t%0.20s\t%10s\t%10s\t%6s\t%6s\t%0.5s\t%0.20s\t%0.70s\n", 
    'bits', 'evalue', 'seqLabel', 'name', 'start', 'end', 'qstart', 'qend', 'trunc', 'shortSpecies', 'description';    
   
    open(SPC,"> $speciesO") || die "FATAL: failed to open $speciesO\n[$!]\n";   
    printf SPC "# %0.4s\t%0.12s\t%0.6s\t%0.20s\t%0.15s\t%0.20s\t%s\n", 
    'bits', 'evalue', 'seqLabel', 'name', 'ncbiId', 'species','taxString';
    
    open(RIN,"> $rinO") || die "FATAL: failed to open $rinO\n[$!]\n";   
    printf RIN "bits\ttype\ttrueOrFalse\ttax\n";
    
    open(RINc,"> $rincO") || die "FATAL: failed to open rincO\n[$!]\n";   
    printf RINc "cnt\ttax\n";
    
    # TODO: don't use grep and sort, use PERL's sort, even though paul wrote this:
    # If you don't like this you can fuck off!:
    # Shell grep & sort are a hell of a lot less resource greedy than perl's equivalents.
    
    # actually parse tblout
    open(TBL, "grep -v ^'#' $tblI | sort -nrk 15 | ") || croak "FATAL: could not open pipe for reading $tblI\n[$!]";

    ## example TBLOUT line:
    ###target name         accession query name           accession mdl mdl from   mdl to seq from   seq to strand trunc pass   gc  bias  score   E-value inc description of target
    ###------------------- --------- -------------------- --------- --- -------- -------- -------- -------- ------ ----- ---- ---- ----- ------ --------- --- ---------------------
    ## AAAA02006309.1      -         RF00014              -          cm        1       85      192      105      -    no    1 0.44   0.0   86.0   1.6e-11 !   Oryza sativa Indica Group chromosome 2 Ctg006309, whole genome shotgun sequence.

    my $tblline;
    while ($tblline = <TBL>) {
	my @tblA = split(/\s+/, $tblline);
	my ($name, $qstart, $qend, $start, $end, $strand, $trunc, $bits, $evalue) = ($tblA[0], $tblA[5], $tblA[6], $tblA[7], $tblA[8], $tblA[9], $tblA[10], $tblA[14], $tblA[15]);
	if($strand eq "+") { $strand = 1; } else { $strand = -1; }
	
	#Fetch info from the DB:
	my ($description, $species, $shortSpecies, $domainKingdom, $taxString, $ncbiId);
	if($name=~/(\S+)\.(\d+)/){
	    $sthDesc->execute($name);
	    my $res = $sthDesc->fetchall_arrayref;
	    if(! defined $res) { die "ERROR unable to fetch desc info for $name"; }
	    foreach my $row (@$res){ $description .= $row->[0]; }
	    
	    $sthTax->execute($name);
	    my $rfres = $sthTax->fetchall_arrayref;
	    if(! defined $rfres) { die "ERROR unable to fetch tax info for $name"; }
	    foreach my $row (@$rfres){
		$species   .= $row->[0];
		$taxString .= $row->[1];
		$ncbiId    .= $row->[2];
	    }

	    $shortSpecies  = species2shortspecies($species);
	    $domainKingdom = tax2kingdom($taxString . '; ' . $species . ';');
	    if(! defined $kingdomCounts{$domainKingdom}) { $kingdomCounts{$domainKingdom} = 0 }
	    $kingdomCounts{$domainKingdom}++;
	}	  

	$description  = 'NA' if not defined $description;
	$species      = 'NA' if not defined $species;
	$shortSpecies = 'NA' if not defined $shortSpecies;
	  
	# determine seqLabel
	my $seqLabel = 'FULL';
	my ($seed_seq, $overlapExtent) = $seedmsa->nse_overlap($name . "/" . $start . "-" . $end);

	if($seed_seq ne "") { 
	    $seedseq_foundH{$seed_seq}++;
	    if ($overlapExtent > 0.1) { $seqLabel = 'SEED'; }
	}
	if (($bits < $ga) && ($seqLabel ne 'SEED')) { $seqLabel = 'NOT' }

	# print out threshold line if nec
	if ( $bits < $ga && $ga<=$prv_bits){
	    $outline = "#***********CURRENT THRESHOLD: $ga bits***********#\n";
	    print OUT $outline;
	    print SPC $outline;
	    printf RIN  "%0.2f\tTHRESH\t\.\t.\n", $ga;
	    $printed_thresh=1;
	}
	if ($evalue > 1 && $prv_evalue <= 1) { 
	    $outline = "#***********E-VALUE OF 1**************#\n";
	    printf OUT $outline;
	    printf SPC $outline;
	}
	$prv_bits = $bits;
	$prv_evalue = $evalue;

	printf OUT ("%0.2f\t%0.12s\t%0.6s\t%0.20s\t%10d\t%10d\t%6d\t%6d\t%0.5s\t%0.20s\t%0.70s\n", 
		    $bits, $evalue, $seqLabel, $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description);
	printf SPC ("%0.2f\t%0.12s\t%0.6s\t%0.20s\t%15d\t%0.20s\t%s\n", 
		    $bits, $evalue, $seqLabel, $name, $ncbiId, $species, $taxString);
	printf RIN  "%0.2f\t%0.6s\t$domainKingdom\n", $bits, $seqLabel;
    } # closes 'while($tblline = <TBL>)'
    
    if ( not defined $printed_thresh){
	printf OUT "#***********CURRENT THRESHOLD: $ga bits***********#\n";
	printf SPC "#***********CURRENT THRESHOLD: $ga bits***********#\n";
	printf RIN "%0.2f\tTHRESH\t\.\t\.\n", $ga;
    }
    
    foreach my $king ( sort{ $a cmp $b } keys %kingdomCounts) {
	printf RINc  "%d\t$king\n", $kingdomCounts{$king};
    }
    
    # complain loudly if seed sequences are missing from the output:
    my @warningsA = ();
    foreach my $n (keys %seedseq_foundH){
	if ($seedseq_foundH{$n} < 1){
	    $outline = "WARNING: SEED sequence $n was not found (not in TBLOUT)\n";
	    warn $outline;
	    push(@warningsA, $outline);
	}
    }
    if(scalar(@warningsA) > 0) { 
	open(W, ">warnings") || croak "unable to open warnings file for writing";
	foreach my $warning (@warningsA) { 
	    print W $warning;
	}
	close(W);
    }
    
    close(TBL);
    close(OUT);
    close(SPC);
    close(RIN);
    close(RINc);

    # run R: 
    system("R CMD BATCH --no-save $RPlotScriptPath") and warn "WARNING: system call for R $RPlotScriptPath failed. Check binary exists and is executable.\n[R CMD BATCH --no-save $RPlotScriptPath]\n";
}

# write scores to SCORES file
sub writeScores {
## example array of values in $scoresObj->regions:
##
#  AGFT01076311.1/81-1           81      1     AGFT01076311.1  46.13   3.02e-03 1        100    0       seed
## <sqacc.version>/<start>-<end> <start> <end> <sqacc.version> <bitsc> <evalue> <qstart> <qend> <trunc> <type> 
## 0                             1       2     3               4       5        6        7      8       9              
    my ($self, $famObj) = @_;

    my $scoresObj = $famObj->SCORES;
    my $scoresAAR = $scoresObj->regions;
    my $nels   = 10; # hard-coded KNOWN number of elements in each array
    my ($i, $j);     # counters
    my $aR;          # convenience ptr to an array
    my $wid;         # width of a field
    my @widthA = (); # max width of each field

    open(SC, ">SCORES") || croak "ERROR unable to open SCORES for writing"; 

    for($j = 0; $j < $nels; $j++) { $widthA[$j] = 0; }

    for($i = 0; $i < $scoresObj->numRegions; $i++) { 
	$aR = $scoresAAR->[$i];
	for($j = 0; $j < $nels; $j++) { 
	    $wid = length($aR->[$j]);
	    $widthA[$j] = ($widthA[$j] > $wid) ? $widthA[$j] : $wid;
	}
    }		

    for($i = 0; $i < $scoresObj->numRegions; $i++) { 
	$aR = $scoresAAR->[$i];
	printf SC ("%-*s  %*s  %*s  %-*s  %*s  %*s  %*s  %*s  %*s  %*s\n", 
	       $widthA[0], $aR->[0], 
	       $widthA[1], $aR->[1], 
	       $widthA[2], $aR->[2], 
	       $widthA[3], $aR->[3], 
	       $widthA[4], $aR->[4], 
	       $widthA[5], $aR->[5], 
	       $widthA[6], $aR->[6], 
	       $widthA[7], $aR->[7], 
	       $widthA[8], $aR->[8], 
	       $widthA[9], $aR->[9]);
    }
}

######################################################################
sub tax2kingdom {
    my ($species) = @_;
    my $kingdom;
    #unclassified sequences; metagenomes; ecological metagenomes.
    if ($species=~/^(.+?);\s+(.+?)\.*?;/){
	$kingdom = "$1; $2";
    }
    die "FATAL: failed to parse a kingdom from species string: [$species]" if not defined $kingdom;
    
    return $kingdom;
}

######################################################################
#species2shortspecies: Given a species string eg. "Homo sapiens
#                      (human)" generate a nicely formated short name
#                      with no whitespace eg. "H.sapiens".
sub species2shortspecies {
    my $species = shift;

    my $shortSpecies;
    
    if ($species=~/(.*)\s+sp\./){
	$shortSpecies = $1;
    }
    elsif ($species=~/metagenome/i or $species=~/uncultured/i){
	$species=~s/metagenome/metag\./gi;
	$species=~s/uncultured/uncult\./gi;
	my @w = split(/\s+/,$species);
	if(scalar(@w)>2){
	    foreach my $w (@w){
		$shortSpecies .= substr($w, 0, 5) . '.';
	    }
	}
	else {
	    $shortSpecies = $species;
	    $shortSpecies =~ s/\s+/_/g;
	}
    }#lots of conditions here. Need else you get some ridiculous species names.
    elsif($species=~/^(\S+)\s+(\S{4,})/ && $species!~/[\/\-\_0-9]/ && $species!~/^[a-z]/ && $species!~/\svirus$/ && $species!~/\svirus\s/ && $species!~/^Plasmid\s/i && $species!~/\splasmid\s/i){
	$shortSpecies = substr($1,0,1) . "." . $2; 
    }
    else {
	$shortSpecies = $species;
    }
    
    $shortSpecies =~ s/\s+/_/g;
    $shortSpecies =~ s/[\'\(\)\:\/]//g;
    $shortSpecies = substr($shortSpecies,0,20) if (length($shortSpecies) > 20);
    
    return $shortSpecies;
}



1;
