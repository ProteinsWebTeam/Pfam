
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
use Cwd;
use File::Copy;
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
  } else {
    $params{'source'} = 'file';
  }

  my $params = {
                'SEED' => {
                           fileLocation => "$dir/$family/SEED",
                           aliType      => 'seed',
                           reqdFormat   => 'Stockhom',
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
                                                {
                                                 prefetch => 'family_files' });
  
  my $files = $row->family_files->first;
  my $regions = $rfamdb->resultset('FullRegion')->allRegions($family);
  
  
  #TODO - Do you have specific storage. You can specify areas as needed.
  my $dir =  File::Temp->newdir( 'CLEANUP' => 1 );
  if (! -d "$dir/$family") {
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
  
  my $descData = {};
  $rfamdb->resultset('Family')->getDESCData($family, $descData);
  
  my $params = {
                source => 'database',
                'SEED' => {
                           fileLocation => "$dir/$family/SEED",
                           aliType      => 'seed',
                           reqdFormat   => 'Stockholm'
                          },
                'DESC'   => $descData,
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
  } elsif ( ref($file) eq "ARRAY" ) {
    @file = @{$file};
  } else {
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
  #ACC      RF99999
  #DESC     An RNA
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
  for ( $i = $$iRef; $i <= scalar(@{$cm->{rawcm}}); ) {  
    $_ = $cm->{rawcm}->[$i];
    if (my ($version) = $_ =~ /(INFERNAL1.*)/) {
      $objHash->{version} = $version;
    } elsif (/NAME\s+(\S+)/) { 
      $objHash->{name} =  $1 ;
    } elsif (/ACC\s+(\S+)/) { 
      $objHash->{acc} =  $1 ;
    } elsif (/DESC\s+(.+)/) { 
      $objHash->{desc} =  $1 ;
    } elsif (/STATES\s+(\d+)/) {
      $objHash->{states} = $1;
    } elsif (/NODES\s+(\d+)/) {
      $objHash->{nodes} = $1;
    } elsif (my ($length) = $_ =~ /^CLEN\s+(\d+)/) {
      $objHash->{clen} = $length;
    } elsif (/^W\s+(\d+)$/) {
      $objHash->{w} = $1;
    } elsif ( my ($alpha) = $_ =~ /^ALPH\s+(\S+)/) {
      $objHash->{alpha} = $alpha;
    } elsif ( my ($rf) = $_ =~ /^RF\s+(no|yes)/) {
      $objHash->{rf} = ($rf eq "no") ? 0 : 1; 
    } elsif ( my ($cons) = $_ =~ /^CONS\s+(no|yes)/) {
      $objHash->{cons} = ($cons eq "no") ? 0 : 1;
    } elsif (my ($map) = $_ =~ /^MAP\s+(no|yes)/) {
      $objHash->{map} = ($map eq "no") ? 0 : 1; 
    } elsif (my ($date) = $_ =~ /^DATE\s+(.*)/) {
      $objHash->{date} =  $date; 
    } elsif (my ($index, $line) = $_ =~ /^COM\s+\[(\d+)\]\s+(.*)$/) {
      $index--;
      if (!exists($objHash->{com})) {
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
    elsif ( my $pbegin = $_ =~ /^PBEGIN\s+(\d+\.\d+)/) {
      $objHash->{pbegin} = $pbegin;
    } elsif ( my $pend = $_ =~ /^PEND\s+(\d+\.\d+)/) {
      $objHash->{pend} = $pend;
    } elsif ( my $wbeta = $_ =~ /^WBETA\s+(\S+)/) {
      $objHash->{wbeta} = $wbeta;
    } elsif ( my $qdbbeta1 = $_ =~ /^QDBBETA1\s+(\S+)/) {
      $objHash->{qdbbeta1} = $qdbbeta1;
    } elsif ( my $qdbbeta2 = $_ =~ /^QDBBETA2\s+(\S+)/) {
      $objHash->{qdbbeta2} = $qdbbeta2;
    } elsif ( my $n2omega = $_ =~ /^N2OMEGA\s+(\S+)/) {
      $objHash->{n2omega} = $n2omega;
    } elsif ( my $n3omega = $_ =~ /^N3OMEGA\s+(\S+)/) {
      $objHash->{n3omega} = $n3omega;
    } elsif ( my $elself = $_ =~ /^ELSELF\s+(\S+)/) {
      $objHash->{elself} = $elself;
    } elsif (my($noSeqs) = $_ =~ /^NSEQ\s+(\d+)/) {
      $objHash->{nSeq} = $noSeqs;
    } elsif ( my($effn) = $_ =~ /^EFFN\s+(\d+\.\d+)/) {
      #EFFN  4.966292
      $objHash->{effn} =  $effn ;
    } elsif ( my ( $cksum ) = $_ =~ /^CKSUM\s+(\d+)/) {
      $objHash->{cksum} = $cksum ;
    } elsif ( my $null = $_ =~ /^NULL\s+(.*)/) {
      $objHash->{null} = $null;
    } elsif ( $_ =~ /^EFP7GF\s+(\S+)\s+(\S+)/) {
      my @values = ($1, $2);
      $objHash->{efp7gf} = \@values;
    }
    
    #If the model is calibrated, parse     
    #ECMLC    0.62369    -8.95393     0.81613     1600000      531557  0.002258
    #ECMGC    0.42792   -14.49103    -3.20105     1600000       50144  0.007977
    #ECMLI    0.53383    -8.38474     2.25076     1600000      350673  0.003422
    #ECMGI    0.47628    -9.31019     0.57693     1600000       44378  0.009013    
    elsif ( $_ =~ /^(ECM\S{2})\s+(.*)/) {
      my $rowLabel = lc($1);
      my @values   = split(/\s+/, $2);
      if (scalar(@values) != 6) {
        die "Expected 6 values on $rowLabel.\n";
      } 
      $objHash->{$rowLabel} = \@values;
    }
    
    #If the CM has had the thresholds entered.    
    elsif (/GA\s+(\S+)/) { 
      $objHash->{hitGA} = $1;
    } elsif (/TC\s+(\S+)/) { 
      $objHash->{hitTC} = $1;
    } elsif (/NC\s+(\S+)/) { 
      $objHash->{hitNC} = $1;
    } elsif ( $_ =~ /^CM/) {
      #Reached the end of the CM header
      $$iRef = $i;
      last;
    } else {
      confess("Got a bad CM header line:$_\n"); 
    }
    $i++;
  }
  $cm->{cmHeader} = $objHash;
  #See if the CM has a field indicating that it has been calibrated.
  if (exists($cm->{cmHeader}->{ecmli})) {
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
      ) {
    push(@{ $cm->{cmBody} }, $cm->{rawcm}->[$i]);
    if ( $cm->{rawcm}->[$i] =~ /\s+\[\s+MATP\s+\d+\s+\]/ ) {
      $cm->{'match_pair_node'} = 1;
    } elsif ( $cm->{rawcm}->[$i] =~ /\/\//) {
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
  #ACC   RF99999
  #DESC  An RNA
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
  for ( $i = $$iRef; $i <= scalar(@{$cm->{rawcm}}); ) {  
    $_ = $cm->{rawcm}->[$i];
    if (my ($version) = $_ =~ /(HMMER3.*)/) {
      $objHash->{version} = $version;
    } elsif (/NAME\s+(\S+)/) { 
      $objHash->{name} =  $1 ;
    } elsif (/ACC\s+(\S+)/) { 
      $objHash->{acc} =  $1 ;
    } elsif (/DESC\s+(.+)/) { 
      $objHash->{desc} =  $1 ;
    } elsif (my ($length) = $_ =~ /^LENG\s+(\d+)/) {
      $objHash->{length} = $length;
    } elsif (/^MAXL\s+(\d+)$/) {
      $objHash->{maxl} = $1;
    } elsif ( my ($alpha) = $_ =~ /^ALPH\s+(\S+)/) {
      $objHash->{alpha} = $alpha;
    } elsif ( my ($rf) = $_ =~ /^RF\s+(no|yes)/) {
      $objHash->{rf} = ($rf eq "no") ? 0 : 1; 
    } elsif ( my ($mm) = $_ =~ /^MM\s+(no|yes)/) {
      $objHash->{mm} = ($mm eq "no") ? 0 : 1;
    } elsif ( my ($cons) = $_ =~ /^CONS\s+(no|yes)/) {
      $objHash->{cons} = ($cons eq "no") ? 0 : 1;
    } elsif (my ($cs) = $_ =~ /^CS\s+(no|yes)/ ) {
      $objHash->{cs} =  ($cs eq "no") ? 0 : 1; 
    } elsif (my ($map) = $_ =~ /^MAP\s+(no|yes)/) {
      $objHash->{map} = ($map eq "no") ? 0 : 1; 
    } elsif (my ($date) = $_ =~ /^DATE\s+(.*)/) {
      $objHash->{date} =  $date; 
    } elsif (my ($index, $line) = $_ =~ /^COM\s+\[(\d+)\]\s+(.*)$/) {
      $index--;
      $objHash->{com}->[$index] = $line;
    } elsif (my($noSeqs) = $_ =~ /^NSEQ\s+(\d+)/) {
      $objHash->{nSeq} = $noSeqs;
    } elsif ( my($effn) = $_ =~ /^EFFN\s+(\d+\.\d+)/) {
      #EFFN  4.966292
      $objHash->{effn} =  $effn ;
    } elsif ( my ( $cksum ) = $_ =~ /^CKSUM (\d+)/) {
      $objHash->{cksum} = $cksum ;
    } elsif (/GA\s+(\S+)\;/) { 
      $objHash->{hitGA} = $1;
    } elsif (/TC\s+(\S+)\;/) { 
      $objHash->{hitTC} = $1;
    } elsif (/NC\s+(\S+)\;/) { 
      $objHash->{hitNC} = $1;
    } elsif ( my ($msv_mu, $msv_lambda ) = $_ =~ /^STATS LOCAL MSV\s+(\S+)\s+(0\.\d+)/) {
      $objHash->{msvStats} = { mu => $msv_mu, lambda => $msv_lambda};
    } elsif ( my ($viterbi_mu, $viterbi_lambda ) = $_ =~ /^STATS LOCAL VITERBI\s+(\S+)\s+(0\.\d+)/) {
      $objHash->{viterbiStats} = { mu => $viterbi_mu, lambda => $viterbi_lambda };
    } elsif ( my ($forward_tau, $forward_lambda ) = $_ =~ /^STATS LOCAL FORWARD\s+(\S+)\s+(0\.\d+)/) {
      $objHash->{forwardStats} = {tau => $forward_tau, lambda => $forward_lambda};
    } elsif ( $_ =~ /^HMM\s+A/) {
      $$iRef = $i;
      last;
    } else {
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
      ) {
    push(@{ $cm->{hmmBody} }, $cm->{rawcm}->[$i]);
  }
}

sub parseScores {
  my ( $self, $file ) = @_;

  my @file;
  if ( ref($file) eq "GLOB" ) {
    @file = <$file>;
  } elsif ( ref($file) eq "ARRAY" ) {
    @file = @{$file};
  } else {
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
  } else {
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
    if ( length($l) > $expLen ) {
      if($l =~ /^WK /){
        warn "$l looks like a long WK line. ".
              "Please break the page title with a slash (/) and carry on to".
              " a second WK line. e.g.\nWK   long_line/\nWK   rest_of_the_line\n";
      }
      croak("\nGot a DESC line that was longer than $expLen, $file[$i]\n\n"
            . "-" x 80
            . "\n" );
    }
    
    chomp($l);
    #Do not permit tabs.
    if($l =~ /\t/ ){
      croak("A tab character or a terminal whitespace found in $l of your DESC file!\n");
    }
    if( $l =~ /\s+\n/ms){
      croak("A terminal whitespace found in $l of your DESC file!\n");
    }
    
    if ( $file[$i] =~ /^(AC|ID|AU|SE|TP|TX|SQ|CL|FR|SN)\s{3}(.*)$/ ) {
      if ( exists( $params{$1} ) ) {
        croak("Found second $1 line, only expecting one\n");
      }
      $params{$1} = $2;
      next;
    }elsif( $file[$i] =~ /^DE\s{3}(.*)$/){
      if ( exists( $params{DE} ) ) {
        croak("Found second DE line, only expecting one\n");
      }
      my $deLine = $1;
      my $error = 0;
      if($deLine =~ /.*\s$/){
        warn "\nFATAL: DE lines should not be plural!, please check and remove if plural\n";
        $error = 1;
      }
      if ($deLine =~ /.*\.$/){
         warn "\nFATAL: DE lines should not end with a fullstop\n";
         $error = 1;
      }
      if ($deLine =~ /.*\;$/){
         warn "\nFATAL: DE lines should not end with a semicolon\n";
         $error = 1;
      }
      
      if($deLine =~ /\t/){
        warn "\nFATAL: DE lines contains a tab character\n";
        $error = 1;
      }
      
      if($error){
        croak("\nError parsing DE line\n\n");
      }else{
        $params{'DE'} = $deLine;
      }
      
    }elsif($file[$i] =~ /^(T[P|X])\s{3}(.*)$/){ 
      my $tag = $1;
      my $tLine = $2;
      
      if ( exists( $params{$tag} ) ) {
        croak("Found second $tag line, only expecting one\n");
      }
     
      if ($tLine !~/\;$/){
        warn  "\nFATAL : Failed $tag line: no terminal semicolon";
      }else{
        $params{$tag} = $tLine;
      }
      
    }elsif($file[$i] =~ /^SS\s{3}(.*)$/ ){
      #Store the SS line
      my $ssLine = $1;
      
      my $error = 0;
      #Have we already seen one.
      if ( exists( $params{SS} ) ) {
        croak("Found second SS line, only expecting one\n");
      }
      
      #Examples of SS lines
      #Published; PMID:17630972
      #Published; PMID:17630972; Bateman A 
      #Predicted; RNAz;
      #Predicted; FSA; RNAalifold
      #Predicted; RNAalifold; Eberhardt R
      #Predicted; Eberhardt R  
      #Predicted; Moxon SJ, Daub J
      
      
      if ( $ssLine =~ /^SS   (Published|Predicted)\; (.*)/){
        my $class = $1;
        my @rest = split(/\; /, $2);
        foreach my $r (@rest) {
          if(/\t/){
            $error = 1;
            warn "\n FATAL: DESC file SS line contains a tab character.\n";
          }
          if(/\;/){
            $error = 1;
            warn "\n FATAL: DESC file SS line contains a semicolon.\n";
          }
          
        }
        
        if($class eq 'Published'){
          #The first element has to end
          my $pmid = shift @rest;
          if($pmid != /^PMID\:\d+$/){
            $error = 1;
            warn "\n FATAL: DESC file SS line wrongly formated. Expexted PMID after published;";
          }
          
          #Okay - now we can have either a method, author or author list.
          #We want methods first, but this is optional.  So, as soon as we
          #match something that looks like an author list, do not allow single
          #words.
          my $authors = 0;
          foreach my $r (@rest){
            if($r =~ /^\w+$/){
              #Should be a method.
              if($authors){
                $error = 1;
                warn "\n FATAL: DESC SS line wrongly formated. Expexted authors after a method.\n";
                
              }
            }elsif( $r =~ /^\w+ \w+(, \w+ \w+)*$/){
              #Looks like an author line.
              $authors++;
              #Once we have seen authors once, we can not see a method
            }else{
              #Unrecognized line
              $error = 1;
              warn "\n FATAL: DESC file SS line wrongly formated. Unrecognized part of SS lines.\n";
            }
          }
          
        }
      }
      
      if($error){
        croak("\nError parsing DESC SS line\n\n");
      }else{
        $params{'SS'} = $ssLine;
      }
    }elsif($file[$i] =~ /^PI\s{3}(.*)$/ ){
      #Store the PI line
      my $piLine = $1;
      
      #Have we already seen one.
      if ( exists( $params{PI} ) ) {
        croak("Found second PI line, only expecting one\n");
      }
      
      #Check the format as best as we can.
      my $error = 0;
      my @ids=split("\; ", $piLine);
      foreach my $i (@ids){
        if ($i=~/\;/){
          $error = 1;
          warn "\nFATAL: DESC file PI lines wrongly formatted, should be semicolon space separated list 'id1; ids2; id3' \n";      
          last;
        }
      }
      
      #If we get an error fail
      if($error){
        croak("\nError parsing DE line\n\n");
      }else{
        $params{'PI'} = $piLine;
      }
    }
#    BM   cmbuild -F CM SEED
#    CB   cmcalibrate --mpi CM
#    SM   cmsearch --cpu 4 --verbose --nohmmonly -E 1000 -Z 549862.597050 CM SEQDB
#         note: SM can be multiple lines, so we concatenate it freely,
#         then we validate it after all DESC lines have been parsed
   
    #This is quite strict to only allow parameters that Eric and Rob think will be used.
    elsif($file[$i] =~ /^BM\s{3}(.*)$/){
      my $bmLine = $1;
      #Have we already seen one.
      if ( exists( $params{BM} ) ) {
        croak("Found second BM line, only expecting one\n");
      }
      
      if($bmLine !~ /^cmbuild\s+(\-\w\s|\-\-\w+\s)*CM SEED$/){
        croak("\nFATAL: Your BM cmbuild line doesn't look right [$bmLine]\n");
      }else{
        $params{'BM'} = $bmLine;
      }
    }elsif($file[$i] =~ /^CB\s{3}(.*)$/){
      my $cbLine = $1;
      #Have we already seen one.
      if ( exists( $params{CB} ) ) {
        croak("Found second CB line, only expecting one\n");
      }
      
      if($cbLine !~ /^cmcalibrate\s+--mpi\sCM$/){
        croak("\nFATAL: Your CB cmcalibrate line doesn't look right [$cbLine]\n");
      }else{
        $params{'CB'} = $cbLine;
      }
    }elsif($file[$i] =~ /^SM\s{3}(.*)$/){
      my $sm = $1;
      if ($params{SM}) {
        $params{SM} .= " "; #simply concatenate it, it can be >1 line, so we validate later
      }
      $params{SM} .= $sm;
      next;
    }elsif ( $file[$i] =~ /^(TC|NC|GA)\s{3}(\S+)$/ ) {
      $params{ "CUT" . $1 } = $2;
    } elsif ( $file[$i] =~ /^\*\*\s{3}(.*)$/ ) {
      $params{private} .= " " if ( $params{private} );
      $params{private} .= $1;
    } elsif ( $file[$i] =~ /^WK\s{3}(\S+)$/ ) {
      my $page = $1;
      if ( $page =~ /^http.*\/(\S+)/ ) {

        #TODO - supress warn, if we can remove URL. Page title sufficient.
        carp( "$page going to be set to $1\n" );
        $page = $1;
      }
      my @bits;
      push(@bits, $page);
      if($page =~ /\/$/){ #Multi line article!
        foreach ( my $j = $i+1 ; $j <= $#file ; $j++ ) {
          if($file[$j] =~ /^WK\s{3}(\S+)$/){
            my $nextBitOfPage = $1;
            push(@bits, $nextBitOfPage);
            $page .= $nextBitOfPage;
            if($nextBitOfPage !~ /\/$/){
              $i = $j;
              last;
            }
          }
        }
      }
      
      $page =~ s/\///g; #Remove all / from the line.....
      if ( defined( $params{"WIKI"} ) ) {
        $params{"WIKI"}->{$page} = \@bits;
      } else {
        $params{"WIKI"} = { $page => \@bits };
      }
    } elsif ( $file[$i] =~ /^CC\s{3}(.*)$/ ) {
      my $cc = $1;
      
      if($cc =~ /\-\!\-/){
        croak("FATAL: DESC CC line  should not contain -!- , please remove\n");
      }
      
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
    } elsif ( $file[$i] =~ /^R(N|C)\s{3}/ ) {
      my $ref;
    REFLINE:
      foreach ( my $j = $i ; $j <= $#file ; $j++ ) {
        chomp($file[$j]);
        if($file[$j] =~ /\t/ or $file[$j] =~ /\s$/ ){
          croak("A terminal whitespace found in |$file[$j]| of your DESC file!\n");
        }
        if ( $file[$j] =~ /^(\w{2})\s{3}(.*?);?$/ ) {
          my $thisTag = $1;
          if ( $ref->{$1} ) {
            $ref->{$1} .= " $2";
          } else {
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
          } elsif (
                   (
                    !$refTags->{$nextTag}
                    or ( $nextTag eq "RN" or $nextTag eq "RC" )
                   )
                   and ( $thisTag eq "RL" )
                  ) {
            $i = $j;
            last REFLINE;
          } else {
            confess("Bad references fromat. Got $thisTag then $nextTag ");
          }
        }
      }
      $ref->{RN} =~ s/\[|\]//g;
      push( @{ $params{REFS} }, $ref );
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
          } else {
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
               {
                db_id => 'SO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   SO; (\d+); (.*);$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
               @{ $params{DBREFS} },
               {
                db_id => 'SO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   GO:(\d+) GO:(.*)$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
               @{ $params{DBREFS} },
               {
                db_id => 'GO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   GO[;:] (\d+); (.*);$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
               @{ $params{DBREFS} },
               {
                db_id => 'GO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   (MIPF); (MIPF\d+);?$/ ) {

          #MIPF; MIPF0000879
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (snornadb);\s(\S+?);?$/ ) {
        
          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (snoRNABase);\s(\S+?);?$/ ) {
          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );

        } elsif ( $file[$i] =~ /^DR   (PKBASE); (PKB\d+);?$/ ) {

          #PKBASE; PKB00277
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (snoopy); ([^;\s]*);?$/ ) {

          #snoopy;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (MIR); ([^;\s]*);?$/ ) {

          #MIR; MI0001007;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (URL); ([^;\s]*);?$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR/ ) {
          confess( "Bad reference line: unknown database |$file[$i]|.\n"
                   . "This may be fine, but we need to know the URL of the xref."
                   . "Talk to someone who knows about these things!\n" );
        } else {

          #We are now on to no DR lines, break out and go back on position
          $i--;
          last;
        }
        if ($com) {
          $params{DBREFS}->[ $#{ $params{DBREFS} } ]->{db_comment} = $com;
        }
      }
    } elsif ( $file[$i] =~ /^(AM|AL)/ ) {

      #old desc lines
      next;
    } else {
      chomp( $file[$i] );
      my $msg = "Failed to parse the DESC line (enclosed by |):|$file[$i]|\n\n"
        . "-" x 80 . "\n";

      #croak($msg);
      die $msg;
    }
  }

  if(exists($params{CC}) and defined($params{CC})){
    $params{CC} =~ s/\t/ /g;
    $params{CC} =~ s/\s+/ /g;
  }

  # validate SM syntax (need to do it here b/c it can be >1 lines)
  # should begin with: --cpu <n> --verbose (-E or -T) <f> -Z <f>
  if(exists($params{SM}) and defined($params{SM})) { 
    if($params{SM} !~ /^cmsearch\s+--cpu \d+ --verbose --nohmmonly -[E|T]\s+\d+(\.\d+)? -Z (\S+) (\-.* )?CM SEQDB$/){
      croak sprintf("\nFATAL: Your SM cmsearch line doesn't look right [%s]\n", $params{SM});
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
  } else {
    $descfile = "DESC";
  }
  
  if(-e $descfile){
    copy($descfile, $descfile.".$$");
  }
  open( D, ">$descfile" )
    or die "Could not open $descfile file for writing to\n"; 
  
  $Text::Wrap::unexpand = 0;
  $Text::Wrap::columns = 80;
  foreach my $tagOrder ( @{ $desc->order } ) {
    if ( length($tagOrder) == 2 ) {
      if ( $desc->$tagOrder and $desc->$tagOrder =~ /\S+/ ) {
        print D wrap( "$tagOrder   ", "$tagOrder   ", $desc->$tagOrder );
        print D "\n";
      }
    } else {
      next unless ( $desc->$tagOrder );
      if ( $tagOrder eq 'CUTTC' ) {
        printf D "TC   %.2f\n", $desc->$tagOrder;
      } elsif ( $tagOrder eq 'CUTGA' ) {
        printf D "GA   %.2f\n", $desc->$tagOrder;
      } elsif ( $tagOrder eq 'CUTNC' ) {
        printf D "NC   %.2f\n", $desc->$tagOrder;
      } elsif ( $tagOrder eq 'WIKI' ) {
        if ( ref( $desc->$tagOrder ) eq 'HASH' ) {
          my @pages = values( %{ $desc->$tagOrder } );
          foreach my $part (@pages) {
            foreach my $p (@$part){
              print D wrap( "WK   ", "WK   ", $p );
              print D "\n";
            }
          }
        }
      } elsif ( $tagOrder eq 'REFS' ) {
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
      } elsif ( $tagOrder eq 'DBREFS' ) {
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
          } else {
            print D "DR   " . $xref->{db_id} . "; " . $xref->{db_link} . ";\n";
          }
        }
      } elsif ( $tagOrder eq 'private' ) {
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
  } else {
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

# make scores 2D array from outlist, then write it by calling writeScores()
sub makeAndWriteScores {
  my ($self, $famObj, $outlistLocation) = @_;

  my $n    = 0;                 # number of hits
  my $nres = 0;                 # total number of residues in all hits
  my $name;                     # sequence name (name/start-end)
  my $tcode;                    # truncation code ("no", "5'", "3'", "5'&3'")
  my @scoresAA = ();            # 2D array of scores
  my $threshold = $famObj->DESC->CUTGA; # threshold
  my $stype;            # lowercase version of type ('seed' or 'full')

  if (! defined $threshold) {
    croak "ERROR GA not set";
  }
  open(OL, '<', $outlistLocation) || croak "ERROR unable to open $outlistLocation for reading";

  # process the outlist
  while (<OL>) {
    if (! m/^\#/) { 
      s/^\s+//; # remove leading whitespace
      my @elA = split(/\s+/);
      # example line
      # 105.50	4.9e-16	FULL	CP000970.1	   1234057	   1234142	     1	    85	no (SPECIES DATA REMOVED)
      # 0         1       2                3          4              5                 6       7   8  
      my ($bits, $evalue, $type, $id, $start, $end, $qstart, $qend, $tstr) = ($elA[0], $elA[1], $elA[2], $elA[3], $elA[4], $elA[5], $elA[6], $elA[7], $elA[8]);
      if ($bits < $threshold) {
        last;
      }
	    
      $name  = "$id/$start-$end";
      if ($tstr eq "no") {
        $tcode = 0;
      } elsif ($tstr eq "5'") {
        $tcode = 5;
      } elsif ($tstr eq "3'") {
        $tcode = 3;
      } elsif ($tstr eq "5'&3'") {
        $tcode = 53;
      } else {
        croak "ERROR invalid truncation string $tstr";
      }

      if ($type eq "SEED") {
        $stype = "seed";
      } elsif ($type eq "FULL") {
        $stype = "full";
      } else {
        croak "ERROR invalid type $type";
      }

      push(@{$scoresAA[$n]}, ($name, $start, $end, $id, $bits, $evalue, $qstart, $qend, $tcode, $stype));
      if ($start <= $end) {
        $nres += ($end - $start + 1);
      } else {
        $nres += ($start - $end + 1);
      }
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
    
=head2 writeTbloutDependentFiles

    Title    : writeTbloutDependentFiles
    Incept   : EPN, Wed Apr  3 05:51:56 2013 [added pod doc]
    Usage    : $io->writeTbloutDependentFiles($famObj, $rfdbh, $seedObj, $thr, $RPlotScriptPath)
    Function : Uses TBLOUT and database to create 'outlist', 'species', 'revoutlist', 'revspecies',
             : 'rin.dat' and 'rinc.dat' files.
    Args     : $famObj:  Bio::Rfam::Family object
             : $rfdbh:   database 
             : $seedmsa: Bio::Rfam::Family::MSA object
             : $ga:      GA threshold
             : $RPlotScriptPath: path for R plot script
             : $require_tax: '1' to require we find tax info in db for all hits
    Returns  : void
    Dies     : upon file input/output error

=cut

sub writeTbloutDependentFiles {
  my ($self, $famObj, $rfdbh, $seedmsa, $ga, $RPlotScriptPath, $require_tax) = @_;

  if (! defined $famObj->TBLOUT->fileLocation) { die "TBLOUT's fileLocation not set"; }

  my $tblI = $famObj->TBLOUT->fileLocation;
  my $rtblI = "REVTBLOUT";

  # output files
  my $outlistO = "outlist";
  my $speciesO = "species";
  my $revoutO  = "revoutlist";
  my $revspcO  = "revspecies";
  my $rinO     = "rin.dat";
  my $rincO    = "rinc.dat";

  # misc variables
  my $prv_bits   = 99999.00;    # previous bit score seen
  my $prv_evalue = 0.;          # previous E-value seen
  my %kingdomCounts;            # counts of hits in each kingdom
  my $printed_thresh;     # TRUE if threshold has already been printed
  my $outline;                  # dividing line

  # create nse HAA in msa for overlap checking
  $seedmsa->nse_createHAA();
  my $idx;
  my %seedseq_foundH = ();
  for ($idx = 0; $idx < $seedmsa->nseq; $idx++) { 
    $seedseq_foundH{$seedmsa->get_sqname($idx)} = 0;
  }

  # Prepare the queries for execution.
  my $sthDesc = $rfdbh->prepare_seqaccToDescription();
  my $sthTax  = $rfdbh->prepare_seqaccToSpeciesTaxStringAndID();

  # open OUTPUT files
  my $outFH; 
  my $spcFH; 
  my $revoutFH; 
  my $revspcFH; 
  open($outFH, "> $outlistO") || die "FATAL: failed to open $outlistO)\n[$!]";
  open($spcFH, "> $speciesO") || die "FATAL: failed to open $speciesO\n[$!]\n";   
  open(RIN,"> $rinO") || die "FATAL: failed to open $rinO\n[$!]\n";   
  printf RIN "bits\ttype\ttax\n";
  open(RINc,"> $rincO") || die "FATAL: failed to open rincO\n[$!]\n";   
  printf RINc "cnt\ttax\n";
    
  # Paul's comment:
  # If you don't like this you can fuck off!:
  # Shell grep & sort are a hell of a lot less resource greedy than perl's equivalents.
  
  # parse REVTBLOUT
  my $tblline;
  my $rev_evalue = "";
  my $chunksize = 100;
  my $nlines = 0;
  if(-s $rtblI) { 
    my @rev_outAA = (); # we'll fill this with data for revoutlist
    my @rev_spcAA = (); # we'll fill this with data for revspecies
    open(RTBL, "grep -v ^'#' $rtblI | sort -nrk 15 | ") || croak "FATAL: could not open pipe for reading $rtblI\n[$!]";
    while ($tblline = <RTBL>) {
      my ($bits, $evalue, $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description, $ncbiId, $species, $taxString) = 
          processTbloutLine($tblline, $sthDesc, $sthTax, 1, $require_tax); # '1' says: yes this is a reversed search
      push(@{$rev_outAA[$nlines]}, ($bits, $evalue, "REV", $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description));
      push(@{$rev_spcAA[$nlines]}, ($bits, $evalue, "REV", $name, $ncbiId, $species, $taxString));
      $nlines++;
      if($rev_evalue eq "") { # first line
        $rev_evalue = $evalue; 
        open($revoutFH, "> $revoutO") || die "FATAL: failed to open $revoutO\n[$!]\n";   
        open($revspcFH, "> $revspcO") || die "FATAL: failed to open $revspcO\n[$!]\n";   
      }
      if($nlines % $chunksize == 0) { 
        writeOutlistOrSpeciesChunk($revoutFH, \@rev_outAA, 1);
        writeOutlistOrSpeciesChunk($revspcFH, \@rev_spcAA, 0);
        @rev_outAA = ();
        @rev_spcAA = ();
        $nlines = 0;
      }
    }
    if ($nlines > 0) { 
      writeOutlistOrSpeciesChunk($revoutFH, \@rev_outAA, 1);
      writeOutlistOrSpeciesChunk($revspcFH, \@rev_spcAA, 0);
    }
    close($revoutFH) if(defined($revoutFH));
    close($revspcFH) if(defined($revspcFH));
  }

  # parse TBLOUT
  my @outAA = (); # we'll fill these with data for outlist
  my @spcAA = (); # we'll fill these with data for species
  my $have_all_tax_info = 1; # set to FALSE if we fail to find a 
  $nlines = 0;
  open(TBL, "grep -v ^'#' $tblI | sort -nrk 15 | ") || croak "FATAL: could not open pipe for reading $tblI\n[$!]";
  while ($tblline = <TBL>) {
    my ($bits, $evalue, $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description, $ncbiId, $species, $taxString, $got_tax) = 
        processTbloutLine($tblline, $sthDesc, $sthTax, 0, $require_tax); #'0' says: no this is not a reversed search 

    if($taxString eq "-") { $have_all_tax_info = 0; }
    my $domainKingdom = Bio::Rfam::Utils::tax2kingdom($taxString . '; ' . $species . ';');
    $kingdomCounts{$domainKingdom}++;

    # determine seqLabel
    my $seqLabel = 'FULL';
    my ($seed_seq, $overlapExtent) = $seedmsa->nse_overlap($name . "/" . $start . "-" . $end);

    if ($seed_seq ne "") { 
      $seedseq_foundH{$seed_seq}++;
      if ($overlapExtent > 0.1) {
        $seqLabel = 'SEED';
      }
    }
    if (($bits < $ga) && ($seqLabel ne 'SEED')) {
      $seqLabel = 'NOT';
    }

    # print out threshold line if nec
    if ( $bits < $ga && $ga<=$prv_bits) {
      $outline = commentLineForOutlistOrSpecies (" CURRENT THRESHOLD: $ga BITS ");
      push(@{$outAA[$nlines]}, ($outline));
      push(@{$spcAA[$nlines]}, ($outline));
      printf RIN  "%0.2f\tTHRESH\t.\n", $ga;
      $printed_thresh=1;
      $nlines++;
    }
    if ( $rev_evalue ne "" && $evalue > $rev_evalue && $prv_evalue <= $rev_evalue) {
      $outline = commentLineForOutlistOrSpecies(" BEST REVERSED HIT E-VALUE: $rev_evalue ");
      push(@{$outAA[$nlines]}, ($outline));
      push(@{$spcAA[$nlines]}, ($outline));
      $nlines++;
    }
    $prv_bits = $bits;
    $prv_evalue = $evalue;

    push(@{$outAA[$nlines]}, ($bits, $evalue, $seqLabel, $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description));
    push(@{$spcAA[$nlines]}, ($bits, $evalue, $seqLabel, $name, $ncbiId, $species, $taxString));
    $nlines++;
    if($nlines % $chunksize == 0) { 
      writeOutlistOrSpeciesChunk($outFH, \@outAA, 1);
      writeOutlistOrSpeciesChunk($spcFH, \@spcAA, 0);
      @outAA = ();
      @spcAA = ();
      $nlines = 0;
    }
    printf RIN  "%0.2f\t%0.6s\t$domainKingdom\n", $bits, $seqLabel;
  } # closes 'while($tblline = <TBL>)'

  # If we have any sequences 
  if (! defined $printed_thresh) {
    $outline = commentLineForOutlistOrSpecies(" CURRENT THRESHOLD: $ga BITS ");
    push(@{$outAA[$nlines]}, ($outline));
    push(@{$spcAA[$nlines]}, ($outline));
    printf RIN "%0.2f\tTHRESH\t\.\n", $ga;
    $nlines++;
  }
  if ($rev_evalue eq "") { 
    if(-e $rtblI) { $outline = commentLineForOutlistOrSpecies(" NO REVERSED HITS (NO HITS FOUND IN REVERSED DB) "); }
    else          { $outline = commentLineForOutlistOrSpecies(" NO REVERSED HITS (NO REVERSED SEARCH PERFORMED) "); }
    push(@{$outAA[$nlines]}, ($outline));
    push(@{$spcAA[$nlines]}, ($outline));
    $nlines++;
  }
  if ($nlines > 0) { 
    writeOutlistOrSpeciesChunk($outFH, \@outAA, 1);
    writeOutlistOrSpeciesChunk($spcFH, \@spcAA, 0);
  }
    
  foreach my $king ( sort{ $a cmp $b } keys %kingdomCounts) {
    printf RINc  "%d\t$king\n", $kingdomCounts{$king};
  }
    
  # complain loudly if seed sequences are missing from the output, if 
  # we have tax info for all hits (i.e. if we're doing a standard Rfam 
  # search)
  my @warningsA = ();
  if($require_tax) { 
    foreach my $n (keys %seedseq_foundH) {
      if ($seedseq_foundH{$n} < 1) {
        $outline = "WARNING: SEED sequence $n was not found (not in TBLOUT)\n";
        warn $outline;
        push(@warningsA, $outline);
      }
    }
    if (scalar(@warningsA) > 0) { 
      open(W, ">warnings") || croak "unable to open warnings file for writing";
      foreach my $warning (@warningsA) { 
        print W $warning;
      }
      close(W);
    }
  }
    
  close(TBL);
  close($outFH);
  close($spcFH);
  close(RIN);
  close(RINc);

  # run R script, if we have tax info for ALL hits 
  if($have_all_tax_info) { 
    Bio::Rfam::Utils::run_local_command("R CMD BATCH --no-save $RPlotScriptPath");
  }
  # Remove the plot_outlist.Rout, rin.dat, and rinc.dat files, 
  # These are really only relevant if the R command failed
  # (returned non-zero status), in which case run_local_command() 
  # would have die'd (and we'd never have gotten to this point).
  if(-e "plot_outlist.Rout") { unlink "plot_outlist.Rout"; } 
  if(-e "rin.dat")           { unlink "rin.dat"; }
  if(-e "rinc.dat")          { unlink "rinc.dat"; }
  return;
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
  my ($i, $j);                  # counters
  my $aR;                       # convenience ptr to an array
  my $wid;                      # width of a field
  my @widthA = ();              # max width of each field

  open(SC, ">SCORES") || croak "ERROR unable to open SCORES for writing"; 

  for ($j = 0; $j < $nels; $j++) {
    $widthA[$j] = 0;
  }

  for ($i = 0; $i < $scoresObj->numRegions; $i++) { 
    $aR = $scoresAAR->[$i];
    for ($j = 0; $j < $nels; $j++) { 
      $wid = length($aR->[$j]);
      $widthA[$j] = ($widthA[$j] > $wid) ? $widthA[$j] : $wid;
    }
  }		

  for ($i = 0; $i < $scoresObj->numRegions; $i++) { 
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

=head2 processTbloutLine

    Title    : processTbloutLine
    Incept   : EPN, Tue Apr 23 13:23:50 2013
    Usage    : processTbloutLine($tblline, $sthDesc, $sthTax, $is_reversed)
    Function : Helper function for writeTbloutDependentFiles(). 
             : Processes a TBLOUT line and returns all the useful information
             : in it, after looking up description and taxonomic information
             : in the database. 
    Args     : $tblline:     a tabular output line from a cmsearch --tblout file.
             : $sthDesc:     prepared database query for fetching description ($rfdbh->prepared_seqaccToDescription())
             : $sthTax:      prepared database query for fetching tax info ($rfdbh->prepared_seqaccToSpeciesTaxStringAndID())
             : $is_reversed: '1' for a line from a reversed search
             : $require_tax: '1' to require each hit desc and tax info is found in database (die if it is not)
             :               '0' to fill all desc and tax info with '-' if not found in database
    Returns  : list of the following values:
             : $bits:         bit score for the hit  
             : $evalue:       E-value of the hit     
             : $name:         name of the sequence the hit is in
             : $start:        start position of the hit
             : $end:          end position of the hit
             : $qstart:       start position on the query model
             : $qend:         end position on the query model
             : $trunc:        truncation string for hit
             : $shortSpecies: short string for species, '-' if not found in db
             : $description:  description of seq, '-' if not found in db
             : $ncbiId:       NCBI id for hit, '-' if not found in db
             : $species:      species string for hit, '-' if not found in db
             : $taxString:    taxonomic string for hit, '-' if not found in db
             : $got_tax:      '1' if tax info is there, '0' if not
    Dies     : upon file input/output error 

=cut
sub processTbloutLine { 
  my ($tblline, $sthDesc, $sthTax, $is_reversed, $require_tax) = @_;

  ## example TBLOUT line:
  ###target name         accession query name           accession mdl mdl from   mdl to seq from   seq to strand trunc pass   gc  bias  score   E-value inc description of target
  ###------------------- --------- -------------------- --------- --- -------- -------- -------- -------- ------ ----- ---- ---- ----- ------ --------- --- ---------------------
  ## AAAA02006309.1      -         RF00014              -          cm        1       85      192      105      -    no    1 0.44   0.0   86.0   1.6e-11 !   Oryza sativa Indica Group chromosome 2 Ctg006309, whole genome shotgun sequence.
  my @tblA = split(/\s+/, $tblline);
  my ($name, $qstart, $qend, $start, $end, $strand, $trunc, $bits, $evalue) = ($tblA[0], $tblA[5], $tblA[6], $tblA[7], $tblA[8], $tblA[9], $tblA[10], $tblA[14], $tblA[15]);
  if ($strand eq "+") { $strand =  1; }
  else                { $strand = -1; }

  my ($description, $species, $shortSpecies, $domainKingdom, $taxString, $ncbiId);
  # potentially remove '-shuffled' suffix if nec from 'reversed searches'
  my $name2lookup = $name;
  if($is_reversed) { $name2lookup =~ s/\-shuffled$//; }

  # fetch description
  my $found_in_db = 1;
  if(defined $sthDesc) { 
    $sthDesc->execute($name2lookup);
    my $res = $sthDesc->fetchall_arrayref;
    foreach my $row (@$res) {
      $description .= $row->[0];
    }
  }
  
  # fetch species, taxonomy string and ncbi id
  if(defined $sthTax) { 
    $sthTax->execute($name2lookup);
    my $rfres = $sthTax->fetchall_arrayref;
    if(defined $rfres) { 
      foreach my $row (@{$rfres}) {
        if (scalar(@{$row}) < 4) { die "ERROR problem fetching tax info for $name2lookup"; }
        $species       .= $row->[0];
        $shortSpecies  .= $row->[1];
        $taxString     .= $row->[2];
        $ncbiId        .= $row->[3];
      }
    }
  }

  if($require_tax) { 
    if(! defined $description)  { die "ERROR unable to fetch description for $name"; }
    if(! defined $species)      { die "ERROR unable to fetch species for $name"; }
    if(! defined $shortSpecies) { die "ERROR unable to fetch shortSpecies for $name"; }
    if(! defined $taxString)    { die "ERROR unable to fetch taxString for $name"; }
    if(! defined $ncbiId)       { die "ERROR unable to fetch ncbiId for $name"; }
  }
  else { 
    if(! defined $description)  { $description  = "-"; }
    if(! defined $species)      { $species      = "-"; }
    if(! defined $shortSpecies) { $shortSpecies = "-"; }
    if(! defined $taxString)    { $taxString    = "-"; }
    if(! defined $ncbiId)       { $ncbiId       = "-"; }
  } 

  return ($bits, $evalue, $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description, $ncbiId, $species, $taxString);
}


# append chunk of lines to 'outlist' or 'species' file 
sub writeOutlistOrSpeciesChunk {
  my ($fh, $aaR, $is_outlist) = @_;

  my ($i, $j, $k);              # counters
  my $aR;                       # convenience ptr to an array
  my $wid;                      # width of a field
  my @widthA = ();              # max width of each field
  my $nlines = scalar(@{$aaR}); # number of lines we'll print

  my @headA = ();
  if($is_outlist) { 
    @headA = ("# bits", "evalue", "seqLabel", "name", "start", "end", "qstart", "qend", "trunc", "species", "description");
  }
  else { # species data 
    @headA = ("# bits", "evalue", "seqLabel", "name", "ncbiId", "species", "taxString");
  }
    
  my $nels = scalar(@headA);

  # determine max width of each column
  for ($j = 0; $j < $nels; $j++) { 
    $widthA[$j] = length($headA[$j]);
  }
  for ($i = 0; $i < $nlines; $i++) { 
    $aR = $aaR->[$i];
    if($aR->[0] !~ m/^\#/) { # IMPORTANT: skip comment lines 
      for ($j = 0; $j < $nels; $j++) { 
        $wid = length($aR->[$j]);
        $widthA[$j] = ($widthA[$j] > $wid) ? $widthA[$j] : $wid;
      }
    }	
  }	

  # print header lines
  printf $fh ("#\n");
  for($j = 0; $j < $nels; $j++) { 
    printf $fh ("%-*s", $widthA[$j], $headA[$j]);
    if($j < ($nels-1)) { printf $fh "  "; }
  }
  printf $fh ("\n");
  for($j = 0; $j < $nels; $j++) { 
    my $dashline = "";
    $wid = $widthA[$j];
    if($j == 0) { $dashline = "#"; $wid--; }
    for($k = 0; $k < $wid; $k++) { $dashline .= "="; }
    printf $fh ("%-*s", $widthA[$j], $dashline);
    if($j < ($nels-1)) { printf $fh "  "; }
  }
  printf $fh ("\n");

  for ($i = 0; $i < $nlines; $i++) { 
    $aR = $aaR->[$i];
    if($aR->[0] =~ m/^\#/) { #comment line, just print it
      print $fh ("$aR->[0]\n");
    }
    else { 
      if($is_outlist) { 
        printf $fh ("%*s  %*s  %*s  %-*s  %*s  %*s  %*s  %*s  %*s  %-*s  %-s\n", 
                    $widthA[0], $aR->[0], 
                    $widthA[1], $aR->[1], 
                    $widthA[2], $aR->[2], 
                    $widthA[3], $aR->[3], 
                    $widthA[4], $aR->[4], 
                    $widthA[5], $aR->[5], 
                    $widthA[6], $aR->[6], 
                    $widthA[7], $aR->[7], 
                    $widthA[8], $aR->[8], 
                    $widthA[9], $aR->[9],
                    $aR->[10]); # final column doesn't need to be fixed-width, just flush left
      }
      else { # species line
        printf $fh ("%*s  %*s  %*s  %-*s  %*s  %-*s  %-s\n",
                    $widthA[0], $aR->[0], 
                    $widthA[1], $aR->[1], 
                    $widthA[2], $aR->[2], 
                    $widthA[3], $aR->[3], 
                    $widthA[4], $aR->[4], 
                    $widthA[5], $aR->[5], 
                    $aR->[6]); # final column doesn't need to be fixed-width, just flush left
      }
    }
  }
}

# return a comment line for output to an outlist or species file
sub commentLineForOutlistOrSpecies {
  my ($str) = $_[0];

  my $tot_len = 120;
  my $pre_len = ($tot_len - length($str)) / 2;
  my $line = "";
  my $i;
  for($i = 0; $i < $pre_len; $i++) { $line .= "#"; }
  $line .= $str;
  $i += length($str);
  for(; $i < $tot_len; $i++) { $line .= "#"; }

  return $line;
}


#-------------------------------------------------
    
=head2 writeTaxinfoFromOutlistAndSpecies

    Title    : writeTaxinfoFromOutlistAndSpecies
    Incept   : EPN, Mon Aug 19 15:04:50 2013
    Usage    : $io->writeTaxinfoFromOutlistAndSpecies($ga, $evalue, $desc, $n2print, $user_level2print, $do_nsort)
    Function : Uses outlist and species to create 'taxinfo' file.
             :
             : $infoHHR, and $groupOHAR must supplied by caller,
             : probably filled by a $io->parseOutlistAndSpecies() call.
             :
             : The purpose of 'taxinfo' is to concisely describe the taxonomic
             : groups represented by all of the hits for a family. To do this
             : the taxonomic strings (e.g. Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Pan.)
             : are summarized by taking a prefix and a 'prefix level' 
             : (the number of tokens in the prefix) that defines 5 distinct
             : taxa groups (at that prefix level) that comprise all SEED seqs.
             : Families with large phylogenetic breadth will have lower
             : prefix levels. Those with narrow breadth will have higher.
             : We go through considerable trouble to do this because
             : we want the taxinfo output file to be relatively short and
             : easy to digest by a curator. Much of the complexity of the 
             : code for this subroutine is for storing the taxonomic 
             : strings in such a way that enables us to find the desired
             : prefix level and for finding that prefix level. 
             : prefix level as well as for actually finding it. 
             :
             : Once the desired prefix level is found, the remainder
             : of the subroutine outputs a sorted list of the taxonomic
             : groups. This is also complicated because we first print
             : out those groups that contain >= 1 seed seqs, in order of
             : decreasing minimum E-value in the group.  Next, those
             : (not yet printed) that contain at least 1 full hit above
             : GA threshold in order of decreasing minimum E-value in
             : the group and then all remaining groups (with only
             : 'other' seqs (not in seed nor full)).
             : 
    Args     : $infoHHR:   ref to 2D hash, key 1: name/start-end (nse), key 2: "rank", "bitsc", "evalue", "sspecies" or "taxstr"
             : $groupOHAR: ref to hash of arrays, nse in score rank order, by group
             : $desc:      Bio::Rfam::Family::DESC object
             : $ga:        GA bit score threshold
             : $evalue:    E-value for $ga
             : $n2print:   target number of SEED taxonomy prefixes to print (-n2print from rfmake.pl)
             : $l2print:   print all unique prefixes of this length, if != 0 (-l2print from rfmake.pl)
             : $do_nsort:  '1' to sort output by counts (-nsort from rfmake.pl)
    Returns  : void
    Dies     : upon file input/output error

=cut

sub writeTaxinfoFromOutlistAndSpecies {
  my ($self, $infoHHR, $groupOHAR, $desc, $ga, $evalue, $n2print, $user_level2print, $do_nsort) = @_;

  ####################################################################
  # Set parameters to their defaults prior to parsing cmd line options
  ####################################################################
  my @groupOA     = ("S", "F", "O"); # order of groups
  my @groupaddA   = ("100", "10", "0"); # how much to add to order of groups
  my $level2print = 1;
  my $emax        = 10;
  my $min_level2print = 3;
  my $taxstr;             # full taxonomy string from species
  my $prefix;             # a taxonomic string prefix 
  my $prv_prefix;         # previous prefix
  my $group;              # group, either 'S' for SEED, 'F' for FULL, or 'O' for OTHER
  my $name;               # name of hit
  my ($i, $j);            # counters
  my %ngroup = ();        # key: group name, value number of seqs in a group
  my $level;              # number of tokens in prefix; e.g. Eukaryota; Metazoa; Mollusca; == 3)
  my $parent_level;       # number of tokens in full parent string this prefix comes from
  my $maxlevel     = 1;   # maximum observed level
  my $nprint       = 5;   # number of prefixes we want to print for SEED group, we find the $level2print that achieves this
  my $nprint_actual;      # actual number of prefixes we will print for SEED group
  my $max_ngroup   = 0;   # maximum count of any prefix in any group
  my $nprefix      = 0;   # number of prefixes to print
  my $best_prefix;        # current prefix to print 
  my $eff_Eexp;           # effective E-value exponent, used for sorting prefixes for printing
  my $min_eff_Eexp;       # min effective E-value exponent, used for sorting prefixes for printing
  my $eff_ct;             # effective count, used for sorting prefixes for printing
  my $max_eff_ct;         # max effective count, used for sorting prefixes for printing
  my $group_string;       # string summarizing what groups a prefix is present in
  my $cur_evalue;         # current e-value
  my $cur_exp;            # current e-value exponent
  my $maxE = 0;           # maximum E-value 
  my %pfix_ctHH     = (); # 1D key group, 2D key prefix, value: count (number of hits for group with taxonomic prefix)
  my %pfix_levelHH  = (); # 1D key group, 2D key prefix, value: prefix level (number of tokens in prefix; e.g. Eukaryota; Metazoa; Mollusca; == 3)
  my %pfix_plevelHH = (); # 1D key group, 2D key prefix, value: number of levels in parent of this prefix
  my %pfix_minEHH   = (); # 1D key group, 2D key prefix, value: minimum E-value of all hits for this group and prefix
  my %pfix_minEexpHH= (); # 1D key group, 2D key prefix, value: minimum E-value EXPONENT of all hits for this group and prefix
  my %toprintH      = (); # key is prefix, value is 1 if we want to print it
  my %toaddH        = (); # value to add to each prefix count when sorting prior to printing
  my %printedH      = (); # key: prefix, value, '1' if we've already printed prefix
  my %nprintedH     = (); # key: group, value: number of total hits all printed prefixes include
  my %prvH          = (); # key: group, value: '1' if previous prefix existed in group 
  my %ngroupH       = (); # key: $group, value: number of sequences in a group
  my @level_ctA     = (); # number of unique taxonomic prefixes at each level 
  my @elA           = (); # for defining taxonomy string prefixes
  my @outputA = (); # all output lines will be pushed here and actually output at the end

  # fill %pfix_ctHH and %pfix_levelHH with counts/level of each prefix for each group
  foreach $group (@groupOA) { # "S", "F", or "O", seed, full or other
    if(! (exists $groupOHAR->{$group})) { 
      $ngroupH{$group} = 0;
    } 
    else { 
      $ngroupH{$group} = scalar(@{$groupOHAR->{$group}});
      
      foreach $name (@{$groupOHAR->{$group}}) { 
        $taxstr = $infoHHR->{$name}{"taxstr"};
        $cur_evalue = $infoHHR->{$name}{"evalue"};
        #@elA = split(" ", $taxstr);
        @elA = split(";", $taxstr);
        $parent_level = scalar(@elA);
        $prv_prefix = "";
        $prefix     = "";
        for($i = 0; $i < scalar(@elA); $i++) { 
          $prefix = $prv_prefix;
          if($prv_prefix ne "") { $prefix .= ";"; }
          $prefix .= $elA[$i];
          $prv_prefix = $prefix;
          $pfix_ctHH{$group}{$prefix}++;
          $pfix_levelHH{$group}{$prefix}  = $i+1;
          $pfix_plevelHH{$group}{$prefix} = $parent_level;
          $cur_exp = _taxinfo_get_sortable_exponent($cur_evalue);
          
          if((! exists $pfix_minEHH{$group}{$prefix}) ||
             ($cur_exp < $pfix_minEexpHH{$group}{$prefix})) { 
            $pfix_minEexpHH{$group}{$prefix} = $cur_exp;
            $pfix_minEHH{$group}{$prefix}    = $cur_evalue;
            if($cur_exp < -1000) { die "ERROR E-value exponent fell below -1000, shouldn't happen"; }
          }
          if(($i+1) > $maxlevel) { 
            $maxlevel = $i+1;
          }
        }
      }
      
      ##################################################
      # Determine 'level2print', the number of prefixes we'll use for taxonomic strings we output.
      # We do this only once for all groups, starting with the first group
      # with > 0 seqs (usually SEED unless a new family)
      #
      if((($group eq "S" && $ngroupH{$group} > 0) ||
          ($group eq "F" && $ngroupH{"S"}   == 0) ||
          ($group eq "O" && $ngroupH{"S"}   == 0 && $ngroupH{"F"} == 0))) {
        # determine number of unique prefixes at each taxonomy level
        @level_ctA = ();
        for($i = 0; $i <= $maxlevel; $i++) { $level_ctA[$i] = 0; }
        foreach $prefix (sort keys(%{$pfix_ctHH{$group}})) { 
          $level = $pfix_levelHH{$group}{$prefix};
          $level_ctA[$level]++;
          # if prefix level == parent level, increase level_ctA[i] for j == level..maxlevel
          # b/c we'll have to print $prefix if level2print is any value level..maxlevel
          if($level == $pfix_plevelHH{$group}{$prefix}) { 
            for($j = $level+1; $j <= $maxlevel; $j++) { 
              $level_ctA[$j]++;
            }
          }
        }
        $level2print = 0;
        $nprint_actual = $level_ctA[0];
        
        # determine maximum taxonomy level at which no more than $nprint prefixes exist
        while(($level2print < $maxlevel) && ($level_ctA[($level2print+1)] < $nprint)) { 
          $level2print++; 
          if($level_ctA[$level2print] > $nprint_actual) { 
            $nprint_actual = $level_ctA[$level2print];
          }
        }
        
        #printf("nprint_actual: $nprint_actual\n");
        #printf("level2print:   $level2print\n");

        # nprint_actual is the number of prefixes we'll print for seed.
        # Now, pick min taxonomy level that has exactly nprint_actual prefixes,
        # to minimize length of prefix strings
        while(($level2print >= 1) && ($level_ctA[$level2print-1] >= $nprint_actual)) { 
          #printf("level_ctA[%d] is %d (<= %d) decreasing level2print by 1\n", $level2print-1, $level_ctA[$level2print-1], $nprint_actual);
          $level2print--; 
        }
        # if level2print is less than our minimum, use that
        if($level2print < $min_level2print) { $level2print = $min_level2print; }
      } # end of if statement for determining level2print
    } # end of else, entered if >= 1 member in group
  } # end of 'foreach group'
  
  # user defined what level2print they want, ignore the one we just determined
  if($user_level2print > 0) { $level2print = $user_level2print; }
  
  # determine max count of any prefix in any group,
  # label which ones to print in %toprintH and 
  # determine and max length of all prefix strings
  my $max_length = length("# taxonomy string prefix (xx levels)");
  foreach $group (@groupOA) { 
    if($ngroupH{$group} != 0) { 
      if($ngroupH{$group} > $max_ngroup) { 
        $max_ngroup = scalar(@{$groupOHAR->{$group}});
      }
      
      # keep track of which prefixes to print
      foreach $prefix (sort keys(%{$pfix_ctHH{$group}})) { 
        # There's two cases in which we'll print this prefix:
        # (1): level of prefix == level2print
        # (2): parent level of prefix == level of prefix AND
        #      parent level of prefix <  level2print 
        #
        # Case 2 is tricky, it makes us (correctly) print 
        # prefixes like "metatgenomes; organismal metagenomes;" (parent level 2)
        # even when level2print is something like 5. (Otherwise, we wouldn't
        # print it).
        # 
        if(($pfix_levelHH{$group}{$prefix} == $level2print) || 
           (($pfix_plevelHH{$group}{$prefix} == $pfix_levelHH{$group}{$prefix}) && 
            ($pfix_plevelHH{$group}{$prefix} < $level2print))) { 
          $toprintH{$prefix} = 1;
          
          if(length($prefix) > $max_length) { 
            $max_length = length($prefix);
          }
        }
      }
    }
  }
  # Now, print the prefixes and their counts out in a particular order
  # First print all prefixes with >=1 members in SEED in sorted order
  # from low to high minimum E-value in group, then print any with >= 1 members in FULL
  # from low to high minimum E-value in group, then print any with >= 1 members in OTHER
  # from low to high minimum E-value in group.
  # (If $do_nsort is '1' we'll sort by total counts per group not 
  # minimum E-value).

  my $div_line = "#";
  my $div_length = $max_length + 2 + 3 + 6 + 13 + 6 + 13 + 6 + 13;
  my $printed_seed_sep = 0;
  
  if($ngroupH{"S"} == 0) { $printed_seed_sep = 1; }
  for($i = 0; $i < ($div_length-1); $i++) { $div_line .= "="; }
  
  push(@outputA, "$div_line\n");
  push(@outputA, "# taxinfo: created by 'rfmake.pl', run 'rfmake.pl -h' for a list of cmd-line options that modify behavior\n");
  push(@outputA, "$div_line\n");
  push(@outputA, sprintf("# family-id:       %s\n", $desc->ID));
  push(@outputA, sprintf("# family-acc:      %s\n", $desc->AC));
  push(@outputA, sprintf("# pwd:             %s\n", getcwd));
  push(@outputA, sprintf("# GA bit-score:    $ga\n"));
  push(@outputA, sprintf("# GA E-value:      %6.1g\n", $evalue));
  push(@outputA, sprintf("# SEED:            %-5d hits (present in SEED)\n", $ngroupH{"S"}));
  push(@outputA, sprintf("# FULL:            %-5d hits (not in SEED, with bitsc >= $ga,  E-value <= %6.1g)\n", $ngroupH{"F"}, $evalue));
  push(@outputA, sprintf("# OTHER:           %-5d hits (not in SEED or FULL, with E-value <= $emax)\n", $ngroupH{"O"}));
  push(@outputA, sprintf("#\n"));

  # first print column headings
  my $tax_header = sprintf("# taxonomy string prefix (%d levels)", $level2print);
  my $tax_uline  = "#";
  for($i = 0; $i < $max_length-1; $i++) { $tax_uline .= "-"; }

  push(@outputA, sprintf("%-*s  %3s      %-13s      %-13s      %-13s\n", $max_length, "#", "", "    SEED", "    FULL", "   OTHER"));
  push(@outputA, sprintf("%-*s  %3s      %-13s      %-13s      %-13s\n", $max_length, "#", "", "-------------", "-------------", "-------------"));
  push(@outputA, sprintf("%-*s  %3s      %5s  %6s      %5s  %6s      %5s  %6s \n", $max_length, $tax_header, "mem", "ct", "minE", "ct", "minE", "ct", "minE"));
  push(@outputA, sprintf("%-*s  %3s      %5s  %6s      %5s  %6s      %5s  %6s \n", $max_length, $tax_uline, "---", "-----", "------", "-----", "------", "-----", "------"));
  
  # this simplifies sorting prefixes when printing
  if($do_nsort) { 
    $toaddH{"S"} = 2 * $max_ngroup;
    $toaddH{"F"} = 1 * $max_ngroup;
    $toaddH{"O"} = 0;
  }
  else { 
    $toaddH{"S"} = -30000;
    $toaddH{"F"} = -20000;
    $toaddH{"O"} = -10000;
  }
  
  $nprefix = scalar(keys %toprintH);
  
  # this is a laborious sort, go through all remaining prefixes that we haven't 
  # printed yet, looking for the correct one to print next.
  for($i = 1; $i <= $nprefix; $i++) { 
    $max_eff_ct   = 0;
    $min_eff_Eexp = 1000000000;
    $best_prefix = "";
    foreach $prefix (keys %toprintH) {
      if(! exists ($printedH{$prefix})) {
        foreach $group (@groupOA) { 
          if(exists ($pfix_ctHH{$group}{$prefix})) {
            $eff_ct   = $pfix_ctHH{$group}{$prefix} + $toaddH{$group};
            $eff_Eexp = $pfix_minEexpHH{$group}{$prefix} + $toaddH{$group};
            
            if($do_nsort) { # sort by maximum count
              if($eff_ct > $max_eff_ct) { 
                #printf("reset max_eff_ct as $max_eff_ct, $prefix\n");
                $max_eff_ct = $eff_ct;
                $best_prefix = $prefix;
              }
            }
            else { # sort by minimum E-value as first key, count as second
              if(($eff_Eexp <  $min_eff_Eexp) || 
                 ($eff_Eexp == $min_eff_Eexp && $eff_ct > $max_eff_ct)) {
                #printf("reset min_eff_Eexp as $eff_Eexp, $prefix ($pfix_minEexpHH{$group}{$prefix}  $pfix_minEHH{$group}{$prefix})\n");
                $min_eff_Eexp = $eff_Eexp;
                $max_eff_ct   = $eff_ct;
                $best_prefix  = $prefix;
              }
            }
          }
        }
      }
    }
    #printf("found next best prefix: $best_prefix ($min_eff_Eexp)\n");
    
    # determine if we need a newline because prev line included >=1 count in SEED or FULL and next line will not
    if($i > 1) { 
      if($prvH{"S"} == 1 && 
         (! exists ($pfix_ctHH{"S"}{$best_prefix}))) { 
        # line above had >= 1 in SEED, but next line has 0
        push(@outputA, "#\n");
        $printed_seed_sep = 1;
      }
      if($prvH{"F"} == 1 && 
         (! exists ($pfix_ctHH{"F"}{$best_prefix})) && 
         $printed_seed_sep) { 
        # line above had >= 1 in FULL, but next line has 0
        push(@outputA, "#\n");
      }
    }
    
    # determine group string (e.g. SFO) summarizing group membership for this prefix
    $group_string = "";
    foreach $group (@groupOA) { 
      if(exists ($pfix_ctHH{$group}{$best_prefix})) { 
        $group_string .= $group;
      }
      else { 
        $group_string .= "-";
      }
    }
    
    push(@outputA, sprintf("%-*s  %3s", $max_length, $best_prefix, $group_string));
    
    # print counts for each group for this prefix
    foreach $group (@groupOA) { 
      if(exists ($pfix_ctHH{$group}{$best_prefix})) { 
        push(@outputA, sprintf("      %5d  %6.1g", 
                               $pfix_ctHH{$group}{$best_prefix}, 
                               $pfix_minEHH{$group}{$best_prefix}));
        $prvH{$group} = 1;
        $nprintedH{$group} += $pfix_ctHH{$group}{$best_prefix};
      }
      else { 
        push(@outputA, sprintf("      %5s  %6s", "-", "-"));
        $prvH{$group} = 0;
      }
    }
    push(@outputA, sprintf("\n"));
    
    $printedH{$best_prefix} = 1;
  }
  my $total_line = "# total hits:";
  for($i = 0; $i < ($max_length-13 + 5); $i++) { $total_line .= " "; }
  foreach $group (@groupOA) { 
    if($ngroupH{$group} == 0) { $nprintedH{$group} = 0; }
    if($nprintedH{$group} != $ngroupH{$group}) { 
      printf STDERR ("ERROR incorrect number of $group seqs (%d != %d)\n", $nprintedH{$group}, $ngroupH{$group}); 
      exit(1);
    }
    $total_line .= sprintf("      %5d        ", $nprintedH{$group});
  }
  $total_line .= "\n";
  push(@outputA, sprintf("%-*s  %3s      %-13s      %-13s      %-13s\n", $max_length, "#", "", "-------------", "-------------", "-------------"));
  push(@outputA, $total_line);
  push(@outputA, "$div_line\n#\n");
  
  my $line;
  open(OUT, ">taxinfo") || die "ERROR unable to open taxinfo for writing";

  # print info on how to interpret the file
  push(@outputA, "# Explanation of data above:\n");
  push(@outputA, "#\n");
  push(@outputA, "# Listed above are counts of hits in various taxonomic groups for the\n");
  push(@outputA, "# three categories of hits (SEED, FULL, OTHER, defined below), for the current\n");
  push(@outputA, "# GA threshold. There are several command-line options that modify this output,\n");
  push(@outputA, "# use rfmake.pl -h for more information.\n");
  push(@outputA, "#\n");
  push(@outputA, "# Column abbreviations:\n");
  push(@outputA, "#   'mem'  column:  three letter summary of which groups have at least 1 hit in this taxonomic group\n");
  push(@outputA, "#   'ct'   columns: number of hits per hit category for this taxonomic group ('-' if none)\n");
  push(@outputA, "#   'minE' columns: minimum E-value of all hits in this category and taxonomic group ('-' if none)\n");
  push(@outputA, "#\n");
  push(@outputA, "# Definition of the three hit categories:\n");
  push(@outputA, "#   [S]EED:  seed sequences\n");
  push(@outputA, "#   [F]ULL:  sequences above current GA\n");
  push(@outputA, "#   [O]THER: sequences below GA, with E <= 10\n");
  push(@outputA, "#\n");

  foreach $line (@outputA) { print OUT $line; }
  close(OUT);
}


#-------------------------------------------------
    
=head2 parseOutlistAndSpecies

    Title    : parseOutlistAndSpecies
    Incept   : EPN, Mon Aug 19 15:17:17 2013
    Usage    : parseOutlistAndSpecies($outlist, $species, $emax, $ga, $infoHHR, $nameOAR, $groupOHAR, $groupOAR)
    Function : Parses $outlist and $species files into data structures used
               by writeTaxinfoFromOutlistAndSpecies().
    Args     : $outlist:   name of outlist file, usually 'outlist'
             : $species:   name of species file, usually 'species'
             : $emax:      maximum E-value to consider, usually 10
             : $ga:        GA bit score threshold
             : $infoHHR:   ref to 2D hash, key 1: name/start-end (nse), key 2: "rank", "bitsc", "evalue", "sspecies" or "taxstr"
             :             can be undefined if caller does not need this
             : $nameOAR:   ref to array, all nse, in order, ranked by score/E-value
             :             can be undefined if caller does not need this
             : $groupOHAR: ref to hash of arrays, nse in score rank order, by group
             :             can be undefined if caller does not need this
    Returns  : void
    Dies     : if outlist and species are not consistent, or do not exist

=cut

sub parseOutlistAndSpecies {
    my($self, $outlist, $species, $emax, $ga, $infoHHR, $nameOAR, $groupOHAR) = @_;

    my ($ct, $outline, $spcline, $name, $i, $i0, $key, $pkey, $group);
    my @out_elA = ();
    my @spc_elA = ();
    my %nameIH  = ();
    $ct = 0;

    open(OUT, $outlist) || die "ERROR unable to open $outlist";
    open(SPC, $species) || die "ERROR unable to open $species";

    while($outline = <OUT>) { 
	$spcline = <SPC>;
	#print("OUT: $outline");
	#print("SPC: $spcline\n");
        $outline =~ s/^\s+//;
        $spcline =~ s/^\s+//;
	chomp $outline;
	chomp $spcline;
	if($outline !~ m/^\#/) { 
	    $ct++;
	    # example outlist line:
            #  27.7  3.2e+02      FULL  AAWR02038290.1     53057    53080       1    24     no  Equus_caballus_(horse)[9796]        Equus caballus cont2.38289, whole genome shotgun sequence.                                                    
            # example species line:
            #  27.7  3.2e+02      FULL  AAWR02038290.1      9796  Equus caballus (horse)                                          Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Laurasiatheria; Perissodactyla; Equidae; Equus.                                                                     
	    @out_elA = split(/\s\s+/, $outline); # note: we separate by double spaces
	    @spc_elA = split(/\s\s+/, $spcline); # note: we separate by double spaces

	    #sanity check
	    for($i = 0; $i <= 3; $i++) { 
              if($out_elA[$i] ne $spc_elA[$i]) { 
                die "ERROR, hit $ct, element $i does not match b/t out.list and species files ($out_elA[$i] ne $spc_elA[$i])"; 
              }
	    }

	    $name = $out_elA[3] . "/" . $out_elA[4] . "-" . $out_elA[5]; 
	    if(defined $nameOAR) { push(@{$nameOAR}, $name); }
	    if(exists ($nameIH{$name})) { die "ERROR $name is duplicated"; }
	    $nameIH{$name} = 1;

	    # determine group
            $group = "";
            if   ($out_elA[2] eq "SEED")  { $group = "S"; } # Seed
            elsif($out_elA[0] >= $ga)     { $group = "F"; } # Full
            elsif($out_elA[1] <= $emax)   { $group = "O"; } # Other
	    if(defined $groupOHAR) { push(@{$groupOHAR->{$group}}, $name); }

	    if(defined $infoHHR) { 
              $infoHHR->{$name}{"rank"}     = $ct;
              $infoHHR->{$name}{"bitsc"}    = $out_elA[0];
              $infoHHR->{$name}{"evalue"}   = $out_elA[1];
              $infoHHR->{$name}{"sspecies"} = $out_elA[9];
              $infoHHR->{$name}{"taxstr"}   = $spc_elA[6];
              $infoHHR->{$name}{"trunc"}    = $out_elA[8];
            }
          }
    }
    close(OUT);
    close(SPC);
}

#-----------------------------------------------------------------


#-------------------------------------------------
    
=head2 writeOldAndNewHitComparison

    Title    : writeOldAndNewHitComparison
    Incept   : EPN, Fri Aug 30 13:28:08 2013
    Usage    : $io->writeOldAndNewHitComparison($infoHHR, $groupOHAR, $new_ga, $new_evalue, $old_ga, $olddir)
    Function : Given 'new' hit information (infoHH and groupOHAR) from a 
             : $io->parseOutlistAndSpecies call, as well as a directory
             : with an 'old' 'out.list' from Rfam 11.0, compare the 
             : hits found in SEED and FULL groups between the new
             : hits and the old hits.
             :
             : Output a summary of the comparison to a file called 'comparison'.
             : Output a subset of the new outlist lines, for the hits found in 
             :   new but missed by old to a file called 'newoutlist'
             : Output a subset of the new species lines, for the hits found in 
             :   new but missed by old to a file called 'newspecies'
             : Output a subset of the old out.list lines, for the hits found in 
             :   old but missed by new to a file called 'oldoutlist'
             : Output a subset of the old species lines, for the hits found in 
             :   old but missed by new to a file called 'oldspecies'
             :
    Args     : $infoHHR:     2D hash ref, filled by caller via parseOutlistAndSpecies
             : $groupOHAR:   hash of arrays ref, filled by caller via parseOutlistAndSpecies
             : $olddir:      path to directory with 'old' results
             : $avg_bitdiff: average bit score difference between old and new hits
             : $ndiff:       number of hits used to get $avg_bitdiff
             : $desc:        Bio::Rfam::Family::DESC object
             : $new_ga:      GA bit score threshold for new searches
             : $new_evalue:  E-value for GA threshold for new searches
             : $old_ga:      GA bit score threshold for old searches
             : $old_evalue:  E-value for GA threshold for old searches             
             : $old_outlist: old search results, usually $old_dir . "/" . 'out.list'
             : $old_species: old search results with tax info, usually $old_dir . "/" . 'species'
             : $new_outlist: new search results, usually "outlist"
             : $new_species: new search results with tax info, usually "species"
    Returns  : void
    Dies     : upon file input/output error

=cut

sub writeOldAndNewHitComparison {
  my ($self, $infoHHR, $groupOHAR, $olddir, $avg_bitdiff, $ndiff, $desc, $new_ga, $new_evalue, $old_ga, $old_evalue, $old_outlist, $old_species, $new_outlist, $new_species) = @_;

  my %newHHA; # 1st key: group ("S" or "F"), 2nd key: seqname (not nse), array of 'start-end';
  my %newctH; # key: group ("S" or "F"), value number of new hits in group
  my %oldctH; # key: group ("S" or "F"), value number of old hits in group
  my %newolH; # key: group ("S" or "F"), value number of new hits that overlap >= 1 old hit
  my %oldolH; # key: group ("S" or "F"), value number of old hits that overlap >= 1 new hit
  # first recast infoHHR into newHHA
  foreach my $group ("S", "F") { 
    $newolH{$group} = 0;
    $oldolH{$group} = 0;
    foreach my $nse (@{$groupOHAR->{$group}}) { 
      my (undef, $name, $start, $end, $str) = Bio::Rfam::Utils::nse_breakdown($nse);
      push(@{$newHHA{$group}{$name}}, $start . ":" . $end);
      $newctH{$group}++;
    }
  }

  # now for each hit in out.list, do we have a match in the new outlist?
  open(OLDOUT, $old_outlist) || die "ERROR unable to open $old_outlist";
  open(OLDSPC, $old_species) || die "ERROR unable to open $old_species";
  open(LOSTOUT, ">lostoutlist") || die "ERROR unable to open lostoutlist for writing";
  open(LOSTSPC, ">lostspecies") || die "ERROR unable to open lostspecies for writing";
  while(my $outline = <OLDOUT>) { 
    my $spcline = <OLDSPC>;
    if($outline =~ m/^\#/) { 
      print LOSTOUT $outline; # print comment lines
      if($spcline !~ m/^\#/) { croak "ERROR old out.list and species lines inconsistent!\n$outline\n$spcline\n"; }
      print LOSTSPC $spcline; # print comment lines
    }
    else { 
      #91.32	8.65e-15	ALIGN	ACFV01061888.1	      1855	      1756	1	100	.	Callithrix_jacchus_w	Callithrix jacchus Contig81.61, whole genome shotgun sequence.
      my @elA = split(/\s+/, $outline);
      my ($group, $bitsc, $evalue, $name, $start, $end) = ($elA[2], $elA[0], $elA[1], $elA[3], $elA[4], $elA[5]);
      # note, we don't check to make sure species line corresponds to out.list line here, but we do below before outputting it
      if($group eq "SEED")     { $group = "S"; }
      elsif($group eq "ALIGN") { $group = "F"; }
      else                     { next; }

      $oldctH{$group}++;
      my $found_overlap = 0;
      # determine if there's an overlap in set of new hits
      #
      # we might get > 1 old hits overlapping with the same
      # new hit but we don't want to double count that, so 
      # we redefine the value $newHHA{$group}{$name}[$i] 
      # by multiplying by -1 once we find an overlap to it,
      # this negative value serves as a flag.
      if(exists $newHHA{$group}{$name}) { 
        for(my $i = 0; $i < scalar(@{$newHHA{$group}{$name}}); $i++) { 
          my $already_hit = 0;
          my ($start2, $end2) = split(":", $newHHA{$group}{$name}[$i]);
          if($start2 < 0) { # this hit already overlapped a previous hit, we marked it previously
            $already_hit = 1;
            $start2 *= -1; # make it positive again
          }
          my $ol = Bio::Rfam::Utils::overlap_nres_or_full($start, $end, $start2, $end2);
          if($ol != 0) { # careful, don't want to do 'if ($ol > 0)', because full overlaps, which have value -1, will be ignored
            if(! $already_hit) { $newolH{$group}++; } # only count if not already marked
            $oldolH{$group}++;
            # update value in newHHA so we know this hit has already overlapped with an old hit
            $newHHA{$group}{$name}[$i] = "-" . $start2 . ":" . $end2;
            $found_overlap = 1;
            last;
          }
        }
      }
      # if we didn't find an overlap output line to 'lostoutlist' and 'lostspecies' files
      if(! $found_overlap) { 
        # before printing, do a sanity check: does species line correspond to the same hit
        my @spcelA = split(/\t/, $spcline);
        #91.32	8.65e-15	ALIGN	ACFV01061888.1	           9483	Callithrix jacchus (	Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Platyrrhini; Cebidae; Callitrichinae; Callithrix.
        if($spcelA[0] ne $bitsc || $spcelA[3] ne $name) { croak "ERROR old out.list and species lines inconsistent!\n$outline\n$spcline\n"; }
        print LOSTOUT $outline; 
        print LOSTSPC $spcline;
      }
    }
  }
  close(LOSTOUT);
  close(LOSTSPC);
  close(OLDOUT);
  close(OLDSPC);
  
  # now go back through outlist and print any new hits to "newoutlist" and "newspecies"
  # first, get a list of all the new hits
  my ($group, $name, $startend);
  my %newhitH = ();
  foreach $group ("S", "F") { 
    foreach $name (keys %{$newHHA{$group}}) { 
      foreach $startend (@{$newHHA{$group}{$name}}) { 
        my ($start, $end) = split(":", $startend);
        if($start > 0) { # above, we marked each found hit by making start negative 
          $newhitH{$name . "/" . $start . "-" . $end}  = 1;;
        }
      }
    }
  }
  # now output them to a file "newoutlist" and "newspecies"
  my $newoutFH;
  my $newspcFH;
  my @outAA = ();
  my @spcAA = ();
  my $nhit = 0;
  my ($outline, $spcline);
  open(NEWOUT, $new_outlist) || die "ERROR unable to open $new_outlist for reading";
  open(NEWSPC, $new_species) || die "ERROR unable to open $new_species for reading";
  open($newoutFH, ">newoutlist") || die "ERROR unable to open newoutlist for writing";
  open($newspcFH, ">newspecies") || die "ERROR unable to open newspecies for writing";
  while($outline = <NEWOUT>) { 
    $spcline = <NEWSPC>;
    if($outline !~ m/^\#/) { 
      $outline =~ s/^\s+//; # remove leading whitespace
      $spcline =~ s/^\s+//; # remove leading whitespace
      my @elA = split(/\s\s+/, $outline);
      # 89.5  7.7e-16      SEED  AADA01319887.1     7078     7176       1   101     no  Pan_troglodytes_(chimpa..[9598]    Pan troglodytes ctg_319886, whole genome shotgun sequence.
      my $nse = $elA[3] . "/" . $elA[4] . "-" . $elA[5];
      if(exists $newhitH{$nse}) { 
        my @spcelA = split(/\s\s+/, $spcline);
        # 89.5  7.7e-16      SEED  AADA01319887.1    9598  Pan troglodytes (chimpanzee)                                    Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Pan.
        if($spcelA[0] ne $elA[0] || $spcelA[3] ne $elA[3]) { 
          croak "ERROR new outlist and species lines inconsistent!\n$outline\n$spcline\n"; 
        }
        push(@{$outAA[$nhit]}, @elA);
        push(@{$spcAA[$nhit]}, @spcelA);
        $nhit++;
      }
    }
  }
  close(NEWOUT);
  close(NEWSPC);
  writeOutlistOrSpeciesChunk($newoutFH, \@outAA, 1);
  writeOutlistOrSpeciesChunk($newspcFH, \@spcAA, 0);

  # Now, print the prefixes and their counts out in a particular order
  # First print all prefixes with >=1 members in SEED in sorted order
  # from low to high minimum E-value in group, then print any with >= 1 members in FULL
  # from low to high minimum E-value in group, then print any with >= 1 members in OTHER
  # from low to high minimum E-value in group.
  # (If $do_nsort is '1' we'll sort by total counts per group not 
  # minimum E-value).

  my $div_line = "#=========================================================";
  
  open(COMP, ">comparison") || die "ERROR unable to open comparison for writing";

  print  COMP ($div_line . "\n");
  print  COMP ("# comparison: created by 'rfmake.pl' with \'-compare\' cmd-line option\n");
  print  COMP ($div_line . "\n");
  printf COMP ("# %s   %s   %s\n", $desc->AC, $desc->ID, $desc->DE);
  printf COMP ("# current directory: %s\n", getcwd);
  printf COMP ("# directory with old results (relative path): $olddir\n");
  printf COMP ("# new GA bit-score:    $new_ga\n");
  printf COMP ("# new GA E-value:      %6.1g\n", $new_evalue);
  printf COMP ("# old GA bit-score:    $old_ga\n");
  printf COMP ("# old GA E-value:      %6.1g\n", $old_evalue);
  printf COMP ("#\n");
  
  if($newolH{"S"} > 0) { 
    printf COMP ("# suggested new GA:    %.2f\n", int($old_ga + $avg_bitdiff + 0.5));
    printf COMP ("# calc'ed as old GA ($old_ga) plus avg bitsc difference b/t %d old and new hits (%.5f) rounded to nearest integer\n", $ndiff, $avg_bitdiff);
  }
  else { 
    printf COMP ("# WARNING: no hits overlap between old and new searches\n");
  }

  printf COMP ("#\n");
  printf COMP ("# counts of hits in SEED and FULL from old and new searches:\n");
  if($old_ga ne $new_ga) { 
    printf COMP ("# NOTE: GA used to define FULL differs b/t old and new ($old_ga != $new_ga)\n");
  }
  printf COMP ("#\n");
  printf COMP ("# %8s  %7s  %7s\n", "", "SEED", "FULL");
  printf COMP ("# %8s  %7s  %7s\n", "", "=======", "=======");
  printf COMP ("%-10s  %7d  %7d\n", "old-total",  $oldctH{"S"}, $oldctH{"F"});
  printf COMP ("%-10s  %7d  %7d\n", "new-total",  $newctH{"S"}, $newctH{"F"});
  printf COMP ("%-10s  %7d  %7d\n", "both",       $newolH{"S"}, $newolH{"F"});
  printf COMP ("%-10s  %7d  %7d\n", "old-unique", $oldctH{"S"} - $newolH{"S"}, $oldctH{"F"} - $newolH{"F"}); 
  printf COMP ("%-10s  %7d  %7d\n", "new-unique", $newctH{"S"} - $newolH{"S"}, $newctH{"F"} - $newolH{"F"}); 
  printf COMP ("#\n");
  printf COMP ("# \'old-total\':  total number of old hits in SEED and FULL\n");
  printf COMP ("# \'new-total\':  total number of new hits in SEED and FULL\n");
  printf COMP ("# \'both\':       number of hits that exist in both old and new results (overlap by at least 1 nt)\n");
  printf COMP ("# \'old-unique\': hits in old results not present in new results\n");
  printf COMP ("# \'new-unique\': hits in new results not present in old results\n");
  printf COMP ("#\n");
  printf COMP ("# see 'lostoutlist' and 'lostspecies' files for list of 'old-unique' hits (lost in new searches).\n");
  printf COMP ("# see 'newoutlist'  and 'newspecies'  files for list of 'new-unique' hits.\n"); 
  close(COMP);
}

#-------------------------------------------------------

=head2 _taxinfo_get_sortable_exponent

    Title    : _taxinfo_get_sortable_exponent
    Incept   : EPN, Tue Aug 20 08:57:09 2013
    Usage    : _taxinfo_get_sortable_exponent($evalue)
    Function : Given an E-value output by cmsearch, return a PERL-sortable
             : version of it. We return it''s exponent plus something extra
             : (see examples below). We need to do this because PERL cannot
             : sort very low E-values (< ~E-20) (they are all treated as zeroes).
             : We take extra care such that an E-value of 0 is always sorted
             : as the minimal possible E-value, by returning -1000 as it''s key,
             : this will be lower than all E-values > 1.0E-1000, which means
             : it will always be the minimum.                                   
    Args     : $evalue:   name of outlist file, usually 'outlist'
    Returns  : sortable version of $evalue
=cut


sub _taxinfo_get_sortable_exponent { 
    #examples: 
    # input($evalue) = 2.3E-5 return -5.77 (0.77 = 1.0 - 0.23)
    # input($evalue) = 6.7E-5 return -5.33 (0.33 = 1.0 - 0.67)
    # input($evalue) = 2.3E+5 return  5.23
    # input($evalue) = 6.7E+5 return  5.67 

    my $evalue = $_[0];
    my $exp = 0;
    my $ret_val;

    if($evalue eq "0") { 
      $ret_val = -1000; 
    }
    elsif($evalue <= 1) { 
      while($evalue <= 1) { $exp--; $evalue *= 10; } 
      $ret_val = $exp - (1. - ($evalue / 10.)); 
    }
    elsif($evalue > 1) { 
      while($evalue > 1) { $exp++; $evalue /= 10; }
      $ret_val = $exp + $evalue;
    }

    return $ret_val;
  }
######################################################################

1;
