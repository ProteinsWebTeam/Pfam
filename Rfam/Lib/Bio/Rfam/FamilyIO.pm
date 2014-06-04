
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
use Scalar::Util qw(reftype);
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
                           reqdFormat   => 'Stockholm',
                           # NOTE: we don't pass 'forceText => 1', if we did we'd read the alignment in text mode...
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
                           reqdFormat   => 'Stockholm',
                           forceText    => 1
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
  #GA       29.00
  #TC       29.10
  #NC       28.50
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
    #GA       29.00
    #TC       29.10
    #NC       28.50
    #EFP7GF   -7.2971 0.71888
    #CM
    elsif ( my ($pbegin) = $_ =~ /^PBEGIN\s+(\d+\.\d+)/) {
      $objHash->{pbegin} = $pbegin;
    } elsif ( my ($pend) = $_ =~ /^PEND\s+(\d+\.\d+)/) {
      $objHash->{pend} = $pend;
    } elsif ( my ($wbeta) = $_ =~ /^WBETA\s+(\S+)/) {
      $objHash->{wbeta} = $wbeta;
    } elsif ( my ($qdbbeta1) = $_ =~ /^QDBBETA1\s+(\S+)/) {
      $objHash->{qdbbeta1} = $qdbbeta1;
    } elsif ( my ($qdbbeta2) = $_ =~ /^QDBBETA2\s+(\S+)/) {
      $objHash->{qdbbeta2} = $qdbbeta2;
    } elsif ( my ($n2omega) = $_ =~ /^N2OMEGA\s+(\S+)/) {
      $objHash->{n2omega} = $n2omega;
    } elsif ( my ($n3omega) = $_ =~ /^N3OMEGA\s+(\S+)/) {
      $objHash->{n3omega} = $n3omega;
    } elsif ( my ($elself) = $_ =~ /^ELSELF\s+(\S+)/) {
      $objHash->{elself} = $elself;
    } elsif (my ($noSeqs) = $_ =~ /^NSEQ\s+(\d+)/) {
      $objHash->{nSeq} = $noSeqs;
    } elsif ( my ($effn) = $_ =~ /^EFFN\s+(\d+\.\d+)/) {
      #EFFN  4.966292
      $objHash->{effn} =  $effn ;
    } elsif ( my ($cksum ) = $_ =~ /^CKSUM\s+(\d+)/) {
      $objHash->{cksum} = $cksum ;
    } elsif ( my ($null) = $_ =~ /^NULL\s+(.*)/) {
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
    elsif (my ($ga) = $_ =~ /^GA\s+(\S+)/) { 
      $objHash->{hitGA} = $ga;
    } elsif (my ($tc) = $_ =~ /TC\s+(\S+)/) { 
      $objHash->{hitTC} = $tc;
    } elsif (my ($nc) = $_ =~ /NC\s+(\S+)/) { 
      $objHash->{hitNC} = $nc;
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

sub parseDESCallowHmmonly {
  my ( $self, $file ) = @_;
  # call parseDesc with special flag to allow --hmmonly in search options
  return $self->parseDESC($file, 1);
}

# parseDESC: normally takes two arguments, but if we want to allow
# the --hmmonly option in 'SM' pass a third argument: '1'.

sub parseDESC {
  my ( $self, $file, $allow_hmmonly ) = @_;

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
      my @ids=split("\;", $piLine);
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
#
#    or   (ONLY if $allow_hmmonly) is passed in as '1':
#
#    SM   cmsearch --cpu 4 --verbose --hmmonly -E 1000 -Z 549862.597050 CM SEQDB
#         (this 2nd SM line will only rarely happen if we use the 'debugging' -hmmonly
#          flag in rfsearch. --hmmonly is NOT permitted when a family is checked in,
#          and Bio::Rfam::QC::checkRequiredFields() enforces this to prevent
#          a check in of an hmmonly search)
#
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
        } elsif ( $file[$i] =~ /^DR   (RFAM); (RF\d+);?$/ ) {

          #RFAM; RF00001
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
         } elsif ( $file[$i] =~ /^DR   (URL); ([^;\s]*);?$/ ) {
          
          #URL; http://www.someRNAresource.org/987654321/page.html
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
      # okay if ($allow_hmmonly) we do one last try it has to be --hmmonly
      if((! $allow_hmmonly) || 
         ($params{SM} !~ /^cmsearch\s+--cpu \d+ --verbose -[E|T]\s+\d+(\.\d+)? -Z (\S+) --hmmonly (\-.* )?CM SEQDB$/)) {
        croak sprintf("\nFATAL: Your SM cmsearch line doesn't look right [%s]\n", $params{SM});
      }
    }    
  }

  my $desc = 'Bio::Rfam::Family::DESC'->new(%params);
  return $desc;

  #End of uber for loop
}

sub writeEmptyDESC {
  my ($self) = @_;

  # we only need 'tmpdesc' below so we can get at the defaultButIllegalFields field
  my $tmpdesc = 'Bio::Rfam::Family::DESC'->new( );

  # default values for fields that don't have to be changed before a checkin
  my %descH = (
               CUTGA => '27.00',
               CUTNC => '27.00',
               CUTTC => '27.00',
               );

  # now add default fields that must be changed prior to checkin 
  # (QC will check that these have been changed away from their default values before a checkin)
  foreach my $key (keys (%{$tmpdesc->defaultButIllegalFields})) { 
    $descH{$key} = $tmpdesc->defaultButIllegalFields->{$key};
  }  

  my $desc = 'Bio::Rfam::Family::DESC'->new( \%descH );
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

  Bio::Rfam::FamilyIO::validate_outlist_format($outlistLocation, 1); # '1' says: require full file, with first 3 lines prefixed with '#'

  open(OL, '<', $outlistLocation) || croak "ERROR unable to open $outlistLocation for reading";

  # process the outlist
  while (<OL>) {
    if (! m/^\#/) { 
      s/^\s+//; # remove leading whitespace
      my @elA = split(/\s+/);
      # example line
      # 111.2  5.3e-19      FULL  AEHL01281174.1    v:75.1   10015   10102   +     1    88  no     33
      # 0      1            2     3                 4        5       6       7     8    8   9      10

      my ($bits, $evalue, $type, $id, $overlap, $start, $end, $strand, $qstart, $qend, $tstr) = ($elA[0], $elA[1], $elA[2], $elA[3], $elA[4], $elA[5], $elA[6], $elA[7], $elA[8], $elA[9], $elA[10]);
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
      } elsif ($tstr eq "-") { # --hmmonly, this should never get into the db
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
             : $logFH:   log output file handle
    Returns  : void
    Dies     : upon file input/output error

=cut

sub writeTbloutDependentFiles {
  my ($self, $famObj, $rfdbh, $seedmsa, $ga, $RPlotScriptPath, $require_tax, $logFH) = @_;

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
  my %seedseq_overlap_above_gaH    = (); # key: SEED sequence, value name of best hit >= GA that overlaps with it
  my %seedseq_overlap_below_gaH    = (); # key: SEED sequence, value name of best hit <  GA that overlaps with it
  my %seedseq_overlap_below_ga_scH = (); # key: SEED sequence, score of best hit   <  GA that overlaps with it

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
    
  # parse TBLOUT to get tbloutHAR, a hash of arrays:
  my %tbloutHA; # hash of arrays, key is source sequence name of hit, value is array of scalars
                # of form: "<start>:<end>:<bitscore>"
  parseTbloutForOverlapCheck("TBLOUT", \%tbloutHA);

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
      # extract data from this REVTBLOUT line into variables we'll print out using processTbloutLine() subroutine
      my ($bits, $evalue, $name, $start, $end, $strand, $qstart, $qend, $trunc, $shortSpecies, $description, $ncbiId, $species, $taxString) = 
          processTbloutLine($tblline, $sthDesc, $sthTax, 1, $require_tax); # '1' says: yes this is a reversed search

      # determine if this reverse hit overlaps with any positive hits
      my $overlap_str = _outlist_species_get_overlap_string(\%tbloutHA, $name, $start, $end, $bits, 1); # '1' says this is a reversed hit

      push(@{$rev_outAA[$nlines]}, ($bits, $evalue, "REV", $name, $overlap_str, $start, $end, $strand, $qstart, $qend, $trunc, $shortSpecies, $description));
      push(@{$rev_spcAA[$nlines]}, ($bits, $evalue, "REV", $name, $overlap_str, $ncbiId, $species, $taxString));
      $nlines++;
      # TODO?: change so top scoring rev hit that does not overlap with any other hits is marked up, not just top rev hit
      #         if($overlap_str eq "-" && $rev_evalue eq "") { # first hit that does not overlap with a true hit of higher value
      if($rev_evalue eq "") { # first hit 
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
    my ($bits, $evalue, $name, $start, $end, $strand, $qstart, $qend, $trunc, $shortSpecies, $description, $ncbiId, $species, $taxString, $got_tax) = 
        processTbloutLine($tblline, $sthDesc, $sthTax, 0, $require_tax); #'0' says: no this is not a reversed search 

    # determine if this hit overlaps with any other hits on opposite strand
    my $overlap_str = _outlist_species_get_overlap_string(\%tbloutHA, $name, $start, $end, $bits, 0); # '0' says this is not a reversed hit

    if($taxString eq "-") { $have_all_tax_info = 0; }
    my $domainKingdom = Bio::Rfam::Utils::tax2kingdom($taxString . '; ' . $species . ';');
    $kingdomCounts{$domainKingdom}++;

    # determine seqLabel
    my $seqLabel = 'FULL';
    my $nse = $name . "/" . $start . "-" . $end;
    my ($seed_seq, $overlapExtent) = $seedmsa->nse_overlap($nse);
    if ($seed_seq ne "") { 
      if ($overlapExtent > 0.1) {
        $seqLabel = 'SEED';
        if($bits >= $ga) { 
          if(! exists $seedseq_overlap_above_gaH{$seed_seq}) { 
            $seedseq_overlap_above_gaH{$seed_seq} = $nse; 
          }
        }
        else { # bits < $ga
          if(! exists $seedseq_overlap_below_gaH{$seed_seq}) { 
            $seedseq_overlap_below_gaH{$seed_seq} = $nse; 
            $seedseq_overlap_below_ga_scH{$seed_seq} = $bits; 
          }
        }
      }
    }
    if (($bits < $ga) && ($seqLabel ne 'SEED')) {
      $seqLabel = 'NOT';
    }

    # print out threshold line if nec
    if ( $bits < $ga && $ga<=$prv_bits) {
      $outline = _commentLineForOutlistOrSpecies (" CURRENT THRESHOLD: $ga BITS ");
      push(@{$outAA[$nlines]}, ($outline));
      push(@{$spcAA[$nlines]}, ($outline));
      printf RIN  "%0.2f\tTHRESH\t.\n", $ga;
      $printed_thresh=1;
      $nlines++;
    }
    if ( $rev_evalue ne "" && $evalue > $rev_evalue && $prv_evalue <= $rev_evalue) {
      $outline = _commentLineForOutlistOrSpecies(" BEST REVERSED HIT E-VALUE: $rev_evalue ");
      push(@{$outAA[$nlines]}, ($outline));
      push(@{$spcAA[$nlines]}, ($outline));
      $nlines++;
    }
    $prv_bits = $bits;
    $prv_evalue = $evalue;

    push(@{$outAA[$nlines]}, ($bits, $evalue, $seqLabel, $name, $overlap_str, $start, $end, $strand, $qstart, $qend, $trunc, $shortSpecies, $description));
    push(@{$spcAA[$nlines]}, ($bits, $evalue, $seqLabel, $name, $overlap_str, $ncbiId, $species, $taxString));
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
    $outline = _commentLineForOutlistOrSpecies(" CURRENT THRESHOLD: $ga BITS ");
    push(@{$outAA[$nlines]}, ($outline));
    push(@{$spcAA[$nlines]}, ($outline));
    printf RIN "%0.2f\tTHRESH\t\.\n", $ga;
    $nlines++;
  }
  if ($rev_evalue eq "") { 
    if(-e $rtblI) { $outline = _commentLineForOutlistOrSpecies(" NO REVERSED HITS (NO HITS FOUND IN REVERSED DB) "); }
    else          { $outline = _commentLineForOutlistOrSpecies(" NO REVERSED HITS (NO REVERSED SEARCH PERFORMED) "); }
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
  # store warnings in separate arrays so we can print them out in order, for easier interpretation by user
  my @warnings_case1A = ();
  my @warnings_case2A = ();
  my @warnings_case3A = ();
  my @warnings_case4A = ();
  if($require_tax) { 
    # for each SEED sequence, determine which of the following 5 cases applies:
    #
    # 0)     exact hit to SEED exists >= GA (this is good)
    # 1) non-exact hit to SEED exists >= GA (this might be okay)
    # 2)     exact hit to SEED exists <  GA 
    # 3) non-exact hit to SEED exists <  GA
    # 4) no        hit to SEED exists at all 
    #
    # where 'hit to SEED' means a hit overlaps more than 10% coverage with the SEED seq
    # We warn the user about cases 1-4. (Case 2 is probably okay though.)
    for ($idx = 0; $idx < $seedmsa->nseq; $idx++) { 
      my $n = $seedmsa->get_sqname($idx);
      if(exists $seedseq_overlap_above_gaH{$n}) { # case 0 or 1
        if($seedseq_overlap_above_gaH{$n} eq $n) { 
          ;# case 0, do nothing.
        }
        else { 
          # case 1, warn
          push(@warnings_case1A, sprintf("! WARNING (case 1): Overlapping hit to SEED sequence exists > GA but it's not an exact match (%s (seed) != %s (hit))\n", 
                                         $n, $seedseq_overlap_above_gaH{$n}));
        }
      }
      elsif(exists $seedseq_overlap_below_gaH{$n}) { # case 2 or 3
        if($seedseq_overlap_below_gaH{$n} eq $n) { 
          # case 2, warn
          push(@warnings_case2A, sprintf("! WARNING (case 2): Exact match to SEED sequence exists but it is below GA (%s < $ga for $n)\n", 
                                         $seedseq_overlap_below_ga_scH{$n}));
        }
        else { 
          # case 3, warn
          push(@warnings_case3A, sprintf("! WARNING (case 3): Overlapping hit to SEED sequence exists but it is below GA and it's not an exact match (%s < $ga and %s (seed) != %s (hit))\n", 
                                         $seedseq_overlap_below_ga_scH{$n}, $n, $seedseq_overlap_below_gaH{$n}));
        }
      }
      else { 
        # case 4, warn
        push(@warnings_case4A, "! WARNING (case 4): No overlapping hit to SEED sequence $n exists in outlist/species\n");
      }        
    }

    my @warnings_allA = (@warnings_case1A, @warnings_case2A, @warnings_case3A, @warnings_case4A);
    if (scalar(@warnings_allA) > 0) { 
      open(W, ">warnings") || croak "unable to open warnings file for writing";
      foreach my $warning (@warnings_allA) {  
        print W $warning;
        Bio::Rfam::Utils::printToFileAndStderr($logFH, $warning);
      }
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("! WARNING: warnings about SEED sequences (%d of them) written to warnings file\n", scalar(@warnings_allA)), 1);
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
  # would have died (and we'd never have gotten to this point).
  if(-e "plot_outlist.Rout") { unlink "plot_outlist.Rout"; } 
  if(-e "rin.dat")           { unlink "rin.dat"; }
  if(-e "rinc.dat")          { unlink "rinc.dat"; }
  if(! -e "outlist.pdf")     { die "ERROR outlist.pdf not created"; }
  if(! -e "species.pdf")     { die "ERROR species.pdf not created"; }
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
             : $strand:       strand of hit ('+' or '-')
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

  my $description  = undef;
  my $species      = undef;
  my $shortSpecies = undef;
  my $taxString    = undef;
  my $ncbiId       = undef;

  # potentially remove '-shuffled' suffix if nec from 'reversed searches'
  my $name2lookup = $name;
  if($is_reversed) { $name2lookup =~ s/\-shuffled$//; }

  # fetch description
  if(defined $sthDesc) { 
    $description = fetchDescription($sthDesc, $name2lookup);
  }
  if(defined $sthTax) { 
    ($species, $shortSpecies, $taxString, $ncbiId) = fetchSpeciesTaxStringAndID($sthTax, $name2lookup);
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

  return ($bits, $evalue, $name, $start, $end, $strand, $qstart, $qend, $trunc, $shortSpecies, $description, $ncbiId, $species, $taxString);
}

=head2 fetchDescription

    Title    : fetchDescription()
    Incept   : EPN, Tue Dec 10 06:34:22 2013
    Usage    : fetchDescription($sthDesc, $seqAcc)
    Function : Fetch a description from RfamLive for a sequence accession
             : given a DBI statement handle ($sthDesc) for executing
             : queries with a single bind value: rfamseq_acc.
    Args     : $sthDesc: prepared database query for fetching description ($rfdbh->prepared_seqaccToDescription())
             : $seqacc:  sequence accession (name) to get description for
    Returns  : $desc:    the description, fetched from RfamLive DB.
    Dies     : if $sthDesc or $seqacc is not defined

=cut

sub fetchDescription { 
  my ($sthDesc, $seqacc) = @_;

  if(! defined $sthDesc) { die "ERROR, fetchDescription, sthDesc is undefined"; }
  if(! defined $seqacc)  { die "ERROR, fetchDescription, seqacc is undefined"; }

  my $description;

  $sthDesc->execute($seqacc);
  my $res = $sthDesc->fetchall_arrayref;
  foreach my $row (@$res) {
    $description .= $row->[0];
  }

  return $description;
}

=head2 fetchSpeciesTaxStringAndID

    Title    : fetchSpeciesTaxStringAndID()
    Incept   : EPN, Tue Dec 10 06:39:12 2013
    Usage    : fetchSpeciesTaxStringAndID($sthTax, $seqAcc)
    Function : Fetch a species string, tax string, and NCBI ID from RfamLive 
             : for a sequence accession given a DBI statement handle 
             : ($sthTax) for executing queries with a single bind 
             : value: rfamseq_acc.
    Args     : $sthTax:  prepared database query for fetching tax info ($rfdbh->prepared_seqaccToSpeciesTaxStringAndID())
             : $seqacc: sequence accession (name) to get description for
    Returns  : list of:
             : $species:      species string
             : $shortSpecies: short version of $species
             : $taxString:    taxonomy string
             : $ncbiId:       NCBI taxonomy id
    Dies     : if $sthTax or $seqacc is not defined

=cut

sub fetchSpeciesTaxStringAndID { 
  my ($sthTax, $seqacc) = @_;

  if(! defined $sthTax) { die "ERROR, fetchSpeciesTaxStringAndID, sthTax is undefined"; }
  if(! defined $seqacc) { die "ERROR, fetchSpeciesTaxStringAndID, seqacc is undefined"; }

  my ($species, $shortSpecies, $taxString, $ncbiId);

  $sthTax->execute($seqacc);
  my $rfres = $sthTax->fetchall_arrayref;
  if(defined $rfres) { 
    foreach my $row (@{$rfres}) {
      if (scalar(@{$row}) < 4) { die "ERROR problem fetching tax info for $seqacc"; }
      $species       .= $row->[0];
      $shortSpecies  .= $row->[1];
      $taxString     .= $row->[2];
      $ncbiId        .= $row->[3];
    }
  }

  return ($species, $shortSpecies, $taxString, $ncbiId);
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
    @headA = ("# bits", "evalue", "seqLabel", "name", "overlap", "start", "end", "str", "qstart", "qend", "trunc", "species", "description");
  }
  else { # species data 
    @headA = ("# bits", "evalue", "seqLabel", "name", "overlap", "ncbiId", "species", "taxString");
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
    if($j < ($nels-1)) { 
      printf $fh ("%-*s  ", $widthA[$j], $headA[$j]);
    }
    else { 
      printf $fh ("%s", $headA[$j]); # final column doesn't need to be fixed-width, just flush left
    }
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
        printf $fh ("%*s  %*s  %*s  %-*s  %*s  %*s  %*s  %*s  %*s  %*s  %*s  %-*s  %-s\n", 
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
                    $widthA[10], $aR->[10],
                    $widthA[11], $aR->[11],
                    $aR->[12]); # final column doesn't need to be fixed-width, just flush left
      }
      else { # species line
        printf $fh ("%*s  %*s  %*s  %-*s  %*s  %*s  %-*s  %-s\n",
                    $widthA[0], $aR->[0], 
                    $widthA[1], $aR->[1], 
                    $widthA[2], $aR->[2], 
                    $widthA[3], $aR->[3], 
                    $widthA[4], $aR->[4], 
                    $widthA[5], $aR->[5], 
                    $widthA[6], $aR->[6], 
                    $aR->[7]); # final column doesn't need to be fixed-width, just flush left
      }
    }
  }
}

# return a comment line for output to an outlist or species file
sub _commentLineForOutlistOrSpecies {
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


#-------------------------------------------------------------------------------

=head2 writeCM

  Title    : writeCM
  Incept   : EPN, Wed May 28 09:20:28 2014
  Usage    : Bio::Rfam::Family::CM::writeCM($cm, $do_append, $outfile)
  Function : Output a CM to a file.
  Args     : $self:       Bio::Rfam::FamilyIO object
           : $cm:         Bio::Rfam::Family::CM object
           : $outfile:    file to print CM to, created or overwritten if $do_append is '0', 
           :              else created or appended to if $do_append is '1'.
           : $do_append:  '1' to append to file $outfile if it exists, '0' to overwrite it.
  Returns  : void
  Dies     : If $outfile is not defined, or if somehow we have some E-value stat lines but not all of them.

=cut

sub writeCM { 
  my ( $self, $cm, $outfile, $do_append) = @_;

  unless ( $cm->isa('Bio::Rfam::Family::CM') ) {
    confess("ERROR, writeCM, you did not pass in a Bio::Rfam::Family::CM object");
  }
  if(! defined $outfile) { die "ERROR in writeCM(), $outfile is not defined."; }
  if(! defined $do_append) { $do_append = 0; }

  # contract check: make sure we either have all E-value stats or none
  my $i_am_calibrated = 0;
  my $have_ecmlc = (exists $cm->{cmHeader}->{ecmlc}) ? 1 : 0;
  my $have_ecmgc = (exists $cm->{cmHeader}->{ecmgc}) ? 1 : 0;
  my $have_ecmli = (exists $cm->{cmHeader}->{ecmli}) ? 1 : 0;
  my $have_ecmgi = (exists $cm->{cmHeader}->{ecmgi}) ? 1 : 0;
  if   ($have_ecmlc + $have_ecmgc + $have_ecmli + $have_ecmgi == 4) { $i_am_calibrated = 1; }
  elsif($have_ecmlc + $have_ecmgc + $have_ecmli + $have_ecmgi != 0) { die "ERROR, some but not all E-value stats are valid in the CM"; }

  if((! $do_append) || (! -e $outfile)) { 
    open(OUT, ">" . $outfile) || die "ERROR unable to open $outfile for writing."; 
  }
  else {
    open(OUT, ">>" . $outfile) || die "ERROR unable to open $outfile for appending."; 
  }

  # print CM header, in a specific order, this is consistent with Infernal 1.1: infernal-1.1/src/cm_file.c::cm_file_WriteASCII
  # and with Bio::Rfam::FamilyIO::_parseCMHeader().
  #
  # Example: 
  #
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
  #
  #
  # most values are stored without the first token (e.g. 'NAME'), the first line ('version') is the exception.
  print  OUT ($cm->{cmHeader}->{version} . "\n");
  printf OUT ("NAME     %s\n", $cm->{cmHeader}->{name});
  if(exists $cm->{cmHeader}->{acc})  { printf OUT ("ACC      %s\n", $cm->{cmHeader}->{acc}); }
  if(exists $cm->{cmHeader}->{desc}) { printf OUT ("DESC     %s\n", $cm->{cmHeader}->{desc}); }
  printf OUT ("STATES   %d\n", $cm->{cmHeader}->{states});
  printf OUT ("NODES    %d\n", $cm->{cmHeader}->{nodes});
  printf OUT ("CLEN     %d\n", $cm->{cmHeader}->{clen});
  printf OUT ("W        %d\n", $cm->{cmHeader}->{w});
  printf OUT ("ALPH     %s\n", $cm->{cmHeader}->{alpha});
  printf OUT ("RF       %s\n", ($cm->{cmHeader}->{rf}   == 1) ? "yes" : "no");
  printf OUT ("CONS     %s\n", ($cm->{cmHeader}->{cons} == 1) ? "yes" : "no");
  printf OUT ("MAP      %s\n", ($cm->{cmHeader}->{map}  == 1) ? "yes" : "no");
  if(exists $cm->{cmHeader}->{date}) { printf OUT ("DATE     %s\n", $cm->{cmHeader}->{date}); }
  if(exists $cm->{cmHeader}->{com})  { 
    for(my $i = 0; $i < scalar(@{$cm->{cmHeader}->{com}}); $i++) { 
      printf OUT ("COM      %s %s\n", "[" . ($i+1) . "]", $cm->{cmHeader}->{com}->[$i]); 
    }
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
  #GA       29.00
  #TC       29.10
  #NC       28.50
  #EFP7GF   -7.2971 0.71888
  #CM
  printf OUT ("PBEGIN   %g\n",   $cm->{cmHeader}->{pbegin});
  printf OUT ("PEND     %g\n",   $cm->{cmHeader}->{pend});
  printf OUT ("WBETA    %g\n",   $cm->{cmHeader}->{wbeta});
  printf OUT ("QDBBETA1 %g\n",   $cm->{cmHeader}->{qdbbeta1});
  printf OUT ("QDBBETA2 %g\n",   $cm->{cmHeader}->{qdbbeta2});
  printf OUT ("N2OMEGA  %6g\n",  $cm->{cmHeader}->{n2omega});
  printf OUT ("N3OMEGA  %6g\n",  $cm->{cmHeader}->{n3omega});
  printf OUT ("ELSELF   %.8f\n", $cm->{cmHeader}->{elself});
  printf OUT ("NSEQ     %d\n",   $cm->{cmHeader}->{nSeq});
  printf OUT ("EFFN     %f\n",   $cm->{cmHeader}->{effn});
  if(exists $cm->{cmHeader}->{cksum}) { printf OUT ("CKSUM    %u\n",   $cm->{cmHeader}->{cksum}); } # unsigned 32-bit 
  printf OUT ("NULL     %s\n",   $cm->{cmHeader}->{null});
  if(exists $cm->{cmHeader}->{hitGA})  { printf OUT ("GA       %.2f\n", $cm->{cmHeader}->{hitGA}); }
  if(exists $cm->{cmHeader}->{hitTC})  { printf OUT ("TC       %.2f\n", $cm->{cmHeader}->{hitTC}); }
  if(exists $cm->{cmHeader}->{hitNC})  { printf OUT ("NC       %.2f\n", $cm->{cmHeader}->{hitNC}); }
  printf OUT ("EFP7GF   %.4f %.5f\n",  $cm->{cmHeader}->{efp7gf}->[0], $cm->{cmHeader}->{efp7gf}->[1]);
  
  #ECMLC    0.62369    -8.95393     0.81613     1600000      531557  0.002258
  #ECMGC    0.42792   -14.49103    -3.20105     1600000       50144  0.007977
  #ECMLI    0.53383    -8.38474     2.25076     1600000      350673  0.003422
  #ECMGI    0.47628    -9.31019     0.57693     1600000       44378  0.009013    
  if($i_am_calibrated) { # we defined this when we entered the subroutine 
    foreach my $rowlabel ("ecmlc", "ecmgc", "ecmli", "ecmgi") { 
      printf OUT ("%s    %.5f  %10.5f  %10.5f  %10.0f  %10d  %.6f\n", 
                  uc($rowlabel), 
                  $cm->{cmHeader}->{$rowlabel}->[0], 
                  $cm->{cmHeader}->{$rowlabel}->[1], 
                  $cm->{cmHeader}->{$rowlabel}->[2], 
                  $cm->{cmHeader}->{$rowlabel}->[3], 
                  $cm->{cmHeader}->{$rowlabel}->[4], 
                  $cm->{cmHeader}->{$rowlabel}->[5]);
    }
  }
  # done with cmHeader

  # print CM body, none of these values will have changed from when the CM was input (purposefully no functions exist to modify them)
  foreach my $line (@{$cm->{cmBody}}) { 
    print OUT $line;
  }

  # print HMM filter header:
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

  # most values are stored without the first token (e.g. 'NAME'), the first line ('version') is the exception.
  print  OUT ($cm->{hmmHeader}->{version} . "\n");
  printf OUT ("NAME  %s\n", $cm->{hmmHeader}->{name});
  if(exists $cm->{hmmHeader}->{acc})  { printf OUT ("ACC   %s\n", $cm->{hmmHeader}->{acc}); }
  if(exists $cm->{hmmHeader}->{desc}) { printf OUT ("DESC  %s\n", $cm->{hmmHeader}->{desc}); }
  printf OUT ("LENG  %d\n", $cm->{hmmHeader}->{length});
  printf OUT ("MAXL  %d\n", $cm->{hmmHeader}->{maxl});
  printf OUT ("ALPH  %s\n", $cm->{hmmHeader}->{alpha});
  printf OUT ("RF    %s\n", ($cm->{hmmHeader}->{rf}   == 1) ? "yes" : "no");
  printf OUT ("MM    %s\n", ($cm->{hmmHeader}->{mm}   == 1) ? "yes" : "no");
  printf OUT ("CONS  %s\n", ($cm->{hmmHeader}->{cons} == 1) ? "yes" : "no");
  printf OUT ("CS    %s\n", ($cm->{hmmHeader}->{cs}   == 1) ? "yes" : "no");
  printf OUT ("MAP   %s\n", ($cm->{hmmHeader}->{map}  == 1) ? "yes" : "no");
  if(exists $cm->{hmmHeader}->{date}) { printf OUT ("DATE  %s\n", $cm->{hmmHeader}->{date}); }
  if(exists $cm->{hmmHeader}->{com})  { 
    for(my $i = 0; $i < scalar(@{$cm->{hmmHeader}->{com}}); $i++) { 
      printf OUT ("COM   %s %s\n", "[" . ($i+1) . "]", $cm->{hmmHeader}->{com}->[$i]); 
    }
  }
  printf OUT ("NSEQ  %d\n",   $cm->{hmmHeader}->{nSeq});
  printf OUT ("EFFN  %f\n",   $cm->{hmmHeader}->{effn});
  if(exists $cm->{hmmHeader}->{cksum}) { printf OUT ("CKSUM %u\n",   $cm->{hmmHeader}->{cksum}); } # unsigned 32-bit 
  if(exists $cm->{hmmHeader}->{hitGA}) { printf OUT ("GA    %.2f\n", $cm->{hmmHeader}->{hitGA}); }
  if(exists $cm->{hmmHeader}->{hitTC}) { printf OUT ("TC    %.2f\n", $cm->{hmmHeader}->{hitTC}); }
  if(exists $cm->{hmmHeader}->{hitNC}) { printf OUT ("NC    %.2f\n", $cm->{hmmHeader}->{hitNC}); }
  printf OUT ("STATS LOCAL MSV      %8.4f %8.5f\n",  $cm->{hmmHeader}->{msvStats}->{mu},      $cm->{hmmHeader}->{msvStats}->{lambda});
  printf OUT ("STATS LOCAL VITERBI  %8.4f %8.5f\n",  $cm->{hmmHeader}->{viterbiStats}->{mu},  $cm->{hmmHeader}->{viterbiStats}->{lambda});
  printf OUT ("STATS LOCAL FORWARD  %8.4f %8.5f\n",  $cm->{hmmHeader}->{forwardStats}->{tau}, $cm->{hmmHeader}->{forwardStats}->{lambda});

  # HMM body
  # print CM body, none of these values will have changed from when the CM was input (purposefully no functions exist to modify them)
  foreach my $line (@{$cm->{hmmBody}}) { 
    print OUT $line;
  }

  close(OUT);
  return;
}

#-------------------------------------------------
    
=head2 taxinfoForHits

    Title    : taxinfoForHits
    Incept   : EPN, Mon Aug 19 15:04:50 2013
    Usage    : $io->taxinfoForHits($infoHHR, $groupOHAR, $groupOAR, $lead_group, $nprint, $user_level2print, $do_nsort)
    Function : Groups hits into taxonomic groups for either the 
             : 'taxinfo' output file of rfmake.pl or the taxonomic
             : grouping definitions for rfseed-add.pl.
             :
             : $infoHHR, and $groupOHAR must supplied by caller,
             : probably filled by a $io->parseOutlistAndSpecies() call.
             :
             : The 'taxinfo' file aims to concisely describe the taxonomic
             : groups represented by all of the hits for a family. To do this
             : the taxonomic strings (e.g. Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Pan.)
             : are summarized by taking a prefix and a 'prefix level' 
             : (the number of tokens in the prefix) that defines 5 distinct
             : taxa groups (at that prefix level) that comprise all SEED seqs.
             : Families with large phylogenetic breadth will have lower
             : prefix levels. Those with narrow breadth will have higher.
             : We go through considerable trouble to do this because
             : we want the 'taxinfo' output file to be relatively short and
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
    Args     : $outFH:          file handle for output, if undef: print only to STDOUT
             : $infoHHR:        ref to 2D hash, key 1: name/start-end (nse), key 2: "rank", "bitsc", "evalue", "sspecies" or "taxstr"
             : $groupOHAR:      ref to hash of arrays, nse in score rank order, by group
             : $groupOAR:       order of groups to use
             : $use_lead_group: '1' to display taxonomic prefixes in per-group sections, with first group first, etc.
             : $nprint:         target number of SEED taxonomy prefixes to print (-nprint from rfmake.pl)
             : $l2print:        print all unique prefixes of this length, if != 0 (-l2print from rfmake.pl)
             : $do_nsort:       '1' to sort output by counts (-nsort from rfmake.pl)
             : $prefixAR:       ref to array of prefixes (taxonomic groups) output in this function
             :                  filled if defined, but can be undefined
             : $also_stdout:    '1' to also print to STDOUT, cannot be 1 if $outFH is undef
    Returns  : $level_printed: prefix token length used for defining groups
    Dies     : upon file input/output error

=cut

sub taxinfoForHits {
  my ($self, $outFH, $infoHHR, $groupOHAR, $groupOAR, $use_lead_group, $nprint, $user_level2print, $do_nsort, $prefixAR, $also_stdout) = @_;

  # contract check
  if((! defined $outFH) && (defined $also_stdout) && ($also_stdout)) { 
    die "ERROR taxinfoForHits(): outFH undef but also_stdout is 1"; 
  }

  ####################################################################
  # Set parameters to their defaults prior to parsing cmd line options
  ####################################################################
  
  my $level2print = 1;
  my $emax        = 10;
  my $min_level2print = 3;
  my $taxstr;             # full taxonomy string from species
  my $prefix;             # a taxonomic string prefix 
  my $prv_prefix;         # previous prefix
  my $group;              # group, e.g. 'SEED'
  my $name;               # name of hit
  my ($i, $j);            # counters
  my $level;              # number of tokens in prefix; e.g. Eukaryota; Metazoa; Mollusca; == 3)
  my $parent_level;       # number of tokens in full parent string this prefix comes from
  my $maxlevel     = 1;   # maximum observed level
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
  my $cur_group     = ""; # highest group (earliest in groupOAR) which contains >= 1 hit to current tax prefix
  my %ngroupH       = (); # key: $group, value: number of sequences in a group
  my @level_ctA     = (); # number of unique taxonomic prefixes at each level 
  my @elA           = (); # for defining taxonomy string prefixes
  my @outputA       = (); # all output lines will be pushed here and actually output at the end
  my $g;                  # counter over group indices
  my @groupaddA     = ();
  my $ngroups = scalar(@{$groupOAR}); # number of groups
  if($ngroups == 0) { die "ERROR no groups in taxinfoForHits()"; }

  # fill %pfix_ctHH and %pfix_levelHH with counts/level of each prefix for each group
  foreach $group (@{$groupOAR}) {
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
        # remove trailing '.' on all elements, so "Proteobacteria." and "Proteobacteria" become equivalent
        for($i = 0; $i < scalar(@elA); $i++) { 
          $elA[$i] =~ s/\.$//;
        }
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
    }
  } # end of foreach group
      
  ##################################################
  # Determine 'level2print', the number of prefixes we'll use for taxonomic strings we output.
  # We do this differently depending on if there is a 'lead group'. 
  # (There is NOT if $lead_group was passed in as "").
  # 
  # If there is a lead group we determine the max taxonomy level that will
  # create $nprint or less taxonomic groups for the lead group only.
  #
  # If there is not a lead group we determine the taxonomy level that will
  # create $nprint total taxonmic groups across all groups.
  #
  my @groups2use = (); # an array of the groups we'll use to determine level2print
  if($use_lead_group) { 
    my $lead_group_idx = 0;
    my $lead_group = $groupOAR->[$lead_group_idx];
    if($ngroupH{$lead_group} == 0) { 
      $lead_group_idx++;
      if($lead_group_idx == $ngroups) { die "ERROR unable to define lead group in taxinfoForHits(), no seqs in any group"; }
      $lead_group = $groupOAR->[$lead_group_idx];
    }
    @groups2use = ($lead_group);
  }
  else { # else just use all groups for determining level2print
    @groups2use = @{$groupOAR};
  }
  
  @level_ctA = ();
  for($i = 0; $i <= $maxlevel; $i++) { $level_ctA[$i] = 0; }
  foreach $group (@groups2use) {
    foreach $prefix (sort keys(%{$pfix_ctHH{$group}})) { 
      # determine number of unique prefixes at each taxonomy level
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
  }        

  # determine maximum taxonomy level at which no more than $nprint prefixes exist
  while(($level2print < $maxlevel) && ($level_ctA[($level2print+1)] < $nprint)) { 
    $level2print++; 
    if($level_ctA[$level2print] > $nprint_actual) { 
      $nprint_actual = $level_ctA[$level2print];
    }
  }
        
  # printf("nprint_actual: $nprint_actual\n");
  # printf("level2print:   $level2print\n");
  
  # nprint_actual is the number of prefixes we'll print for seed.
  # Now, pick min taxonomy level that has exactly nprint_actual prefixes,
  # to minimize length of prefix strings
  while(($level2print >= 1) && ($level_ctA[$level2print-1] >= $nprint_actual)) { 
    #printf("level_ctA[%d] is %d (<= %d) decreasing level2print by 1\n", $level2print-1, $level_ctA[$level2print-1], $nprint_actual);
    $level2print--; 
  }
  # if level2print is less than our minimum, use that
  if($level2print < $min_level2print) { $level2print = $min_level2print; }
  
  # user defined what level2print they want, ignore the one we just determined
  if($user_level2print > 0) { $level2print = $user_level2print; }

  # finished determining level2print
  ###########################################################
  
  # determine max count of any prefix in any group,
  # label which ones to print in %toprintH
  foreach $group (@{$groupOAR}) { 
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
        }
      }
    }
  }

  # We now know all the prefixes we will print. Go back through and determine 
  # the index of the first unique token in each prefix, we will capitalize this 
  # when we print it out below. This makes it easier for a user to get an idea
  # of how far apart (diverged) each group is from all other groups.
  
  # make a 2D array of the tokens of each prefix
  my @pAA = ();  # array of arrays, each prefix broken down into tokens
  my @pA  = ();  # temporary full prefix array
  my %pfix_unq_levelH = (); # key: $prefix ($toprintH{$prefix} is 1);  value is 1st token level that is unique (differs from all other prefixes we'll print at that level)
  my @ntokA    = (); # 0..$i..$nprefix: number of tokens in prefix $i
  my $x_ntok   = 0;  # maximum number of tokens in all prefixes
  my @xtoklenA = (); # 0..$i..$x_ntok-1: maximum length of all tokens at level $i in all prefixes

  $nprefix = scalar(keys %toprintH);
  # fill pAA with all tokens, remember to remove whitespace
  # also determine maximum length token at each level for pretty output formatting
  $i = -1;
  foreach $prefix (keys %toprintH) { 
    push(@pA, $prefix);
    $i++;
    my @tmpA = split(";", $prefix);
    # remove trailing '.' on all elements, so "Proteobacteria." and "Proteobacteria" become equivalent
    for(my $z = 0; $z < scalar(@tmpA); $z++) { 
      $tmpA[$z] =~ s/\.$//;
    }
    $ntokA[$i] = scalar(@tmpA);
    if($ntokA[$i] > $x_ntok) { 
      for($j = $x_ntok; $j < $ntokA[$i]; $j++) { 
        $xtoklenA[$j] = 0;
      }
      $x_ntok = $ntokA[$i]; 
    }
    for($j = 0; $j < scalar($ntokA[$i]); $j++) { 
      if(length($tmpA[$j]) > $xtoklenA[$j]) { $xtoklenA[$j] = length($tmpA[$j]); }
      $tmpA[$j] =~ s/\s+//g; # remove whitespace
    }
    push(@pAA, [@tmpA]);
  }
  # determine total width of prefix string in output
  my $prefix_width = 0;
  for($j = 0; $j < $x_ntok; $j++) { 
    $prefix_width += $xtoklenA[$j] + 2; # the + 2 is for ' |' which we append to each token
  }
  if($prefix_width < length("# taxonomy string prefix (xx levels)")) { 
    $prefix_width = length("# taxonomy string prefix (xx levels)");
  }

  # foreach prefix, find first token level that is unique
  for($i = 0; $i < $nprefix; $i++) { 
    $prefix = $pA[$i];
    # printf STDERR "prefix: $prefix\n";
    for($level = 0; $level < $ntokA[$i]; $level++) { 
      my $tok = $pAA[$i][$level];
      for($j = 0; $j < $nprefix; $j++) { 
        # printf STDERR ("\tj: $j i: $i level: $level ntokA[$j] $ntokA[$j]\n");
        if($j != $i && $level < $ntokA[$j]) { # skip self and any tokens that have run off bounds of this prefix
          if($pAA[$j][$level] eq $tok) { 
            # match, $tok is not unique, break loop 
            $j = $nprefix + 1; # this will be our flag for knowing $tok is NOT unique
          }
        }
      }
      if($j == $nprefix) { 
        # $tok is unique
        $pfix_unq_levelH{$prefix} = $level;
        # printf STDERR ("setting pfix_unq_levelH{$prefix} to $level\n");
        $level = $ntokA[$i] + 1; 
        # this breaks us out of the 'for (my $level' loop and acts as a flag for 'we found a unique token'
        # for that will fail the 'if($level == $ntokA[$i])' statement below
      }
    }
    # *!* Do not check to make sure that each prefix has a unique token, some may not.
    # for example: 
    #   Bacteria; Gemmatimonadetes; Gemmatimonadales
    #   Bacteria; Spirochaetes; environmental samples.
    #   Bacteria; Gemmatimonadetes; environmental samples.
    #
    # The third prefix does not have a unique token. We could automatically
    # capitalize the final token in these cases, but currently we don't
    # capitalize any when this happens.
    #
    if($level == $ntokA[$i]) { 
      $pfix_unq_levelH{$prefix} = -1;
    }
  }

  # Now, print the prefixes and their counts out in a particular order
  # First print all prefixes with >=1 members in SEED in sorted order
  # from low to high minimum E-value in group, then print any with >= 1 members in FULL
  # from low to high minimum E-value in group, then print any with >= 1 members in OTHER
  # from low to high minimum E-value in group.
  # (If $do_nsort is '1' we'll sort by total counts per group not 
  # minimum E-value).
  
  # first determine width for each group block
  my %group_lenH  = ();  # length of each group block
  my %ct_lenH     = ();  # length of each ct string
  my $div_length = $prefix_width + 2;
  if($ngroups <= 3) { $div_length += 3; }
  else              { $div_length += $ngroups; }
  foreach $group (@{$groupOAR}) { 
    $ct_lenH{$group}    = 7;
    $group_lenH{$group} = $ct_lenH{$group} + 2 + 6; # 2 spaces after 'ct' column, 6 for 'minE' column
    if($group_lenH{$group} < length($group)) { 
      $group_lenH{$group} = length($group);
      $ct_lenH{$group}    = $group_lenH{$group} - (6+2); 
    }
    $div_length += $group_lenH{$group};
  }
  my $div_line = "#" . Bio::Rfam::Utils::monocharacterString("=", $div_length);

  my $printed_sep = 0;
  if($use_lead_group && $ngroupH{$groupOAR->[0]} == 0) { $printed_sep = 1; }
  push(@outputA, "$div_line\n");

  # first print column headings
  my $tax_header = sprintf("# taxonomy string prefix (%d levels)", $level2print);
  my $tax_uline  = "#";
  for($i = 0; $i < $prefix_width-1; $i++) { $tax_uline .= "-"; }

  my $mem_length = ($ngroups < 3) ? 3 : $ngroups;

  # line 1
  my $outstr = sprintf("%-*s  %-*s", $prefix_width, "#", $mem_length, "");
  foreach $group (@{$groupOAR}) {
    $outstr .= sprintf("      %*s", $group_lenH{$group}, $group);
  }
  $outstr .= "\n";
  push(@outputA, $outstr);

  # line 2
  $outstr = sprintf("%-*s  %-*s", $prefix_width, "#", $mem_length, "");
  foreach $group (@{$groupOAR}) { 
    $outstr .= sprintf("      %s", Bio::Rfam::Utils::monocharacterString("-", $group_lenH{$group}));
  }
  $outstr .= "\n";
  push(@outputA, $outstr);

  # line 3
  $outstr = sprintf("%-*s  %-*s", $prefix_width, $tax_header, $mem_length, "mem");
  foreach $group (@{$groupOAR}) { 
    $outstr .= sprintf("      %*s  %6s", $ct_lenH{$group}, "ct", "minE");
  }
  $outstr .= "\n";
  push(@outputA, $outstr);

  # line 4
  $outstr = sprintf("%-*s  %-*s", $prefix_width, $tax_uline, $mem_length, "---");
  foreach $group (@{$groupOAR}) { 
    $outstr .= sprintf("      %-*s  %6s", $ct_lenH{$group}, Bio::Rfam::Utils::monocharacterString("-", $ct_lenH{$group}), "------");
  }
  $outstr .= "\n";
  push(@outputA, $outstr);

  # this simplifies sorting prefixes when printing
  for($g = 0; $g < $ngroups; $g++) { 
    if($do_nsort) { 
      $toaddH{$groupOAR->[$g]} = ($ngroups - ($g + 1)) * $max_ngroup; 
    }
    else { 
      $toaddH{$groupOAR->[$g]} = -10000 * ($ngroups - $g);
    }
  }
  
  # this is a laborious sort, go through all remaining prefixes that we haven't 
  # printed yet, looking for the correct one to print next.
  for($i = 1; $i <= $nprefix; $i++) { 
    $max_eff_ct   = 0;
    $min_eff_Eexp = 1000000000;
    $best_prefix = "";
    foreach $prefix (keys %toprintH) {
      if(! exists ($printedH{$prefix})) {
        foreach $group (@{$groupOAR}) { 
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

    # determine if we need a newline because prev line included >=1 count in current group and next line will not
    if($use_lead_group && $i > 1 
       && (! exists ($pfix_ctHH{$cur_group}{$best_prefix}))) { 
      # line above had >= 1 in cur_group, but next line has 0
      push(@outputA, "#\n");
    }
    
    # determine group string (e.g. 'SFO') summarizing group membership for this prefix
    $group_string = "";
    foreach $group (@{$groupOAR}) { 
      my $letter = substr($group, 0, 1);
      if(exists ($pfix_ctHH{$group}{$best_prefix})) { 
        $group_string .= $letter;
      }
      else { 
        $group_string .= "-";
      }
    }
    # when printing the prefix, capitalize the first unique token in each prefix (we calculated
    # which token this is already above and stored it pfix_unq_levelH), 
    # we'll call this $prefix2print.
    if(! exists ($pfix_unq_levelH{$best_prefix})) { die "ERROR unexpectedly don't have token level to capitalize for $best_prefix"; }
    my $cap_prefix;
    if($pfix_unq_levelH{$best_prefix} == -1) { # our flag for 'this token has no unique prefix' (see example above, search for '*!*')
      $cap_prefix = $best_prefix;
    }
    else { # this prefix DOES have a unique token:
      $cap_prefix = Bio::Rfam::Utils::capitalize_token_in_taxstring($best_prefix, $pfix_unq_levelH{$best_prefix});
    }
    my $prefix2print = Bio::Rfam::Utils::pad_tokens_in_taxstring($cap_prefix, \@xtoklenA, " |"); 
    push(@outputA, sprintf("%-*s  %3s", $prefix_width, $prefix2print, $group_string));
    if(defined $prefixAR) { push(@{$prefixAR}, $prefix2print); }

    # print counts for each group for this prefix
    $cur_group = undef;
    for($g = 0; $g < $ngroups; $g++) { 
      $group = $groupOAR->[$g];
      if(exists ($pfix_ctHH{$group}{$best_prefix})) { 
        push(@outputA, sprintf("      %*d  %6.1g", 
                               $ct_lenH{$group},
                               $pfix_ctHH{$group}{$best_prefix}, 
                               $pfix_minEHH{$group}{$best_prefix}));
        if(! defined $cur_group) { $cur_group = $group; }
        $nprintedH{$group} += $pfix_ctHH{$group}{$best_prefix};
      }
      else { 
        push(@outputA, sprintf("      %*s  %6s", $ct_lenH{$group}, "-", "-"));
      }
    }
    push(@outputA, "\n");
    $printedH{$best_prefix} = 1;
  } # end of loop over all prefixes
  # make dividing line
  my $tmp_line = "";
  $tmp_line .= sprintf("%-*s  %3s", $prefix_width, "#", "");
  foreach $group (@{$groupOAR}) { 
    $tmp_line .= sprintf("      %-s", Bio::Rfam::Utils::monocharacterString("-", $group_lenH{$group}));
  }
  $tmp_line .= "\n";
  push(@outputA, $tmp_line);

  $tmp_line = "# total hits:";
  for($i = 0; $i < ($prefix_width-13 + 5); $i++) { $tmp_line .= " "; }
  foreach $group (@{$groupOAR}) { 
    if($ngroupH{$group} == 0) { $nprintedH{$group} = 0; }
    if($nprintedH{$group} != $ngroupH{$group}) { 
      printf STDERR ("ERROR incorrect number of $group seqs (%d != %d)\n", $nprintedH{$group}, $ngroupH{$group}); 
      exit(1);
    }
    $tmp_line .= sprintf("      %*d        ", $ct_lenH{$group}, $nprintedH{$group});
  }
  $tmp_line .= "\n";
  push(@outputA, $tmp_line);

  push(@outputA, "$div_line\n#\n");

  # potentially print to a file handle
  if(defined $outFH) { 
    foreach my $line (@outputA) { print $outFH $line; }
  }
  # potentially print to STDOUT
  if((! defined $outFH) || (defined $also_stdout && $also_stdout)) { 
    foreach my $line (@outputA) { print STDOUT $line; }
  }
  return $level2print;
}

#-------------------------------------------------
    
=head2 parseOutlistAndSpecies

    Title    : parseOutlistAndSpecies
    Incept   : EPN, Mon Aug 19 15:17:17 2013
    Usage    : parseOutlistAndSpecies($outlist, $species, $emax, $ga, $do_allseed, $infoHHR, $nameOAR, $groupOHAR)
    Function : Parses $outlist and $species files into infoHHR, nameOAR, groupOHAR
             : and groupOAR data structures, which are used by writeTaxinfoFromOutlistAndSpecies()
             : among other functions. 
    Args     : $outlist:    name of outlist file, usually 'outlist'
             : $species:    name of species file, usually 'species'
             : $emax:       maximum E-value to consider, usually 10
             : $ga:         GA bit score threshold
             : $minsc:      only collect info on hits above this score
             :              if undefined or "", collect info on all hits
             : $do_allseed: '1' to force all seed sequences be included if $minsc != ""
             : $infoHHR:    ref to 2D hash, key 1: name/start-end (nse), key 2: "rank", "bitsc", "evalue", "sspecies" or "taxstr"
             :              can be undefined if caller does not need this
             : $nameOAR:    ref to array, all nse, in order, ranked by score/E-value
             :              can be undefined if caller does not need this
             : $groupOHAR:  ref to hash of arrays, nse in score rank order, by group
             :              can be undefined if caller does not need this
    Returns  : void
    Dies     : if outlist and species are not consistent, or do not exist

=cut

sub parseOutlistAndSpecies {
  my($self, $outlist, $species, $emax, $ga, $minsc, $do_allseed, $infoHHR, $nameOAR, $groupOHAR) = @_;
  
  my ($ct, $outline, $spcline, $name, $i, $i0, $key, $pkey, $group);
  my @out_elA = ();
  my @spc_elA = ();
  my %nameIH  = ();
  $ct = 0;
  
  Bio::Rfam::FamilyIO::validate_outlist_format($outlist, 0); # '0' says, don't require 'full' outlist file
  Bio::Rfam::FamilyIO::validate_species_format($species, 0); # '0' says, don't require 'full' species file
  
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
      # 108.5  4.2e-20      SEED  Z97632.3          v:73.4   23636   23554    -       1    83     no  Homo_sapiens_(human)[9606]        Human DNA sequence from clone RP1-196E23 on chromosome Xq26.1-27.2 Description...
      # example species line:
      # 108.5  4.2e-20      SEED  Z97632.3          v:73.4    9606  Homo sapiens (human)                                        Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Homo.
      @out_elA = split(/\s\s+/, $outline); # note: we separate by double spaces
      @spc_elA = split(/\s\s+/, $spcline); # note: we separate by double spaces

      if((defined $minsc && $minsc ne "" && $out_elA[0] < $minsc) && (! $do_allseed)) { 
        last; # breaks us out of 'while($outline = <OUT>)' loop
      }
      
      if((! defined $minsc || $minsc eq "")          || # no minimum being enforced
         ($out_elA[0] >= $minsc)                     || # we're above our minimum
         ($do_allseed && $out_elA[2] eq "SEED")) {      # we're forcing info collection on all seed seqs and we've got one here        
        #sanity check
        for($i = 0; $i <= 3; $i++) { 
          if($out_elA[$i] ne $spc_elA[$i]) { 
            die "ERROR, hit $ct, element $i does not match b/t outlist and species files ($out_elA[$i] ne $spc_elA[$i])"; 
          }
        }
        
        $name = $out_elA[3] . "/" . $out_elA[5] . "-" . $out_elA[6]; 
        if(defined $nameOAR) { push(@{$nameOAR}, $name); }
        if(exists ($nameIH{$name})) { die "ERROR $name is duplicated"; }
        $nameIH{$name} = 1;
        
        # determine group
        $group = "";
        if   ($out_elA[2] eq "SEED")  { $group = "SEED"; } 
        elsif($out_elA[0] >= $ga)     { $group = "FULL"; } 
        elsif($out_elA[1] <= $emax)   { $group = "OTHER"; } 
        if(defined $groupOHAR) { push(@{$groupOHAR->{$group}}, $name); }
        
        if(defined $infoHHR) { 
          $infoHHR->{$name}{"rank"}     = $ct;
          $infoHHR->{$name}{"bitsc"}    = $out_elA[0];
          $infoHHR->{$name}{"evalue"}   = $out_elA[1];
          $infoHHR->{$name}{"sspecies"} = $out_elA[11];
          $infoHHR->{$name}{"taxstr"}   = $spc_elA[7];
          $infoHHR->{$name}{"trunc"}    = $out_elA[10];
        }
      }
    }
  }
  close(OUT);
  close(SPC);
}

#-------------------------------------------------
    
=head2 nseArrayFromOutlistOrTblout

    Title    : nseArrayFromOutlistOrTblout
    Incept   : EPN, Thu Nov 14 09:34:21 2013
    Usage    : nseArraryFromOutlistOrTblout($infile, $intype, $min_bitsc, $nseAR)
    Function : Read an outlist or --tblout Infernal file and push each name/start-end
             : above $min_bitsc to $nseAR. 
    Args     : $infile:    outlist file or tblout file with list of hits
             : $intype:    either 'outlist' or 'tblout', so we know format of $infile
             : $min_bitsc: fetch all hits >= $min_bitsc, if undefined or "", do all hits
             : $nseAR:     ref to array to push "name/start-end" to
    Returns  : void
=cut

sub nseArrayFromOutlistOrTblout {
  my ($infile, $intype, $min_bitsc, $nseAR) = @_;

  if($intype eq "outlist") { 
    Bio::Rfam::FamilyIO::validate_outlist_format($infile, 0); # '0' says, don't require 'full' species file
  }
  elsif($intype ne "tblout") { 
    die "ERROR Bio::Rfam::FamilyIO::nseArrayFromOutlistOrTblout: intype is not \'outlist\' nor \'tblout\'";
  }

  my $fetch_all_hits = 0;
  my $cur_bitsc = "";
  if(defined $min_bitsc && $min_bitsc ne "") { 
    $cur_bitsc = $min_bitsc + 1;
  }
  else { 
    $fetch_all_hits = 1;
  }

  open(IN, $infile) || die "ERROR unable to open $infile"; 
  while(my $line = <IN>) {
    if($line !~ m/^\#/) { 
      $line =~ s/^\s+//; # remove leading whitespace
      my ($cur_bitsc, $name, $start, $end);
      if($intype eq "outlist") { 
        # example outlist line:
        # 122.1  9.5e-28      FULL  CAAA01222947.1         -    8801    8703    -       1    99     no  Mus_musculus_(house_mou..[10090]   Mus musculus whole genome shotgun assembly contig 222946
        my @elA = split(/\s\s+/, $line); # note: we separate by double spaces
        ($cur_bitsc, $name, $start, $end) = ($elA[0], $elA[3], $elA[5], $elA[6]);
      }
      else { # tblout
        # example tblout line:
        # CAAA01222947.1       -         mir-351              RF00805    cm        1       99     8801     8703      -    no    1 0.51   0.0  122.1   9.5e-28 !   Mus musculus whole genome shotgun assembly contig 222946
        my @elA = split(/\s+/, $line); # note: we separate by single spaces
        ($cur_bitsc, $name, $start, $end) = ($elA[14], $elA[0], $elA[7], $elA[8]);
      }
      if($fetch_all_hits || $cur_bitsc >= $min_bitsc) { 
        my $nse = "$name/$start-$end";
        # validate it
        my ($validated, undef, undef, undef, undef) = Bio::Rfam::Utils::nse_breakdown($nse);
        if(! $validated) { die "ERROR something wrong with outlist or TBLOUT line, can't break it down to name/start-end format ($line)"; }
        # if we get here, it's been validated
        push(@{$nseAR}, $nse);
      }
      # we could optimize slightly here, by stopping if we're an outlist 
      # and we've dropped below our min score, but that would only work
      # if the outlist were sorted, which it should be, but it's probably
      # better not to assume that, so we just go through all lines
    }
  }
  close(IN);

  return;
}

#-----------------------------------------------------------------

=head2 fetchFromOutlistOrTblout

    Title    : fetchFromOutlistOrTblout
    Incept   : EPN, Tue Nov 12 14:27:20 2013
    Usage    : fetchFromOutlist($infile, $intype, $fetchfile, $min_bitsc, $outfile, $logFH)
    Function : Fetch all hits listed in an 'outlist' file with bit scores
             : >= $min_bitsc (if $min_bitsc is "", fetch all seqs) 
             : from $fetchfile and output to $outfile (or return 
             : seqstring if $outfile is "" or undefined).
    Args     : $infile:    outlist file or tblout file with list of hits
             : $intype:    either 'outlist' or 'tblout', so we know format of $infile
             : $fetchfile: file to fetch seqs from
             : $min_bitsc: fetch all hits >= $min_bitsc, if undefined or "", fetch all hits
             : $outfile:   output file for fetched seqs, if undefined or "", return $seqstring
             : $logFH:     file handle to output progress info on fetching to, unless undefined
             : $do_stdout: output progress to stdout too
    Returns  : $nseq:      number of sequences fetched
             : $nres:      number of residues fetched
             : $seqstring: string of all seqs, IFF $outfile is undefined or ""
=cut

sub fetchFromOutlistOrTblout {
  my ($infile, $intype, $fetchfile, $min_bitsc, $outfile, $logFH, $do_stdout) = @_;

  my @nseA = (); 

  # get array of name/start-end, this function will validate the input also
  Bio::Rfam::FamilyIO::nseArrayFromOutlistOrTblout($infile, $intype, $min_bitsc, \@nseA);

  # and fetch the seqs
  return Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@nseA, $fetchfile, undef, $outfile, $logFH, $do_stdout); #undef: default line length for fasta seqs
}

#-------------------------------------------------
    
=head2 parseTbloutForOverlapCheck

    Title    : parseTbloutForOverlapCheck
    Incept   : EPN, Wed Oct  9 12:58:13 2013
    Usage    : parseTbloutForOverlapCheck($tblout, $tbloutHAR)
    Function : Parses $tblout out into a very specific hash, referenced by $tbloutHR
             : key is target sequence name of each hit, value is an array of scalars
             : of the form "<start>:<end>:<bitsc>".
             : This hash of arrays is used to determine the highest scoring overlapping 
             : hit on the opposite strand (if any) which is part of 'outlist' and 'species'.
    Args     : $tblout:    name of TBLOUT file, usually 'TBLOUT'
             : $tbloutHAR: ref to hash of arrays, key is target sequence name, value is array
             :             of scalars of form "<start>:<end>:<bitscore>"
    Returns  : void
    Dies     : if TBLOUT is not readable

=cut

sub parseTbloutForOverlapCheck {
    my($tblout, $tbloutHAR) = @_;

    open(TBL, $tblout) || die "ERROR unable to open $tblout";
    # note that we do not need to sort by score
    while(my $line = <TBL>) { 
      if($line !~ m/^\#/) { 
        my ($bits, $evalue, $name, $start, $end, $strand, $qstart, $qend, $trunc, $shortSpecies, $description, $ncbiId, $species, $taxString) = 
            processTbloutLine($line, undef, undef, 0, 0); 
        #                               sthDesc, sthTax, is_reversed, require_tax: we don't care about tax info
        push(@{$tbloutHAR->{$name}}, $start . ":" . $end . ":" . $bits);
      }
    }
    close(TBL);
}

#-------------------------------------------------
    
=head2 parseTbloutForMinimumScore

    Title    : parseTbloutForMinimumScore
    Incept   : EPN, Wed Nov 20 12:30:20 2013
    Usage    : parseTbloutForMinimumScore($tblout, $do_pertarget)
    Function : Returns minimum score in a 'tblout' file. If $do_pertarget we return
             : minimum per-target score, so if one target has > 1 hits, only its
             : highest score is considered a candidate for the minimum score returned.
    Args     : $tblout:       name of TBLOUT file, usually 'TBLOUT'
             : $do_pertarget: TRUE to only consider top scoring hit per target
    Returns  : minimum score
    Dies     : if TBLOUT is not readable

=cut

sub parseTbloutForMinimumScore {
  my($tblout, $do_pertarget) = @_;
  
  my $minbits = undef;
  
  if(! $do_pertarget) { # simpler case, just look at all lines and return lowest scoring hit
    open(TBL, $tblout) || die "ERROR unable to open $tblout";
    # note that we do not need to sort by score
    while(my $line = <TBL>) { 
      if($line !~ m/^\#/) { 
        my ($bits) = processTbloutLine($line, undef, undef, 0, 0); 
        #                                     sthDesc, sthTax, is_reversed, require_tax: we don't care about tax info
        if((! defined $minbits) || $bits < $minbits) { $minbits = $bits; }
      }
    }
    close(TBL);
  }
  else { # $do_pertarget is TRUE, more complex case, need to do two passes
    # first, use parseTbloutForOverlapCheck() to get an array of hits for each target sequence
    my %tbloutHA = ();
    Bio::Rfam::FamilyIO::parseTbloutForOverlapCheck($tblout, \%tbloutHA);
    # now go through each target, find it's maximum and use that as a candidate for the minimum
    foreach my $target (keys %tbloutHA) { 
      my $max = undef;
      foreach my $startendbits (@{$tbloutHA{$target}}) { 
        my ($start, $end, $bits) = split(":", $startendbits);
        if((! defined $max) || $max < $bits) { $max = $bits; }
      }
      if((! defined $minbits) || $minbits > $max) { $minbits = $max; }
    }
  }
  return $minbits;
}

#-----------------------------------------------------------------
    
=head2 writeHitComparison

    Title    : writeHitComparison
    Incept   : EPN, Fri Aug 30 13:28:08 2013
    Usage    : $io->writeHitComparison($infoHHR, $groupOHAR, $new_ga, $new_evalue, $old_ga, $olddir)
    Function : Given 'new' hit information (infoHH and groupOHAR) from a 
             : $io->parseOutlistAndSpecies call, as well as another
             : 'old' directory with outlist and species files, compare the
             : hits found in SEED and FULL groups between the new
             : hits and the old hits.
             :
             : If ($old_flag) is '1', the 'old' directory is actually
             : Rfam 11.0, with an 'out.list' file instead of 'outlist'
             : which has differently formatted lines.
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
    Args     
             
             : $infoHHR:     2D hash ref, filled by caller via parseOutlistAndSpecies
             : $groupOHAR:   hash of arrays ref, filled by caller via parseOutlistAndSpecies
             : $old_outlist: old search results, usually $old_dir . "/outlist" OR $old_dir . "/out.list"
             : $old_species: old search results with tax info, usually $old_dir . "/" . 'species'
             : $new_outlist: new search results, usually "outlist"
             : $new_species: new search results with tax info, usually "species"
             : $old_flag:    '1' if $old_dir is old Rfam 11.0 format, '0' if its the current format.
             : $concat_flag: '1' to concatenate 'comparison' file output to existing file, else create a new one

    Returns  : void
    Dies     : upon file input/output error

=cut

sub writeHitComparison {
  my ($self, $infoHHR, $groupOHAR, $old_outlist, $old_species, $new_outlist, $new_species, $old_flag, $concat_flag) = @_;

  my %newHHA; # 1st key: group ("SEED" or "FULL"), 2nd key: seqname (not nse), array of 'start-end';
  my %newctH; # key: group ("SEED" or "FULL"), value number of new hits in group
  my %oldctH; # key: group ("SEED" or "FULL"), value number of old hits in group
  my %nnewH;  # key: group ("SEED" or "FULL"), value number of new hits with no overlaps in old hits
  my %nlostH; # key: group ("SEED" or "FULL"), value number of old hits with no overlaps in new hits
  my %newolH; # key: group ("SEED" or "FULL"), value number of new hits that overlap >= 1 old hit
  my %oldolH; # key: group ("SEED" or "FULL"), value number of old hits that overlap >= 1 new hit
  # first recast infoHHR into newHHA
  foreach my $group ("SEED", "FULL") { 
    $nlostH{$group}   = 0;  # number of hits lost (in old results but not new results)
    $nnewH{$group}    = 0;  # number of new hits  (in new results but not old results)
    $newolH{$group} = 0;  # number of new hits that overlap with an old hit 
    $oldolH{$group} = 0;  # number of old hits that overlap with a  new hit 
    foreach my $nse (@{$groupOHAR->{$group}}) { 
      my (undef, $name, $start, $end, $str) = Bio::Rfam::Utils::nse_breakdown($nse);
      push(@{$newHHA{$group}{$name}}, $start . ":" . $end);
      $newctH{$group}++;
    }
  }

  # now for each hit in out.list, do we have a match in the new outlist?
  open(OLDOUT, $old_outlist) || die "ERROR unable to open $old_outlist";
  open(OLDSPC, $old_species) || die "ERROR unable to open $old_species";
  my $out_lostoutlist = "lostoutlist";
  my $out_lostspecies = "lostspecies";
  open(LOSTOUT, ">$out_lostoutlist") || die "ERROR unable to open $out_lostoutlist for writing";
  open(LOSTSPC, ">$out_lostspecies") || die "ERROR unable to open $out_lostspecies for writing";
  my $ncomment = 0;
  while(my $outline = <OLDOUT>) { 
    my $spcline = <OLDSPC>;
    if($outline =~ m/^\#/) { 
      if($ncomment < 3) { print LOSTOUT $outline; } # print first 3 comment lines
      if($spcline !~ m/^\#/) { croak "ERROR old out.list and species lines inconsistent!\n$outline\n$spcline\n"; }
      if($ncomment < 3) { print LOSTSPC $spcline; } # print first 3 comment lines
      $ncomment++;
    }
    else { 
      my ($group, $bitsc, $evalue, $name, $start, $end);
      if($old_flag) {
        # example OLD RFAM 11.0 out.list format:
        #91.32	8.65e-15	ALIGN	ACFV01061888.1	      1855	      1756	1	100	.	Callithrix_jacchus_w	Callithrix jacchus Contig81.61, whole genome shotgun sequence.
        my @elA = split(/\s+/, $outline);
        ($group, $bitsc, $evalue, $name, $start, $end) = ($elA[2], $elA[0], $elA[1], $elA[3], $elA[4], $elA[5]);
      }
      else { 
        # example 'current' outlist line:
        # 108.5  4.2e-20      SEED  Z97632.3          v:73.4   23636   23554    -       1    83     no  Homo_sapiens_(human)[9606]        Human DNA sequence from clone RP1-196E23 on chromosome Xq26.1-27.2 Description...
        $outline =~ s/^\s+//; # remove leading spaces
        my @elA = split(/\s+/, $outline);
        ($group, $bitsc, $evalue, $name, $start, $end) = ($elA[2], $elA[0], $elA[1], $elA[3], $elA[5], $elA[6]);
      }
      # note, we don't check to make sure species line corresponds to outlist line here, but we do below before outputting it
      if   ($group eq "SEED")  { $group = "SEED"; }
      elsif($group eq "ALIGN") { $group = "FULL"; }
      elsif($group eq "FULL")  { ; } # leave it alone
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
            $i = scalar(@{$newHHA{$group}{$name}}); # breaks us out of the 'for(my $i' loop
          }
        }
      }
      # if we didn't find an overlap output line to 'lostoutlist' and 'lostspecies' files
      if(! $found_overlap) { 
        # before printing, do a sanity check: does species line correspond to the same hit
        my @spc_elA = ();
        if($old_flag) { 
          # example OLD RFAM 11.0 species format
          #91.32	8.65e-15	ALIGN	ACFV01061888.1	           9483	Callithrix jacchus (	Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Platyrrhini; Cebidae; Callitrichinae; Callithrix.
          @spc_elA = split(/\t/, $spcline);
        }
        else { 
          # example 'current' outlist line
          # 89.5  7.7e-16      SEED  AADA01319887.1        -    9598  Pan troglodytes (chimpanzee)                                    Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Pan.
          $spcline =~ s/^\s+//; # remove leading spaces
          @spc_elA = split(/\s\s+/, $spcline); # note: we separate by double spaces
        }
        if($spc_elA[0] ne $bitsc || $spc_elA[3] ne $name) { croak "ERROR old outlist and species lines inconsistent!\n$outline\n$spcline\n"; }
        print LOSTOUT $outline; 
        print LOSTSPC $spcline;
        $nlostH{$group}++;
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
  foreach $group ("SEED", "FULL") { 
    foreach $name (keys %{$newHHA{$group}}) { 
      foreach $startend (@{$newHHA{$group}{$name}}) { 
        my ($start, $end) = split(":", $startend);
        if($start > 0) { # above, we marked each found hit by making start negative 
          $newhitH{$name . "/" . $start . "-" . $end}  = 1;;
          $nnewH{$group}++;
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
  Bio::Rfam::FamilyIO::validate_outlist_format($new_outlist, 1); # '1' says, require 'full' outlist file
  Bio::Rfam::FamilyIO::validate_species_format($new_species, 1); # '1' says, require 'full' species file
  open(NEWOUT, $new_outlist) || die "ERROR unable to open $new_outlist for reading";
  open(NEWSPC, $new_species) || die "ERROR unable to open $new_species for reading";
  my $out_newoutlist = "newoutlist";
  my $out_newspecies = "newspecies";
  open($newoutFH, ">$out_newoutlist") || die "ERROR unable to open $out_newoutlist for writing";
  open($newspcFH, ">$out_newspecies") || die "ERROR unable to open $out_newspecies for writing";
  while($outline = <NEWOUT>) { 
    $spcline = <NEWSPC>;
    if($outline !~ m/^\#/) { 
      $outline =~ s/^\s+//; # remove leading whitespace
      $spcline =~ s/^\s+//; # remove leading whitespace
      chomp $outline;
      chomp $spcline;
      my @elA = split(/\s\s+/, $outline);
      # 108.5  4.2e-20      SEED  Z97632.3          v:73.4   23636   23554    -       1    83     no  Homo_sapiens_(human)[9606]        Description...
      my $nse = $elA[3] . "/" . $elA[5] . "-" . $elA[6];
      if(exists $newhitH{$nse}) { 
        my @spcelA = split(/\s\s+/, $spcline);
        # 108.5  4.2e-20      SEED  Z97632.3          v:73.4    9606  Homo sapiens (human)                                        Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Homo.
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

  my $comparison = "comparison";
  if($concat_flag) { 
    open(COMP, ">>" . $comparison) || die "ERROR unable to open $comparison for concatenated output";
  }
  else { 
    open(COMP, ">" . $comparison) || die "ERROR unable to open $comparison for writing";
  }
  if($newolH{"SEED"} == 0 && $newolH{"FULL"} == 0) { 
    printf COMP ("# WARNING: no hits overlap between old and new searches\n#\n");
  }
  printf COMP ("# counts of hits in SEED and FULL from old and new searches:\n");
  printf COMP ("#\n");
  printf COMP ("# %8s  %7s  %7s\n", "", "SEED", "FULL");
  printf COMP ("# %8s  %7s  %7s\n", "", "=======", "=======");
  printf COMP ("%-10s  %7d  %7d\n", "old-total",  $oldctH{"SEED"},   $oldctH{"FULL"});
  printf COMP ("%-10s  %7d  %7d\n", "new-total",  $newctH{"SEED"},   $newctH{"FULL"});
  printf COMP ("%-10s  %7d  %7d\n", "old-both",   $oldolH{"SEED"},   $oldolH{"FULL"});
  printf COMP ("%-10s  %7d  %7d\n", "new-both",   $newolH{"SEED"},   $newolH{"FULL"});
  printf COMP ("%-10s  %7d  %7d\n", "old-unique", $nlostH{"SEED"},   $nlostH{"FULL"});
  printf COMP ("%-10s  %7d  %7d\n", "new-unique", $nnewH{"SEED"},    $nnewH{"FULL"}); 
  printf COMP ("#\n");
  printf COMP ("# \'old-total\':  total number of old hits in SEED and FULL\n");
  printf COMP ("# \'new-total\':  total number of new hits in SEED and FULL\n");
  printf COMP ("# \'old-both\':   number of old hits that overlap with a  new hit by at least 1 nt\n");
  printf COMP ("# \'new-both\':   number of new hits that overlap with an old hit by at least 1 nt\n");
  printf COMP ("# \'old-unique\': hits in old results not present in new results\n");
  printf COMP ("# \'new-unique\': hits in new results not present in old results\n");
  printf COMP ("#\n");
  printf COMP ("# see '$out_lostoutlist' and '$out_lostspecies' files for list of 'old-unique' hits (lost in new searches).\n");
  printf COMP ("# see '$out_newoutlist'  and '$out_newspecies'  files for list of 'new-unique' hits.\n"); 
  close(COMP);
}

#-------------------------------------------------------

=head2 cmsearchReadySearchopts

    Title    : cmsearchReadySearchopts
    Incept   : EPN, Tue Nov 12 10:55:54 2013
    Usage    : cmsearchReadySearchopts($desc)
    Function : Return a string we can pass to cmsearch as an options string.
             : This requires stripping off CM, SEQDB etc. from $desc->SM.
    Args     : $desc:  the DESC object, with SM
             : $ncpu:  number of CPUs to use, if undefined or "", use whats in $desc->SM
    Returns  : string of options that can be passed to cmsearch
=cut
    
sub cmsearchReadySearchopts {
  my ($desc, $ncpu) = @_;
  
  my $searchopts = $desc->{'SM'};
  $searchopts =~ s/\s*CM\s*/ /;     # remove 'CM',  
  $searchopts =~ s/\s*SEQDB\s*/ /;  # remove 'SEQDB'
  $searchopts =~ s/cmsearch//;      # remove 'cmsearch'

  if(defined $ncpu && $ncpu ne "") { 
    if($searchopts !~ /\-\-cpu\s+\S+/) { die "ERROR, SM from DESC ($searchopts) does not include --cpu <>, it should..."; }
    $searchopts =~ s/\-\-cpu\s+\S+/\-\-cpu $ncpu/;  
  }

  return $searchopts;
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

#-------------------------------------------------------

=head2 _outlist_species_get_overlap_string

    Title    : _outlist_species_get_overlap_string
    Incept   : EPN, Thu Oct 10 09:40:31 2013E
    Usage    : _outlist_species_get_overlap_string($tbloutHAR, $name, $start, $end, $bits, $is_reversed)
    Function : Given a hit H and a tbloutHA reference (defined below) determine 
             : if hit H overlaps with any of the hits in the tbloutHA, and if so
             : create an 'overlap string' for the outlist and species file.
             : If not the overlap string is '-'.
             :
             : The $tbloutHAR refers to a hash of arrays filled from the 'TBLOUT' file
             : by parseTbloutForOverlapCheck(). Each key is a target sequence name T, and each
             : value is an array of strings that define all hits found in T.
             : The strings have a specific format: "<start>:<end>:<bitscore>"
             : A contrived format that includes all the necessary information
             : for doing this overlap check.
             : 
             : Obviously the hit may overlap with itself (if ! $is_reversed).
             : We allow this, but check to make sure that the bit scores
             : are identical, else we die.
             :
    Args     : $tbloutHAR:    ref to hash of arrays (see 'Function' section)
             : $name:         target sequence name of hit H we are checking overlaps for
             : $start:        start position of hit H (if > end, hit is on opposite strand)
             : $end:          end position of hit H
             : $bits:         bit score of hit H
             : $is_reveresed: '1' if hit is a reversed hit, we will remove 
    Returns  : sortable version of $evalue
    Dies     : if there is an overlap on the *same strand* to any hit *and* $is_reversed 
             : is FALSE (0), because we do not expect any overlapping hits on same
             : strand returned from Infernal.
             : If hit overlaps with itself in $tbloutHAR, but bit scores do not match
=cut  

sub _outlist_species_get_overlap_string { 
  my ($tbloutHAR, $name, $start, $end, $bits, $is_reversed) = @_;
  
  my $overlap_str = "-";
  
  # potentially remove '-shuffled' suffix if nec from 'reversed searches'
  my $name2lookup = $name;
  if($is_reversed) { $name2lookup =~ s/\-shuffled$// }
  
  if(exists($tbloutHAR->{$name2lookup})) { 
    my $overlap_bits = "";  # initial value doesn't matter, overlap_str of "-" triggers redefinition
    my $startendbits; 
    # there's at least one hit somewhere on this target sequence
    foreach $startendbits (@{$tbloutHAR->{$name2lookup}}) { 
      my ($start2, $end2, $bits2) = split(":", $startendbits);
      # check if it is ourself (identical), and if so move on (only do this if we're not reversed)
      if((! $is_reversed) && ($start == $start2 && $end == $end2)) { 
        if($bits ne $bits2) { die "ERROR, two hits overlap perfectly, with different scores ($name $start $end $bits != $bits2)" }
      }
      else { # not identical hit, determine if there's any overlap
        my ($nres_overlap, $strand1, $strand2) = Bio::Rfam::Utils::overlap_nres_either_strand($start, $end, $start2, $end2);
        # printf STDERR ("calling overlap_nres_either_strand($start, $end, $start2, $end2) got ($nres_overlap, $strand1, $strand2) bits2: $bits2 overlap_bits: $overlap_bits\n");
        if(($nres_overlap > 0) && # we have overlap 
           ($overlap_str eq "-" || $bits2 > $overlap_bits)) { # either first overlap, or highest scoring overlap
          if($bits2 > $bits)       { $overlap_str  = "^"; }
          else                     { $overlap_str  = "v"; }
          if($is_reversed) { 
            if($strand1 eq $strand2) { $overlap_str .= ":same"; }
            else                     { $overlap_str .= ":oppo"; }
          }
          elsif($strand1 eq $strand2) { 
            die "ERROR, two hits overlap on same strand ($name/$start-$end and $name/$start2-$end2)"; 
          }
          $overlap_str .= ":$bits2";
          $overlap_bits = $bits2;
        }
      }
    }
  }
  return $overlap_str;
}


#-------------------------------------------------------------------------------

=head2 validate_outlist_format

  Title    : validate_outlist_format
  Incept   : EPN, Fri Nov  1 06:08:04 2013
  Usage    : Bio::Rfam::FamilyIO::validate_outlist_format($outlist)
  Function : Validate an outlist file is up-to-date by ensuring its header line 
           : is what we expect.
  Args     : $outlist:     outlist file to validate
           : $requireFull: '1' to require full file, first 2 lines should be '#' prefixed
  Returns  : void
  Dies     : if outlist header line is unexpected indicating
           : outlist is in an old format.

=cut

sub validate_outlist_format { 
  my ($outlist, $requireFull) = @_;

  open(IN, $outlist) || die "ERROR unable to open $outlist to validate it's format";

  if($requireFull) { 
    # Expected line 1: 
    # #
    my $line = <IN>;
    chomp $line;
    if($line !~ m/^\#$/) { die "ERROR unable to validate outlist format (first line not \"\#\"); rerun rfmake.pl"; }
    
    # Expected line 2: 
    # # bits  evalue   seqLabel  name            overlap  start    end      str  qstart  qend  trunc  species                            description                                                                                                  
    $line = <IN>;
    chomp $line;
    if($line !~ m/^\#\s+bits\s+evalue\s+seqLabel\s+name\s+overlap\s+start\s+end\s+str\s+qstart\s+qend\s+trunc\s+species\s+description/) { 
      die "ERROR unable to validate outlist format (second line invalid); rerun rfmake.pl"; 
    }
  }
  else { # requireFull is FALSE, check that first non-comment line has correct format
    # example line
    # 93.2  5.3e-16      FULL  AAPY01617272.1        -      690      825    +       1   106     no  Tupaia_belangeri_(north..[37347]   Tupaia belangeri cont1.617271, whole genome shotgun sequence.
    my $line = <IN>;
    my $passed = 0;
    while($line =~ m/^\#/) { $line = <IN>; }
    chomp $line;
    if($line =~ m/^\s*\-?\d*\.\d\s+\S+\s+\w+\s+\S+\s+\S+\s+\d+\s+\d+\s+[\-+]\s+\d+\s+\d+\s+\S+\s+\S+\s+/) { 
      $passed = 1;
    }
    else { 
      die "ERROR unable to validate outlist format (first non-comment line invalid); rerun rfmake.pl"; 
    }
    if(! $passed) { 
      die "ERROR unable to validate outlist format (no non-comment lines found!); rerun rfmake.pl"; 
    }
  }

  close(IN);
  return;
}

#-------------------------------------------------------------------------------

=head2 validate_species_format

  Title    : validate_species_format
  Incept   : EPN, Fri Nov  1 06:14:15 2013
  Usage    : Bio::Rfam::FamilyIO::validate_species_format($species)
  Function : Validate an species file is up-to-date by ensuring its header line 
           : is what we expect.
  Args     : $species:     species file to validate
           : $requireFull: '1' to require full file, first 2 lines should be '#' prefixed
  Returns  : void
  Dies     : if species header line is unexpected indicating
           : species is in an old format.

=cut

sub validate_species_format { 
  my ($species, $requireFull) = @_;

  open(IN, $species) || die "ERROR unable to open $species to validate it's format";

  if($requireFull) { 
    # Expected line 1: 
    # #
    my $line = <IN>;
    chomp $line;
    if($line !~ m/^\#$/) { die "ERROR unable to validate species format (first line not \"\#\"); rerun rfmake.pl"; }
    
    # Expected line 2: 
    # # bits  evalue   seqLabel  name            overlap  ncbiId  species                                                         taxString
    $line = <IN>;
    chomp $line;
    if($line !~ m/^\#\s+bits\s+evalue\s+seqLabel\s+name\s+overlap\s+ncbiId\s+species\s+taxString/) { 
      die "ERROR unable to validate species format (second line invalid); rerun rfmake.pl"; 
    }
  } 
  else { # requireFull is FALSE, check that first non-comment line has correct format
    # example line: 
    # 93.2  5.3e-16      FULL  AAPY01617272.1        -   37347  Tupaia belangeri (northern tree shrew)                      Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Scandentia; Tupaiidae; Tupaia.
    my $line = <IN>;
    my $passed = 0;
    while($line =~ m/^\#/) { $line = <IN>; }
    chomp $line;
    if($line =~ m/^\s*\-?\d*\.\d\s+\S+\s+\w+\s+\S+\s+\S+\s+\S+.+  .*$/) { 
      $passed = 1;
    }
    else { 
      die "ERROR unable to validate species format (first non-comment line invalid); rerun rfmake.pl"; 
    }
    if(! $passed) { 
      die "ERROR unable to validate species format (no non-comment lines found!); rerun rfmake.pl"; 
    }
  }

  close(IN);
  return;
}

1;
