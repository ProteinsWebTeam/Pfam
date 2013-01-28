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
  }else{
    $params{'source'} = 'file';
  }
  

                
  my $params = {'SEED'     => { fileLocation => "$dir/$family/SEED",
                                aliType      => 'seed' },
                'DESC'     => $self->parseDESC( "$dir/$family/DESC"),
                'CM'       => $self->parseCM("$dir/$family/CM") };
           
                #'CM'       => to_CMRfam($params{'HMM'}),
                #'DESC'     => to_DESCRfam($params{'DESC'}) };
#                'scores'   => to_ScoresRfam($params{'scores'})};
                
  #Use Moose to coerce these through!
  my $famObj = Bio::Rfam::Family->new( $params );

  return ($famObj);
}


sub loadDfamFromSVN {
  my ( $self, $family, $client ) = @_;

  my $dir = File::Temp->newdir( 'CLEANUP' => 1 );
  mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");

  foreach my $f ( @{ $self->{config}->mandatoryFiles } ) {
    my $fh;
    open( $fh, ">$dir/$family/$f" ) or die "Could not open $dir/$f";
    $client->catFile( $family, $f, $fh );
    close($fh);
  }
  my $famObj = $self->loadDfamFromLocalFile( $family, $dir, 'svn' );
  return $famObj;
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
  my %regions;
  foreach my $line (@file) {
    if ( $line =~ /^(\S+)\s+(\S+)\/(\d+)\-(\d+)\s+(\d+)\-(\d+)/ ) {
      push(
        @{ $regions{$2} },
        {
          start    => $3,
          end      => $4,
          score    => $1,
          aliStart => $5,
          aliEnd   => $6,
        }
      );
      $noHits++;
    }
    else {
      die "Failed to parse line in scores file: [$line]\n";
    }
  }
  my $scoresObj = Bio::Dfam::Family::Scores->new(
    {
      numRegions => $noHits,
      regions    => \%regions
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
      croak ("\nGot a DESC line that was longer than $expLen, $file[$i]\n\n"
          . "-" x 80
          . "\n" );
    }

    if ( $file[$i] =~ /^(AC|ID|DE|PI|AU|SE|SS|TP|SQ|CL|FR|SN)\s{3}(.*)$/ ) {
      if(exists($params{$1})){
        croak("Found second $1 line, only expecting one\n");  
      }
      $params{$1} = $2;
      next;
    }elsif ($file[$i] =~ /^BM\s{3}(.*)$/){
      push(@{$params{"BM"}}, $1);
    }
    elsif ( $file[$i] =~ /^(TC|NC|GA)\s{3}(\S+)$/ ) {
      $params{ "CUT" . $1 } = $2;
    }
    elsif ( $file[$i] =~ /^\*\*\s{3}(.*)$/ ) {
      $params{private} .= " " if ( $params{private} );
      $params{private} .= $1;
    }
    elsif ( $file[$i] =~ /^WK\s{3}(.*)$/ ) {
      my $page = $1;
      if($page =~ /^http.*\/(\S+)/){
        #TODO - supress warn, if we can remove URL. Page title sufficient.
        warn "$page going to be set to $1\n";
        $page=$1; 
      }
        if ( defined( $params{"WIKI"} ) ) {
          $params{"WIKI"}->{$page}++;
        }
        else {
          $params{"WIKI"} = { $page => 1 };
        }
    } elsif ( $file[$i] =~ /^SM\s{3}(.*)$/ ) {
      my $sm = $1;
      if ( $params{SM} ) {
        $params{SM} .= " ";
      }
      $params{SM} .= $sm;
      next;
    }elsif ( $file[$i] =~ /^CC\s{3}(.*)$/ ) {
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
        if ( $file[$i] =~ /^DR   SO:(\d+) SO:(.*)$/ ) {
          #GO:0010628 GO:positive regulation of gene expression
          push( @{ $params{DBREFS} }, { db_id => 'SO', db_link => $1, other_params => $2 } );
        }elsif ( $file[$i] =~ /^DR   GO:(\d+) GO:(.*)$/ ) {
          #GO:0010628 GO:positive regulation of gene expression
          push( @{ $params{DBREFS} }, { db_id => 'GO', db_link => $1, other_params => $2 } );
        }elsif ( $file[$i] =~ /^DR   (MIPF); (MIPF\d+)$/ ) {
          #MIPF; MIPF0000879
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }elsif( $file[$i] =~ /^DR   (snornadb);\s(.*?\d+)$/ ) {
          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
#TODO - check that these are really different.
        }elsif( $file[$i] =~ /^DR   (snoRNABase);\s(.*?\d+)$/ ) {
          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
          
        }elsif( $file[$i] =~ /^DR   (PKBASE); (PKB\d+)$/ ) {
          #PKBASE; PKB00277
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }elsif( $file[$i] =~ /^DR   (snoopy); (.*)$/ ) {
          #snoopy;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }elsif( $file[$i] =~ /^DR   (MIR); (.*)$/ ) {
          #MIR; MI0001007;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }elsif( $file[$i] =~ /^DR   (URL); (.*)$/ ) {
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
    BM    => 'hmmbuild  -o /dev/null HMM SEED;',
    SM    => 'nhmmer -Z ' . $self->{config}->dbsize . ' -E 1 HMM dfamseq',
    TP    => 'Repeat'
  );

  my $desc = 'Bio::Dfam::Family::DESC'->new(\%desc);
  $self->writeDESC($desc);
}

sub writeDESC {
  my ( $self, $desc, $path ) = @_;

  unless ( $desc->isa('Bio::Dfam::Family::DESC') ) {
    confess("You did not pass in a  Bio::Dfam::Family::DESC object");
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
        printf D "TC   %.2f;\n", $desc->$tagOrder;
      }
      elsif ( $tagOrder eq 'CUTGA' ) {
        printf D "GA   %.2f;\n", $desc->$tagOrder;
      }
      elsif ( $tagOrder eq 'CUTNC' ) {
        printf D "NC   %.2f;\n", $desc->$tagOrder;
      }
      elsif ( $tagOrder eq 'NESTS' ) {
        foreach my $n ( @{ $desc->$tagOrder } ) {
          print D wrap( "NE   ", "NE   ", $n->{dom} . ";" );
          print D "\n";
          print D wrap( "NL   ", "NL   ",
            $n->{seq} . "/" . $n->{from} . "-" . $n->{to} . ";" );
          print D "\n";
        }
      }elsif( $tagOrder eq 'CLASS' ){
        foreach my $key (qw(Type Class Superfamily Comment)){
          next if(!exists($desc->$tagOrder->{$key}));
          my $value =   $desc->$tagOrder->{$key};
          if($key eq 'Comment'){
            print D wrap( "CN   ", "CN   ", $value );  
            print D "\n";
          }else{
            print D "CT   ".$key."; ".$value.";\n" 
          }
        }
        
      }elsif ( $tagOrder eq 'WIKI' ) {
        if ( ref( $desc->$tagOrder ) eq 'HASH' ) {
          my @pages = keys( %{ $desc->$tagOrder } );
          foreach my $p (@pages){
            print D wrap( "WK   ", "WK   ", $p );
            print D "\n"
          }
        }
      }elsif ( $tagOrder eq 'MSP' ) {
        if ( ref( $desc->$tagOrder ) eq 'HASH' ) {
         
          print D wrap( "MS   ", "MS   ", "TaxId:".
                          $desc->$tagOrder->{TaxId}. "; TaxName:".$desc->$tagOrder->{TaxName}.";"  );
            print D "\n"
         }
      }
      elsif( $tagOrder eq 'CLASS' ){
        foreach my $key (qw(Type Class Superfamily Comment)){
          next if(!exists($desc->$tagOrder->{$key}));
          my $value =   $desc->$tagOrder->{$key};
          if($key eq 'Comment'){
            print D wrap( "CN   ", "CN   ", $value );  
            print D "\n";
          }else{
            print D "CT   ".$key."; ".$value.";\n" 
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

sub updateDfamAInRDB {
  my ( $self, $famObj, $dfamDB, $isNew, $depositor ) = @_;

  #This essential updates all of the content contained in the desc file
  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

  unless ( $dfamDB and $dfamDB->isa('Bio::Dfam::DfamLiveDBManager') ) {
    confess("Did not get a Bio::Dfam::DfamLiveDBManager object");
  }

  if ($isNew) {
    $dfamDB->createDfamA( $famObj, $depositor );
  }
  else {
    $dfamDB->updateDfamA($famObj);
  }


  if ( $famObj->DESC->WIKI ) {
    $dfamDB->updateDfamAWikipedia($famObj);
  }

  if ( $famObj->DESC->REFS ) {
    $dfamDB->updateDfamALitRefs($famObj);
  }

  if ( $famObj->DESC->DBREFS ) {
    $dfamDB->updateDfamADbXrefs($famObj);
  }

  if ( $famObj->DESC->NESTS ) {
    $dfamDB->updateDfamANested($famObj);
  }

  if ( $famObj->DESC->EDITS ) {
    $dfamDB->updateEdits($famObj);
  }

}

sub updateDfamARegions {
  my ( $self, $famObj, $dfamDB ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

  unless ( $dfamDB and $dfamDB->isa('Bio::Dfam::DfamLiveDBManager') ) {
    confess("Did not get a Bio::Dfam::DfamLiveDBManager object");
  }
  
  $dfamDB->updateDfamARegSeed($famObj);
  $dfamDB->updateDfamARegFull($famObj);  
  
}


sub moveDfamAInRDB {
  my ( $self, $famObj, $dfamDB ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

  unless ( $dfamDB and $dfamDB->isa('Bio::Dfam::DfamLiveDBManager') ) {
    confess("Did not get a Bio::Dfam::DfamLiveDBManager object");
  }

  $dfamDB->updateDfamA($famObj);

}

sub deleteEntryInRDB {
  my ( $self, $family, $dfamDB, $comment, $forward, $user ) = @_;

  unless ( $family and $family =~ /RF\d{5}/ ) {
    confess("Did not get a Dfam accession\n");
  }

  unless ( $dfamDB and $dfamDB->isa('Bio::Dfam::LiveDBManager') ) {
    confess("Did not get a Bio::Dfam::LiveDBManager object");
  }

  unless ($comment) {
    confess('Did not get a comment as to why this family is being killed');
  }

  $dfamDB->deleteEntry( $family, $comment, $forward, $user );

}

sub uploadDfamAHMM {
  my ( $self, $famObj, $dfamDB, $dir, $isNew ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

  unless ( $dfamDB and $dfamDB->isa('Bio::Dfam::DfamLiveDBManager') ) {
    confess("Did not get a Bio::Dfam::DfamLiveDBManager object");
  }

  #my $hmmio = Bio::Dfam::HMM::HMMIO->new;

  #my ($fh, $filename) = tempfile();
  #$hmmio->writeHHMM($fh, $famObj->HMMDfamA);
  #close($fh)
  my $family;
  if ($isNew) {
    $family = $famObj->DESC->ID;
  }
  else {
    $family = $famObj->DESC->AC;
  }
  open( HMM, "$dir/$family/HMM" )
    or die "Could not open $dir/$family/HMM:[$!]\n";
  my $hmmString;
  while (<HMM>) {
    $hmmString .= $_;
  }

  $dfamDB->uploadDfamAHMM( $famObj, $hmmString );

}

sub uploadDfamAAligns {
  my ( $self, $famObj, $dfamDB, $dir, $isNew ) = @_;
  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

  unless ( $dfamDB and $dfamDB->isa('Bio::Dfam::DfamLiveDBManager') ) {
    confess("Did not get a Bio::Dfam::DfamLiveDBManager object");
  }

  my $family;
  if ($isNew) {
    $family = $famObj->DESC->ID;
  }
  else {
    $family = $famObj->DESC->AC;
  }

  my $full;
  my $seed;

  #Read the FULL alignment into a string
  open( F, "$dir/$family/ALIGN" )
    or die "Could not open $dir/$family/ALIGN:[$!]\n";
  while (<F>) {
    $full .= $_;
  }
  close(F);

  #Read the SEED alignment into a string;
  open( S, "$dir/$family/SEED" )
    or die "Could not open $dir/$family/SEED:[$!]\n";
  while (<S>) {
    $seed .= $_;
  }
  close(S);

  $dfamDB->uploadDfamAInternal( $famObj, $seed, $full );

}

sub results {
  my($self) = @_;
  my $resIO = Bio::Dfam::HMM::HMMResultsIO->new();
  my $dfamout = abs_path($resIO->dfamout);
  my $reversed = abs_path($resIO->reversed) if(-e $resIO->reversed);
  my $scores = $dfamout;
  $scores =~ s/DFAMOUT$/scores/;
  my $files = {dfamout => $dfamout,
               scores  => $scores };
  $files->{reversed} = $reversed if($reversed);
  my $resObj = Bio::Dfam::HMM::HMMResults->new( $files ); 
  return $resObj;
}

sub writeScoresFile {
  my($self, $resObj) = @_;
  my $resIO = Bio::Dfam::HMM::HMMResultsIO->new();
  $resIO->writeScoresFile($resObj);
}

sub hitBitsCutoffFromEvalue {
  my($self, $resObj, $evalue, $descObj, $model_file, $config) = @_;
  my $resIO = Bio::Dfam::HMM::HMMResultsIO->new();
  my $bit = $resIO->hitBitsCutoffFromEvalue($resObj, $evalue, $descObj, $model_file, $config);
  return defined($bit) ? $bit : undef;
}

sub hitBitsBasedOnFDR {
  my($self, $resObj, $descObj, $model_file, $fdr, $config) = @_;
  my $resIO = Bio::Dfam::HMM::HMMResultsIO->new();
  $resIO->hitBitsBasedOnFDR( $resObj, $descObj, $model_file, $fdr, $config);
}

sub determineFDRBasedOnBits {
  my($self, $resObj, $descObj,$model_file, $ga, $tc, $config) = @_;
  my $resIO = Bio::Dfam::HMM::HMMResultsIO->new();
  $resIO->determineFDRBasedOnBits( $resObj, $descObj, $model_file, $ga, $tc, $config);
}


sub parseCM {
  my ($self, $file) = @_;
  
  my $fh;
  if(defined($file)){
    if(ref($file) eq "GLOB"){
      $fh = $file;
    }else{
      open($fh, '<', $file) or die "Failed to open $file for reading: [$!]\n";
    }
  }else{
    die "Please specify a file that contains the CM for parsing.\n";
  }
  
  my @cm = <$fh>;
  
  
  
  
}


1;
