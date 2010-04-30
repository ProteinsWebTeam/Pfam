# FamilyIO.pm
#
# Author:        rdf
# Maintainer:    $Id: FamilyIO.pm,v 1.2 2010-01-13 11:23:28 jm14 Exp $
# Version:       $Revision: 1.2 $
# Created:       Nov 29, 2008
# Last Modified: $Date: 2010-01-13 11:23:28 $

=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::FamilyIO;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: FamilyIO.pm,v 1.2 2010-01-13 11:23:28 jm14 Exp $

=head1 COPYRIGHT

File: FamilyIO.pm

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

use strict;
use warnings;
use File::Temp;
use Text::Wrap;
use Carp;
use Data::Dumper;

use Bio::Pfam::Config;
use Bio::Pfam::Family::PfamA;
use Bio::Pfam::Family::DESC;
use Bio::Pfam::Family::Scores;

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

sub new {
  my ($caller) = @_;

  my $class = ref($caller) || $caller;
  my $self;

  $self->{config} = Bio::Pfam::Config->new;

  return bless( $self, $class );
}

sub loadPfamAFromLocalFile {
  my ( $self, $family, $dir, $source ) = @_;

  unless ( -d "$dir/$family" ) {
    confess("Could not find family directory $dir/$family");
  }

  my %params;
  foreach my $f ( @{ $self->{config}->mandatoryFamilyFiles } ) {
    unless ( -s "$dir/$family/$f" ) {
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

  my $famObj = Bio::Pfam::Family::PfamA->new(%params);

  return ($famObj);
}

sub loadPfamAFromSVN {
  my ( $self, $family, $client ) = @_;

  my $dir = File::Temp->newdir( 'CLEANUP' => 0 );
  mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");

  foreach my $f ( @{ $self->{config}->mandatoryFamilyFiles } ) {
    my $fh;
    open( $fh, ">$dir/$family/$f" ) or die "Could not open $dir/$f";
    $client->catFile( $family, $f, $fh );
    close($fh);
  }
  my $famObj = $self->loadPfamAFromLocalFile( $family, $dir, 'svn' );
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
  my $scoresObj = Bio::Pfam::Family::Scores->new(
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
      confess( "\nGot a DESC line that was longer the $expLen, $file[$l]\n\n"
          . "-" x 80
          . "\n" );
    }

    if ( $file[$i] =~ /^(AC|ID|DE|PI|AU|SE|TP|SQ|BM|SM|CL)\s{3}(.*)$/ ) {
      $params{$1} = $2;
      next;
    }
    elsif ( $file[$i] =~ /^(TC|NC|GA)\s{3}(\S+)\s+(\S+)\;$/ ) {
      $params{ "CUT" . $1 } = { seq => $2, dom => $3 };
    }
    elsif ( $file[$i] =~ /^\*\*\s{3}(.*)$/ ) {
      $params{private} .= " " if ( $params{private} );
      $params{private} .= $1;
    }
    elsif ( $file[$i] =~ /^WK\s{3}(.*)$/ ) {
      my $page = $1;
        if ( defined( $params{"WIKI"} ) ) {
          $params{"WIKI"}->{$page}++;
        }
        else {
          $params{"WIKI"} = { $page => 1 };
        }
    }
    elsif ( $file[$i] =~ /^CC\s{3}(.*)$/ ) {
      my $cc = $1;
      while ( $cc =~ /(\w+):(\S+)/g ) {
        my $db  = $1;
        my $acc = $2;

        if ( $db eq 'Swiss' ) {
          $acc =~ s/\W+//g;
          unless ( $acc =~ /^\S{6}$/ ) {
            confess(
"\nDESC file format for link to Swiss-Prot is wrong $acc is not a valid accession\n"
                . "-" x 80
                . "\n" );
          }
        }

        if ( $db eq 'Pfam' ) {
          $acc =~ s/\.|\,//g;
          unless ( $acc =~ /^PF\d{5}/ ) {
            warn(
"\nDESC file format for link to Pfam is wrong $acc is not a valid accession\n"
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
    elsif ( $file[$i] =~ /^D\w\s{3}/ ) {
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

        if ( $file[$i] =~ /^DR   PRINTS;\s/ ) {
          if ( $file[$i] !~ /^DR   (PRINTS);\s+(PR\d{5});$/ ) {
            confess("Bad prints reference [$file[$i]]\n");
          }
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   PROSITE;\s/ ) {
          if ( $file[$i] !~ /^DR   (PROSITE);\s+(PDOC\d{5});$/ ) {
            confess("Bad prosite reference [$file[$i]]\n");
          }
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   PROSITE_PROFILE/ ) {
          if ( $file[$i] !~ /^DR   (PROSITE_PROFILE);\s(PS\d{5});$/ ) {
            confess("Bad prosite reference [$file[$i]]\n");
          }
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   HOMSTRAD/ ) {
          if ( $file[$i] !~ /^DR   (HOMSTRAD);\s(\S+);$/ ) {
            confess("Bad homstrad reference [$file[$i]]\n");
          }
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   TC/ ) {
          if ( $file[$i] !~ /^DR   (TC);\s(\d+\.\w+\.\d+);$/ ) {
            confess("Bad TC reference [$file[$i]]\n");
          }
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   SCOP;\s/ ) {
          if ( $file[$i] !~ /^DR   (SCOP);\s+(\S{4});\s+(\w+);$/ ) {
            confess( "Bad SCOP reference [ " . $file[$i] . "]\n" );
          }
          my $id   = $1;
          my $link = $2;
          my $tag  = $3;
          unless ( $tag eq "sf" || $tag eq "fa" || $tag eq "pr" ) {
            confess "Bad SCOP reference (must have sf or fa tag) [$file[$i]]\n";
          }

          push(
            @{ $params{DBREFS} },
            { db_id => $id, db_link => $link, other_params => $tag }
          );
        }
        elsif ( $file[$i] =~ /^DR   (URL);\s+(\S+);$/ ) {
          print STDERR "Please check the URL $2\n";
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (MIM); (\d{6});$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (MEROPS); (\S\d+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (LOAD); (\S+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (CAZY); ((GH|GT|CBM|PL|CE)\d+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (SMART); (\w+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR/ ) {
          confess( "Bad reference line: unknown database [$file[$i]].\n"
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

#confess("Failed to parse the DESC line (enclosed by |):|$file[$i]|\n\n". "-" x 80 ."\n");
    }
  }

  #print Dumper %params;
  my $desc = Bio::Pfam::Family::DESC->new(%params);
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
    CUTGA => { seq => '27.00', dom => '27.00' },
    CUTNC => { seq => '27.00', dom => '27.00' },
    CUTTC => { seq => '27.00', dom => '27.00' },
    BM    => 'hmmbuild  -o /dev/null HMM SEED;',
    SM    => 'hmmsearch -Z ' . $self->{config}->dbsize . ' -E 1000 HMM pfamseq',
    TP    => 'Family'
  );

  my $desc = Bio::Pfam::Family::DESC->new(%desc);
  $self->writeDESC($desc);
}

sub writeDESC {
  my ( $self, $desc, $path ) = @_;

  unless ( $desc->isa('Bio::Pfam::Family::DESC') ) {
    confess("You did not pass in a  Bio::Pfam::Family::DESC object");
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

  $Text::Wrap::columns = 75;
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
        printf D "TC   %.2f %.2f;\n", $desc->$tagOrder->{seq},
          $desc->$tagOrder->{dom};
      }
      elsif ( $tagOrder eq 'CUTGA' ) {
        printf D "GA   %.2f %.2f;\n", $desc->$tagOrder->{seq},
          $desc->$tagOrder->{dom};
      }
      elsif ( $tagOrder eq 'CUTNC' ) {
        printf D "NC   %.2f %.2f;\n", $desc->$tagOrder->{seq},
          $desc->$tagOrder->{dom};
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
      elsif ( $tagOrder eq 'WIKI' ) {
        if ( ref( $desc->$tagOrder ) eq 'HASH' ) {
          my @pages = keys( %{ $desc->$tagOrder } );
          foreach my $p (@pages){
            print D wrap( "WK   ", "WK   ", $p );
            print D "\n"
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

sub updatePfamAInRDB {
  my ( $self, $famObj, $pfamDB, $isNew, $depositor ) = @_;

  #This essential updates all of the content contained in the desc file
  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  unless ( $pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager') ) {
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object");
  }

  if ($isNew) {
    $pfamDB->createPfamA( $famObj, $depositor );
  }
  else {
    $pfamDB->updatePfamA($famObj);
  }


  if ( $famObj->DESC->WIKI ) {
    $pfamDB->updatePfamAWikipedia($famObj);
  }

  if ( $famObj->DESC->REFS ) {
    $pfamDB->updatePfamALitRefs($famObj);
  }

  if ( $famObj->DESC->DBREFS ) {
    $pfamDB->updatePfamADbXrefs($famObj);
  }

  if ( $famObj->DESC->NESTS ) {
    $pfamDB->updatePfamANested($famObj);
  }

  if ( $famObj->DESC->EDITS ) {
    $pfamDB->updateEdits($famObj);
  }

}

sub updatePfamARegions {
  my ( $self, $famObj, $pfamDB ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  unless ( $pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager') ) {
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object");
  }
  
  $pfamDB->updatePfamARegSeed($famObj);
  $pfamDB->updatePfamARegFull($famObj);  
  
}


sub movePfamAInRDB {
  my ( $self, $famObj, $pfamDB ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  unless ( $pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager') ) {
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object");
  }

  $pfamDB->updatePfamA($famObj);

}

sub deletePfamAInRDB {
  my ( $self, $family, $pfamDB, $comment, $forward, $user ) = @_;

  unless ( $family and $family =~ /PF\d{5}/ ) {
    confess("Did not get a family accession\n");
  }

  unless ( $pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager') ) {
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object");
  }

  unless ($comment) {
    confess('Did not get a comment as to why this family is being killed');
  }

  $pfamDB->deletePfamA( $family, $comment, $forward, $user );

}

sub uploadPfamAHMM {
  my ( $self, $famObj, $pfamDB, $dir, $isNew ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  unless ( $pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager') ) {
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object");
  }

  #my $hmmio = Bio::Pfam::HMM::HMMIO->new;

  #my ($fh, $filename) = tempfile();
  #$hmmio->writeHHMM($fh, $famObj->HMMPfamA);
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

  $pfamDB->uploadPfamAHMM( $famObj, $hmmString );

}

sub uploadPfamAAligns {
  my ( $self, $famObj, $pfamDB, $dir, $isNew ) = @_;
  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  unless ( $pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager') ) {
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object");
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

  $pfamDB->uploadPfamAInternal( $famObj, $seed, $full );

}
1;
