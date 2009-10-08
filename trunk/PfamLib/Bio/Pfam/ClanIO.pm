# ClanIO.pm
#
# Author:        rdf
# Maintainer:    $Id: ClanIO.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Version$
# Created:       Apr 24, 2009
# Last Modified: $Date: 2009-10-08 12:27:28 $

=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::ClanIO;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: ClanIO.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: ClanIO.pm

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
use Bio::Pfam::Clan::Clan;
use Bio::Pfam::Clan::DESC;


#-------------------------------------------------------------------------------

=head1 METHODS

=cut


#-------------------------------------------------------------------------------

=head2 new 

  Title    : new
  Usage    : Bio::Pfam::ClanIO->new 
  Function : Creates a new ClanIO object 
  Args     : None
  Returns  : A brand new shiny blessed ClanIO object reference.
  
=cut


sub new {
  my ($caller) = @_;

  my $class = ref($caller) || $caller;
  my $self;

  $self->{config} = Bio::Pfam::Config->new;

  return bless( $self, $class );
}


#-------------------------------------------------------------------------------
=head2 loadClanFromLocalFile

  Title    : Load a clandesc file for local file
  Usage    : $clanIO->loadClanFromLocalFile('/my/workspace/clans', 'CL0001', 'file'); 
  Function : At the moments, just takes a clandesc file and makes an object to represent it.
  Args     : A directory containing clandesc file, source (optional)
  Returns  : A clan object
  
=cut

sub loadClanFromLocalFile {
  my ( $self, $clan, $dir, $source ) = @_;

  unless ( -d "$dir" ) {
    confess("Could not find clan directory $dir");
  }

  my (%params, $fh);
  open( $fh, "$dir/$clan/CLANDESC" ) or confess("Could not open $dir/$clan/CLANDESC:[$!]");
  $params{'DESC'} = $fh;
  
  
  if($source){
    $params{'source'} = $source; 
  }
  
  my $clanObj = Bio::Pfam::Clan::Clan->new(%params);

  return ($clanObj);
}



sub loadClanFromRDB {
  my ($self, $clan, $rdbObj) = @_;
  
  my $clanData = $rdbObj->getClanData($clan);
  
  unless($clanData){
    die "$clan does not appear to be in the database!\n";  
  }
    
  my $params; 
  
  
  $params->{DESC}->{AC} = $clanData->clan_acc if($clanData->clan_acc);
  $params->{DESC}->{ID} = $clanData->clan_id if($clanData->clan_id);
  $params->{DESC}->{DE} = $clanData->clan_description if($clanData->clan_description);
  $params->{DESC}->{AU} = $clanData->clan_author if($clanData->clan_author);
  $params->{DESC}->{CC} = $clanData->clan_comment if($clanData->clan_comment);
  $params->{source} = 'database';
  my $membership = $rdbObj->getClanMembership($clan);
  
  my @memberAcc;
  foreach my $m (@{$membership}){
    push(@memberAcc, $m->auto_pfama->pfama_acc);   
  }
  
  $params->{DESC}->{MEMB} = \@memberAcc;
  
  my $clanObj = Bio::Pfam::Clan::Clan->new($params);
 
  
}




sub loadClanFromSVN {
  my ( $self, $clan, $client ) = @_;

  my $dir = File::Temp->newdir( 'CLEANUP' => 0 );
  mkdir("$dir/$clan") or confess("Could not make $dir/$clan:[$!]");
 
  my $fh;
  open( $fh, ">$dir/$clan/CLANDESC" ) or die "Could not open $dir/$clan/CLANDESC";
  $client->catFile( $clan, "CLANDESC", $fh );
  close($fh);
  
  my $clanObj = $self->loadClanFromLocalFile( $clan, $dir->dirname, 'svn' );
  return $clanObj;
}


#-------------------------------------------------------------------------------

=head2 parseCLANDESC 

  Title    : parseCLANDESC
  Usage    : $clanIO->parseCLANDESC($descFH)
  Function : Uses the desc file name or handle and reads in the CLANDESC file 
           : and generates a Bio::Pfam::Clan::DESC object
  Args     : Filename or filehandle
  Returns  : Bio::Pfam::Clan::DESC object
  
=cut

sub parseCLANDESC {
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
    RC => { RC => 1,
            RN => 1 },
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
      confess("Got a DESC line that was longer the $expLen, $file[$l]\n");
    }

    if ( $file[$i] =~ /^(AC|ID|PI|DE|AU)\s{3}(.*)$/ ) {
      $params{$1} = $2;
      next;
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
"DESC file format for link to Swiss-Prot is wrong $acc is not a valid accession\n"
            );
          }
        }

        if ( $db eq 'Pfam' ) {
          $acc =~ s/\.|\,//g;
          unless ( $acc =~ /^PF\d{5}/ ) {
            warn(
"DESC file format for link to Pfam is wrong $acc is not a valid accession\n"
            );
          }
        }
     
        if ( $db eq 'EC' ) {
          unless (
            $acc =~ /^\(?(\d+)(\.(\d+|\-)){1,3}(\.|\,|\)){0,2}$/ )
          {
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
          if($j == $#file){
              $i = $j;
              last REFLINE;
          }
          my ($nextTag) = $file[ $j + 1 ] =~ /^(\w{2})\s{3}/;

          #Now lets check that the next field is allowed
          if ( $refTags->{$thisTag}->{$nextTag} ) {
            next REFLINE;
          }
          elsif ( ( !$refTags->{$nextTag} or ( $nextTag eq "RN" or $nextTag eq "RC") )
            and ( $thisTag eq "RL" ) )
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
          if( $file[$i] =~ /^DC\s{3}(.*)/ ) {
            $com .= " " if($com);
            $com = $1;
          }else{
            last;
          }
        }  

        if(!$file[$i]){
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
          if (  $file[$i] !~ /^DR   (HOMSTRAD);\s(\S+);$/ ) {
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
          if ( $file[$i] !~ /^DR   (SCOP);\s+(\d+);$/ ) {
            confess("Bad SCOP reference [ ".$file[$i]."]\n");
          }
          my $id   = $1;
          my $link = $2;
          
          push(
            @{ $params{DBREFS} },
            { db_id => $id, db_link => $link }
          );
        }
        elsif ( $file[$i] =~ /^DR   (URL);\s+(\S+);$/ ) {
          print STDERR "Please check the URL $2\n";
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (MIM); (\d{6});$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (MEROPS); (\S+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (MEROPS); (\S+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (LOAD); (\S+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (CAZY); ((GH_|GT_|CBM_|PL_|CE_)\d+);$/ )
        {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (SMART); (\w+);$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        }
        elsif ( $file[$i] =~ /^DR   (CATH); (\S+);$/ ) {
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
        if($com){
          $params{DBREFS}->[ $#{ $params{DBREFS} } ]->{db_comment} = $com; 
        }
      }
    }elsif($file[$i] =~ /^(MB)\s{3}(PF\d{5})\;$/ ) {
      push( @{ $params{MEMB} },  $2 );
      next;
    }else {
      confess("Failed to parse the DESC line $file[$i]");
    }
  }

  #print Dumper %params;
  my $desc = Bio::Pfam::Clan::DESC->new(%params);
  return $desc;
      #End of uber for loop
}

sub writeCLANDESC {
  my ( $self, $desc, $path ) = @_;

  unless ( $desc->isa('Bio::Pfam::Clan::DESC') ) {
    confess("You did not pass in a  Bio::Pfam::Clan::DESC object");
  }
  
  my $descfile;
  if($path){
    $descfile = $path."/CLANDESC"; 
  }else{
    $descfile = "CLANDESC";
  }
  open(D, ">$descfile") or die "Could not open $descfile file for writing to\n";   
  
  $Text::Wrap::columns = 75;
  foreach my $tagOrder ( @{ $desc->order } ){
    if(length($tagOrder) == 2 ){
      if($desc->$tagOrder){
        print D wrap("$tagOrder   ","$tagOrder   ", $desc->$tagOrder) ;
        print D "\n";
      } 
    }else{
      next unless($desc->$tagOrder);
      if($tagOrder eq 'REFS'){
        foreach my $ref ( @{$desc->$tagOrder}){
          if ($ref->{RC}){ 
            print D wrap("RC   ","RC   ",$ref->{RC});
            print D "\n";
          }
          print D "RN   [".$ref->{RN}."]\n";
          print D "RM   ".$ref->{RM}."\n";
          print D wrap("RT   ","RT   ", $ref->{RT});
          print D "\n";
          print D wrap("RA   ","RA   ", $ref->{RA});
          print D "\n";
          print D "RL   ".$ref->{RL}."\n";
          
        }
      }elsif($tagOrder eq 'DBREFS'){
        foreach my $xref (@{$desc->$tagOrder}){
          #Print out any comment
          if($xref->{db_comment}){
             print D wrap "DC   ", "DC   ", $xref->{db_comment};
             print D "\n";
          }
        
          if($xref->{other_params}){
            print D "DR   ".$xref->{db_id}."; ". $xref->{db_link} ."; " .$xref->{other_params}.";\n";
          }else{
            print D "DR   ".$xref->{db_id}."; ".$xref->{db_link}.";\n";
          }
        }
      }elsif($tagOrder eq 'MEMB'){
        if( $desc->$tagOrder){
          foreach my $acc (@{ $desc->$tagOrder }){
            print D "MB   ".$acc.";\n";
          }
        }
      }
      
    }    
  }
  close(D);
}

sub updateClanInRDB {
  my($self, $clanObj, $pfamDB, $isNew, $depositor) = @_;
  
  unless($clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan')){
    confess("Did not get a Bio::Pfam::Clan::Clan object"); 
  }
  
  unless($pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager')){
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object"); 
  }
  
  if($isNew){
    $pfamDB->createClan( $clanObj, $depositor );  
  }else{
    $pfamDB->updateClan( $clanObj );
  }
    
  if($clanObj->DESC->REFS){
    $pfamDB->updateClanLitRefs( $clanObj );
  }
  
  if($clanObj->DESC->DBREFS){
    $pfamDB->updateClanDbXrefs( $clanObj );
  }
}

sub moveClanInRDB {
  my ( $self, $clanObj, $pfamDB ) = @_;
  
  unless($clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan')){
    confess("Did not get a Bio::Pfam::Clan::Clan object"); 
  }
  
  unless($pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager')){
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object"); 
  }
  
  $pfamDB->updateClan( $clanObj );
  
}

sub deleteClanInRDB {
  my ( $self, $clan, $pfamDB, $comment, $forward, $user ) = @_;  
  
  unless($clan and $clan =~/CL\d{4}/){
    confess("Did not get a clan accession\n"); 
  }
  
  unless($pfamDB and $pfamDB->isa('Bio::Pfam::PfamLiveDBManager')){
    confess("Did not get a Bio::Pfam::PfamLiveDBManager object"); 
  }
  
  unless($comment){
    confess('Did not get a comment as to why this family is being killed');
  }
  
  $pfamDB->deleteClan($clan, $comment, $forward, $user);
  
}

 
1;
