# Compete.pm
#
# Author:        jm14
# Maintainer:    $Id: Compete.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Jul 22, 2009
# Last Modified: $Date: 2009-10-08 12:27:28 $

=head1 Bio::Pfam::Clan::Compete

Bio::Pfam::Clan::Compete - Competes the family hits for clan members

=cut

package  Bio::Pfam::Clan::Compete;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: Compete.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: Compete.pm

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

use Bio::Pfam::PfamLiveDBManager;
use Data::Dump qw(dump);

#-------------------------------------------------------------------------------

=head1 METHODS


=head2 competeClan

 Title   : competeClan
 Usage   : competeClan($clan_acc, $db_object)
 Function: Competes a clan
 Returns : Nothing
 Args    : clan accession, Bio::Pfam::PfamLiveDBManager object


=cut

sub competeClan {
  my ( $clan_acc, $db ) = @_;
  my $dbh = $db->getSchema->storage->dbh;

  #Get the clan information
  my $clan =
  $db->getSchema->resultset("Clan")->find( { clan_acc => $clan_acc } );

  #*******************************************************************
  #NOTE! Really, really important that the seq_start/seq_end remain at
  #poisitons in query for SEED regions and FULL regions - otherwise the
  #overlap call will explode!!!!!
  #*******************************************************************

  my $sthSeedRegs =
  $dbh->prepare( "select s.pfamA_acc, pfamseq_acc, seq_start,"
    . " seq_end from pfamA_reg_seed s, clan_membership c"
    . " where c.pfamA_acc=s.pfamA_acc and clan_acc=\'"
    . $clan->clan_acc 
    . "\'");
  $sthSeedRegs->execute;
  my %clanSeed;
  foreach my $row ( @{ $sthSeedRegs->fetchall_arrayref } ) {
    push(@{$clanSeed{ $row->[1] }}, $row);    #keyed off pfamseq_acc
  }

  #Get nested data for clan
  my $sthNest =
  $dbh->prepare( "select n.pfamA_acc, nested_pfamA_acc from "
    . "nested_locations n, clan_membership c where "
    . "c.pfamA_acc=n.pfamA_acc and clan_acc=\'"
    . $clan->clan_acc 
    . "\'");
  $sthNest->execute;
  my %nested;
  foreach my $row ( @{ $sthNest->fetchall_arrayref } ) {
    $nested{ $row->[0] }{ $row->[1] } = 1;    #keyed off pfamA_acc
  }

  #Get all full data for clan
  my $sthFullRegs =
  $dbh->prepare( "select s.pfamA_acc, pfamseq_acc, ali_start, "
    . "ali_end, domain_evalue_score, in_full, auto_pfamA_reg_full from "
    . "pfamA_reg_full_significant s, clan_membership c where "
    . "c.pfamA_acc=s.pfamA_acc and clan_acc=\'"
    . $clan->clan_acc
    . "\'"
    . " order by pfamseq_acc, domain_evalue_score" );
  $sthFullRegs->execute;

  my $updateSth =
  $dbh->prepare(
    "update pfamA_reg_full_significant set in_full=? where auto_pfamA_reg_full=?"
  );

  my @seqRegions;
  my $currentPfamseq;
  my $count = 0;
  $dbh->{AutoCommit} = 0;
  while ( my @row = $sthFullRegs->fetchrow_array ) {
    if ( defined($currentPfamseq) and $currentPfamseq ne $row[1] ) {
      my $loseRef = _competeSequence( \@seqRegions, \%clanSeed, \%nested );
      #Loop over and set any region that is out competed to be in_full=0, based 
      #on the region index;      
      foreach my $region ( @seqRegions ) {
        if(exists($loseRef->{ $region->[6] })){
          #update pfamA_reg_full_significant, in_full=0 based on index
          $updateSth->execute( 0, $region->[6] );
        }else{
          $updateSth->execute( 1, $region->[6] );
        }
      }

      @seqRegions = ();
      if ( $count > 1000 ) {
        $dbh->commit;
        $count=0;
      }
    }
    $count++;
    $currentPfamseq = $row[1];
    push( @seqRegions, \@row );
  }

  my $loseRef = _competeSequence(\@seqRegions, \%clanSeed, \%nested, $updateSth );
  foreach my $region ( @seqRegions ) {
    if(exists($loseRef->{ $region->[6] })){
      #update pfamA_reg_full_significant, in_full=0 based on index
      $updateSth->execute( 0, $region->[6] );
    }else{
      $updateSth->execute( 1, $region->[6] );
    }
  }

  $dbh->commit;
  $dbh->{AutoCommit} = 1;

  #Update competed flag in clans table
  $clan->update( { competed => 1 } );
  my $clanMembership = $db->getClanMembership($clan_acc);
  foreach my $mem (@$clanMembership) {
    $mem->pfama_acc->update( { updated => \'NOW()' } );
  }
}

sub _competeSequence {
  my ( $seqRegionsRef, $clanSeedRef, $nestedRef ) = @_;

#Collect all regions which need removing from pfamA_reg_full_significant in this hash
  my ( %lose, %seed );

#Now go through all the sequence regions in clan and identify those that need to be removed
  foreach my $region1 ( @{$seqRegionsRef} ) {
    #Each regions should be an array ref of:
    #pfamA_acc, 0
    #pfamseq_acc,1
    #ali_start,2
    #ali_end, 3
    #domain_evalue_score,4
    #in_full, 5
    #auto_pfamA_reg_full, 6

    my $overlap = "";

    #Identify any seed overlaps
    if ( defined($clanSeedRef) and exists( $clanSeedRef->{ $region1->[1] } ) ) {
      foreach my $seed_region (  @{ $clanSeedRef->{ $region1->[1] } } ) {
        #Does this region overlap with a SEED region?
        $overlap = _overlap( $seed_region, $region1 );
        #If there is an overlap, can we tolerate it?
        if ($overlap) {
          #Are these the same families?
          if ( $seed_region->[0] eq $region1->[0] )
          {    #Seed regions stay with family
            $overlap = 0;
            $seed{ $region1->[6] }++;
            last;
          }

          if (
            $nestedRef->{ $region1->[0] }->{ $seed_region->[0] }
              or $nestedRef->{ $seed_region->[0] }->{ $region1->[0] }

          )
          {
            $overlap = 0;
            next;
          }

          if ($overlap == 1){
            $lose{ $region1->[6] } = 1;
            last; #We do not need check any more seed regions  
          }

        }
      }
    }
  }


  foreach my $region1 ( @{$seqRegionsRef} ) {
    next if ( exists( $lose{ $region1->[6] } ) );
    #Then identify whether region overlaps with another regions
    foreach my $region2 ( @{$seqRegionsRef} ) {

      #Use the index to skip the same region or if we have already discounted
      next if ( $region1->[6] eq $region2->[6] );
      #Allow overlaps within the same family
      next if ( $region1->[0] eq $region2->[0] );
      next if ( exists( $lose{ $region2->[6] } ) );

      #Are these two domains allowed to nest within the clans?
      next if (
        $nestedRef->{ $region1->[0] }->{ $region2->[0] }
          or $nestedRef->{ $region2->[0] }->{ $region1->[0] }
      );

      my $overlap = _overlap( $region1, $region2 );
      next unless ($overlap);

      #If we get here the region overlaps so either region1 or region2 needs to be removed
      #Check the evalue score, higher is worse
      if ( $region1->[4] >= $region2->[4] ) {
        if(!exists($seed{$region1->[6]})){
          $lose{$region1->[6]} = 1;
        }elsif( exists($seed{$region2->[6]})){
          #We still want to get rid of one, as both overlap with SEED regions, but 2 is better
          $lose{$region1->[6]} = 1;
        }else{
          $lose{$region2->[6]} = 1;
        }
        last;
      }else{
        if(!exists($seed{$region1->[6]})){
          $lose{$region2->[6]} = 1;
        }elsif( exists($seed{$region1->[6]})){
          #We still want to get rid of one, as both overlap with SEED regions, but 2 is better
          $lose{$region2->[6]} = 1;
        }else{
          $lose{$region1->[6]} = 1;
        }
        last;
      }
    }
  }

  return(\%lose);

}

#-------------------------------------------------------------------------------

=head2 uncompeteClan 

  Title    : uncompeteClan
  Usage    : Bio::Pfam::Clan::Compete::uncompeteClan($clan, \@clanMembership, $pfamDB) 
  Function : Resets all of the in_full regions back to 1 for all clan members
  Args     : clan accession, array references containing a list of Pfam accessions that 
           : are members of the clan, a Bio::Pfam::PfamLiveDBManager objects
  Returns  : Nothing

=cut

sub uncompeteClan {
  my ( $clan, $membership, $db ) = @_;

  foreach my $fam (@$membership) {
    my $pfam = $db->getPfamData($fam);
    unless ($pfam) {
      warn "Failed to get pfam row for $fam\n";
    }

    $db->resetInFull( $pfam->pfama_acc );
  }

}

=head2 _overlap

 Title   : _overlap
 Usage   : _overlap($region1, $region2)
 Function: Determines whether two regions overlap

 Returns : 1 if there is overlap, undefined if no overlap
 Args    : Region from pfamA_reg_full or pfamA_reg_seed table in rdb


=cut

sub _overlap {

  my ( $region1, $region2 ) = @_;
  my $overlap = 0;

  if (
    (
      $region1->[2] >= $region2->[2]
        and $region1->[2] <= $region2->[3]
    )
      or (  $region1->[3] >= $region2->[2]
        and $region1->[3] <= $region2->[3] )
      or (  $region1->[2] <= $region2->[2]
        and $region1->[3] >= $region2->[3] )
  )
  {

    $overlap = 1;

  }
  return $overlap;
}

=head2 competeClanUniprot

 Title   : competeClanUniprot
 Usage   : competeClanUniprot($clan_acc, $db_object)
 Function: Competes a clan using uniprot sequences
 Returns : Nothing
 Args    : clan accession, Bio::Pfam::PfamLiveDBManager object


=cut

sub competeClanUniprot {
  my ( $clan_acc, $db ) = @_;
  my $dbh = $db->getSchema->storage->dbh;

  #Get the clan information
  my $clan =
  $db->getSchema->resultset("Clan")->find( { clan_acc => $clan_acc } );

  #*******************************************************************
  #NOTE! Really, really important that the seq_start/seq_end remain at
  #poisitons in query for SEED regions and FULL regions - otherwise the
  #overlap call will explode!!!!!
  #*******************************************************************

  my $sthSeedRegs =
  $dbh->prepare( "select s.pfamA_acc, pfamseq_acc, seq_start,"
    . " seq_end from pfamA_reg_seed s, clan_membership c"
    . " where c.pfamA_acc=s.pfamA_acc and clan_acc=\'"
    . $clan->clan_acc 
    . "\'");
  $sthSeedRegs->execute;
  my %clanSeed;
  foreach my $row ( @{ $sthSeedRegs->fetchall_arrayref } ) {
    push(@{$clanSeed{ $row->[1] }}, $row);    #keyed off pfamseq_acc
  }

  #Get nested data for clan
  my $sthNest =
  $dbh->prepare( "select n.pfamA_acc, nested_pfamA_acc from "
    . "nested_locations n, clan_membership c where "
    . "c.pfamA_acc=n.pfamA_acc and clan_acc=\'"
    . $clan->clan_acc 
    . "\'");
  $sthNest->execute;
  my %nested;
  foreach my $row ( @{ $sthNest->fetchall_arrayref } ) {
    $nested{ $row->[0] }{ $row->[1] } = 1;    #keyed off pfamA_acc
  }

  #Get all full data for clan
  my $sthFullRegs =
  $dbh->prepare( "select u.pfamA_acc, uniprot_acc, ali_start, "
    . "ali_end, domain_evalue_score, in_full, auto_uniprot_reg_full from "
    . "uniprot_reg_full u, clan_membership c where "
    . "c.pfamA_acc=u.pfamA_acc and clan_acc=\'"
    . $clan->clan_acc
    . "\'"
    . " order by uniprot_acc, domain_evalue_score" );
  $sthFullRegs->execute;

  my $updateSth =
  $dbh->prepare(
    "update uniprot_reg_full set in_full=? where auto_uniprot_reg_full=?"
  );

  my @seqRegions;
  my $currentUniprot;
  my $count = 0;
  $dbh->{AutoCommit} = 0;
  while ( my @row = $sthFullRegs->fetchrow_array ) {
    if ( defined($currentUniprot) and $currentUniprot ne $row[1] ) {
      my $loseRef = _competeSequence( \@seqRegions, \%clanSeed, \%nested );
      #Loop over and set any region that is out competed to be in_full=0, based 
      #on the region index;      
      foreach my $region ( @seqRegions ) {
        if(exists($loseRef->{ $region->[6] })){
          #update uniprot_reg_full, in_full=0 based on index
          $updateSth->execute( 0, $region->[6] );
        }else{
          $updateSth->execute( 1, $region->[6] );
        }
      }

      @seqRegions = ();
      if ( $count > 1000 ) {
        $dbh->commit;
        $count=0;
      }
    }
    $count++;
    $currentUniprot = $row[1];
    push( @seqRegions, \@row );
  }

  my $loseRef = _competeSequence(\@seqRegions, \%clanSeed, \%nested, $updateSth );
  foreach my $region ( @seqRegions ) {
    if(exists($loseRef->{ $region->[6] })){
      #update uniprot_reg_ful, in_full=0 based on index
      $updateSth->execute( 0, $region->[6] );
    }else{
      $updateSth->execute( 1, $region->[6] );
    }
  }

  $dbh->commit;
  $dbh->{AutoCommit} = 1;

  #Update competed flag in clans table
  $clan->update( { uniprot_competed => 1 } );
  
  #my $clanMembership = $db->getClanMembership($clan_acc);
  #foreach my $mem (@$clanMembership) {
  #  $mem->pfama_acc->update( { updated => \'NOW()' } );
  #}
}


1;
