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

  #Get clan
  my $clan =
    $db->getSchema->resultset("Clans")->find( { clan_acc => $clan_acc } );


  #Get all seed data for clan
  my @pfamA_seed_data =
    $db->getSchema->resultset("PfamaRegSeed")
    ->search( { 'clan_membership.auto_clan' => $clan->auto_clan },
    { join => [qw(clan_membership pfamseq pfama)],
      prefetch => [qw(pfamseq pfama)]
      
       });
  my %clan_seed;
  foreach my $row (@pfamA_seed_data) {
    push( @{ $clan_seed{ $row->pfamseq_acc } }, $row );
  }
  
  #Get all full data for clan
  my @pfamA_full_data =
    $db->getSchema->resultset("PfamaRegFullSignificant")->search(
    { 'clan_membership.auto_clan' => $clan->auto_clan },
    {
      join     => [qw(clan_membership pfamseq pfama)],
      prefetch => [qw(pfamseq pfama)],
      order_by => 'me.domain_evalue_score DESC'
    }
    );

  my %clan_full;
  foreach my $row (@pfamA_full_data) {
    push( @{ $clan_full{ $row->pfamseq_acc } }, $row );
  }

  #Get nested data for clan
  my @nested_data =
    $db->getSchema->resultset("NestedLocations")
    ->search( { 'clan_membership.auto_clan' => $clan->auto_clan },
    { join => [qw(clan_membership )] } );
  my %nested;
  foreach my $n (@nested_data) {

    my $f1 = $db->getSchema->resultset('Pfama')->find({auto_pfama => $n->auto_pfama->auto_pfama });
    my $f2 = $db->getSchema->resultset('Pfama')->find({auto_pfama => $n->nested_auto_pfama});

    $nested{ $f1->pfama_acc }{ $f2->pfama_acc } = 1;
  }
  
#Collect all regions which need removing from pfamA_reg_full/pfamA_reg_full_significant in this hash
  my (%lose);

#Now go through all the sequence regions in clan and identify those that need to be removed
  foreach my $auto_pfamseq ( keys %clan_full ) {

    foreach my $region1 ( @{ $clan_full{$auto_pfamseq} } ) {

      next if ( exists( $lose{$region1} ) );

      my $overlap = "";
      my $seed    = "";

      #Identify any seed overlaps
      if ( exists( $clan_seed{$auto_pfamseq} ) ) {
        foreach my $seed_region ( @{ $clan_seed{$auto_pfamseq} } ) {

          if ( $seed_region->pfama_acc eq
            $region1->pfama_acc )
          {    #Seed regions stay with family
            $seed = 1;
            last;
          }
          next
            if (
            exists(
              $nested{ $region1->pfama_acc }{ $seed_region->pfama_acc }
            )
            );    #Ignore any nested regions
          next
            if (
            exists(
              $nested{ $seed_region->pfama_acc }{ $region1->pfama_acc }
            )
            );

          $overlap = _overlap( $seed_region, $region1 );

          last if ($overlap);
        }
      }

      next if ($seed);    #Seed regions always stay

      if ($overlap) {
        $lose{$region1} = $region1;

        #update pfamA_reg_full_significant
        $region1->update( { in_full => 0 } );
        next;
      }

      #Then identify whether region overlaps with another non seed region

      foreach my $region2 ( @{ $clan_full{$auto_pfamseq} } ) {

        next if ( $region1 eq $region2 );
        next if ( exists( $lose{$region2} ) );

        next
          if (
          exists( $nested{ $region1->pfama_acc }{ $region2->pfama_acc } ) )
          ;    #Ignore any nested regions
        next
          if (
          exists( $nested{ $region2->pfama_acc }{ $region1->pfama_acc } ) );

        $overlap = _overlap( $region1, $region2 );
        next unless ($overlap);

#If we get here the region overlaps so either region1 or region2 needs to be removed
        if ( $region1->domain_evalue_score >= $region2->domain_evalue_score ) {
          $lose{$region1} = $region1;

          #update pfamA_reg_full_significant
          $region1->update( { in_full => 0 } );
          last;
        }
        else {
          $lose{$region2} = $region2;

          #update pfamA_reg_full_significant
          $region2->update( { in_full => 0 } );
        }
      }
    }
  }

  #Update competed flag in clans table
  $clan->update( { competed => 1 } );
  my $clanMembership = $db->getClanMembership($clan_acc);
  foreach my $mem (@$clanMembership){
    $mem->auto_pfama->update({updated => \'NOW()'});
  }
  
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
  my ($clan, $membership, $db) = @_;
  
 
  foreach my $fam (@$membership) {
    my $pfam = $db->getPfamData(  $fam );
    unless( $pfam ){
      warn "Failed to get pfam row for $fam\n";
    }
    
    $db->resetInFull($pfam->auto_pfama);
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
  my $overlap = "";

  if (
    (
          $region1->seq_start >= $region2->seq_start
      and $region1->seq_start <= $region2->seq_end
    )
    or (  $region1->seq_end >= $region2->seq_start
      and $region1->seq_end <= $region2->seq_end )
    or (  $region1->seq_start <= $region2->seq_start
      and $region1->seq_end >= $region2->seq_end )
    )
  {

    $overlap = 1;

  }
  return $overlap;
}

1;
