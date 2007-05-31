
#
# BioPerl module for Bio::Pfam::PfamDBManager
#
# $Author

package Bio::Pfam::PfamDBManager;
use strict;
use warnings;
use PfamDB;
use Data::Dumper;
use Carp;

sub new {
    my $caller = shift;
    my $class = ref($caller) || $caller;
    my %dbiParams = ();
    my $self = { user      => "pfam_web_ro",
		             host      => "pfamdb1",
		             port      => "3306",
		             database  => "pfam_21_0_web",
		             driver    => "mysql",
	               debug     => 0,
		             @_,};
		 
    #print STDERR Dumper($self);
    
    eval{
     $self->{'schema'} = PfamDB->connect("dbi".
				       ":".$self->{driver}.
				       ":".$self->{database}.
				       ":".$self->{host}.
				       ":".$self->{port},
				       $self->{user},
				       $self->{password},
				       \%dbiParams);
    };
    if($@){
      croak("Failed to get schema for databse:".$self->database.". Error:[$@]\n");     
    }
    
    #print STDERR "HERE".Dumper($self);
    
    return bless($self, $caller);
} 


sub getSchema{
    my $self = shift;
    return $self->{schema};
}

sub DESTROY {
  my $self = shift;
  $self->{'schema'}->storage->disconnect;
}


#Select methods should go in here.

#General shared by all Pfam DB managers
sub id2acc {
 my ($self, $id) = @_;
 my $result = $self->getSchema
                    ->resultset("Pfam")
                     ->find({"pfamA_id" => $id});
 if($result && $result->pfamA_acc){
  return ($result->pfamA_acc);
 }
}

sub acc2id {
 my ($self, $acc) = @_;
 my $result = $self->getSchema
                    ->resultset("Pfam")
                     ->find({"pfamA_acc" => $acc});
 if($result && $result->pfamA_id){
  return ($result->pfamA_id);
 }
}


#Try to find in the incoming family is part of a clan
sub getClanDataByPfam {
 my ($self, $family) = @_;
 
 my $result;
 if($family =~ /PF\d+/){
  #Looks like an accession
  carp("Looking up information for $family. I think this is an accession") if $self->{'debug'}; 
  $result = $self->getSchema
                     ->resultset("Clan_membership")
                      ->find({ "pfam.pfamA_acc" => $family},
                             {join     => [qw/pfam clans/],
                              prefetch => [qw/clans/] });
  
 }elsif($family =~ /\S{1,16}/){
  #Looks like we have a family id
  carp("Looking up information for $family. I think this is an id") if $self->{'debug'}; 
  $result = $self->getSchema
                     ->resultset("Clan_membership")
                      ->find({ "pfam.pfamA_id" => $family},
                             {join     => [qw/pfam clans/],
                              prefetch => [qw/clans/] });
                              
 }else{
  cluck("$family does not look like a pfamA accession or id");
 }
 #Return something if we have found something
 if($result && $result->clan_acc){
   carp("Found clan information for\n") if $self->{'debug'};
  return ($result);
 }
 carp("Did not find any clan information for $family") if $self->{'debug'};
}



sub getClanData {
 my($self, $clan) = @_;
 my $clanData;
 if($clan =~ /CL\d{4}/){
 carp("Looking up information for $clan. I think this is an accession") if $self->{'debug'};
 $clanData = $self->getSchema
                    ->resultset("Clans")
                     ->find({"clan_acc" => $clan});
                     
 }elsif($clan =~ /\S{1,16}/){
  carp("Looking up information for $clan. I think this is an id") if $self->{'debug'};
  $clanData = $self->getSchema
                    ->resultset("Clans")
                     ->find({"clan_id" => $clan});
 }else{
  cluck("$clan does not look like a clan accession or id")
 }
 if(ref($clanData)){
   carp("Found clan information for $clan") if $self->{'debug'};
  return ($clanData);
 } 
 carp("Did not find clan information for $clan") if $self->{'debug'};
}

sub getClanMembership{
  my($self, $clan) = @_;
  my @clanData;
  if($clan =~ /CL\d{4}/){
    carp("Looking up information for $clan. I think this is an accession") if $self->{'debug'};
    @clanData = $self->getSchema
                     ->resultset("Clan_membership")
                      ->search({"clans.clan_acc" => $clan},
                               { join     => [ qw/clans pfam/],
                                 prefetch => [ qw/clans pfam/] });
                      
  }elsif($clan =~ /\S{1,16}/){
   carp("Looking up information for $clan. I think this is an id") if $self->{'debug'};
   @clanData = $self->getSchema
                     ->resultset("Clan_membership")
                      ->search({"clans.clan_id" => $clan},
                               { join     =>  qw( clans pfam ),
                                 prefetch =>  qw( clans pfam ) });
  }else{
   cluck("$clan does not look like a clan accession or id")
  }
  
  if($#clanData){
    carp("Found clan information for $clan") if $self->{'debug'};
   return (\@clanData);
  }else{ 
    carp("Did not find clan membership for $clan") if $self->{'debug'};
  }
}

sub getClanAllClanData {
  my($self) = shift; 
  my @clanData = $self->getSchema
                        ->resultset("Clans")
                          ->search();
  return \@clanData;
}

sub getPfamData{
  my($self, $family) = @_;
  my $familyData;
  if($family =~ /PF\d{5}/){
    carp("Looking up information for $family. I think this is an accession") if $self->{'debug'};
    $familyData = $self->getSchema
                         ->resultset("Pfam")
                           ->find({"pfamA_acc" => $family});
                     
  }elsif($family =~ /\S{1,16}/){
    carp("Looking up information for $family. I think this is an id") if $self->{'debug'};
    $familyData = $self->getSchema
                      ->resultset("Pfam")
                       ->find({"pfamA_id" => $family});
 }else{
  cluck("$family does not look like a family accession or id")
 }
 if(ref($familyData)){
   carp("Found family information for $family") if $self->{'debug'};
  return ($familyData);
 } 
 carp("Did not find family information for $family") if $self->{'debug'};
}

sub getPfamInterPro{
  my($self, $family) = @_;
  my $familyData;
  if($family =~ /PF\d{5}/){
    carp("Looking up information for $family. I think this is an accession") if $self->{'debug'};
    $familyData = $self->getSchema
                         ->resultset("Pfam")
                           ->find({"pfamA_acc" => $family},
                                  {join        => qw( interpro ),
                                   prefetch    => qw( interpro )});
                     
  }elsif($family =~ /\S{1,16}/){
    carp("Looking up information for $family. I think this is an id") if $self->{'debug'};
    $familyData = $self->getSchema
                      ->resultset("Pfam")
                       ->find({"pfamA_id" => $family},
                              {join        => qw( interpro ),
                               prefetch    => qw( interpro )});
 }else{
  cluck("$family does not look like a family accession or id")
 }
 if(ref($familyData)){
   carp("Found family information for $family") if $self->{'debug'};
  return ($familyData);
 } 
 carp("Did not find family information for $family") if $self->{'debug'};
}

sub getPfamGO{
  my($self, $family) = @_;
  my @familyGO;
  if($family =~ /PF\d{5}/){
    carp("Looking up information for $family. I think this is an accession") if $self->{'debug'};
    @familyGO = $self->getSchema
                         ->resultset("Pfam")
                           ->search({"pfamA_acc" => $family},
                                    {join        => qw( go ),
                                     prefetch    => qw( go )});
                     
  }elsif($family =~ /\S{1,16}/){
    carp("Looking up information for $family. I think this is an id") if $self->{'debug'};
    @familyGO = $self->getSchema
                         ->resultset("Pfam")
                           ->search({"pfamA_id" => $family},
                                    {join        => qw( go ),
                                     prefetch    => qw( go )});
 }else{
  cluck("$family does not look like a family accession or id")
 }
 if(scalar(@familyGO)){
   carp("Found family information for $family") if $self->{'debug'};
  return (\@familyGO);
 } 
 carp("Did not find family information for $family") if $self->{'debug'};
}

sub getNSEInFull {
  my($self, $family) = @_;
  my @familyNSE;
  if($family =~ /PF\d{5}/){
    carp("Looking up information for $family. I think this is an accession") if $self->{'debug'};
    @familyNSE = $self->getSchema
                         ->resultset("PfamA_reg_full_significant")
                           ->search({"pfamA.pfamA_acc" => $family,
                                     "in_full"        => 1},
                                    {join        => [ qw( pfamA pfamseq ) ] ,
                                     prefetch    => [ qw( pfamA pfamseq ) ]});
                     
  }elsif($family =~ /\S{1,16}/){
    carp("Looking up information for $family. I think this is an id") if $self->{'debug'};
    @familyNSE = $self->getSchema
                         ->resultset("PfamA_reg_full_significant")
                           ->search({"pfamA.pfamA_id" => $family,
                                     "in_full"        => 1},
                                    {join        => [ qw( pfamA pfamseq ) ],
                                     prefetch    => [ qw( pfamA pfamseq ) ]});

   }else{
    cluck("$family does not look like a family accession or id")
  }
  if(scalar(@familyNSE)){
    carp("Found family information for $family") if $self->{'debug'};
    return (\@familyNSE);
  } 
  carp("Did not find family information for $family") if $self->{'debug'};
}

sub getPfamRegionsForSeq{
  my($self, $seq) = @_;
  my @pfamRegions;
  if($seq =~ /\S{6}/){
    carp("Looking up information for $seq. I think this is an accession") if $self->{'debug'};
    @pfamRegions = $self->getSchema
                         ->resultset("PfamA_reg_full_significant")
                           ->search({"pfamseq.pfamseq_acc" => $seq,
                                     "in_full"             => 1},
                                    {join        => [ qw( pfamA pfamseq ) ],
                                     prefetch    => [ qw( pfamA pfamseq ) ]});
                     
  }elsif($seq =~ /\S+_\S+/){
    carp("Looking up Pfam information for $seq. I think this is a seq id") if $self->{'debug'};
    @pfamRegions = $self->getSchema
                         ->resultset("PfamA_reg_full_significant")
                           ->search({"pfamseq.pfamseq_id" => $seq,
                                     "in_full"        => 1},
                                    {join        => [ qw( pfamA pfamseq ) ],
                                     prefetch    => [ qw( pfamA pfamseq ) ]});
  
  }elsif($seq =~ /\S{33}/){
    carp("Looking up Pfam information for $seq. I think this is a seq MD5 checksum") if $self->{'debug'};
    @pfamRegions = $self->getSchema
                         ->resultset("PfamA_reg_full_significant")
                           ->search({"pfamseq.pfamseq_md5" => $seq,
                                     "in_full"        => 1},
                                    {join        => [ qw( pfamA pfamseq ) ],
                                     prefetch    => [ qw( pfamA pfamseq ) ]});
  }else{
    cluck("$seq does not look like a family accession or id");
  }
  if(scalar(@pfamRegions)){
    carp("Found family information for $seq") if $self->{'debug'};
    return (\@pfamRegions);
  } 
  carp("Did not find family information for $seq") if $self->{'debug'};
}

#Specific insert/update methods should go here



=head1 COPYRIGHT

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


1;



