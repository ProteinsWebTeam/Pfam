
#
# BioPerl module for Bio::Pfam::PfamDBManager
#

package Bio::Pfam::PfamDBManager;
use strict;
use warnings;
use PfamDB;
use Data::Dumper;
use strict;
use Carp;

sub new {
 print "In PfamLive new\n\n";
    my $caller = shift;
    my $class = ref($caller) || $caller;
    my %dbiParams = ();
    my $self = { user      => "pfamro",
		 host      => "pfam",
		 port      => "3306",
		 database  => "pfam_21_0",
		 driver    => "mysql",
		 @_,};


    print STDERR Dumper($self) && $self->{'debug'};
    
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
                               { join     => [ qw/clans pfam/],
                                 prefetch => [ qw/clans pfam/] });
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

#Specific insert/update methods should go here



1;


