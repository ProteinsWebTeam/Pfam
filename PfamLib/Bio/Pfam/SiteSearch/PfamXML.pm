package Bio::Pfam::SiteSearch::PfamXML;

=head1 COPYRIGHT

File: PfamXML.pm

Copyright (c) 2009: Genome Research Ltd.

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
use Carp;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

use base ( 'Sanger::SiteSearch::XML' );

#Override it as we probably can not get the whole of what we want into memory

sub createNewDump{
  my( $self, $type ) = @_;
  
  
  unless($type and $type =~ /^(sequence|family|clan)$/){
    croak ( "Need a type of file that you want to dump") unless($type);
    croak ( "$type must match one of sequence, family, clan\n");
  }
  
  #Now we need to get a pfamDB manager object
  my $config = Bio::Pfam::Config->new;
  my $connect = $config->pfamlive;

  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
    %{ $connect }
  );
  
  $self->_writeXMLtag(1);
  
  my $version = $pfamDB->getVersion;
  
  if($type eq 'family'){
    my $data;
    $data->{name}        = 'PfamFamily';
    $data->{count}       = $version->number_families;
    $data->{description} = 'The protein families contained within the database';
    $data->{release}     = $version->pfam_release;
    $data->{releaseDate} = $version->pfam_release_date;
    $self->_databaseHeader( $data );
    $self->_databaseEntries( $pfamDB );
  }
  elsif($type eq 'sequence'){
    my $data; 
    $data->{name}        = 'PfamSequence';
    $data->{count}       = $config->dbsize;
    $data->{description} = 'The sequences and their domain annotation contained within the database';
    $data->{release}     = $version->pfam_release;
    $data->{releaseDate} = $version->pfam_release_date;
    $self->_databaseHeader( $data );
    $self->_databaseSeqEntries( $pfamDB );
  }
  elsif($type eq 'clan') {
    my $data;
    my @clan=$pfamDB->getSchema->resultset('Clan')->search();
    $data->{name}        = 'PfamClan';
    $data->{count}       = scalar(@clan);
    $data->{description} = 'The clans contained within the database';
    $data->{release}     = $version->pfam_release;
    $data->{releaseDate} = $version->pfam_release_date;
    $self->_databaseHeader( $data );
    $self->_databaseClanEntries( $pfamDB );
  }
      
  $self->_databaseFooter();
}

sub _databaseEntries{
  my($self, $pfamDB) = @_;
  
  my $families = $pfamDB->getAllPfamFamilyData;
 
  my $reg_full="regfull_dump_site_search.txt";

  unless(-s $reg_full) {
    my $regionsCommand = "mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database}." --skip-column-names --quick -e 'select pfamseq_acc, pfamA_acc from pfamA_reg_full_significant where in_full=1' > $reg_full";
    system("$regionsCommand") and die "Couldn't run mysql command [$regionsCommand], $!";
  }

  my %regs;
  open(REGS, $reg_full) or die "Couldn't open fh to $reg_full, $!";
  while(<REGS>) {
    if(/^(\S+)\s+(\S+)/) {
      my ($pfamseq_acc, $pfamA_acc) = ($1, $2);
      $regs{$pfamA_acc}{$pfamseq_acc}=1;
    }   
  }
  close REGS;


  my $fh = $self->_filehandle;
  print $fh "<entries>";
  foreach my $pfama (@$families){
    my $data;
    $data->{id}  = $pfama->pfama_id;
    $data->{acc} = $pfama->pfama_acc;
    $data->{name} = $pfama->pfama_id;
    $data->{authors} = $pfama->author;
    $data->{description} = $pfama->description;
    $data->{dates}->{creation} = $pfama->created;
    $data->{dates}->{last_modified} = $pfama->updated;    
    
    $data->{addFields}->{comment} = $pfama->comment if($pfama->comment);
    $data->{addFields}->{type}    = $pfama->type;

    #Get Interpro Xrefs;
    my $interPro = $pfamDB->getPfamInterPro($pfama->pfama_acc);
  
    if($interPro){
      if( defined( $interPro->interpros->first)){
        $data->{addFields}->{interproAbstract} = $interPro->interpros->first->abstract;     
        $data->{xrefs}->{interpro} = $interPro->interpros->first->interpro_id;
      }
    }
  
    #Get GO terms
    my $go = $pfamDB->getPfamGO( $pfama->pfama_acc );
    if( $go ){
      foreach my $g (@{$go}){
       foreach my $go ($g->gene_ontologies){ 
        $data->{addFields}->{$go->category}->{$go->go_id.';'.$go->term}=1;  
        $data->{xrefs}->{go}->{$go->go_id}=1;
       }
     }  
   }
   
   #Get clan data
   my $clanResult = $pfamDB->getSchema->resultset("ClanMembership")->find(
     { "pfama_acc.pfama_acc" => $pfama->pfama_acc },
     {  join     => [qw/pfama_acc clan_acc/], prefetch => [qw/clan_acc/] } );

   if($clanResult) {
     $data->{xrefs}->{pfam_clans}->{$clanResult->clan_acc->clan_acc}=1;
   }

   #Add pfamseq data
   foreach my $pfamseq_acc (keys %{$regs{$pfama->pfama_acc}}) {
     $data->{xrefs}->{pfam_seqs}->{$pfamseq_acc} = 1;
   }
  
                  
   $self->_databaseEntry($data);  
  }
  print $fh "</entries>\n";

}

sub _databaseClanEntries {
  my($self, $pfamDB) = @_;

  my $clans = $pfamDB->getAllClanData;
 
  my @membership = $pfamDB->getSchema->resultset("ClanMembership")
                  ->search( {} );

  my %membership;
  foreach my $c (@membership) {
    $membership{$c->clan_acc->clan_acc}{$c->pfama_acc->pfama_acc}=1;
  }


  my $fh = $self->_filehandle;
  print $fh "<entries>";
  foreach my $clan (@$clans){
    my $data;
    $data->{id}  = $clan->clan_id;
    $data->{acc} = $clan->clan_acc;
    $data->{name} = $clan->clan_id;
    $data->{authors} = $clan->clan_author;
    $data->{description} = $clan->clan_description;
    $data->{dates}->{creation} = $clan->created;
    $data->{dates}->{last_modified} = $clan->updated;    
    
    $data->{addFields}->{comment} = $clan->clan_comment if($clan->clan_comment);
  
    #Link to families in the clan 
    foreach my $pfamA_acc (keys %{$membership{$clan->clan_acc}} ) {
      $data->{xrefs}->{pfam_entries}->{$pfamA_acc}=1;
    }
    $self->_databaseEntry($data);  
  }
  print $fh "</entries>\n";
}



sub _databaseSeqEntries{
  my ($self, $pfamDB ) = @_;

  my $pfamseq_file="pfamseq_dump_site_search.txt";
  my $reg_full="regfull_dump_site_search.txt";

  unless(-s $pfamseq_file) {
    my $pfamseqCommand = "mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database}." --skip-column-names --quick -e 'select pfamseq_acc, pfamseq_id, description, species, ncbi_taxid from pfamseq' > $pfamseq_file";
    system("$pfamseqCommand") and die "Couldn't run mysql command [$pfamseqCommand], $!";
  }

  unless(-s $reg_full) {
    my $regionsCommand = "mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database}." --skip-column-names --quick -e 'select pfamseq_acc, pfamA_acc from pfamA_reg_full_significant where in_full=1' > $reg_full";
    system("$regionsCommand") and die "Couldn't run mysql command [$regionsCommand], $!";
  }

  my %regs;
  open(REGS, $reg_full) or die "Couldn't open fh to $reg_full, $!";
  while(<REGS>) {
    if(/^(\S+)\s+(\S+)/) {
      my ($pfamseq_acc, $pfamA_acc) = ($1, $2);
      $regs{$pfamseq_acc}{$pfamA_acc}=1;
    }
  }
  close REGS;  

  my $fh = $self->_filehandle;
  print $fh "<entries>";

  open(PFAMSEQ, $pfamseq_file) or die "Couldn't open fh to $pfamseq_file, $!";
  while(<PFAMSEQ>) {
    my @line = split(/\t/, $_);
    my ($pfamseq_acc, $pfamseq_id, $description, $species, $ncbi_taxid) = ($line[0], $line[1], $line[2], $line[3], $line[4]); 
    chomp $ncbi_taxid;

    my $data;
    $data->{id}  = $pfamseq_id;
    $data->{acc} = $pfamseq_acc;
    $data->{name} = $pfamseq_id;
    $data->{description} = $description;
    $data->{addFields}->{species} = $species;
    $data->{xrefs}->{ncbi_taxid} = $ncbi_taxid;

      
    #Add uniprot reference
    $data->{xrefs}->{uniprotkb}->{$pfamseq_acc}=1;
    
    #Add pfamA references, if any
    if($regs{$pfamseq_acc}) {
      foreach my $pfamA_acc (keys %{$regs{$pfamseq_acc}}) {
        $data->{xrefs}->{pfam_entries}->{$pfamA_acc} = 1;
      }
    }

    $self->_databaseEntry($data);  

  }
  close PFAMSEQ;
  print $fh "</entries>\n";
}

1;

