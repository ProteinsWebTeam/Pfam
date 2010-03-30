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
  
  
  unless($type and $type =~ /^(protein|family|clan)$/){
    croak ( "Need a type of file that you want to dump") unless($type);
    croak ( "$type must match one of protein, family, clan\n");
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
  }elsif($type eq 'sequence'){
    my $data; 
    $data->{name}        = 'PfamSequence';
    $data->{count}       = $pfamDB->numberOfPfamseq;
    $data->{description} = 'The sequences and their domain annotation contained within the database';
    $data->{release}     = $version->pfam_release;
    $data->{releaseDate} = $version->pfam_release_date;
    $self->_databaseHeader( $data );
    $self->_databaseSeqEntries( $pfamDB );
  }
      
  $self->_databaseFooter();
}

sub _databaseEntries{
  my($self, $pfamDB) = @_;
  
  my $families = $pfamDB->getAllPfamFamilyData;
  
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
    #  #Get Interpro Xrefs;
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
        $data->{addFields}->{$go->category} = $go->go_id.';'.$go->term;  
        $data->{xrefs}->{ go } = $go->go_id;
       }
     }  
   }                     
    $self->_databaseEntry($data);  
  }
  print $fh "</entries>\n";

}

sub _databaseSeqEntries{
  my ( $pfamDB ) = @_;
}
1;

