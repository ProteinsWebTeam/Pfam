#
# pg6 20000826
# iPfamInteraction.pm
# das source which provides interaction data between pfam domains and 
# ligands present in the iPfam database

=head1 Name

Bio::Das::ProServer::SourceAdaptor::iPfamInteraction - Sourceadaptor which displays the domain ligand interactions

=cut

package Bio::Das::ProServer::SourceAdaptor::iPfamInteraction;

use strict;
use warnings;
use Data::Dump qw( dump );

#use base 'Bio::Das::ProServer::SourceAdaptor';
use base qw(Bio::Das::ProServer::SourceAdaptor);
=head1 Description

Sourceadaptor to get the domain-ligand interactions from the ipfam database and the data is served via DAS.

=cut

my $DEBUG = 1;

=head1 Methods

=head2 init

Method which initialises the object created by the parent class SourceAdaptor.

=cut

sub init{
  my $self = shift;
  $self->{ capabilities}{ interaction } = '1.0';
}

#-------------------------------------------------------------------------------------------------------

=head2 build_interaction

Method called from the parent class for the das request, it validates the input data
and trigger other methods.

=cut

sub build_interaction {
  my ( $self, $opts ) = @_;
  print "the dump of the opts is ".dump($opts )."\n";
  
  # now check whether we do get any interactors or not;
  unless (exists $opts->{ interactors } ){
    print "Options is not defined\n";
    return;
  } 
  
  # now we need to check for the valid interactor;
  # now i get only one, but later may get a list of interactors
  # so usign foreach loop;
  
  my $ligand_interactions = {};
  foreach my $interactor( @{ $opts->{ interactors } } ){
    
    if( $interactor =~ /^([a-zA-Z0-9.-]+)/ ){
      print "private method to get ligand data is called for $1\n";
      $ligand_interactions->{ $1 } = $self->_get_ligand_interactions( $1 );
    }else{
      print " Interactor-$interactor contains invalid characters\n";
    }  
  }
  
  # now call the private method build_interactors to create a datastructure;
  my ( $interactions, $interactors ) = $self->_build_interactors( $ligand_interactions );
  return {
        'interactors'  => [values %$interactors],
        'interactions' => $interactions,
   };
}

#-------------------------------------------------------------------------------------------------------

=head1 Private Methods

=head2 _build_interactors

Method which parses the database output and generates a datastructure.

=cut

sub _build_interactors {
  my ( $self, $dbInteractions ) = @_;
  print "Interactions inside the build_interactors ".dump( $dbInteractions )."\n";
#     <INTERACTOR intId="PF08542pfamA" shortLabel="Rep_fac_C" dbSource="pfamA"
#      dbAccessionId="PF08542" dbCoordSys="Pfam,Protein Sequence" dbVersion="22.0">
#<INTERACTION name="PF00004-PF05496" dbSource="iPfam" dbAccessionId="PF00004-PF05496" dbVersion="22.0">
#<PARTICIPANT intId="PF00004pfamA"/>
#<PARTICIPANT intId="PF05496pfamA"/>
#</INTERACTION>
  
  # if multiple interactors are provided it  
  my ( $interactors, @interactions );
  foreach my $key ( %$dbInteractions  ){
    
    # now get the ligand interactions for individual families;
    foreach my $row( @{ $dbInteractions->{ $key } } ){
      
      my $pfama_acc = $row->{ pfama_acc };
      
      # first add the family to the datastructure;
      unless ( exists $interactors->{ $pfama_acc } ){
        print "the row is ".$row->{ pfama_acc }."\n";
        # now building the interactors;
        $interactors->{ $pfama_acc }->{ id }            = "$pfama_acc".'pfamA';
        $interactors->{ $pfama_acc }->{ label }         = $row->{ pfama_id }; 
        $interactors->{ $pfama_acc }->{ dbSource }      = 'iPfam';
        $interactors->{ $pfama_acc }->{ dbAccession }   = $pfama_acc;
        $interactors->{ $pfama_acc }->{ dbCoordSys }    = "Pfam,Protein Sequence";
        $interactors->{ $pfama_acc }->{ dbVersion }     = '22.0'; # now hard coded later change as to get it from db;            
        my %details;
        $details{ 'property' }                          = 'description';
        $details{ 'value' }                             = ' I woudlnt worry about value';
        
        push ( @{ $interactors->{$pfama_acc }->{ details } }, \%details );
      }
      
      # now get all the ligands;
      my $ligand_acc = $row->{ ligand_id };
      $interactors->{ $ligand_acc }->{ id }            = "$ligand_acc";
      $interactors->{ $ligand_acc }->{ label }         = "$row->{ three_letter_code }"; 
      $interactors->{ $ligand_acc }->{ dbSource }      = 'iPfam';
      $interactors->{ $ligand_acc }->{ dbAccession }   = "$ligand_acc";
      $interactors->{ $ligand_acc }->{ dbCoordSys }    = "Pfam,Protein Sequence";
      $interactors->{ $ligand_acc }->{ dbVersion }     = '22.0';
      
      
      my %details;
      $details{ 'property' }                          = 'description';
      $details{ 'value' }                             = ' I woudlnt worry about value';
      push ( @{ $interactors->{$ligand_acc }->{ details } }, \%details );
      
      my %interactions;
      # now building the interactions;
      $interactions{ name }           = $pfama_acc.'-'.$row->{ three_letter_code };
      $interactions{ dbSource }       = 'iPfam';
      $interactions{ dbVersion }      = '22.0';
      #$interactions{ dbAccession }  = $ligand_acc;
      
      push ( @{ $interactions{ participants } }, {id => $pfama_acc.'pfamA'}, { id => $ligand_acc } );
      push ( @interactions, \%interactions );
      
    } # end of foreach my $row
     
  } # end of foreach key
  print "teh interactions is ".dump( \@interactions )."\nthe interactors are ".dump( values %$interactors );
  return ( \@interactions, $interactors );
}

#-------------------------------------------------------------------------------------------------------

=head2 _get_ligand_interactions

Method which gets the input parameters, validates it and get the corresponding ligand interactions
for the specified family.

=cut

sub _get_ligand_interactions {
  my ( $self, $entry ) = @_;

  # we need to prepare the query for using pfam 
  my $query;
  my $family = $self->transport->dbh->quote( $entry );
  
  if( $entry =~ /^PF\d{5}/){
    print "its a valid pfam accession\n"; 
    $query = "SELECT me.pfama_acc, me.pfama_id,me.domcount, me.ligcount, me.nacount, 
              domains.region_source_db, dlis.internal_ligand_id, internal_ligand_id.accession, 
              internal_ligand_id.ligand_id, internal_ligand_id.ligand_number,
              internal_ligand_id.internal_ligand_id, ligand_id.ligand_id, ligand_id.lig_code,
              ligand_id.three_letter_code, ligand_id.name,
              ligand_id.category, ligand_id.formula, ligand_id.molecular_weight 
              FROM pfama me LEFT JOIN domain domains ON ( domains.pfam_acc = me.pfama_acc ) 
              LEFT JOIN dli dlis ON ( dlis.region_id = domains.region_id )  
              JOIN ligands internal_ligand_id ON ( internal_ligand_id.internal_ligand_id = dlis.internal_ligand_id )  
              JOIN ligand_chemistry ligand_id ON ( ligand_id.ligand_id = internal_ligand_id.ligand_id ) 
              WHERE ( me.pfama_acc = $family ) 
              GROUP BY ligand_id.ligand_id ORDER BY domains.pfam_acc, dlis.region_id";     
  }else{
    print "assuming that its a pfama_id\n";
    $query = "SELECT me.pfama_acc, me.pfama_id,me.domcount, me.ligcount, me.nacount, 
              domains.region_source_db, dlis.internal_ligand_id, internal_ligand_id.accession, 
              internal_ligand_id.ligand_id, internal_ligand_id.ligand_number,
              internal_ligand_id.internal_ligand_id, ligand_id.ligand_id, ligand_id.lig_code,
              ligand_id.three_letter_code, ligand_id.name,
              ligand_id.category, ligand_id.formula, ligand_id.molecular_weight 
              FROM pfama me LEFT JOIN domain domains ON ( domains.pfam_acc = me.pfama_acc ) 
              LEFT JOIN dli dlis ON ( dlis.region_id = domains.region_id )  
              JOIN ligands internal_ligand_id ON ( internal_ligand_id.internal_ligand_id = dlis.internal_ligand_id )  
              JOIN ligand_chemistry ligand_id ON ( ligand_id.ligand_id = internal_ligand_id.ligand_id ) 
              WHERE ( me.pfama_id = $family ) 
              GROUP BY ligand_id.ligand_id ORDER BY domains.pfam_acc, dlis.region_id";
  }
  my $ligand_interactions = $self->transport->query( $query );
  return $ligand_interactions;
} 

#-------------------------------------------------------------------------------------------------------

1;




















