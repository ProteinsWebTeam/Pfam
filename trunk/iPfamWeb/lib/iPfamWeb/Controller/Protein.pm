
# Protein.pm
# pg6 20090921 WTSI
#
# $Id: Protein.pm,v 1.4 2010-01-18 13:25:39 pg6 Exp $

=head1 Name

iPfamWeb::Controller::Protein - controller for protein-related 
sections of the site.

=cut

package iPfamWeb::Controller::Protein;

=head1 Description

=cut

use strict;
use warnings;
use Data::Dump qw( dump );

use Storable qw( thaw );
use JSON qw( -convert_blessed_universally );

use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use base 'Catalyst::Controller::REST';

#-------------------------------------------------------------------------------

=head1 Methods

=head2 begin : Private 

=cut

sub begin : Private {
  my ( $this, $c ,$entry_arg ) = @_;
  
  $c->log->debug( "Protein::begin: the entry_arg is |$entry_arg|" );
  
  $c->stash->{ pageType } = 'protein';
  # now get the input arguments and detaint it.
  my $tainted_entry = $c->req->param( 'entry' )||
                      $c->req->param( 'acc' )||
                      $c->req->param( 'id' )||
                      $c->req->query_keywords || 
                      $entry_arg ||
                      '';
  my $entry;
  if( $tainted_entry ne '' ){
    
    $c->log->debug( "Protein::begin: we did get a input entry |$tainted_entry|" );
    # now detaint the input entry.
    ( $entry ) = $tainted_entry =~ /^([A-Za-z0-9\.\_]*)$/;   # now the protein entry has '.', so not validating strictly.
    
    unless( defined $entry ){
      $c->log->debug( "Protein::begin::Invalid input, entry contains invalid characters." );
      $c->stash->{ errorMsg } ||= 'Input entry contains invalid characters;' ;
      
      #stash the error template in case if the request type was html.
      $c->stash->{ template } ||= 'components/blocks/protein/error.tt';
      
      #return;  
    }else{
      
      # now look for whether we could find any protein accession for the entry;
       my $rs = $c->model( 'iPfamDB::Protein' )
                  ->search ( [ { accession  => "$entry" },
                               { id =>  "$entry" } ] );
      
      my $protein = $rs->first if ( defined $rs );
      
      # now check whether we got any protein;
      if( defined $protein ){
        
        # now stash the contents of the protein;
        $c->stash->{ rest }->{ protein } = {
          'id'          =>  $protein->id,
          'accession'   =>  $protein->get_column( 'accession' ),
          'seq_version' =>  $protein->seq_version,
          'md5'         =>  $protein->md5,
          'source_db'   =>  $protein->source_db,
          'length'      =>  $protein->get_column( 'length' ),     
          'ncbi_code'   =>  $protein->ncbi_code, 
          'sequence'    =>  $protein->sequence
        };
        
      }else{
        $c->log->debug( "Protein::begin: Cant find any protein for the input entry |$entry|" );
        $c->stash->{ errorMsg } ||= "No Protein found for entry $entry"; 
        
        $c->stash->{ template } = 'components/blocks/protein/error.tt';
        
        #return;
      } # end of else $protein;
      
    } # end of else loop of defined entry;
    
  }else{
    $c->log->debug( "Protein::begin: No input entry found " );
    
    $c->stash->{ errroMsg } ||= 'Please enter Uniprot id or name';
    
    #stash the error template in case if the request type was html.
    $c->stash->{ template } ||= 'components/blocks/protein/error.tt';
    
    #return; 
  } # end of else loop of unless tainted_entry
  
  # get the domain graphics for this sequence;
  $c->forward( 'get_graphics' );
  
  # now change the status of the request and return;
  if( $c->stash->{ errorMsg } ){
    $c->log->debug( "Protein::begin::Error occured in begin" );
    
    $this->status_bad_request(
      $c,
      message =>  $c->stash->{ errorMsg }
    );
    #stash the error template in case if the request type was html.
    $c->stash->{ template } ||= 'components/blocks/protein/error.tt';
    return;
  
  }
    
} # end of begin;

#-------------------------------------------------------------------------------

sub protein : Path : ActionClass('REST'){}

sub protein_GET{
  my( $this, $c ) = @_;
  $c->log->debug( "Protein::GET: inside the method GET " );
  $c->log->debug( "Protein::GET: forwarding to get_data " );
  $c->forward( 'get_data') unless( $c->stash->{ errorMsg } );
}

sub protein_POST{
  my( $this, $c ) = @_;
  $c->log->debug( "Protein::POST: inside the method POST " );
  $c->log->debug( "Protein::POST: forwarding to get_data " );
  $c->forward( 'get_data') unless( $c->stash->{ errorMsg } );
  
}
#-------------------------------------------------------------------------------

=head2 get_data: private

Retireves the protein information for the input id

=cut

sub get_data : Private {
  my ( $this, $c ) = @_;
   
  # after this, I am not worried about the returned data so setup the template.
  #stash the error template in case if the request type was html.
    $c->stash->{ template } = 'pages/layout.tt';
  #----------------------------------------
  
  # now change the status of the request;
   $this->status_ok(
      $c,
      entity => $c->stash->{rest}
    );
  
  # if this request originates at the top level of the object hierarchy,
  # i.e. if it's a call on the "default" method of the Family object,
  # then we'll need to do a few extra things, otherwise, we're done
  return unless ref $this eq 'iPfamWeb::Controller::Protein';
  
  #----------------------------------------
  # now get the interactions
  $c->forward( 'get_protein_interactions' );   
  
}

#-------------------------------------------------------------------------------

=head2 get_protein_interactions : Private 

Retrieves the proteins interacting with the input entry.

=cut

sub get_protein_interactions : Private{
  my ( $this, $c ) = @_;
  $c->log->debug( "Protein::get_protein_interactions: inside this function" );
  # get the proteins interacting with this protein;
  my @rs = $c->model( 'iPfamDB::Ppi')->search( { 'protein_acc_a' =>  $c->stash->{ rest }->{ protein }->{ accession } } );
  
  my @interaction;
  foreach my $interaction ( @rs ){
    $c->log->debug( "Protein:get_protein_interactions: the proteins interaction are ".$interaction->get_column( 'protein_acc_b') );  
    push( @interaction, $interaction->get_column( 'protein_acc_b') );
  }
  $c->stash->{ rest }->{ interactions } = \@interaction;
  $c->log->debug( "Protein::get_protein_interactions: the total number of interactions are".scalar(@{ $c->stash->{ rest }->{ interactions } }) );
}

#-------------------------------------------------------------------------------

=head2 get_graphics : Private 

Retrieves the domains present on that sequence and add markup to it.

=cut

sub get_graphics : Private {
  my( $this, $c ) = @_;
  $c->log->debug( 'Protein::get_graphics: inside the subroutine');
  
  #----------------------------------------
  my $rs = $c->model( 'iPfamDB::ProteinAnnseq')->
               search( { 'accession' => $c->stash->{ rest}->{ protein }->{ accession } } );
               
  my $annotation = $rs->first;
  
  unless( defined $annotation ){
    #$c->stash->{ errorMsg } ||= 'No Protein information present for the input entry';
    return;   
  }
  
  my $storable = thaw $annotation->get_column( 'annseq_storable' );
  $c->log->debug( "Protein::get_graphics:the storable value is ".$storable );
  
  if ( defined $storable ) {
    $c->stash->{seqs} = [ $storable ];

    my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
    $lm->layoutSequences( $c->stash->{seqs} );
    
    #$c->log->debug( "Protein::get_graphics:the layout value is ".dump( $c->stash->{ seqs } ) );
    
    # configure the JSON object to correctly stringify the layout manager output
    my $json = new JSON;
    # $json->pretty(1);
    $json->allow_blessed;
    $json->convert_blessed;

    # encode and stash the sequences as a JSON string
    $c->stash->{ rest }->{ layout } = $json->encode( $c->stash->{seqs} );
  }

  #----------------------------------------
}

1;








