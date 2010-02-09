
# ProtProtInteraction.pm
# 
# pg6 20101101 WTSI
#
# $Id: ProtProtInteraction.pm,v 1.1 2010-01-18 13:26:39 pg6 Exp $

=head1 Name

iPfamWeb::Controller::ProtProtInteraction - Controller to build protein protein interaction pages.

=head1 Description

This is the class which represents a specific protein protein interaction;
    
=cut

package iPfamWeb::Controller::ProtProtInteraction;

use Data::Dump qw( dump );

use Storable qw( thaw );
use JSON qw( -convert_blessed_universally );

use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use base 'Catalyst::Controller::REST';

#---------------------------------------------------------------------------------------------------------

=head1 Methods

=head2 begin: Private

This method detaints and finds the protein information for the input params.

=cut

sub begin : Private {
  my( $this, $c ) = @_;
  
  # first stash the pagetype to get the config for building the page;
  $c->stash->{ pageType } = 'protprotint';
  
  # now look for the input from and to params and detaint it;
  unless( defined $c->req->param( 'from' ) and defined $c->req->param( 'to' ) ){
    $c->log->debug("ProtProtInteraction:begin:No input accessions provided ");
    $c->stash->{ errorMsg } = 'Please input protein accessions';
    return;
  }
  
  my ( $from ) = $c->req->param( 'from' ) =~ /([A-Za-z0-9\.\_]+)/ ;
  my ( $to ) = $c->req->param( 'to' ) =~ /([A-Za-z0-9\.\_]+)/ ;
  $c->log->debug( "Protein:begin: the from and to input is |$from|$to|" );
  
  # now check whether we do have details about the input protien id or accession;
  if( defined $from ){
    # now get the protein from the db;
    my $rs = $c->model( 'iPfamDB::Protein')->search([
                { 'accession' =>  $from },
                { 'id'        =>  $from }, 
               ] );
    my $protFrom = $rs->first;
    
    # stash the from result;
    $c->stash->{ rest }->{ from } = {
      'accession' =>  $protFrom->get_column( 'accession' ),
      'id'        =>  $protFrom->get_column( 'id' ),
      'sequence'  =>  $protFrom->get_column( 'sequence')
      # currenlt only id,accession and sequence is added, but might add some more information later;  
    };
                   
  }else{
    $c->stash->{ errorMsg } ||= 'Invalid input accession:|$from|';
    return;
  }
  
  if( defined $to ){
    # now get the protein from the db;
    my $rs = $c->model( 'iPfamDB::Protein')->search([
                { 'accession' =>  $to },
                { 'id'        =>  $to }, 
               ] );
    my $protFrom = $rs->first;
    
    # stash the from result;
    $c->stash->{ rest }->{ to } = {
      'accession' =>  $protFrom->get_column( 'accession' ),
      'id'        =>  $protFrom->get_column( 'id' ),
      'sequence'  =>  $protFrom->get_column( 'sequence')
      # currenlt only id,accession and sequence is added, but might add some more information later;  
    };
                   
  }else{
    $c->stash->{ errorMsg } ||= 'Invalid input accession:|$to|';
    return;
  }
  
}

#---------------------------------------------------------------------------------------------------------

sub protprotint : Path( '/protprotinteraction'): ActionClass( 'REST' ){}

sub protprotint_GET{
  my( $this, $c ) = @_;
  $c->log->debug( 'ProtProtInteraction:Inside the method function GET');
  if( defined $c->stash->{ errorMsg } ){
    $c->forward( 'spit_error' );
  }else{
    $c->forward( 'get_data' );
  }
  
}

sub protprotint_GET{
  my( $this, $c ) = @_;
  $c->log->debug( 'ProtProtInteraction:Inside the method function POST');
  
  if( defined $c->stash->{ errorMsg } ){
    $c->forward( 'spit_error' );
  }else{
    $c->forward( 'get_data' );
  }
  
}

#---------------------------------------------------------------------------------------------------------

=head2 Private Methods

=cut

sub get_data : Private {
  my( $this, $c ) = @_;
  
  $c->stash->{ template } = 'pages/layout.tt';
  
  # get the storables from the db;
  $c->forward( 'get_storables' );
  
  # now get the residues interacting for the input proteins;
  $c->forward( 'get_interacting_residues' );
  
}


#---------------------------------------------------------------------------------------------------------

sub get_interacting_residues : Private {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'ProtProtInteraction:get_interacting_residues');
  
  my $from = $c->stash->{ rest }->{ from }->{ accession};
  my $to   = $c->stash->{ rest }->{ to }->{ accession};
  
  my $bond_types = {
    'vanderwaals'   => 'V',
    'h-bond_back'   => 'B', 
    'h-bond_side'   => 'S', 
    'electrostatic' => 'E', 
    'disulphide'    => 'D',
    'covalent'      => 'C'
  } ;
  
  # now get the residual interaction between the proteins;
  my @residues = $c->model( 'iPfamDB::PpiRes')
             ->search(
                { 'me.protein_acc_a' => $from,
                  'me.protein_acc_b' => $to
                },
                { prefetch  =>  qw/ppi/ }
             );
  $c->log->debug( 'The total values present is '.scalar( @residues) );
  my ( $from_int, $to_int, $from_bond, $to_bond );
  
  foreach my $residue ( @residues ){
    my $bond  = $residue->get_column( 'bond' );
    my $res_a = $residue->get_column( 'residue_a');
    my $res_b = $residue->get_column( 'residue_b');
    
    push( @{ $from_int->{ $res_a } }, $res_b );
    push( @{ $to_int->{ $res_b } }, $res_a );
    push( @{ $from_bond->{ $res_a } }, $bond_types->{ $bond } );
    push( @{ $to_bond->{ $res_b } }, $bond_types->{ $bond } );
  }
  
  
  # now stash the residues
  $c->stash->{ graphics }->{ from }->{ interactions } =  to_json( $from_int );
  $c->stash->{ graphics }->{ to }->{ interactions } =  to_json( $to_int );
  $c->stash->{ graphics }->{ from }->{ bonds } =  to_json( $from_bond );
  $c->stash->{ graphics }->{ to }->{ bonds } =  to_json( $to_bond );
  
  $c->log->debug( 'the inteactions are '.dump( $c->stash->{ graphics }->{ from }->{ interactions } ) );
  
}

#---------------------------------------------------------------------------------------------------------

=head2 get_storables : Private 

Retrieves the domains present on that sequence and add markup to it.

=cut

sub get_storables : Private {
  my( $this, $c ) = @_;
  $c->log->debug( 'ProtProtInteraction::get_graphics: inside the subroutine');
  
  my $rs = $c->model( 'iPfamDB::ProteinAnnseq')->
               search( { 'accession' => 'A7S9L8' });
               
  my $annotation = $rs->first;
  
  unless( defined $annotation ){
    #$c->stash->{ errorMsg } ||= 'No Protein information present for the input entry';
    return;   
  }
  
  my $storable = thaw $annotation->get_column( 'annseq_storable' );
  $c->log->debug( "ProtProtInteraction::get_graphics:the storable value is ".$storable );
  
  if ( defined $storable ) {
    $c->stash->{seqs} = [ $storable ];

    my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
    $lm->layoutSequences( $c->stash->{seqs} );
    
    #$c->log->debug( "ProtProtInteraction::get_graphics:the layout value is ".dump( $c->stash->{ seqs } ) );
    
    # configure the JSON object to correctly stringify the layout manager output
    my $json = new JSON;
    # $json->pretty(1);
    $json->allow_blessed;
    $json->convert_blessed;

    # encode and stash the sequences as a JSON string
    $c->stash->{ graphics }->{ from }->{ layout } = $json->encode( $c->stash->{seqs} );
  }
}

#---------------------------------------------------------------------------------------------------------


sub spit_error : Private {
  my( $this, $c ) = @_; 
  
  # if we find error msg here, change the status of the request and return;s
  if( defined $c->stash->{ errorMsg } ){
    $c->log->debug( 'ProtProtInteraction:begin: the errormsg is'.$c->stash->{ errorMsg } );
    
    $c->stash->{ template } = 'components/blocks/protprotint/error.tt';
    $this->status_bad_request( 
      $c,
      message =>  $c->stash->{ errorMsg }
    );
    return;
  }

}

#---------------------------------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;