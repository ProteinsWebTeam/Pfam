# Ligand.pm
# pg6 250708
#
# $Id: Ligand.pm,v 1.5 2010-01-04 13:16:07 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Ligand - controller to build the main iPfam ligand page

=cut

package iPfamWeb::Controller::Ligand;

=head1 Description

This is intented to be the base call for ligand requests across the site. The method
begin will fetch the ligand id from the URI and load the model.

=cut

use strict;
use warnings;
use Data::Dump qw( dump );


use base 'iPfamWeb::Controller::REST';

#use base 'iPfamWeb::Controller::Section';

# set the name of the section

#__PACKAGE__->config( SECTION  =>  'ligand' ); 

#----------------------------------------------------------------------------------------

=head1 Methods

=head2 begin:: Private

This method get the ligand accession from the URI and expects one of the three parameters:

=over

=item id or entry /

a valid ligand id

=back

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;
  
  #stash the template and pagetype in the begining;
  $c->stash->{ pageType } ||= 'ligand';
  
#  # decide what format to emit. The default is HTML, in which case
#  # we don't set a template here, but just let the "end" method on
#  # the Section controller take care of us
#  if ( defined $c->req->param('output') and
#       $c->req->param('output') eq 'xml' ) {
#    $c->stash->{output_xml} = 1;
#    $c->res->content_type('text/xml');    
#  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $c->req->query_keywords || # accept getacc-style params
                      $entry_arg              ||
                      '';
  
  # although these next checks might fail and end up putting an error message
  # into the stash, we don't "return", because we might want to process the 
  # error message using a template that returns XML rather than simply HTML
  
  my $entry;
  
  if ( $tainted_entry ) {
    $c->log->debug( "Ligand::begin: we DID get a tainted_entry: |$tainted_entry|" )
      if $c->debug;
    ( $entry ) = $tainted_entry =~ m/^([\w\.-]+)$/;
    $c->stash->{errorMsg} = 'Invalid Ligand accession or ID' unless defined $entry;
    
    # now try to find contents for the input ligand entry,
    if( $entry ){
#      # stash the entry and use it later
#      $c->stash->{ rest }->{ entry } = $entry;
      
      my $rs = $c->model( 'iPfamDB::LigandChemistry')->search( 
                  [
                    { 'ligand_id' =>  $entry },
                    { 'three_letter_code' =>  $entry },
                    { 'lig_code'  =>  $entry},                  
                  ]
               );
  
      my $ligand = $rs->first if( defined $rs );
                         
      unless( defined $ligand ){
        $c->log->debug( "Ligand::get_ligand_data: No ligand data retrieved for input entry ")
          if $c->debug;
       
        #stash the error template in case if the request type was html.
        $c->stash->{ template } ||= 'components/blocks/ligand/error.tt';
        
        $c->stash->{errorMsg} ||= 'No Ligands found for input entry';
        
        return;
        
      }# end of unless ligand
      else{
        
        # stash the details of the ligand;s
        $c->stash->{ rest }->{ acc }    = $ligand->ligand_id;
        $c->stash->{ rest }->{ ligand } = {
          'three_letter_code' =>  $ligand->three_letter_code,
          'name'              =>  $ligand->name,
          'formula'           =>  $ligand->formula,
          'num_all_atoms'     =>  $ligand->num_all_atoms,
          'molecular_weight'  =>  $ligand->molecular_weight,
          'charge'            =>  $ligand->charge,
          'category'          =>  $ligand->category,
          'systematic_name'   =>  $ligand->systematic_name,
          'stereo_smiles'     =>  $ligand->stereo_smiles,
          'non_stereo_smiles' =>  $ligand->non_stereo_smiles
        };
        
      } # end of unless else loop of ligand
      
    }else{
      $c->stash->{errorMsg} = 'Invalid ligand three_letter_code or accession ';
    } # end of if else( $entry );
      
  }
  else {
    $c->log->debug( "Ligand::begin: we did not get a tainted entry" )
      if $c->debug;
  
    $c->stash->{errorMsg} = 'No Ligand accession or ID specified';
  }
  
  # if we found an errorMsg stash the template
  
  if( $c->stash->{ errorMsg } ){
    $c->log->debug( "Family::begin::Error occured in begin" );
    
    $this->status_bad_request(
      $c,
      message =>  $c->stash->{ errorMsg }
    );
    #stash the error template in case if the request type was html.
    $c->stash->{ template } ||= 'components/blocks/family/error.tt';
    return;
  }
  
  
} # end of begin

#----------------------------------------------------------------------------------------

sub ligand : Path : ActionClass( 'REST' ){}

sub ligand_GET {
  my( $this, $c ) = @_;
  $c->log->debug( "Ligand::GET: inside the method GET " );
  $c->log->debug( "Ligand::GET: forwarding to get_ligand_data " );
  $c->forward( 'get_ligand_data') unless( $c->stash->{ errorMsg } );
  
}

sub ligand_POST {
  my( $this, $c ) = @_;
  $c->log->debug( "Ligand::GET: inside the method POST " );
  $c->log->debug( "Ligand::Post: forwarding to get_ligand_data " );
  $c->forward( 'get_ligand_data') unless( $c->stash->{ errorMsg } );
}

#----------------------------------------------------------------------------------------

=head2 get_ligand_data

This action is responsible for fetching the information about the input ligand from the database.

=cut

sub get_ligand_data : Private {
  my ( $this, $c ) = @_;
  
  # after this, I am not worried about the returned data so setup the template.
  #stash the error template in case if the request type was html.
    $c->stash->{ template } = 'pages/layout.tt';
  #----------------------------------------
  
  # now look for the domains interacting with this ligand
  $c->forward( 'get_domain_interactions');
  
  $this->status_ok(
      $c,
      entity => $c->stash->{rest}
    );
 
  #----------------------------------------
   
}

#----------------------------------------------------------------------------------------
=head2 get_domain_interactions

This retrieves the domains interacting with the ligand

=cut 

sub get_domain_interactions : Private {
  my( $this, $c ) = @_;
  
  # now get the domain interactios
  my @rs = $c->model( 'iPfamDB::Pfama')->search (
    #{ 'three_letter_code' =>  $c->stash->{ acc } },
    { 'ligand_id.ligand_id' =>  $c->stash->{ rest }->{ acc } },
    {
      prefetch  =>  [
        { 'domains' => { 'dlis' => { 'internal_ligand_id' =>  'ligand_id' } } }   
      ],
      'group_by'  =>  [ qw/domains.pfam_acc pfama_id/ ]
    }
  );
  
  # stash the total number of interactions in the summary data as it is used...
  $c->stash->{ rest }->{ summaryData }->{ domInt } = scalar( @rs ) if( @rs );
  $c->log->debug( "Ligand:get_domain_interactions: the total domains interacting with this ligands is ".scalar( @rs ) );
  my @interaction; 
  
  foreach my $r ( @rs ){
    push @interaction, {
                          'pfamA_id'  =>  $r->pfama_id,
                          'pfamA_acc' =>  $r->pfama_acc,
                       };
    $c->log->debug( "Ligand::get_domain_interactions: the pfama_id is ".$r->pfama_id." and the acc is ".$r->pfama_id );                       
  }
  $c->stash->{ rest }->{ 'DomainInteractions' } = \@interaction;
  
  $c->log->debug( "Ligand::get_interactions:DUMP IS ".dump( \@interaction )."\n\n" );
}
#----------------------------------------------------------------------------------------

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













