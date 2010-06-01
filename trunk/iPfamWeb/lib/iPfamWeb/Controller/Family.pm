
# Family.pm
# jt6 20060411 WTSI
#
# $Id: Family.pm,v 1.11 2010-01-06 10:52:47 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Family - controller to build the main Pfam family
page

=cut

package iPfamWeb::Controller::Family;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Pfam
families across the site. The L<begin|/"begin : Private"> method tries
to extract a Pfam ID or accession from the captured URL and tries to
load a Pfam object from the model.

Generates a B<tabbed page>.

$Id: Family.pm,v 1.11 2010-01-06 10:52:47 pg6 Exp $

=cut

use strict;
use warnings;
use Data::Dump qw( dump );
use JSON;

use base 'Catalyst::Controller::REST';

sub begin : Private{
  
  my( $this, $c,$entry_arg ) = @_;
  $c->log->debug( "Family::begin: inside the begin action " );
  
  #stash the template and pagetype in the begining;
  $c->stash->{ pageType } ||= 'family';
  #$c->stash->{ template } ||= 'pages/layout.tt';
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $c->req->query_keywords || # accept getacc-style params
                      $entry_arg              ||
                      '';
  
  # detaint the input entry and make sure it passes the validation;
  # if it fails, stash the error message and continue to avoid repetition of code.

  my $entry;
  if ( $tainted_entry ) {
    $c->log->debug( "Family::begin: we DID get a tainted_entry: |$tainted_entry|" )
      if $c->debug;
    ( $entry ) = $tainted_entry =~ m/^([\w\.-]+)$/;
     
    if( defined $entry ){
      
      # now check for the Pfam family;
      my $rs = $c->model('iPfamDB::Pfama')
                 ->search( [ { pfama_acc => $entry },
                             { pfama_id  => $entry } ] );
      
      my $pfam = $rs->first if defined $rs;
      
      # if we dont have any pfam family then stash the error message;
      unless ( $pfam ) {
        
        $c->log->debug( 'Family::begin: failed to retrieve family data' )
          if $c->debug;
        
        #stash the error template in case if the request type was html.
        $c->stash->{ template } ||= 'components/blocks/family/error.tt';
        
        $c->stash->{ errorMsg } ||= 'No valid Pfam family accession or ID';
        
        return;
        
      } # end of unless $pfam
      else{
        # now we have got the pfam family, stash it for further use;
        $c->stash->{ pfam } = $pfam;
        
        # now stash the data for the entry under rest for serializing;
        $c->stash->{ rest }->{ pfam } = {
          'pfama_id'    =>  $pfam->pfama_id,
          'pfama_acc'   =>  $pfam->pfama_acc,
          'description' =>  $pfam->description,
          'interpro_id' =>  $pfam->interpro_id,
          'interpro_abstract' =>  $pfam->interpro_abstract  
        };
        
      } # end of unless else;
      
    }else{
      $c->stash->{errorMsg} = 'Invalid Pfam family accession or ID';
    }
    
  } # end of if tainted entry;
  else {
    $c->log->debug( "Family::begin: we did not get a tainted entry" )
      if $c->debug;
  
    $c->stash->{errorMsg} = 'No Pfam family accession or ID specified';
  }
  
  # now change the status of the request and return;
  
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
  
}

sub family : Path : ActionClass( 'REST' ){}

sub family_GET {
  my( $this, $c ) = @_;
  $c->log->debug( "Family::GET: inside the method GET " );
  $c->log->debug( "Family::GET: forwarding to get_data " );
  $c->forward( 'get_data') unless( $c->stash->{ errorMsg } );
  
}

sub family_POST {
  my( $this, $c ) = @_;
  $c->log->debug( "Family::POST: inside the method POST " );
  $c->log->debug( "Family::POST: forwarding to get_data " );
  $c->forward( 'get_data') unless( $c->stash->{ errorMsg } );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves family data for the given entry. Accepts the entry ID or accession
as the first argument. Does not return any value but drops the L<ResultSet>
for the relevant row into the stash.

=cut

sub get_data : Private {
  my ( $this, $c) = @_;
  
  # after this, I am not worried about the returned data so setup the template.
  #stash the error template in case if the request type was html.
    $c->stash->{ template } = 'pages/layout.tt';
  #----------------------------------------
  
  # Always look for the go terms, if the family has any then get it using th efollowing method;
  $c->forward( 'get_go_data' );
  
   $this->status_ok(
      $c,
      entity => $c->stash->{rest}
    );
  # if this request originates at the top level of the object hierarchy,
  # i.e. if it's a call on the "default" method of the Family object,
  # then we'll need to do a few extra things, otherwise, we're done
  return unless ref $this eq 'iPfamWeb::Controller::Family';
  
  #----------------------------------------
  
  $c->log->debug( 'Family::get_data: adding extra family info' ) if $c->debug;
  
  # get all the interactors corresponding to this family;
  $c->forward( 'get_domain_interactions' );
  
  $c->forward( 'get_summary_data' );
}

#-------------------------------------------------------------------------------

=head2 get_domain_interactions : Private

Retrieves all the domains interacting with this family.

=cut

sub get_domain_interactions : Private {
  my ( $this, $c ) = @_;
  
  # get all the interactors name by combining ddi table with domain table and join against
  # pfama table to get the name of the pfama_id ;
  $c->log->debug( "Family:get_interactions : inside the action get_interaction");

  my @interactors = $c->model( 'iPfamDB::Ddi' )->search( 
    { 'region_id_a.pfam_acc'  =>  $c->stash->{ rest }->{ pfam }->{ pfama_acc } },
    {
      prefetch  =>  [
        'region_id_a',
        { 'region_id_b' =>  'pfam_acc' },
      ],
      '+select'   => [{ count => 'pfam_acc.pfama_id' } ],
      '+as'    => [ qw/totalSeq/ ], 
      'group_by'  => [qw/ pfam_acc.pfama_id/],      
    }
  );
  
  my @interaction;
  foreach my $interactor ( @interactors ){
    my $pfama_id  =   $interactor->region_id_b->pfam_acc->get_column( 'pfama_id' );
    my $pfama_acc =   $interactor->region_id_b->pfam_acc->get_column( 'pfama_acc');
    push @interaction, {
                          'pfamA_id'  =>  $pfama_id,
                          'pfamA_acc' =>  $pfama_acc,
                          'totalSeq'  =>  $interactor->get_column( 'totalSeq' ),
                       };
    #$c->log->debug(  $interactor->region_id_b->pfam_acc->get_column( 'pfama_id' )."|".$interactor->region_id_b->pfam_acc->get_column( 'description' )."\n" );
  }
  
  $c->stash->{ rest }->{ 'DomainInteractions' } = \@interaction;
  
  #$c->log->debug( "Family:get_interactions:DUMP IS ".dump( \@interaction )."\n\n" );
  
}
#-------------------------------------------------------------------------------

=head2 get_go_data : Private

Retrieves the GO terms for the family.

=cut

sub get_go_data : Private {
  my ( $this, $c ) = @_;
  
  my @go_data = $c->model( 'iPfamDB::GeneOntology' )->search(
    { pfama_acc => $c->stash->{ rest }->{ pfam }->{ pfama_acc } },    
  );
 
  my @goTerms;
  foreach my $go ( @go_data ){
    push @goTerms,{
      'go_id'     =>  $go->go_id,
      'term'      =>  $go->term,
      'category'  =>  $go->category  
    };
  } 
  $c->log->debug( "Family::get_go_data:dump of the goterms is".dump( \@goTerms ) );
  $c->stash->{ rest }->{ goTerms } = \@goTerms;
   
}

#-------------------------------------------------------------------------------

=head2 getSummaryData : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the PfamA object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub get_summary_data : Private {
  my( $this, $c ) = @_;

  my $summaryData = {};

  $summaryData->{domInt} = $c->stash->{ pfam }->domcount;  
  $summaryData->{ligInt} = $c->stash->{ pfam }->ligcount;
  $summaryData->{naInt}  = $c->stash->{ pfam }->nacount;
  
  $c->stash->{ rest }->{summaryData} = $summaryData;
  
  # call get_ligand_interactions method if the total ligCount is > 0;
  if( $summaryData->{ ligInt } > 0 ){
    $c->log->debug( "Family::get_summary_data: the total ligCount is ".$summaryData->{ ligInt } );
    $c->forward( "get_ligand_interactions" );
  }
  
}

#-------------------------------------------------------------------------------

=head2 get_ligand_interactions : Private

Retrieves the ligands interacting with this family

=cut

sub get_ligand_interactions : Private {
  
  my ( $this, $c ) = @_;
  
  my @ligands = $c->model( 'iPfamDB::Domain' )->search( 
    { 'pfam_acc' =>  $c->stash->{ rest }->{ pfam }->{ pfama_acc } },
    { prefetch  =>[
        { 'dlis'  =>  
          { 'internal_ligand_id'  => "ligand_id" } }
      ],
      'group_by'  =>  [ qw/pfam_acc ligand_id.name/ ]
    }
  );
  
  my @ligInteractions;
  # for testing i am creating a hash with three letter code as key and ligand name as value;
  my %ligdata;
  
  $c->log->debug( "Family::get_ligands_interactions: fetching the ligand details from resultset");
  foreach my $ligand( @ligands ){
    
    my $dlis = $ligand->dlis;
    while( my $lig = $dlis->next ){
 
      my $ligand_id   =   $lig->internal_ligand_id->ligand_id->get_column( "ligand_id" );
      my $name        =   $lig->internal_ligand_id->ligand_id->get_column( "name" );
      my $code        =   $lig->internal_ligand_id->ligand_id->get_column( "three_letter_code" );
      my $formula     =   $lig->internal_ligand_id->ligand_id->get_column( "formula" );
      $c->log->debug("Family:get_ligands_interactions:the ligand_id is|$ligand_id|$name|");
      
      # the ligand name contains the character "'" which cause problems when converted to JSOn object, so escape it.
      
      my $esc_name = $name;
      if( $name =~ /'/ ){
        $esc_name  =~ s/'/\@/g;
        $c->log->debug( "Family:get_ligands_interaction:prime character escaped:|$name|$esc_name|\n\n" );
      };
      
      push @ligInteractions,{
        'ligand_id'           =>  $name,
        'three_letter_code'   =>  $code,
        'formula'             =>  $formula,
      };
      $ligdata{ $code } = $esc_name;
    }
      
  }# end of foreach
  
  #$c->stash->{ ligInteractions } = to_json( \@ligInteractions );
  $c->stash->{ ligInteractions } = \@ligInteractions ;
  
  $c->stash->{ rest }->{ ligdata } = to_json( \%ligdata );
  
#  $c->log->debug( "Family::get_ligands_interaction::trying to dump the json object:".dump($c->stash->{ ligInteractions } ));
#  my $from = from_json( $c->stash->{ ligInteractions } );
#  $c->log->debug( "Family::get_ligands_interaction::trying to dump the converted object:".dump($from ) );
  
}

#-------------------------------------------------------------------------------

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
