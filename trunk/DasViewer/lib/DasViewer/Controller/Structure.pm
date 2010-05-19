
# DasViewer::Controller::Structure;
#
# Controller to get the pdb id's which are similar to the uniprot sequences;
#
# $Id$

package DasViewer::Controller::Structure;

use strict;
use warnings;

use parent 'Catalyst::Controller';


use LWP::Simple;
use Data::Dump qw( dump );
use JSON;

#ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ

=head2 getStructure : Local 

Method which gets the Structures which are similar to the uniprot accession

=cut

sub getStructure : Local {
  
  my( $self, $c ) = @_;
  
  # get the accession from the request;
  $c->stash->{ acc } = $c->req->param( 'acc' );
  
  #Ênow get the Bio::Das::Lite object
  $c->forward( 'getDasLite' );
  
  # now get the mappings between the Uniprot and pdb;
  my $lite = $self->{ daslite };
  
  #Ênow set the dsn to the mapping url;
  $lite->dsn( $self->{ mappingUrl } );
  
  #Ênow retrieve the alignmetns and stash it;
  $c->stash->{ rawAlignments } = $lite->alignment({
                                                    query  => $c->stash->{ acc } 
                                                  } );
  
  
  # now process the alignments to get the PDB accessions and coordinates;
  $c->forward( 'processAlignment' );
  
  $c->log->debug( 'Structure::getStructure: the strucuter are '.$c->stash->{ structures } );
  
  my $structures;
  
  if( defined $c->stash->{ errorMSG } ){
    $structures = "'".$c->stash->{ errorMSG }."'";
  }else{
    $structures = $c->stash->{ structures };  
  }
  
  my $string = <<EOF;
  
  var structure = $structures;
  
EOF

  $c->res->content_type( 'text/javascript');
  $c->res->body( $string );
  
} # end of getStructure;


#ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ

=head2 processAlignment : Private

Method which process the alignment and returns the pdb structures similar to it.

=cut

sub processAlignment : Private {
  my( $self, $c ) = @_;
  
  my $structures = [];
  
  # now walk through the alignments response and parse it;
  foreach( values %{ $c->stash->{ rawAlignments} } ){
    
    unless ( defined $_ ){
      $c->log->debug('I dont have alignments' ); 
      $c->stash->{ errorMSG }  ||= 'No structures found for '.$c->stash->{ acc };
      return;  
    }
    
    foreach my $alignment( @{ $_ } ){
      #print dump( $alignment->{ block }->[ 0 ]->{ segment } );
      my $segments = {};
      push @{ $structures }, $segments;
#      foreach my $segment ( @{ $alignment->{ block }->[ 0 ]->{ segment } } ){
#        
#        # for time being, skip if the intObjectId is equal to input acc;
#        next if( $segment->{ segment_intObjectId } eq $c->stash->{ acc } );
#        
#        push @{ $structures },{ acc   =>  $segment->{ segment_intObjectId }, 
#                                start =>  $segment->{ segment_start },
#                                end   =>  $segment->{ segment_end },
#                              };
#        
#      } # end of the segmetns
      foreach my $segment ( @{ $alignment->{ block }->[ 0 ]->{ segment } } ){
        
        # for time being, skip if the intObjectId is equal to input acc;
        if( $segment->{ segment_intObjectId } eq $c->stash->{ acc } ){
          $segments->{ pacc } = $segment->{ segment_intObjectId }, 
          $segments->{ pstart } =  $segment->{ segment_start },
          $segments->{ pend }  =  $segment->{ segment_end }
        }else{
          $segments->{ acc } = $segment->{ segment_intObjectId }, 
          $segments->{ start } =  $segment->{ segment_start },
          $segments->{ end }  =  $segment->{ segment_end },
        }
        
      } # end of the segmetns
          
    } #Êend of alignemtns
  } # end of values
  
  $c->stash->{ structures } = to_json( $structures );
}

#ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ

=head2 get_pdb

This action retrieves the pdb from FTP site of MSD

=cut

sub getpdb: Local {
  my( $self, $c ) = @_;
  
  unless( defined $c->req->param( 'id') ){
    $c->log->debug( "Structure:GetPDB::getPDB: we did some mistake as three_letter_code of the Structure is not found." );
    #$c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
    return;
  }
  
  my ( $pdb_id ) = $c->req->param( 'id' ) =~ /(\w+)/;
  $c->log->debug( "Structure:GetPDB::getPdb: The input Structure pdb id is $pdb_id" );
  #$c->log->debug( "Structure:GetPDB::getPdb: The dump of the object is ".dump( $self ) );
  
  unless( defined $pdb_id ){
    $c->log->debug( "Structure:GetPDB::getPDB: input id contains invalid characters");
    $c->stash->{ errorMsg } ||= 'Input Structure PDB id contains invalid characters';
    return;
  }
  
  my$uri = $self->{pdb_uri};
  
  unless( defined $uri ){
    $c->log->debug( "Structure:GetPDB::getPDB: Structure uri could not be found in config");
    $c->stash->{ errorMsg } ||= 'Please add the Structure uri to the config file';
    return;
  }
  
  $uri .='pdb'.$pdb_id .'.ent';
  
  $c->stash->{ ligPDB } = get( $uri );
  
  if( defined $c->stash->{ ligPDB } ){
    $c->res->body( $c->stash->{ ligPDB } );
  } else{
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
  }
  
}

#ÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐÐ

=head2 getDasLite : Private 

Method which returns the  Bio::Das::Lite object;

=cut

sub getDasLite : Private {
  my ( $self, $c ) = @_;

  my ($proxy) = $self->{proxy} || '' =~ /^([\w\:\/\.\-\?\#]+)$/;

  $self->{daslite} = Bio::Das::Lite->new( { timeout => $self->{timeout}, } );

  $self->{daslite}->{http_proxy} = $proxy if ( defined $proxy );

}

#---------------------------------------------------------------------------------------------

1;