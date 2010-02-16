
# DasViewer::Controller::Feature.pm
#
# pg6 20100215
# 
# $Id: Feature.pm,v 1.1 2010-02-16 09:47:08 pg6 Exp $
# 
# Controller to get feature requests for the  accession;

package DasViewer::Controller::Feature;

use strict;
use warnings;

use Data::Dump qw( dump );
use JSON;

use parent 'Catalyst::Controller';


sub feature : Path( '/feature' ){
  my ( $self, $c ) = @_;
  
  $c->stash->{ template } = 'components/modelFeature.tt';
}


sub getFeature : Local {
  my( $self, $c ) =@_;
  
  #$c->log->debug( 'Feature:getFeature:the dump of the bject is '.dump( $self ) );
  
  # parse the arguments;
  my $acc     = $c->req->param( 'acc' );
  my @sources = $c->req->param( 'Das' );
  
  $c->log->debug("Feature::getFeature::the dump of the sources is ".dump( \@sources ) );
  
  # now stash the accession and the sources;
  $c->stash->{ acc }      = $acc;
  $c->stash->{ sources }  = \@sources;
  
  $c->stash->{ json_sources } = to_json( \@sources );
  
  # as we need to wire this into pfam code, develop it as a self-dependent code,
  # get the lite object;
  $c->forward( 'getDasLite' );
  
  # get the features of the protein;
  $c->forward( 'getDasFeatures' );
  
  $c->stash->{ template } = 'components/dasFeature.tt';
  
}

#---------------------------------------------------------------------------------------------

=head2 getDasFeatures : Private

method which returns the features for the das sources;

=cut

sub getDasFeatures : Private {
  my ( $self, $c ) = @_;
  
  my $dasFeatures;
  
  # get the feature for all the clicked sources and generate the seq object for giving it to the domain graphics;
  my $lite = $self->{ daslite };
  foreach my $ds_id ( @{ $c->stash->{ sources } } ){
    
    $lite->dsn( $self->{ $ds_id }->{ url } );
    my $featureResponse = $lite->features( $c->stash->{ acc } );
    #$c->log->debug( 'Feature::getDasFeatures:dump fo the features for $ds_id is '.dump( $features ) );
    
    my ( $url, $features ) = each( %{ $featureResponse } );
 
    # build the sequence object;
    my $seqObject = $c->forward( 'buildSequence', [ $features ]);
    
    $dasFeatures->{ $ds_id } = $seqObject;
    
  } # end of foreach
  
  $c->stash->{ dasFeatures } = to_json( $dasFeatures );
  #$c->log->debug( 'Features:getDasFeatures: the dump of the result is '.dump( $c->stash->{ dasFeatures } ) );
  
}

#---------------------------------------------------------------------------------------------
=head2 buildSequence : Private

method to build sequence object so that its used to draw the graphics using domain graohics library;

=cut

sub buildSequence : Private {
  my ( $self, $c, $features ) = @_;
  
  #$c->log->debug( "te dump fo the features is".dump( $features ) );
  
  my $seqObj = {}; 
  
  foreach my $feature ( @{ $features } ){
    
    # get the length of the sequence;
    if( exists $feature->{ segment_stop } ){
      $seqObj->{ length } = $feature->{ segment_stop } ;
    }else{
      $seqObj->{ length } = 700;
    }
    
    
    # now get the feature start and end coordinates;
    if( exists $feature->{ start } && exists $feature->{ end } ){
      
      #$c->log->debug( "Feature:buildSequence: the start and end coordinates are defined for ".$feature->{ feature_id } );
      push @{ $seqObj->{ motifs } },{
        start     =>      $feature->{ start },
        end       =>      $feature->{ end },
        colour    =>     "#ff8800",
        metadata  =>{
          database  =>  "Phobius",
          type      => "sig_p",
          start     =>  $feature->{ start },
          end       =>  $feature->{ end },
        },
      };
      
    } # end of feature start and end;
      
  }# end of foreach $feature;
  $c->log->debug("Feature::buildSequence: seqObj is ".dump( $seqObj ) );
  #return to_json( $seqObj );
  return $seqObj;
}

#---------------------------------------------------------------------------------------------
=head2 getDasLite : Private 

Method which returns the  Bio::Das::Lite object, for features retrieval;

=cut

sub getDasLite : Private {
  my( $self, $c )  = @_;
  
  my ( $proxy )      = $self->{proxy} || '' =~ /^([\w\:\/\.\-\?\#]+)$/;
  
  $self->{ daslite } = Bio::Das::Lite->new( {
    timeout     =>  $self->{ timeout },
  });
  
  $self->{ daslite }->{ http_proxy } = $proxy if( defined $proxy );
  
}

#---------------------------------------------------------------------------------------------

1;