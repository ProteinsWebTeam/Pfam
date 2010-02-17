
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

#---------------------------------------------------------------------------------------------

sub feature : Path('/feature') {
  my ( $self, $c ) = @_;

  $c->stash->{template} = 'components/modelFeature.tt';
}

#---------------------------------------------------------------------------------------------

sub getFeature : Local {
  my ( $self, $c ) = @_;

  #$c->log->debug( 'Feature:getFeature:the dump of the bject is '.dump( $self ) );

  # parse the arguments;
  my $acc     = $c->req->param('acc');
  my @sources = $c->req->param('Das');

  $c->log->debug(
    "Feature::getFeature::the dump of the sources is " . dump( \@sources ) );

  # now stash the accession and the sources;
  $c->stash->{acc}     = $acc;
  $c->stash->{sources} = \@sources;
  
  # stash the names of the sources;
  my @source_names;
  foreach( @sources ){
    push @source_names, $self->{ $_ }->{ name };
  }
  #$c->stash->{json_sources} = to_json( \@source_names );
  $c->stash->{json_sources} = to_json( \@source_names );
  
  # as we need to wire this into pfam code, develop it as a self-dependent code,
  # get the lite object;
  $c->forward('getDasLite');

  # get the features of the protein;
  $c->forward('getDasFeatures');

  $c->stash->{template} = 'components/dasFeature.tt';

}

#---------------------------------------------------------------------------------------------

=head2 getDasFeatures : Private

method which returns the features for the das sources;

=cut

sub getDasFeatures : Private {
  my ( $self, $c ) = @_;

  my $dasFeatures;
  
  my $totalRows = 0;
# get the feature for all the clicked sources and generate the seq object for giving it to the domain graphics;
  my $lite = $self->{daslite};
  foreach my $ds_id ( @{ $c->stash->{sources} } ) {

    $lite->dsn( $self->{$ds_id}->{url} );
    my $featureResponse = $lite->features( $c->stash->{acc} );
    my ( $url, $features ) = each( %{$featureResponse} );

    # build the sequence object;
    my $resolvedFeature = $c->forward( 'resolveOverlaps', [$features] );
    
    # increment the total rows with the new one;
    $totalRows += scalar( @{ $resolvedFeature } );
    
    $dasFeatures->{ $ds_id } = $resolvedFeature;
    
  }    # end of foreach
  
  $c->log->debug( 'The featuresets is '.dump( $dasFeatures ) );
  # now stash the totalRows for defining the height of hte canvas;
  $c->stash->{ dasTracks }  = $totalRows;
  $c->log->debug( "the toatl da tracks to be drawn are ". $c->stash->{ dasTracks } );
  
  # now build the sequence object for giving it to domain graphics code;
  my $seqObject = $c->forward( 'buildSequence', [ $dasFeatures ] );
  
  $c->stash->{dasFeatures} = to_json( $seqObject );

  $c->log->debug( 'Features:getDasFeatures: the dump of the result is '.dump( $c->stash->{ dasFeatures } ) );

}

#---------------------------------------------------------------------------------------------

=head2 buildSequence : Private

method to build sequence object so that its used to draw the graphics using domain graohics library;

=cut

sub buildSequence : Private {
  my ( $self, $c, $dasFeatures ) = @_;
  
  my $length;
  
  my $seqObject = {};
  
  my $length;
  
  # this is at top level getting all the features which are placed as separate values of an array
  # to avaoid overlpas;
  
  foreach my $ds_id ( keys %{ $dasFeatures } ){
    
    $c->log->debug( "the total featureSets present for $ds_id is ".scalar( @{ $dasFeatures->{ $ds_id } }) );
    
    # add the ds_id as a key;
    #$seqObject->{ $ds_id } = [];
    
    # getting one level deeper for specific row;
    for( my $i = 0; $i < scalar( @{ $dasFeatures->{ $ds_id } } ); $i++ ){
      
      my $featureRow = $dasFeatures->{ $ds_id }->[ $i ];
      
      my $rowSeqObj = {};
      
      # all the features for that specific row;
FEAT:      for( my $j = 0; $j < scalar( @{ $featureRow } ); $j++ ){
        
        my $feature = $featureRow->[ $j ];
        
        unless( defined $length ){
          
          if( exists $feature->{ segment_stop } ){
            $length =  $feature->{ segment_stop };
          }
            
        } # end of unless $length;  
        
        $rowSeqObj->{ length }   = $length;
        $rowSeqObj->{ tips }     = "true";
        $rowSeqObj->{ imageMap } = "true";
        $rowSeqObj->{ lables }   = "true";
        
        # if the feature has got start and end, then build the sequence;
        if( exists $feature->{start} && exists $feature->{end}  ){
          
          if( $feature->{ start } == 0 && $feature->{ end } == 0 ){
            next FEAT;  
          }
          
          push @{ $rowSeqObj->{motifs} },
          {
            start    => $feature->{start},
            end      => $feature->{end},
            colour   => $self->{ $ds_id }->{ color },
            metadata => {
              database => $self->{ $ds_id }->{ name },
              type     => $feature->{ feature_id },
              start    => $feature->{start},
              end      => $feature->{end},
              display  => "true"
            },
          };
          
        } # exists feature->start;  
          
      } # end of $j
      
      push @{ $seqObject->{ $self->{ $ds_id }->{ name } } }, $rowSeqObj ;
        
    } # for $i,$dasFeatures->{ $ds_id }; 
    
  } # end of foreach $dasFeatures;
  
  #stash the length of th sequence so that can be used for defining canvas;
  $c->stash->{ seqLength } = $length;
  
  return $seqObject;
}

#---------------------------------------------------------------------------------------------

=head2 resolveOverlaps : Private 

method which resolves overlaping regions between the features and creates a new set so that multiple tracks could be drawn;

=cut

sub resolveOverlaps : Private {
  my( $self, $c, $features ) = @_;
  
  my $featureSets = [];   # store the features as sets;
  my $length;             # store the length of the segment provided;
  
  unless ( scalar( @{ $features } ) > 0 ){
    $c->log->debug( 'resolveOverlaps:no features so returning' );
    return;
  }
  
  FEATURE: for( my $i = 0; $i < scalar( @{ $features } ) ; $i++ ){
    
    my $assignedToSet = 0;
    
    SET:  for( my $j = 0; $j < scalar( @$featureSets ) ; $j++ ){
      my $overlapWithSet = 0;
      
      foreach my $feature ( @{ $featureSets->[$j] } ) {
        
        #Need to take into account that markups may have no end...
        if ( ( $features->[$i]->{'start'} <= $feature->{'end'}
            && $features->[$i]->{'start'} >= $feature->{'start'}
          )
          || ( $features->[$i]->{'end'} <= $feature->{'end'}
            && $features->[$i]->{'end'} >= $feature->{'start'} )
          || ( $features->[$i]->{'start'} <= $feature->{'start'}
            && $features->[$i]->{'end'} >= $feature->{'end'} )
          )
        {
          $overlapWithSet = 1;
          last;
        }
        
      } # end of foreach my $feature @{ $featuerSets->[ $j ] }
      
      if ( !$overlapWithSet ) {
        push( @{ $featureSets->[$j] }, $features->[$i] );

        $assignedToSet = 1;
        next FEATURE;
      }
          
    } # end of scalar( @$featureSets );
    
    if ( !$assignedToSet ) {
      $featureSets->[ scalar(@$featureSets) ] = [ $features->[$i] ];
      next FEATURE;
    }
      
  } # end of FEATURE;
  return $featureSets;  
}

#---------------------------------------------------------------------------------------------

=head2 getDasLite : Private 

Method which returns the  Bio::Das::Lite object, for features retrieval;

=cut

sub getDasLite : Private {
  my ( $self, $c ) = @_;

  my ($proxy) = $self->{proxy} || '' =~ /^([\w\:\/\.\-\?\#]+)$/;

  $self->{daslite} = Bio::Das::Lite->new( { timeout => $self->{timeout}, } );

  $self->{daslite}->{http_proxy} = $proxy if ( defined $proxy );

}

#---------------------------------------------------------------------------------------------

1;
