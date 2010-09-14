
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

=head2 getFeatureViewer : Path 

=cut

sub getFeatureViewer : Path( '/javascripts/FeatureViewer.js'){
  my( $self, $c ) = @_;
  
  my $jsFile = $c->view( 'TT')->render( $c, 'components/FeatureViewer.tt' );
  
  $c->res->content_type( 'text/javascript');
  $c->res->body( $jsFile );
}

#---------------------------------------------------------------------------------------------

sub getFeature : Local {
  my ( $self, $c ) = @_;

  # parse the arguments;
  unless ( defined $c->req->param('acc') and
           $c->req->param('acc') =~ m/^[A-Za-z0-9]+$/ ) {
    $c->res->status(400); # Bad Request
    $c->res->body( 'You did not specified a valid sequence accession.' );
    return;
  }
  my $acc = $c->req->param('acc');

  my @sources;
  foreach ( $c->req->param('sources') ) {
    unless ( m/^DS_\d+$/ ) {
      $c->res->status(400); # Bad Request
      $c->res->body( 'One or more or your source IDs was invalid' );
      return;
    }
    push @sources, $_;
  }
  $c->log->debug( 'Feature::getFeature::the dump of the sources is ' . dump( \@sources ) )
    if $c->debug;

  # now stash the accession and the sources;
  $c->stash->{response}->{acc}     = $acc;
  $c->stash->{response}->{sources} = \@sources;
  
  # stash the names of the sources;
  my @source_names;
  foreach( @sources ){
    push @source_names, $self->{ $_ }->{ name };
  }
  $c->stash->{ response}->{json_sources} = \@source_names;
  
  # as we need to wire this into pfam code, develop it as a self-dependent code,
  # get the lite object;
  $c->forward('getDasLite');
  
  # get the length of the sequence by making a request to the uniprot das source.
  # as part of das spec, all sources need to give start adn end coordinates for a segment,
  # however some sources, dont, hence we get the end coordiantes, by makign a das call.
  $c->forward( 'getSeqLength');

  # get the features of the protein;
  $c->forward('getDasFeatures');
  
  if( defined $c->stash->{ response}->{ errorSources } ){
    $c->stash->{ response}->{ json_error_sources }  = $c->stash->{ response}->{ errorSources };
  }else{
    $c->stash->{ response}->{ json_error_sources }  = {};
  }
  
  # check whether the totalError is equal to the total das sources requested,
  if( defined $c->stash->{ response}->{ totalError } ){
    if( $c->stash->{ response}->{ totalError } == scalar( @{ $c->stash->{ response}->{sources} } ) ){
      $c->stash->{ response}->{ errorMsg } = "Invalid accession or No features found for ".$c->req->param('acc')." for the selected Das sources.";
    } 
  }
  
  my $features = to_json( $c->stash->{ response } );
  my $string = <<EOF;
  
  var features = $features;
  
EOF

  $c->res->content_type( 'text/javascript');
  $c->res->body( $string );
}

#---------------------------------------------------------------------------------------------

=head2 getSeqLength : Private

method to get the length of the sequence by querying Uniprot das source.

=cut;

sub getSeqLength : Private {
  my ( $self, $c ) = @_;
  
  # set the dsn name ;
  $self->{daslite}->dsn( $self->{ uniprotURL } );

  my $seqResponse = $self->{daslite}->sequence( $c->stash->{response}->{acc} );
  $c->log->debug( 'the seqRespinse is '.dump( $seqResponse ) )
    if $c->debug;
  my ( $url, $sequence ) = each( %{ $seqResponse } );

  $c->stash->{ response }->{ seqLength } = $sequence->[0]->{ sequence_stop }; 
  $c->log->debug( 'the sequence Length retrieved is '.$c->stash->{ response }->{ seqLength } )
    if $c->debug;
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
  
SOURCE:  foreach my $ds_id ( @{ $c->stash->{ response}->{sources} } ) {
    
    my $featureResponse;
    
    # there might be cases where connecting to a specific source may fail;
    eval{
      $self->{daslite}->dsn( $self->{$ds_id}->{url} );
      $featureResponse = $self->{daslite}->features( $c->stash->{ response}->{acc} );  
    };
    
    if( $@ ){
      $c->log->debug( 'there is a problem connecting to this source '.$self->{$ds_id}->{url});
      $c->stash->{ response}->{ totalError }++;
      $c->stash->{ response}->{ errorSources }->{ $ds_id } = 1;
      next SOURCE;
    }
    
    my ( $url, $features ) = each( %{$featureResponse} );
    #if( $ds_id eq 'DS_634'){
    #  $c->log->debug( 'the dump of the efature reso=ponsefor BM is '.dump( $featureResponse ) );
    #} 
    unless( defined $features && ( ref $features eq 'ARRAY' ) && scalar( @{ $features } ) > 0 ){
      $c->log->debug( "Feature::getDasFeatures:: cant get features for the source ".$self->{$ds_id}->{name});
      $c->stash->{ response}->{ totalError }++;
      $c->stash->{ response}->{ errorSources }->{ $ds_id } = 1;
      next SOURCE;
    }
    # build the sequence object;
    my $resolvedFeature = $c->forward( 'resolveOverlaps', [$features] );
    # this is dodgy. There's no reason to wrap $features as an array ref, and doing so 
    # means that, in "resolveOverlaps", there's all kinds of extra cruft required. I'd
    # remove it but I don't want to mess with something that (nominally works).
    # jt6 20100913 WTSI
    
    # add another check, to measure the resolved feature count should be more than zero;
    if( scalar( @{ $resolvedFeature } ) == 0 ){
      
      $c->log->debug( "Feature::getDasFeatures:: got features for the source but they dont have start&end".scalar( @{ $resolvedFeature }) );
      $c->stash->{ response}->{ totalError }++;
      $c->stash->{ response}->{ errorSources }->{ $ds_id } = 1;
      next SOURCE;
      
    } 
    # increment the total rows with the new one;
    $totalRows += scalar( @{ $resolvedFeature } );
    
    $dasFeatures->{ $ds_id } = $resolvedFeature;
    
  }    # end of foreach
  
  # now stash the totalRows for defining the height of hte canvas;
  $c->stash->{ response}->{ dasTracks }  = $totalRows;
  $c->log->debug( "the toatl das tracks to be drawn are ". $c->stash->{ response}->{ dasTracks } );
  
  # now build the sequence object for giving it to domain graphics code;
  my $seqObject = $c->forward( 'buildSequence', [ $dasFeatures ] );
  
  $c->stash->{ response}->{dasFeatures} = $seqObject;

}

#---------------------------------------------------------------------------------------------

=head2 buildSequence : Private

method to build sequence object so that its used to draw the graphics using domain graohics library;

=cut

sub buildSequence : Private {
  my ( $self, $c, $dasFeatures ) = @_;
  
  my $seqObject = {};
  
  # we have to use the dasFeatures, but to define the length which is not present in some sources,
  # I am walking down the sources array so that pfam would always return the end;
  foreach my $ds_id ( @{ $c->stash->{ response}->{ sources } } ){
    
    # there may be cases where the das sources might not return value and would be populated in error sources;
    # so first check whether they exist in the response;
    unless( exists  $dasFeatures->{ $ds_id } ){
      $c->log->debug( " this is an error_source ".$ds_id );
      next;
    }
    
    $c->log->debug( "the total featureSets present for $ds_id is ".scalar( @{ $dasFeatures->{ $ds_id } }) );
    
    # getting one level deeper for specific row;
    for( my $i = 0; $i < scalar( @{ $dasFeatures->{ $ds_id } } ); $i++ ){
      
      my $featureRow = $dasFeatures->{ $ds_id }->[ $i ];
      
      my $rowSeqObj = {};
      
      # all the features for that specific row;
FEAT: for( my $j = 0; $j < scalar( @{ $featureRow } ); $j++ ){
        
        my $feature = $featureRow->[ $j ];
        
        unless( defined $c->stash->{ response}->{ seqLength } ){
          if( exists $feature->{ segment_stop } ){
            $c->stash->{ response}->{ seqLength } =  $feature->{ segment_stop };
          }
            
        } # end of unless $c->stash->{ response}->{ seqLength };  

        #$c->log->debug( 'settign the seqlength to be '.$c->stash->{ response }->{ seqLength } .' for source '.$ds_id );
 
        $rowSeqObj->{ length }   = $c->stash->{ response}->{ seqLength };
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
    
    # check whether the start and end are greater than 0;
    unless( ( $features->[$i]->{'start'} > 0 ) && ( $features->[$i]->{'end'} > 0 ) ){
      next FEATURE;
    }
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

  # cache the Bio::Das::Lite object for future requests
  return if defined $self->{daslite};

  my ($proxy) = $self->{ das }->{proxy} || '' =~ /^([\w\:\/\.\-\?\#]+)$/;
  
  $c->log->debug( "getDasLite: the proxy to be set is $proxy ")
    if $c->debug;
  $self->{daslite} = Bio::Das::Lite->new( { timeout => $self->{timeout}, } );

  $self->{daslite}->{http_proxy} = $proxy if ( defined $proxy );

}

#---------------------------------------------------------------------------------------------

sub end : ActionClass( 'RenderView' ) {}

1;
