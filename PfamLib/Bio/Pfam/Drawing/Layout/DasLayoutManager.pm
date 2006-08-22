package Bio::Pfam::Drawing::Layout::DasLayoutManager;

use strict;
use vars qw($AUTLOAD @ISA @EXPORT $VERSION);
use Exporter;
use Bio::Pfam::Drawing::Layout::LayoutManager;
use Bio::Pfam::Drawing::Layout::Config::GenericDasSourceConfig;
use Data::Dumper;
@ISA = qw(Bio::Pfam::Drawing::Layout::LayoutManager);



sub layout_DAS_sequences_and_features {
  my ($self, $sequence, $features) = @_;
    
  #Okay, the way das works is going to be slightly different.
  #For each source, we want to display the sequence at the top,
  #the draw all features below the sequence.
  my $featureSetsAdded =0;
  my $uid = 1;
  foreach my $source (keys %$features){
	#first get the sequence object.
	my $sourceId;
	#print STDERR "\n\n\n***** $source, ";

	if($source =~ /(smart|superfamily|cath_sptr|dssp|uniprot|das\/pfam)/i){
	    my $id = lc($1);
	    $id =~ s/_|\///g;
	    $sourceId = $id."Das";
	}else{
	  $sourceId = "genericDasSource";
	}
	#print STDERR "$sourceId *****\n\n\n\n";
	#What we do with das is slightly different.  We display all features (of a type that are accepted)
	
	if(ref($features->{$source}) eq "ARRAY"){ 
	    my $config = $self->getSourceConfigurator($sourceId);
	    
	    $config->configureSource($features->{$source});
	    my $featureSetsRef = $self->resolveOverlaps($features->{$source}, \$uid);
	    
	    foreach my $featureSet (@$featureSetsRef){
		#print Dumper($featureSet);
	      $featureSetsAdded++;
	      my $l_seq = Bio::Pfam::Drawing::Layout::Sequence->new();
		#$l_seq->hidden(1);
	      $l_seq->colour1(Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "EEEEEE"));
	      $l_seq->colour2(Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "DDDDDD"));
	      $l_seq->convertDasSeqAndFeatures($sequence, $source, $featureSet );
	      $self->add_seq($l_seq);

		
	    }
	}
    }
  return $featureSetsAdded;
}




sub scale_x {
  my ($self, $scale_x) = @_;
  
  if ($scale_x){
      
      $self->{'scale_x'} = $scale_x;
  }
  if(!$self->{'scale_x'}){
    $self->{'scale_x'} = 1.0;
  }
  return $self->{'scale_x'};
}

sub scale_y {
  my ($self, $scale_y) = @_;
  if ($scale_y){
    $self->{'scale_y'} = $scale_y;
  }
  if(!$self->{'scale_y'}){
    $self->{'scale_y'} = 0.5;
  }
  return $self->{'scale_y'};
}



sub getSourceConfigurator {
    my ($self, $source) = @_;
    
    if(!$self->{'config'}->{$source}){
	
	#See if we can find a config object
	my $sourceConf = ucfirst $source;
	$sourceConf = "Bio::Pfam::Drawing::Layout::Config::".$sourceConf."Config"; #Check
	my $config;
	warn ("The requested Config class, $sourceConf is not available:$@\n") unless (eval "require $sourceConf");
	
	eval{
	    $config = $sourceConf->new();
	};
	if($@){
	    #looks like we can not find a config for thisregion
	    warn "\n**Using default config for $source (Could not find  $sourceConf) :$@**\n\n";
	    $config = Bio::Pfam::Drawing::Layout::Config::GenericDasSourceConfig->new();
	}
	$self->{'config'}->{$source} = $config; 
    }
    return $self->{'config'}->{$source};

}


sub resolveOverlaps{
    my ($self, $features, $uidRef) = @_;
    my $featureSets = [];
  FEATURE:
    for(my $i = 0; $i < scalar(@$features); $i++){
	next if ($features->[$i]->{'hidden'});
	$features->[$i]->{'uid'} = $$uidRef;
	$$uidRef++;
	my $assignedToSet = 0;
      SET:
	for (my $j = 0; $j < scalar(@$featureSets); $j++){
	    my $overlapWithSet = 0;
	    foreach my $feature (@{$featureSets->[$j]}){
		next SET if($features->[$i]->{'drawingType'} ne $feature->{'drawingType'});
		if($feature->{'_displayGroup'} || $features->[$i]->{'_displayGroup'}){
		  #Lets keep similar features together
		  next SET unless ($feature->{'_displayGroup'} && $features->[$i]->{'_displayGroup'});
		  next SET unless ($feature->{'_displayGroup'} eq $features->[$i]->{'_displayGroup'});
		}

		#Need to take into account that markups may have no end...
		if(($features->[$i]->{'start'} <= $feature->{'end'} &&  
		    $features->[$i]->{'start'} >= $feature->{'start'}) 
		   || ($features->[$i]->{'end'} <= $feature->{'end'} &&  
		       $features->[$i]->{'end'} >= $feature->{'start'})
		   || ($features->[$i]->{'start'} <= $feature->{'start'} &&  
		       $features->[$i]->{'end'} >= $feature->{'end'})
		  ){
		    $overlapWithSet = 1;
		    
		    last;
		}
	    }
	   
	    if(!$overlapWithSet){
		push(@{$featureSets->[$j]}, $features->[$i]);
		
		$assignedToSet = 1;
		next FEATURE;;
	    }
	    #If we
	}
	if(!$assignedToSet){
	    $featureSets->[scalar(@$featureSets)] =  [$features->[$i]];
	    next FEATURE;
	}
	warn "error resolving overlaps, I should never get here\n";
    }
    return($featureSets);
}


1;
