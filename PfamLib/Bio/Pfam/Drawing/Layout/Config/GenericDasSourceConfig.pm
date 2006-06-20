package Bio::Pfam::Drawing::Layout::Config::GenericDasSourceConfig;
use strict;
use Data::Dumper;
use vars qw($AUTOLOAD @ISA $VERSION);



sub new {
  my $caller = shift;
  my $self = bless {}, ref($caller) || $caller;
  return $self;
}

#Configure the DAS source.

sub configureSource {
    my($self, $features) = @_;
    $self->_setDrawingType($features);
    $self->_setDrawingStyles($features);

}


sub _setDrawingType{
    my($self, $feature) = @_;
    #Note, feature is an array ref....
    for(my $i = 0; $i < scalar(@$feature); $i++){
	if($feature->[$i]->{'end'} && $feature->[$i]->{'start'}){
	    if($feature->[$i]->{'end'} == $feature->[$i]->{'start'}){
		$feature->[$i]->{'drawingType'} = "Markup";
	    }else{
		$feature->[$i]->{'drawingType'} = "Region";
	    }
	}else{
	    $feature->[$i]->{'hidden'} = 1 ;
	}
    }
}

sub _setDrawingStyles{
    my ($self,$features) = @_;
    
    for(my $i = 0; $i < scalar(@$features); $i++){
	if($features->[$i]->{'drawingType'} eq "Region"){
	    
	    #Now construct the URL
	    #$self->_construct_URL($region);
	    
	    #Now contruct the label
	    #$self->_construct_label($region);
	    $self->_setRegionColours($features->[$i]);
	}elsif($features->[$i]->{'drawingType'} eq "Markup"){
	    #_headColour
	    #_headStyle
	    #$self->_lineColour($markup);
	    #$self->_lineStyle($markup);
	    #v_align
	  $features->[$i]->{'hidden'} = 1;
	} 
    }
}


sub _setRegionColours{
    my($self, $feature, $colour1, $colour2) = @_;
    if($colour1){
	$feature->{'colour1'} = $colour1;
    }else{
	$feature->{'colour1'} = "cccccc";
    }
    if($colour2){
	$feature->{'colour2'} = $colour2;	
    }else{
	$feature->{'colour2'} = "a2a2a2";
    }
}

sub _setHeadColour{

}


sub resolveOverlaps{

}

1;
