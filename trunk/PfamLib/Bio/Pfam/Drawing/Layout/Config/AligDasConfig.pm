
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::AligDasConfig;
use strict;
use warnings;

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
		$feature->[$i]->{'drawingType'} = "Region";
    }
}

sub _setDrawingStyles{
    my ($self,$features) = @_;
    for(my $i = 0; $i < scalar(@$features); $i++){
	    $self->_setRegionColours($features->[$i]);
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
