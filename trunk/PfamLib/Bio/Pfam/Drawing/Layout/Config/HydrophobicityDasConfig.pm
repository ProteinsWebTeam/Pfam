package Bio::Pfam::Drawing::Layout::Config::HydrophobicityDasConfig;

use strict;
use Bio::Pfam::Drawing::Layout::Config::GenericDasSourceConfig;
use Data::Dumper;
use vars qw($AUTOLOAD @ISA $VERSION);
@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericDasSourceConfig);



#Configure the DAS source.
sub _setDrawingType{
    my($self, $feature) = @_;
    #Note, feature is an array ref....
    for(my $i = 0; $i < scalar(@$feature); $i++){
	if($feature->[$i]->{'end'} && $feature->[$i]->{'start'}){
	    $feature->[$i]->{'drawingType'} = "Graph";
	}else{
	    $feature->[$i]->{'hidden'} = 1 ;
	}
    }
}

sub _setDrawingStyles{
    my ($self,$features) = @_;
    
    for(my $i = 0; $i < scalar(@$features); $i++){
	
	if($features->[$i]->{'drawingType'} eq "Graph"){
	    $self->_setGraphColours($features->[$i], "FF99FF","666666");
	}else{
	    $features->[$i]->{'hidden'} = 1;
	}
    }
}

1;