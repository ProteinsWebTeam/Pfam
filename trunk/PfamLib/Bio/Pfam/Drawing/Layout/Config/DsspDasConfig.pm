
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::DsspDasConfig;
use strict;
use warnings;

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
	    $feature->[$i]->{'drawingType'} = "Region";
	}else{
	    $feature->[$i]->{'hidden'} = 1 ;
	}
    }
}

sub _setDrawingStyles{
    my ($self,$features) = @_;
    
    for(my $i = 0; $i < scalar(@$features); $i++){
	
	if($features->[$i]->{'type'} eq "HELIX"){
	    $features->[$i]->{'feature_label'} = "Helix";
	    $self->_setRegionColours($features->[$i], "FF0033" );
	}elsif($features->[$i]->{'type'} eq "STRAND"){
	    $features->[$i]->{'feature_label'} = "Strand";
	    $self->_setRegionColours($features->[$i], "006633" );
	}else{
	    $features->[$i]->{'hidden'} = 1;
	}
    }
}

1;
