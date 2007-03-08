
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::SuperfamilyDasConfig;
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
	#print STDERR $features->[$i]->{'feature_label'}."\n";
	#print STDERR $features->[$i]->{'type_id'}."\n";
	if($features->[$i]->{'feature_id'} =~ /DOMAIN/){
	    if($features->[$i]->{'feature_label'} !~ /ACC\:/){    
		my $newLabel = $features->[$i]->{'type_id'}."(ACC:".$features->[$i]->{'feature_label'}.")";
		$features->[$i]->{'feature_label'} = $newLabel;
	    }
	    $self->_setRegionColours($features->[$i], "CCFFCC","339900");
	}else{
	    $features->[$i]->{'hidden'} = 1;
	}
    }
}

1;
