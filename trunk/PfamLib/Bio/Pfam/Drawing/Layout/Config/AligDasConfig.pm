
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
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;

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

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;

