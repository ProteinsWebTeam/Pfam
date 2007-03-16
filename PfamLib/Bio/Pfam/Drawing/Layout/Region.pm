
=head1 NAME

Bio::Pfam::Drawing::Region - Representation of any region on a sequence.  This region can be given a name and URL for linking purposes

=head1 SYNOPSIS

    use Bio::Pfam::Drawing::Region;

Bio::Pfam::Drawing::Region( '-START' => $start,
                 '-END' => $end,
                 '-LABEL' => $label,
                 '-URL' => $url,
                 '-COLOUR1' => $colour1);



=head1 DESCRIPTION


=cut

# $Author: jt6 $

# Let the code begin...
package Bio::Pfam::Drawing::Layout::Region;
use vars qw($AUTOLOAD $VERSION);
$VERSION = "0.1";
use strict;
use warnings;
use Bio::Pfam::Drawing::Colour::hexColour;
use Bio::Pfam::Drawing::Colour::rgbColour;

=head2 new

    Title   : new
    Usage   : my $region = drawing::Region->new()
    Function: Generate a new region object
    Retruns : A region object
    Args    : 

=cut


sub new {
  my $caller = shift;
  my %params = @_;
  my $self = bless {}, ref($caller) || $caller; 

  my($start, $end, $label, $url, $colour);

  $start = ($params{'-START'}||$params{'-start'});
  $end = ($params{'-END'}||$params{'-end'});
  $label = ($params{'-ACCESSION'}||$params{'-accession'});
  $url = ($params{'-URL'}||$params{'-url'});
  $colour = ($params{'-colour1'}||$params{'-colour1'});
  
  $self->start( $start );
  $self->end( $end );
  $self->label($label);
  $self->url($url);
  $self->colour1($colour);

  return $self;
}


=head2 label

    Title   : label
    Usage   : $region->label("Domain X")
    Function: gets/sets the label 
    Retruns : This will return the label if no argument is passed.
    Args    : a label (optional)

=cut

sub label{
   my ($self, $label) = @_;

   if (defined $label) {
       $self->{'label'} = $label;
   }
   return $self->{'label'};
}

=head2 start

    Title   : start
    Usage   : $region->start("3")
    Function: gets/sets the start of the region.  This is always with respect to the sequence coos position! 
    Retruns : This will return the start position if no argument is passed
    Args    : the start co-ordinate of the domain (optional)

=cut

sub start {
   my ($self, $start) = @_;
   if (defined $start) {
       $self->{'start'} = $start;
   }else{
       return $self->{'start'};
   }
}

=head2 end

    Title   : end   
    Usage   : $region->end("156")
    Function: gets/sets the end of the region.  This is always with respect to the sequence coos position! 
    Retruns : This will return the end position if no argument is passed
    Args    : the end co-ordinate of the region (optional)

=cut

sub end {
   my ($self, $end) = @_;
   if (defined $end) {
       $self->{'end'} = $end;
   }else{
       return $self->{'end'};
   }
}


=head2 url

    Title   : url
    Usage   : $region->url("http://www.sanger.ac.uk//cgi-bin/Pfam/getacc?PF00621")
    Function: gets/sets a url to be assigned to the image 
    Retruns : This will return the URL if no argument is passed
    Args    : a url (optional)

=cut

sub url {
    my ($self, $value) = @_;
    if (defined $value) {
	$self->{'url'} = $value;
    }else{
	return $self->{'url'};
    }
}

sub solid {
    my ($self, $value) = @_;
    if (defined $value) {
	$self->{'solid'} = $value;
    }else{
	return $self->{'solid'};
    }
}


sub uid {
    my ($self, $value) = @_;
    if (defined $value) {
	$self->{'uid'} = $value;
    }else{
	return $self->{'uid'};
    }
}

=head2 colour1

    Title   : colour1   
    Usage   : $region->colour1($aDrawingColourObject);
    Function: gets/sets the colour.
    Retruns : This will return the colour object
    Args    : a Bio::Pfam::Drawing::Colour::xxxColour object

=cut


sub colour1 {
    my ($self, $colour) = @_;
    if($colour){
      if($colour->isa("Bio::Pfam::Drawing::Colour::hexColour") ||$colour->isa("Bio::Pfam::Drawing::Colour::rgbColour")){
	$self->{'colour1'} = $colour;
      }else {
        warn "$colour is not a Bio::Pfam::Drawing::Colour::xxxColour";
      }
    }
    return $self->{'colour1'};
}

=head2 region2XMLDOM

    Title   : region2XMLDOM  
    Usage   : $region->region_length()
    Function: This will return the length of the region. Units are the sequence units e.g. Amino acids 
    Retruns : The length of the region
    Args    : 

=cut


sub region2XMLDOM{
    my ($self, $dom) = @_;
    my $element = $dom->createElement("region");
    $element->setAttribute("start", $self->start);
    $element->setAttribute("end", $self->end);
    if($self->label){ 
      $element->setAttribute("label", $self->label);
    }
    if($self->url){
      $element->setAttribute("link_URL", $self->url);
    }
    
    if($self->solid){
	$element->setAttribute("solid", 1);
    }
    if($self->uid){
	$element->setAttribute("unique_id", $self->uid);
    }
    

    #colour1
    my $c1 = $dom->createElement("colour1");
    $element->appendChild($c1);
    my $colour = $self->colour1->colour2XMLDOM($dom);
    $c1->appendChild($colour);
    
    if($self->type eq "bigShape"){
      my $c2 = $dom->createElement("colour2");
      $element->appendChild($c2);
      my $colour2 = $self->colour2->colour2XMLDOM($dom);
      $c2->appendChild($colour2);
      my $shape = $dom->createElement($self->type);
      $element->appendChild($shape);
      $shape->setAttribute("leftStyle", $self->leftstyle);
      $shape->setAttribute("rightStyle", $self->rightstyle);
    }else{
      my $shape = $dom->createElement($self->type);
      $element->appendChild($shape);
    }


    return $element;
}

=head2 type

    Title   : type
    Usage   : $region->type($region_type)
    Function: Get the region type.
    Retruns : the region type
    Args    : None
            

=cut

sub type {
    my($self, $type) = @_;
    if($type){
      $self->{'type'} = $type;
    }
    return $self->{'type'};
}

=head2 colour2

     See colour1

=cut


sub colour2 {
    my ($self, $colour) = @_;
    if($colour){
      if($colour->isa("Bio::Pfam::Drawing::Colour::hexColour") || $colour->isa("Bio::Pfam::Drawing::Colour::rgbColour")){
	$self->{'colour2'} = $colour;
      }else {
	warn "$colour is not a Bio::Pfam::Drawing::Colour::xxxColour";
      }
    }
    return $self->{'colour2'};
}

=head2 leftsytle

    Title   : leftstyle
    Usage   : $bigregion->leftstyle($style)
    Function: Sets the style for the left edge of the region images
    Retruns : A style for the left edge
    Args    : style for the left edge (setting only).  
            : If you are using this to generate the XML,
            : the xml schema has this restricted to: straight, jagged, curved

=cut

sub leftstyle {
    my ($self, $style) = @_;
    if($style){
	$self->{'leftstyle'} = $style;
    }
    return $self->{'leftstyle'};
}

=head2 rightsytle

    Title   : rightstyle
    Usage   : $region->rightstyle($style)
    Function: Sets the style for the right edge of the region image
    Retruns : A style for the right edge
    Args    : style for the right edge (setting only).
            : If you are using this to generate the XML,
            : the xml schema has this restricted to: straight, jagged, curved 

=cut

sub rightstyle {
    my ($self, $style) = @_;
    if($style){
	$self->{'rightstyle'} = $style ;
    }
    return $self->{'rightstyle'};
}

=head2 BioAnnotatedRegion

    Title   : BioAnnotatedRegion
    Usage   : $region->BioAnnotatedRegion($)
    Function: Sets the style for the right edge of the region image
    Retruns : A Bio::Pfam::AnnotatedRegion
    Args    : A Bio::Pfam::AnnotatedRegion (optional)
 
=cut

sub BioAnnotatedRegion {
  my ($self, $region) = @_;
  if($region){
    $self->{'BioAnnRegion'} = $region;
  }else{
    return $self->{'BioAnnRegion'};
  }
}


sub convert_reg{
  my ($self, $region) = @_;
  $self->BioAnnotatedRegion($region);
  $self->start($region->from);
  $self->end($region->to);

}

sub convertDasRegion{
    my($self, $feature, $source, $seq_id) = @_;
    $self->start($feature->{'start'});
    $self->end($feature->{'end'});
    $self->BioAnnotatedRegion(Bio::Pfam::AnnotatedRegion->new('-SEQ_ID' => $seq_id,
							      '-FROM' => $feature->{'start'},
							      '-TO' => $feature->{'end'},
							      '-TYPE' => $source));
    $self->label($feature->{'feature_label'});
    $self->solid(1);
    $self->colour1(Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $feature->{'colour1'}));
    $self->colour2(Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $feature->{'colour2'}));
    $self->type("smlShape");
    $self->uid($feature->{'uid'});
    $self->url($feature->{'link'}[0]{'href'});
}

sub hidden {
    my ($self, $hidden) = @_;
    if($hidden){
	$self->{'hidden'} = $hidden;
    }else{
	return $self->{'hidden'};
    } 
}


sub cloneRegion{
    my $self = shift;
    my $clone = $self->new;
    $clone->start($self->start);
    $clone->end($self->end);
    my $region = $self->BioAnnotatedRegion;
    my $ref = ref($region);
    my $region_copy = $ref->new;
    $clone->BioAnnotatedRegion($region_copy);
    foreach (keys %$region){
	$region_copy->{$_} = $region->{$_};
    }
    return $clone;
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

