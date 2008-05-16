
=head1 NAME

Bio::Pfam:Drawing::Markup

=head1 SYNOPSIS

    

=head1 DESCRIPTION


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...

package Bio::Pfam::Drawing::Layout::Markup;
use vars qw($AUTOLOAD @ISA @EXPORT);
use strict;
use warnings;
use Exporter;

=head2 new

    Title   : new
    Usage   : my $region = Bio::Pfam::Drawing::Markup->new()
    Function: Generate a new Markup object
    Retruns : A markup object
    Args    : A hash of all parameters can be supplied. Read the documentation above.

=cut

sub new {
  my $caller = shift;
  my %params = @_;
  my $self = bless {}, ref($caller) || $caller; 
  my( $start, $end, $line, $line_colour, $head, $head_colour, $v_align, $overlap_counter, $label);

  $start = ($params{'-START'}||$params{'-start'});
  $end = ($params{'-END'}||$params{'-end'});
  $line = ($params{'-LINE'}||$params{'-line'});
  $line_colour = ($params{'-LINE_COLOUR'}||$params{'-line_colour'});
  $head = ($params{'-HEAD'}||$params{'-head'});
  $head_colour = ($params{'-HEAD_COLOUR'}||$params{'-head_colour'});
  $v_align = ($params{'-V_ALIGN'}||$params{'-v_align'});
  $label = ($params{'-LABEL'}||$params{'-label'});
  
  $self->start( $start );
  $self->end( $end );
  $self->line( $line );
  $self->line_colour( $line_colour );
  $self->head( $head );
  $self->head_colour( $head_colour );
  $self->v_align( $v_align );
  $self->label($label);

  return $self;
}

=head2 line

    Title   : line
    Usage   : $markup->line($value)
    Function: Get/set that specifies the line style
    Retruns : the line style
    Args    : a line style. If you are using this to generate the XML,
            : the xml schema has this restricted to: bold, dash

=cut


sub line {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'line'} = $value;
   }
   return $self->{'line'};
}

=head2 line_colour

    Title   : line_colour
    Usage   : $markup->line_colour($colourString, $colourStyle)
    Function: Get/set the line colour
    Retruns : the a colour object
    Args    : a colour, this could be a word e.g. Red, a rgb string 255,0,51 or
            : a hex colour (FF0033)

=cut

sub line_colour {
   my ($self, $value) = @_;
   if (defined $value) {
     $value=~ s/\s+//g;
       $self->{'line_colour'} = $value;
   }
   return $self->{'line_colour'};
}


=head2 head

    Title   : head
    Usage   : $markup->head($headStyle)
    Function: Get/set the head style
    Retruns : the head style
    Args    : a head style. If you are using this to generate the XML,
            : the xml schema has this restricted to: circle, square

=cut

sub head {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'head'} = $value;
   }
   return $self->{'head'};
}

=head2 line_colour

    Title   : head_colour
    Usage   : $markup->head_colour($colourString, $colourStyle)
    Function: Get/set the line colour
    Retruns : the a colour object
    Args    : a colour, this could be a word e.g. Red, a rgb string 255,0,51 or
            : a hex colour (FF0033)

=cut

sub head_colour{
   my ($self, $value) = @_;
   if (defined $value) {
     $value=~ s/\s+//g;
     $self->{'head_colour'} = $value;       
   }
   return $self->{'head_colour'};
}


=head2 v_align

    Title   : v_align
    Usage   : $markup->v_align($markupPosition)
    Function: Get/set the position of the markup
    Retruns : the position of the markup.
    Args    : a markup position, i.e. if the markup is going to be displayed 
            : above or below the region image. If you are using this to 
            : generate the XML, the xml schema has this restricted to: top, 
            : bottom

=cut

sub v_align {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'v_align'} = $value;
   }
   return $self->{'v_align'};
}


=head2 start

    Title   : start
    Usage   : $markup->start($start_pos)
    Function: Get/set the start markup position in sequence coos
    Retruns : the markup start position
    Args    : the markup position (only for setting)
       
=cut


sub start {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'start'} = $value;
   }else{
       return $self->{'start'};
   }
}


=head2 end 

    Title   : end
    Usage   : $markup->end($end_pos)
    Function: Get/set the end markup position in sequence coos
    Retruns : the markup end position
    Args    : the markup end position (only for setting)
       
=cut

sub end {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'end'} = $value;
   }
   return $self->{'end'};
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

=head2 markup2XMLDOM

    Title   : markup2XMLDOM
    Usage   : $markup->markup2XMLDOM($dom_object)
    Function: 
            : 
    Retruns : 
    Args    : 

=cut


sub markup2XMLDOM{
#    warn "This has not been written\n";
    my ($self, $dom) = @_;
    my $element = $dom->createElement("markup");
    
    #set the attributes
    $element->setAttribute("start", $self->start);
    if($self->end){
      $element->setAttribute("end", $self->end);
    }
    $element->setAttribute("v_align", $self->v_align);
    
    if($self->label){ 
      $element->setAttribute("label", $self->label);
    }
    if($self->url){
      $element->setAttribute("link_URL", $self->url);
    }
    
    #Set the elements
    #The line element
    my $line = $dom->createElement("line");
    $element->appendChild($line);
    $line->setAttribute("style", $self->line);
    #The line colour
    my $colour = $self->line_colour->colour2XMLDOM($dom);
    $line->appendChild($colour);
    
    #head style
    if($self->head){
      my $head = $dom->createElement("head");
      $element->appendChild($head);
      $head->setAttribute("style", $self->head);
      #The line colour
      my $colour = $self->head_colour->colour2XMLDOM($dom);
      $head->appendChild($colour);
    }

    return $element;
}

sub convert_feature{
  my ($self, $feature) = @_;
  $self->BioSeqFeature($feature);
  $self->start($feature->start);
  $self->end($feature->end) if($feature->end);
}


sub convertDasMarkup{
   my($self, $feature) = @_;
   $self->start($feature->{'start'});
   $self->end($feature->{'end'}) unless ($feature->{'end'} == $feature->{'start'});
   $self->BioSeqFeature(Bio::SeqFeature::Generic->new('-from' => $feature->{'start'},
				 '-to' => $feature->{'end'},
				 '-display_name' => $feature->{'feature_label'},
				 ));

   $self->v_align($feature->{'_valign'});
   if($feature->{'_headColour'}){
     $self->head_colour(Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $feature->{'_headColour'}));
   }
   if($feature->{'_headStyle'}){
     $self->head($feature->{'_headStyle'});
   }
   if($feature->{'_lineStyle'}){
     $self->line($feature->{'_lineStyle'});
   }

   if($feature->{'_lineColour'}){
     $self->line_colour(Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $feature->{'_lineColour'}));
   }

   $self->label($feature->{'feature_label'});
   $self->url($feature->{'link'}[0]{'href'});
}


=head2 BioSeqFeature

    Title   : BioSeqFeature
    Usage   : $region->BioSeqFeature($_)
    Function: Sets the style for the right edge of the region image
    Retruns : A Bio::SeqFeature object
    Args    : A Bio::SeqFeature (optional)
 
=cut

sub BioSeqFeature {
  my ($self, $feature) = @_;
  if($feature){
    $self->{'BioFeature'} = $feature;
  }else{
    return $self->{'BioFeature'};
  }
}


=head2 label

    Title   : label
    Usage   : $markup->label($label)
    Function: Get/set the label for this feature
    Retruns : the label
    Args    : the label (only for setting)
       
=cut

sub label {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'label'} = $value;
   }else{
       return $self->{'label'};
   }
}

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


1;

