=head1 Name

Bio::Pfam::Drawing::Layout::LayoutManager - Manages the layout of Bio::Seq objects for Pfam grpahics

=head1 Description

This layout manager takes a set of bioperl sequence objects and prepares them
to be written to the XML format that is usd by the drawing code to display
the sequences as graphical images.

First, the layout manager will convert each sequence into objects that reflects 
the structure of the XML.  Based on the configuration of the LayoutManager,
which is performed when the the LayoutManager object is created, the manager
will resolve internal overlaps, then external_overlaps.  Having removed all 
overlaps the domain image styles will be configured. Finally, the colours will 
be set.

A good example to follow is glifs

=cut

# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::LayoutManager;

use strict;
use warnings;

use vars qw($AUTOLOAD @ISA $VERSION @EXPORT);
use XML::LibXML;
use Bio::Pfam::Drawing::Layout::Region;
use Bio::Pfam::Drawing::Layout::Sequence;
use Bio::Pfam::Drawing::Layout::Markup;
use Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;
use Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;
use Exporter;
use Data::Dumper;

@ISA = qw(Exporter);
@EXPORT = qw();
$VERSION = "0.1";

use strict;

sub new {
  my $caller = shift;
  my %params = @_;
  my $self = bless {}, ref($caller) || $caller;
  $self->{'sequences'} = [];
  $self->{'config'} = undef;
  $self->{'scale'} = undef;
  $self->{'layout'} = undef;
  $self->{'format'} = undef;
  
  $self->{ns} = $ENV{PFAM_XML_NS} || 'http://pfam.sanger.ac.uk/static/documents/pfamDomainGraphics.xsd';
  
  #Set up the defaults or override with the params
  $self->scale_x($params{'scale_x'});
  $self->scale_y($params{'scale_y'});
  $self->layout($params{'layout'});
  $self->format($params{'format'});
  
  return $self;
}

=head2 layout_sequences

    Title   : layout_sequences
    Usage   : $layout_manager->layout_sequences(@seqs)
    Function: This is the main method of the layout manager.  It takes the
            : configurators and converts the bioperl sequence objects into
            : psuedo XML::DOM objects so that they can be printed to XML
    Retruns : nothing
    Args    : An array of bioperl sequences

=cut

sub layout_sequences{
  my $self = shift;
  my @seqs = @_;
  foreach my $seq (@seqs){
    my $l_seq = Bio::Pfam::Drawing::Layout::Sequence->new();
    $l_seq->convert_seq($seq);
    $self->add_seq($l_seq);
  }
  $self->resolve_overlaps;
  $self->_set_graphic_styles;
}

=head2 layout_sequences_with_regions_and_features

    Title   : layout_sequences
    Usage   : $layout_manager->layout_sequences(@seqs)
    Function: This is the main method of the layout manager.  It takes the
            : configurators and converts the bioperl sequence objects into
            : psuedo XML::DOM objects so that they can be printed to XML
    Retruns : nothing
    Args    : An array of bioperl sequences

=cut

sub layout_sequences_with_regions_and_features{
  my $self = shift;
  my $seqs = shift;
  my $regionsAndFeatures = shift;
  foreach my $seq (@$seqs){
    my $l_seq = Bio::Pfam::Drawing::Layout::Sequence->new();
    $l_seq->convert_seq($seq, $regionsAndFeatures);
    $self->add_seq($l_seq);
  }
  $self->resolve_overlaps;
  $self->_set_graphic_styles;
}



=head2 _set_graphic_styles

    Title   : _set_graphic_styles
    Usage   : $self->_set_graphics_styles()
    Function: Goes through all of the different regions found on the sequnces and
            : trys to get a Config object for that region.  If not, defaults to the 
            : generic one
    Retruns : nothing
    Args    : nothing

=cut

sub _set_graphic_styles{
  my $self = shift;

  foreach my $seq ($self->each_seq){
    foreach my $region ($seq->eachRegion){
      #What region are you? pfamA, smart, context.....
      	my $config = $self->_get_region_configurator($region->BioAnnotatedRegion->type);#Check
	$config->configure_Region($region);
    }
    foreach my $markup ($seq->eachMarkup){
	if($markup){
	    
	    my $config = $self->_get_feature_configurator($markup->BioSeqFeature->primary_tag);#Check
		$config->configure_Markup($markup);
	}
    }
  }
}


=head2 _get_region_configurator

    Title   : _get_region_configurator
    Usage   : $self->_get_region_configurator
    Function: For the given region name, we try and load the appropriate config module.
            : If it fails, we just get a default config.  Note, we store the config object,
            : so this is only done when we have not come across that region before in the whole
            : layout process. i.e. 10000 pfam domain, 1 load!
    Retruns : nothing
    Args    : A Config object.

=cut

sub _get_region_configurator{
  my ($self, $region) = @_;
  $region = lc $region;
  if(!$self->{'config'}->{$region}){
     
    #See if we can find a config object
    my $regionConf = ucfirst $region;
    $regionConf = "Bio::Pfam::Drawing::Layout::Config::".$regionConf."Config"; #Check
    my $config;
    warn ("The requested Config class, $regionConf is not available:$@\n") unless (eval "require $regionConf");

    eval{
      $config = $regionConf->new();
    };
    if($@){
      #looks like we can not find a config for thisregion
      warn "\n**Using default config for $region (Could not find  $regionConf) :$@**\n\n";
      $config = Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig->new();
    }
    $self->{'config'}->{$region} = $config; 
  }
  return $self->{'config'}->{$region};
}

sub _get_feature_configurator{
  my ($self, $feature) = @_;
  $feature = lc $feature;
  if(!$self->{'config'}->{'$feature'}){
    #See if we can find a config object
    my $featureConf = ucfirst $feature;
    $featureConf =~ s/\s+//g;
    $featureConf = "Bio::Pfam::Drawing::Layout::Config::".$featureConf."Config"; #Check
    my $config;
    
    warn ("The requested Config class, $featureConf is not available:$@\n") unless (eval "require $featureConf");

    eval{
      $config = $featureConf->new();
    };
    if($@){
      #looks like we can not find a config for thisregion
      warn "\n**Using default config for $feature **\n\n";
      $config = Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig->new();
    }
    $self->{'config'}->{$feature} = $config;
  }
  return $self->{'config'}->{$feature};
}


=head2 layout_to_XML

    Title   : layout_to_XML
    Usage   : $layoutmanager->layout_to_XML
    Function: prints the XML out to fle
    Retruns : nothing
    Args    : Filehandle

=cut

sub layout_to_XML {
  my ($self, $FH) = @_;
  my $dom = $self->layout_to_XMLDOM();
  if($FH){
      print $FH $dom->toString ;
  }
}

=head2 layout_to_HTML

    Title   : layout_to_HTML
    Usage   : $layoutmanager->layout_to_XML
    Function: prints the XML out to HTML
    Retruns : html string
    Args    : nothing

=cut

sub layout_to_HTML {
  my ($self) = @_;
  my $dom = $self->layout_to_XMLDOM();
  return $dom->toStringHTML();
}

=head2 layout_toString

    Title   : layout_to_HTML
    Usage   : $layoutmanager->layout_toString
    Function: prints the XML out to string using pretty print
    Retruns : string
    Args    : nothing

=cut

sub layout_toString {
  my ($self ) = @_;
  my $dom = $self->layout_to_XMLDOM();
  return $dom->toString(1);
}




=head2 layout_to_XMLDOM

    Title   : layout_to_XMLDOM
    Usage   : $layoutmanager->layout_to_XML
    Function: converts the layout manager to and XML::DOM object that conforms to the
            : pfamDomainGraphics schema
    Retruns : nothing
    Args    : nothing

=cut

sub layout_to_XMLDOM {
  my $self = shift;

  #Okay, this is nasty, but is the way it is done elsewhere
  #my $parser = XML::LibXML->new;
  #my $dom = $parser->parse("<_/>");
  ##$dom->removeChild($dom->getFirstChild);
  #$dom->createXMLDecl();
  my $dom = XML::LibXML->createDocument(); #This should generate an empty document.....
  my $image = $dom->createElement( "image");

  $image->setNamespace( $self->{ns}, "", 0);
  $image->setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
  $image->setAttribute("xsi:schemaLocation", $self->{ns} . ' ' . $self->{ns} );

  #Set up the root element
  $image->setAttribute("layout", $self->layout);
  $image->setAttribute("format", $self->format);
  $image->setAttribute("scale_x", $self->scale_x);
  $image->setAttribute("scale_y", $self->scale_y);

  $dom->setDocumentElement($image);

  foreach ($self->each_seq){
    my $seqDOM = $_->sequence2XMLDOM($dom);
    $image->appendChild($seqDOM);
  }
  return $dom;
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
    $self->{'scale_y'} = 1.0;
  }
  return $self->{'scale_y'};
}

sub format {
  my ($self, $format) = @_;
  if($format){
    if($format =~ /^(png|gif|jpg)$/i){
      $self->{'format'} = lc $format;
    }else{
      warn "Unrecognised image format $format, must be one of png,gif,jpg\n";
    }
  }
  if(!$self->{'format'}){
    $self->{'format'}= "png";
  }
  return $self->{'format'};
}

sub layout {
  my ($self, $layout) = @_;
  if($layout){
    if($layout =~ /^(continuous|discontinuous)$/){
      $self->{'layout'} = $layout;
    }else{
      warn "Unrecognised layout - $layout, must be continuous or discontinous\n";
    }
  }
  if(!$self->{'layout'}){
    $self->{'layout'} = "continuous";
  }
  return ($self->{'layout'});
}

=head2 add_seq

    Title   : add_seq
    Usage   : $layoutmanager->add_seq($layoutseq)
    Function: Adds each sequence (Layout::sequence object) to the LayoutManager 
    Retruns : nothing
    Args    : a Layout::Sequence object

=cut

sub add_seq {
  my ($self, $seq) = @_;
  if($seq->isa("Bio::Pfam::Drawing::Layout::Sequence")){
    push(@{$self->{'seqs'}}, $seq);
  }else{
    warn "You did not pass a Bio::Pfam::Drawing::Layout::Sequence object\n";
  }
}

sub each_seq{
  my $self = shift;
  return @{$self->{'seqs'}};
}

sub seqHash {
    my $self = shift;
    my %hash = map{ $_->name => $_ } $self->each_seq;
    return %hash;
}

sub resolve_overlaps{
  my $self = shift;
  foreach my $seq ($self->each_seq){
      my %done;
      $self->hide_regions_not_in_order($seq);
      foreach my $type ($self->region_order){
	  $self->resolve_internal_region_overlaps($seq, $type);
	  $self->resolve_external_region_overlaps($seq, $type, \%done); 
      }
      
  }
}



sub hide_regions_not_in_order{
    my ($self, $seq) = @_;
    my @ro = $self->region_order;
    my %order = map{$_ => 1}@ro;
    foreach my $reg ($seq->eachRegion){
	$reg->hidden(1) if(!$order{lc($reg->BioAnnotatedRegion->type)});
    }
}


sub resolve_internal_region_overlaps{
    my ($self, $seq, $type) = @_;
    #N-terminal always wins 
    my @regs = $seq->eachRegionOfType($type);
    if(scalar(@regs)>1){
	for(my $i=0; $i<$#regs; $i++){
	    for(my $j = $i+1; $j<=$#regs; $j++){
		
		if($regs[$i]->end >= $regs[$j]->start){
		    $regs[$j]->start($regs[$i]->end + 1);
		    if($type =~ /pfama/i){
			$regs[$j]->BioAnnotatedRegion->model_from($regs[$j]->BioAnnotatedRegion->model_from + 1);
		    }
		    $regs[$j]->hidden(1) if ($regs[$j]->end <= $regs[$j]->start);
		}
	    }
	}
    }
}

sub resolve_external_region_overlaps{
    my ($self, $seq, $type, $done) = @_;
    
    my @region = sort{$a->start <=> $b->start}$seq->eachRegionOfType($type);
    foreach my $order ($self->region_order){
	next if($$done{$order} || $order eq $type);
	
	my @regs = sort{$a->start <=> $b->start}$seq->eachRegionOfType($order);
	foreach my $reg1 (@region){
	    #reg1 always wins
	    next if ($reg1->hidden);
	    foreach my $reg2 (@regs){
		
		next if ($reg2->hidden);
		if($reg1->start <= $reg2->start && $reg1->end >= $reg2->end){
		    #reg1 completely covers region;
		    $reg2->hidden(1);
		    
		}elsif($reg2->start <= $reg1->start && $reg2->end >= $reg1->end){
		    #reg1 is within reg2;
		    #Therefore we need to split region
		    my $reg2a = $reg2->cloneRegion; #2a will become the C-terminal half 
		    
		    
		    #push(@regs, $reg2a);
		    $reg2->end($reg1->start - 1);
		    $reg2a->start($reg1->end + 1);
		    if($order =~ /pfama/i){
			$reg2->BioAnnotatedRegion->model_to($reg2->BioAnnotatedRegion->model_to - 1);
			$reg2a->BioAnnotatedRegion->model_from($reg2a->BioAnnotatedRegion->model_from + 1);
		    }
		    
		    $reg2a->hidden(1) if ($reg2a->end <= $reg2a->start);
		    $reg2a->hidden(1) if ((($reg2a->end - $reg2a->start)*$self->scale_x) <= 1);
		    
		    $seq->addRegion($reg2a);
		    push(@regs, $reg2a);
		}elsif($reg1->end >= $reg2->start && $reg1->start < $reg2->start ){
		    #N-term od reg2 overlaps with C-term of reg1
		     $reg2->start($reg1->end + 1);
		     if($order =~ /pfama/i){
			$reg2->BioAnnotatedRegion->model_from($reg2->BioAnnotatedRegion->model_from + 1);
		    }
		     
		}elsif($reg2->end >= $reg1->start && $reg2->start < $reg1->start ){
		    #C-term of reg2 overlaps with N-term of reg1
		    $reg2->end($reg1->start - 1);
		    if($order =~ /pfama/i){
			$reg2->BioAnnotatedRegion->model_to($reg2->BioAnnotatedRegion->model_to - 1);
		    }
		    
		}
		if ($reg2->end <= $reg2->start){
		    $reg2->hidden(1);
		}
		$reg2->hidden(1) if ((($reg2->end - $reg2->start)*$self->scale_x)  <=1);
	    }
	}
    }

    $$done{$type}++;
}

sub region_order {
  my $self = shift;
  my @order = @_;
  
  if(@order){
    $self->{'region_order'} = \@order;
  }
  if(!$self->{'region_order'}){
    my @do = $self->_default_region_order;
    $self->{'region_order'} = \@do;
    #This should be populated by subclass
  }
  return @{$self->{'region_order'}};
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

