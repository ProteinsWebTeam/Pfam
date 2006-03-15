package Bio::Pfam::Drawing::Image::ImageSet;

use GD;
#use XML::DOM;
use XML::LibXML;
use XML::LibXML::XPathContext;
use Bio::Pfam::Drawing::Image::Image;

my $ns = "http://www.sanger.ac.uk/Software/Pfam/xml/pfamDomainGraphics.xsd";

sub new{
  my $class = shift;
  my $self = bless {}, ref($class) || $class;
  $self->{ 'images' } = [];
  return $self;
}

# if "skipReparse" is set to true, the method will NOT re-parse the
# supplied dom.

sub create_images {
  my ($self, $origDom, $skipReparse ) = @_;

  my %stored_images;
  #print STDERR "received: |$origDom|\n";

  # a hideous hack. Dump the XML into a string and then parse it back
  # in to get a new DOM. This seems to be the only way to make the
  # whole thing work... we're putting it down to the insertion of
  # empty text nodes at various places in the re-parsed DOM, which, if
  # missing, screw up the XPaths that are executed on the original,
  # hand-crafted DOM. It's all very ugly.
  # jt6 20060120 WTSI.

  my $dom;
  unless( $skipReparse ) {
	my $parser = XML::LibXML->new;
	$dom = $parser->parse_string( $origDom->toString );
  } else {
	$dom = $origDom;
  }


  my $root = $dom->documentElement;
  my $xc = XML::LibXML::XPathContext->new( $root );
  $xc->registerNs( "pf" => $ns );

  foreach my $seqNode ( $xc->findnodes( "pf:sequence" ) ) {
	
	#print STDERR "creating image for |$seqNode|", $seqNode->nodeName, "|\n";

    my $image = Bio::Pfam::Drawing::Image::Image->new;

	#print STDERR "imageset: length: |", $seqNode->getAttribute( "length" ), "|\n";

    $image->scale_x( $root->getAttribute( "scale_x" ) );
    $image->scale_y( $root->getAttribute( "scale_y" ) );
    $image->format(  $root->getAttribute( "format"  ) );
    $image->bg_colour();#not yet implemented
    $image->create_image( $seqNode, \%stored_images );

    $self->add_image( $image );
  }
}

sub add_image {
  my ($self, $pointer) = @_;
  if($pointer){
    push(@{$self->{'images'}}, $pointer);
	#print STDERR "imageset: added image: |$pointer|\n";
  }
}

sub each_image {
  my $self = shift;
  return @{$self->{'images'}};
}



1;
