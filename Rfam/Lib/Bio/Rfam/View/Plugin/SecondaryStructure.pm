#!/usr/bin/env perl

package Bio::Rfam::View::Plugin::SecondaryStructure;

use Data::Dumper;
use Moose;
with 'MooseX::Role::Pluggable::Plugin';
use IO::Compress::Gzip qw(gzip $GzipError);
use Bio::Easel::MSA;
use File::Slurp;
use SVG;
use SVG::Parser;

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self = shift;
  # print 'Work on this ' . $self->parent->family->SEED->path . "\n";
  
  #$self->makeRchie;
  $self->makeBling;
}


sub makeRchie {
	my ($self) = @_;
	
	my $rfamdb = $self->parent->config->rfamlive;
	my $rfam_acc = $self->parent->family->DESC->AC;
	
	my $location = "/nfs/nobackup2/xfam/rfam";
	my $seed_loc = "$location/$rfam_acc";
	my $rchie_img = "$location/$rfam_acc.rchie.png";
	my $msa = $self->parent->family->SEED;
        $msa->write_msa($seed_loc);
	
	my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );
	if (!defined($famRow)) {
		croak ("Failed to find entry in the Family table for $rfam_acc.");
	}

	my $Rchie_cmd = "stockholm2Arc.R $seed_loc $rchie_img 2> $location/$$.err";
	print "Making arc diagram for $rfam_acc\n";
	system ($Rchie_cmd);
	if ($? == -1) {
		croak ("Failed to generate Rchie image for $rfam_acc!\n");
	}
	my $fileGzipped;
	gzip $rchie_img => \$fileGzipped;
	
	my $resultset = $rfamdb->resultset('SecondaryStructureImage')->find(
										{rfam_acc => $rfam_acc,
										type => 'rchie'},
										{key => 'acc_and_type'});
	if (!$resultset) {
		$rfamdb->resultset('SecondaryStructureImage')->create({
				rfam_acc => $rfam_acc,
				type => 'rchie',
				image => $fileGzipped
		});
	} else {
		$rfamdb->resultset('SecondaryStructureImage')->update({
				rfam_acc => $rfam_acc,
				type => 'rchie',
				image => $fileGzipped}
		);
	}
	unlink($seed_loc);
	unlink($rchie_img);
}

sub makeBling {
  my ($self) = @_;;

  my $rfamdb = $self->parent->config->rfamlive;
  my $rfam_acc = $self->parent->family->DESC->AC;

  my $location = "/nfs/nobackup2/xfam/rfam";
  my $seed_loc = "$location/$rfam_acc";
  my $RNAplot_img = "$location/$rfam_acc"."_ss.svg";
  my $RNAplot = "$location/rnaplot";  

  my $conservationSVG = "$location/$rfam_acc.conservation.svg";
  my $covariationSVG = "$location/$rfam_acc.covariation.svg";
  my $entropySVG = "$location/$rfam_acc.entropy.svg";
  my $fcbpSVG = "$location/$rfam_acc.fcbp.svg"; 

  # This is simply to get around a 'not digitized' error that easel is giving
  my $msa = $self->parent->family->SEED;
  $msa->write_msa($seed_loc);
  $msa=Bio::Easel::MSA->new({ fileLocation => $seed_loc}); 

  # Remove 'gapped' columns from the MSA object
  $msa->remove_rf_gap_columns();

  # Generate the Most Informative Sequence (MIS)
  my $MIS = $msa->most_informative_sequence();

  # Retrieve the Consensus RNA Secondary Structure
  my $SScons = $msa->get_ss_cons_dot_parantheses();

  # Create a hash of positions that form basepairs for generating specific bp images
  my $i=0;
  my %bpHash;
  foreach my $bpPos (split //, $SScons) {
    if($bpPos eq '.') {$bpHash{$i}=0}
    else {$bpHash{$i}=1};
    $i++;
  }

  # Create the base unannotated SS Image with RNAplot
  open(my $fh, ">", "$RNAplot" ) or die "$RNAplot";
  print $fh (">$rfam_acc\n$MIS\n$SScons\n"); 
  close $fh;

  chdir($location);
  
  use IPC::Run qw(run);
  my @cmd2 = ("/nfs/production/xfam/rfam/software/bin/RNAplot", "-o", "svg");
  run \@cmd2, '<', $RNAplot;

  #my $pid = open2($RNAplot_img, $RNAplot, "/nfs/production/xfam/rfam/software/bin/RNAplot -o svg");

  #waitpid( $pid, 0 );
  #my $child_exit_status = $? >> 8;

  #my @RNAplotArgs = ("/nfs/production/xfam/rfam/software/bin/RNAplot", "-o svg", "< /nfs/nobackup2/xfam/rfam/rnaplot");
  #system(@RNAplotArgs) == 0 or die "system @RNAplotArgs failed: $?";

  #system("/nfs/production/xfam/rfam/software/bin/RNAplot -o svg </nfs/nobackup2/xfam/rfam/rnaplot");

  unless(-e $RNAplot_img) {
    die ("Error in creating original RNA SVG image \n")
  }

  # Create a colour hash. This links a value between 1 and 37 with the RGB colour gradient
  my %colours =     ( 1  => "255,0,255",   2  => "219,0,255",   3  => "184,0,255",
                      4  => "148,0,255",   5  => "113,0,255",   6  => "77,0,255",
                      7  => "42,0,255",    8  => "7,0,255",     9  => "0,28,255",
                      10 => "0,63,255",    11 => "0,99,255",    12 => "0,134,255",
                      13 => "0,169,255",   14 => "0,205,255",   15 => "0,240,255",
                      16 => "0,255,240",   17 => "0,255,198",   18 => "0,255,162",
                      19 => "0,255,127",   20 => "0,255,92",    21 => "0,255,56",
                      22 => "0,255,21",    23 => "0,255,0",     24 => "14,255,0",
                      25 => "49,255,0",    26 => "84,255,0",    27 => "120,255,0",
                      28 => "155,255,0",   29 => "191,255,0",   30 => "226,255,0",
                      31 => "255,247,0",   32 => "255,212,0",   33 => "255,177,0",
                      34 => "255,141,0",   35 => "255,105,0",   36 => "255,70,0",
                      37 => "255,0,0");

  # Perform calculations to generate the following different SS annotations 
  # where annotation is a hash with the position as a key and a RGB colour 
  # as the value. We must also record max value for some annotations 
  # and scale accordingly.

  # 1) Sequence coonservation
  # 2) Base pair conservation (Fraction of Canonical Base Pairs)
  # 3) Covariation
  # 4) Sequence Entropy
  # 5) Maximum parse of the covariance model
  # 6) Normal (A different colour for each stem loop)
  # 7) Motifs

  my $rgbColourNumber;
  my $k;

  # Image 1: Sequence Conservation
  my @conservation = $msa->Bio::Easel::MSA::pos_conservation();
  my %conservationColour;
  $k = 0;
  foreach my $conservationValue (@conservation) {
    $rgbColourNumber = int(((($conservationValue/1)*36)+1) + 0.5);
    $conservationColour{$k} = $colours{$rgbColourNumber};
    $k++;
  }

  # Image 2: Fraction of Canonical Base Pairs (fcbp)
  my @fcbp = $msa->Bio::Easel::MSA::pos_fcbp();
  my %fcbpColour;
  $k = 0;
  foreach my $fcbpValue (@fcbp) {
    if ($bpHash{$k} eq 0) {
      $fcbpColour{$k} = "-";
    }
    else{
      $rgbColourNumber = int(((($fcbpValue/1)*36)+1) + 0.5);
      $fcbpColour{$k} = $colours{$rgbColourNumber};
    }
    $k++;
  }

  # Image 3: Covariation
  my @covariation = $msa->Bio::Easel::MSA::pos_covariation();
  my %covariationColour;
  $k = 0;
  foreach my $covariationValue (@covariation) {
    if ($bpHash{$k} eq 0) {
      $covariationColour{$k} = "-";
    }   
    else{
      $rgbColourNumber = int(((($covariationValue+2)*9)+1) + 0.5);
      $covariationColour{$k} = $colours{$rgbColourNumber};
    }
    $k++;
  }

  # Image 4: Sequence Entropy 
  my @SeqEntropy = $msa->Bio::Easel::MSA::pos_entropy();
  my $maxEntropy = 0.0;
  my %SeqEntropyColour;
  foreach my $SeqEntValue (@SeqEntropy) {
    $maxEntropy = $SeqEntValue if( $maxEntropy < $SeqEntValue);
  }
  $k = 0;
    foreach my $SeqEntValue (@SeqEntropy) {
    $rgbColourNumber = int(((($SeqEntValue/$maxEntropy)*36)+1) + 0.5);
    $SeqEntropyColour{$k} = $colours{$rgbColourNumber};
    $k++;
  } 

  # Image 5: Maximum Parse of CM Model
  # !TODO!

  # Image 6: Normal Blocks
  # !TODO!

  # Image 7: Motif Blocks
  # !TODO!

  # Generate the image legends
  my $conservationLegend = SVGLegend('Sequence Conservation',0,1,\%colours);
  my $fcbpLegend = SVGLegend('Frac. Canonical BPs',0,1,\%colours);
  my $covariationLegend = SVGLegend('Covariation',-2,2,\%colours);
  my $seqEntropyLegend = SVGLegend('Relative Entropy',0,$maxEntropy,\%colours);


  # Generate the SVG images
  my $conservationSVGobj = annotateSVG($RNAplot_img, \%conservationColour, $conservationLegend);
  my $fcbpSVGobj = annotateSVG($RNAplot_img, \%fcbpColour, $fcbpLegend);
  my $covariationSVGobj = annotateSVG($RNAplot_img, \%covariationColour, $covariationLegend);
  my $entropySVGobj = annotateSVG($RNAplot_img, \%SeqEntropyColour, $seqEntropyLegend);

  # Save the SVG images to disk
  my ($conservationSVGhandle, $fcbpSVGhandle, $covariationSVGhandle, $entropySVGhandle);
  
  open ($conservationSVGhandle, '>', $conservationSVG) or die ("Unable to open $conservationSVGhandle");
  print $conservationSVGhandle $conservationSVGobj;
  close ($conservationSVGhandle) or die ("Unable to close $conservationSVGhandle");
 
  open ($fcbpSVGhandle, '>', $fcbpSVG) or die ("Unable to open $fcbpSVG");
  print $fcbpSVGhandle $fcbpSVGobj;
  close ($fcbpSVGhandle) or die ("Unable to close $fcbpSVGhandle");

  open ($covariationSVGhandle, '>', $covariationSVG) or die ("Unable to open $covariationSVGhandle");
  print $covariationSVGhandle $covariationSVGobj;
  close ($covariationSVGhandle) or die ("Unable to close $covariationSVGhandle");

  open ($entropySVGhandle, '>', $entropySVG) or die ("Unable to open $entropySVGhandle");
  print $entropySVGhandle $entropySVGobj;
  close ($entropySVGhandle) or die ("Unable to close $entropySVGhandle");
  
  # Put the SVG images in the database
 
  # Unlink the file

}
 
# Sub for creating the legend for bling images as an SVG group element which contains the legend
sub SVGLegend {
  my ($legend_name,$min_value,$max_value,$colours) = @_;
  
  $max_value = sprintf("%.2f", $max_value);
  $min_value = sprintf("%.2f", $min_value);

  my $tmpLegendObj = SVG->new;
  my $legendElementsGroup=$tmpLegendObj->group(id        => 'legendElements',
                                               transform => 'translate(25,500)');
  my $legendTitle = $legendElementsGroup->text(
                                                id     => 'legendTitle',
                                                x      => 71,
                                                y      => 27,
                                                style  => { 'font-family'  => "Arial,Helvetica",
                                                            'font-size'    => "14px",
                                                            'text-anchor'  => "middle",
                                                            'fill'         => "dimgrey" } )->cdata($legend_name);
  my $leftLegendNumber = $legendElementsGroup->text( 
                                                id     => 'leftLegendNumber',
                                                x      => 0,
                                                y      => -2,
                                                style  => { 'font-family'  => "Arial,Helvetica",
                                                            'font-size'    => "14px",
                                                            'text-anchor'  => "left",
                                                            'fill'         => "dimgrey" } )->cdata($min_value);
  my $rightLegendNumber = $legendElementsGroup->text(
                                                id     => 'rightLegendNumber',
                                                x      => 122,
                                                y      => -2,
                                                style  => { 'font-family'  => "Arial,Helvetica",
                                                            'font-size'    => "14px",
                                                            'text-ancor'   => "left",
                                                            'fill'         => "dimgrey" } )->cdata($max_value);
  my $legendBox = $legendElementsGroup->group(id => 'legendBoxElements',
                                              x  =>  0,
                                              y  =>  0 );
  my $xPos = 0;
  my $yPos = 0;
  
  my @colourKeys = keys % {$colours};
  foreach my $colourKey (sort {$a<=>$b} @colourKeys) {
    $legendBox->rectangle(  x     =>  $xPos,
                            y     =>  $yPos,
                            width =>  4,
                            height => 15,
                            style => { 'fill' => "rgb(".$ {$colours} {$colourKey}.")" } ) ;
    $xPos = $xPos + 4;   
  }  
  return $tmpLegendObj;
}                      

# Subroutine for annotating an image given the SVG image and an annotation hash
sub annotateSVG {
  my ($svgFile, $annotationHash, $SVGlegend) = @_;
  
  # Create a reference to the legend group
  my $legendElements = $SVGlegend->getElementByID("legendElements");

  my $svg_text=read_file($svgFile);
  my $parser=new SVG::Parser();
  my $svg=$parser->parse($svg_text);

 # Navigate the DOM and create references to relevent places in the DOM
 my $seqElements = $svg->getElementByID("seq");
 my $outlineElements = $svg->getElementByID("outline");
 my $pairsElements = $svg->getElementByID("pairs");
 my $seqElementParent=$seqElements->getParentElement();
 my @children = $seqElements->getChildren();

  # Change with width & height of the SVG to allow addition of the legend
  # The default from RNAplot is 452 x 452  so we increase it to 500 x 600
  use Data::Dumper;
  my $SVGfirstChild=$svg->getFirstChild(); 
  my $SVGwidth  = "600";
  my $SVGheight = "550";
  $SVGwidth     = $SVGfirstChild->setAttribute('width',$SVGwidth);
  $SVGheight    = $SVGfirstChild->setAttribute('height',$SVGheight);

  # Move the image over to allow space for the legend 
  my $transformAttribute=$seqElementParent->getAttribute('transform');
  $transformAttribute =~ /translate\((-?\d+.\d+),(-?\d+.\d+)\)/;
  my $xTranslate = $1;
  my $yTranslate = $2;
  $xTranslate = $xTranslate + 100;
  $yTranslate = $yTranslate + 25;
  $transformAttribute =~ s/translate\(-?\d+.\d+,-?\d+.\d+\)/translate($xTranslate,$yTranslate)/;
  $transformAttribute = $seqElementParent->setAttribute('transform', $transformAttribute); 
 
  # Deference the annotation hash
  my %annotationColour = %$annotationHash;
 
  # Change the stroke width of the lines
  $outlineElements->setAttribute('style', 'fill: none; stroke: black; stroke-width: 0.75');
  $pairsElements->setAttribute('style', 'fill: none; stroke: black; stroke-width: 0.75');
 
  # Change the font of the nucleotides
  $seqElements->setAttribute('style','font-family: Arial,Helvetica');

  # Create a hash of the Coordinates
  my %seqPosHash;
  my $pos = 0;
  foreach my $child (@children) {
    my $xCoord = ($child->getAttribute("x"));
    my $yCoord = ($child->getAttribute("y"));
    %seqPosHash = addSeqPosObj(\%seqPosHash,$pos,$xCoord,$yCoord);
    $pos++;
  }

  # Create a new SVG group object which will contain the annotation circles
  my $tmpSVGObj = SVG->new;
  my $circleElementsGroup=$tmpSVGObj->group(id => 'circleElements');

  # Add the annotation element for each position
  my $tag;
  foreach my $key (sort {$a<=>$b} keys %seqPosHash) {
    unless ($annotationColour{$key}  eq  '-') {
      $tag = $circleElementsGroup->circle(   cx   =>   $seqPosHash{$key}->{'X'},
                                             cy   =>   $seqPosHash{$key}->{'Y'},
                                             r    =>   8,
                                             style=>   { 'fill-opacity'=> 1,
                                                         'fill'=>"rgb($annotationColour{$key})"});
    }
  }
  
  # Place the groups in the correct position within the SVG DOM
  $seqElementParent->insertBefore($circleElementsGroup, $outlineElements);
  $SVGfirstChild->insertBefore($legendElements, $seqElementParent);

  # Generate the annotated SVG file
  my $outputSVG = $svg->xmlify();
  
return $outputSVG;
}

#---------------------------------------------------------------------------
# Subroutine for adding coordinates to the coordinate hash 
sub addSeqPosObj {
  my ($seqPosHashRef,$position,$xCoord,$yCoord)  = @_;
  $seqPosHashRef->{$position} = {
    X      =>  $xCoord,
    Y      =>  $yCoord };
 return %$seqPosHashRef;
}
1;
