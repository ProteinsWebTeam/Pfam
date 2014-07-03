#!/usr/bin/env perl

package Bio::Rfam::View::Plugin::SecondaryStructure;

use Data::Dumper;
use Moose;
with 'MooseX::Role::Pluggable::Plugin';
use IO::Compress::Gzip qw(gzip $GzipError);
use Bio::Easel::MSA;
use File::Slurp;
use File::Path;
use SVG;
use SVG::Parser;

use Bio::Rfam::FamilyIO;
use Bio::Rfam::Utils;

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self = shift;
  # print 'Work on this ' . $self->parent->family->SEED->path . "\n";
  
  $self->makeRchie;
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

        my $resultset= $rfamdb->resultset('SecondaryStructureImage')->update_or_create(
                                                                    {rfam_acc => $rfam_acc,
                                                                     type => 'rchie'},
                                                                    {key => 'acc_and_type'});
	
	unlink($seed_loc);
	unlink($rchie_img);
}

sub makeBling {
  my ($self) = @_;
  
  my $config = $self->parent->config;
  my $rfamdb = $self->parent->config->rfamlive;
  my $rfam_acc = $self->parent->family->DESC->AC;

  my $location = "/homes/evan/public_html/ss_images/$rfam_acc";
  File::Path::make_path($location);
  my $seed_loc = "$location/$rfam_acc.SEED";
  my $CM_loc = "$location/$rfam_acc.CM";
  my $RNAplot_img = "$location/$rfam_acc"."_ss.svg";
  my $RNAplot_img_maxCM = "$location/$rfam_acc.maxcm_ss.svg"; 
  my $RNAplot = "$location/rnaplot";  
  my $RNAplot_maxCM = "$location/rnaplot_maxCM";

  my $conservationSVG = "$location/$rfam_acc.conservation.svg";
  my $covariationSVG = "$location/$rfam_acc.covariation.svg";
  my $entropySVG = "$location/$rfam_acc.entropy.svg";
  my $fcbpSVG = "$location/$rfam_acc.fcbp.svg"; 
  my $normalSVG = "$location/$rfam_acc.normal.svg"; 
  my $maxCMparseSVG = "$location/$rfam_acc.maxcm.svg";

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

  # Write the CM file from the database
  Bio::Rfam::FamilyIO->writeCM($self->parent->family->CM, $CM_loc);

  # Create a hash of positions that form basepairs for generating specific bp images
  my $i=0;
  my $j=0;
  my @jArray;
  my %bpHash;
  foreach my $bpPos (split //, $SScons) {
    if($bpPos eq '.') {
      $bpHash{$i}=0;
      push (@jArray, $j);
    }
    elsif ($bpPos eq '(') { 
      $bpHash{$i}=1;
      $j++;
      push (@jArray, $j);
    }
    elsif ($bpPos eq ')') {
      $bpHash{$i}=1;
      push (@jArray, $j);
      $j--;     
   }
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
  # 6) Normal (A different colour for each perfect stem)

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
  my $maxEntropy = 0.001;
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
  my $cmemitPath = $config->infernalPath . "cmemit";
  my $cmalignPath = $config->infernalPath . "cmalign";
  my $out_tfile = "$location/$rfam_acc.tfile";
  my $clen = $self->parent->family->CM->cmHeader->{clen};  #consensus length of the CM 
  my @escAR = ();
  my @cseqAR = ();
  my $cpos;

  Bio::Rfam::Utils::run_local_command("$cmemitPath -c $CM_loc | cmalign --notrunc -g --tfile $out_tfile $CM_loc - > /dev/null");

  open(IN, $out_tfile) || die "ERROR unable to open $out_tfile for reading";

  for($cpos = 0; $cpos < $clen; $cpos++) {
    $escAR[$cpos]  = "";
    $cseqAR[$cpos] = "";
  }

  while(my $line = <IN>) {
    if($line =~ m/^\s+\d+\s+\d+/) {
      $line =~ s/^\s+//;
      my @elA = split(/\s+/, $line);
      my ($emitl, $emitr, $state, $esc) = ($elA[1], $elA[2], $elA[3], $elA[9]);
      my ($lchar, $rchar, $lpos, $rpos) = ("", "", -1, -1);
      if ($emitl =~ m/^(\d+)([A-Z])/) {
        $lpos = $1; $lchar = $2;
      }
      if ($emitr =~ m/^(\d+)([A-Z])/) {
        $rpos = $1; $rchar = $2;
      }
      if ($lchar ne "") {
        if ($state !~ m/ML/ && $state !~ m/MP/) {
          die "ERROR, unexpected left  emission in trace file: $line";
        }
        $escAR[$lpos-1]  = ($state =~ m/MP/) ? $esc/2. : $esc; # split MP scores over both positions by dividing by 2
        $cseqAR[$lpos-1] = $lchar;
      }
      if ($rchar ne "") {
        if($state !~ m/MR/ && $state !~ m/MP/) {
          die "ERROR, unexpected right emission in trace file: $line";
        }
      $escAR[$rpos-1]  = ($state =~ m/MP/) ? $esc/2. : $esc; # split MP scores over both positions by dividing by 2
      $cseqAR[$rpos-1] = $rchar;
      }
    }
  }

  my %maxCMparseColour;
  my $maxMaxCMparseValue = 0.0; 
  for($cpos = 0; $cpos < $clen; $cpos++) {
    if($escAR[$cpos]  eq "") { 
      die "ERROR, did not fill esc for consensus position $cpos"; 
    }
    if($cseqAR[$cpos] eq "") { 
      die "ERROR, did not fill cseq for consensus position $cpos"; 
    }
    if($escAR[$cpos] > $maxMaxCMparseValue) {
      $maxMaxCMparseValue = ($escAR[$cpos]);
    }
  }

  for($cpos = 0; $cpos < $clen; $cpos++) {
    $rgbColourNumber = sprintf("%.0f", ($escAR[$cpos]/$maxMaxCMparseValue)*37);
    $maxCMparseColour{$cpos}= $colours{$rgbColourNumber};
  }

  unlink $out_tfile;  
  
  my $maxCMletters = join("",@cseqAR);
  open(my $maxcm_fh, ">", "$RNAplot_maxCM" ) or die "$RNAplot_maxCM";
  print $maxcm_fh (">$rfam_acc.maxcm\n$maxCMletters\n$SScons\n");
  close $maxcm_fh;

  chdir($location);

  run \@cmd2, '<', $RNAplot_maxCM; 

  unless(-e $RNAplot_img_maxCM) {
    die ("Error in creating original RNA SVG image \n")
  }

  # Image 6: Normal Blocks
  my %normalColourHash;
  $i = 0;
  my $block = 0;
  my $blockColour;
  foreach $j (@jArray) {
    if ($bpHash{$i} == 1) {
      $k = 0;
      foreach my $l (@jArray) {
        if ($k > $i && $l == $j && $bpHash{$k} == 1) {
          unless (defined $normalColourHash{$i} || defined $normalColourHash{$k}) {
            # We have found a base pairing match
            if ($block == 37) {$block=$block+2};
            if (defined $normalColourHash{$i-1} &&  defined $normalColourHash{$k+1}) {
              if ($normalColourHash{$i-1} eq $normalColourHash{$k+1}){
                # Continue the same colour as we are on the same stem
                $blockColour = $colours{((76-$block)%38)};
                $normalColourHash{$i} = $blockColour;
                $normalColourHash{$k} = $blockColour;
              }
              else {
                # New colour as we are on a new stem              
                $block++;
                $blockColour = $colours{((76-$block)%38)};
                $normalColourHash{$i} = $blockColour;
                $normalColourHash{$k} = $blockColour;
               }
             }
            else {
              # New colour as we are on a new stem
              $block++;
              $blockColour = $colours{((76-$block)%38)};
              $normalColourHash{$i} = $blockColour;
              $normalColourHash{$k} = $blockColour;
            }
          }       
        }
        else {$k++}
      }
    }
    else {
      $normalColourHash{$i} = '-';
    }
  $i++;
  }

  # Generate the image legends
  my $conservationLegend = SVGLegend('Sequence Conservation',0,1,\%colours);
  my $fcbpLegend = SVGLegend('Frac. Canonical BPs',0,1,\%colours);
  my $covariationLegend = SVGLegend('Covariation',-2,2,\%colours);
  my $seqEntropyLegend = SVGLegend('Relative Entropy',0,$maxEntropy,\%colours);
  my $maxCMlegend = SVGLegend('Max. CM Parse',0,$maxMaxCMparseValue,\%colours);

  # Generate the SVG images
  my $conservationSVGobj = annotateSVG($RNAplot_img, \%conservationColour, $conservationLegend);
  my $fcbpSVGobj = annotateSVG($RNAplot_img, \%fcbpColour, $fcbpLegend);
  my $covariationSVGobj = annotateSVG($RNAplot_img, \%covariationColour, $covariationLegend);
  my $entropySVGobj = annotateSVG($RNAplot_img, \%SeqEntropyColour, $seqEntropyLegend);
  my $normalSVGobj = annotateSVG($RNAplot_img, \%normalColourHash);
  my $maxCMparseSVGobj = annotateSVG($RNAplot_img_maxCM, \%maxCMparseColour, $maxCMlegend);

  # Save the SVG images to disk as SVG, gzip and then place in database
  my ($conservationSVGhandle, $fcbpSVGhandle, $covariationSVGhandle, $entropySVGhandle, $normalSVGhandle, $maxCMSVGhandle);
  my @handleArray = ([$conservationSVG,'cons'],
                     [$fcbpSVG,'fcbp'],
                     [$covariationSVG,'cov'],
                     [$entropySVG,'ent'],
                     [$normalSVG,'norm'],
                     [$maxCMparseSVG,'maxcm']);
 
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

  open ($normalSVGhandle, '>', $normalSVG) or die ("Unable to open $normalSVGhandle");
  print $normalSVGhandle $normalSVGobj;
  close ($normalSVGhandle) or die ("Unable to close $normalSVGhandle");
  
  open ($maxCMSVGhandle, '>', $maxCMparseSVG) or die ("Unable to open $maxCMSVGhandle");
  print $maxCMSVGhandle $maxCMparseSVGobj;
  close ($maxCMSVGhandle) or die ("Unable to close $maxCMSVGhandle");

  # Insert the SVG images in the database
  foreach my $imageHandleRef (@handleArray) {
    my $fileGzipped;
    gzip $imageHandleRef->[0] => \$fileGzipped;
    my $resultset= $rfamdb->resultset('SecondaryStructureImage')->update_or_create(
                                                                    {rfam_acc => $rfam_acc,
                                                                     type => $imageHandleRef->[1]},
                                                                    {key => 'acc_and_type'});

  }

  # Clean up the temp files
  unlink($RNAplot_img);
  unlink($RNAplot_img_maxCM);
  unlink($RNAplot);
  unlink($RNAplot_maxCM);
  unlink($seed_loc);
  unlink($CM_loc);

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
  my $legendElements;
  if (defined $SVGlegend) { 
    $legendElements = $SVGlegend->getElementByID("legendElements");
  }
  
  my $svg_text=read_file($svgFile);
  my $parser=new SVG::Parser();
  my $svg=$parser->parse($svg_text);

  # Navigate the DOM and create references to relevent places in the DOM
  my $seqElements = $svg->getElementByID("seq");
  my $outlineElements = $svg->getElementByID("outline");
  my $pairsElements = $svg->getElementByID("pairs");
  my $seqElementParent=$seqElements->getParentElement();
  my @children = $seqElements->getChildren();
  my $SVGfirstChild=$svg->getFirstChild();
  my @scriptElements = $SVGfirstChild->getElements("script");
  my $scriptElement = $scriptElements[0];
  
  # Change with width & height of the SVG to allow addition of the legend
  # The default from RNAplot is 452 x 452  so we increase it to 500 x 600
  my $SVGwidth  = "700";
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
 
  # Dereference the annotation hash
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
    my $xCoord = ($child->getAttribute("x")+1);
    my $yCoord = ($child->getAttribute("y")-1);
    %seqPosHash = addSeqPosObj(\%seqPosHash,$pos,$xCoord,$yCoord);
    $pos++;
  }

  # Add 5' and 3' labels
  my $fiveCoordx  = ($seqPosHash{0}->{'X'})+4;
  my $fiveCoordy  = ($seqPosHash{0}->{'Y'})-15;
  my $threeCoordx = ($seqPosHash{$pos-1}->{'X'})+4;
  my $threeCoordy = ($seqPosHash{$pos-1}->{'Y'})-15;
  
  my $fivePrime = $seqElements->text(
                                 id     => 'fivePrime',
                                 x      => $fiveCoordx,
                                 y      => $fiveCoordy,
                                 style  => { 'font-family'  => "Arial,Helvetica",
                                             'font-size'    => "10px",
                                             'text-ancor'   => "middle",
                                             'fill'         => "dimgrey" } )->cdata("5'");
 
  my $threePrime = $seqElements->text(
                                 id     => 'threePrime',
                                 x      => $threeCoordx,
                                 y      => $threeCoordy,
                                 style  => { 'font-family'  => "Arial,Helvetica",
                                             'font-size'    => "10px",
                                             'text-ancor'   => "middle",
                                             'fill'         => "dimgrey" } )->cdata("3'");

  # Create a new SVG group object which will contain the annotation circles
  my $tmpSVGObj = SVG->new;
  my $circleElementsGroup=$tmpSVGObj->group(id => 'circleElements');

  # Add the annotation element for each position
  my $tag;
  foreach my $key (sort {$a<=>$b} keys %seqPosHash) {
    unless ($annotationColour{$key}  eq  '-') {
      $tag = $circleElementsGroup->circle(   cx   =>   $seqPosHash{$key}->{'X'},
                                             cy   =>   $seqPosHash{$key}->{'Y'},
                                             r    =>   7,
                                             style=>   { 'fill-opacity'=> 1,
                                                         'fill'=>"rgb($annotationColour{$key})"});
    }
  }
  
  # Place the groups in the correct position within the SVG DOM
  $seqElementParent->insertBefore($circleElementsGroup, $outlineElements);
  
  if (defined $SVGlegend) {
    $SVGfirstChild->insertBefore($legendElements, $seqElementParent);
  }

  # Remove the old script element
  $SVGfirstChild->removeChild($scriptElement);

  # Add the new script element
  my $newScriptElement=$svg->script(-type=>'text/ecmascript');
  $newScriptElement->CDATA(qq|
        var shown = 1;
        function click() {
             var seq = document.getElementById("seq");
             var lines = document.getElementById("outline");
             var pairs = document.getElementById("pairs");         
   
             if (shown==1) {
               seq.setAttribute("style", "font-family: Arial,Helvetica; visibility: hidden");
               lines.setAttribute("style", "fill: none; stroke: black; stroke-width: 0.75; visibility: visible");
               pairs.setAttribute("style", "fill: none; stroke: black; stroke-width: 0.75; visibility: visible");

               shown = 2;
             } else if (shown==2) {
               seq.setAttribute("style", "font-family: Arial,Helvetica; visibility: visible");
               pairs.setAttribute("style", "fill: none; stroke: black; stroke-width: 0.75; visibility: hidden");
               lines.setAttribute("style", "fill: none; stroke: black; stroke-width: 0.75; visibility: hidden");

               shown = 3;
             } else if (shown==3) { 
               seq.setAttribute("style", "font-family: Arial,Helvetica; visibility: visible");
               lines.setAttribute("style", "fill: none; stroke: black; stroke-width: 0.75; visibility: visible");
               pairs.setAttribute("style", "fill: none; stroke: black; stroke-width: 0.75; visibility: visible"); 
               shown = 1;
             }
         }
  |);

  my $onclickRect = $SVGfirstChild->getFirstChild();
  $onclickRect->setAttribute('width', '700');
  $onclickRect->setAttribute('height', '550');

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
