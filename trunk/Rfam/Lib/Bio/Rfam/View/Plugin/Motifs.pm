package Bio::Rfam::View::Plugin::Motifs;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

use IO::File;
use File::Path;
use File::Slurp;
use Cwd;

use Bio::Rfam::MotifMatch;
use Bio::Rfam::MotifIO;
use RfamLive;

use SVG;
use SVG::Parser;

use IO::Compress::Gzip qw(gzip $GzipError);

has foo => (
  is  => 'rw',
  isa => 'Int'
);
 
sub process {
  my $self = shift;
  $self->findMotifs;
}


# Main Subroutine that peforms scan etc
sub findMotifs {
	my ($self) = @_;
	
	my $rfamdb      = $self->parent->config->rfamlive;
	my $rfam_acc    = $self->parent->family->DESC->AC;
        my $originalCWD = cwd;

        # Confirm the family exists in the database
        my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );
        if (!defined($famRow)) {
                croak ("Failed to find entry in the Family table for $rfam_acc.");
        }
	
	# Set/make locations 
        my $location    = "/homes/evan/motifs_results";
	my $results_loc = "$location/$rfam_acc";
        File::Path::make_path($results_loc);
        my $SEED        = "$results_loc/SEED";
        my $CMdb	= "/nfs/production/xfam/rfam/MOTIFS/cmdb/CM";        

        # Write the seed to file
	my $msa = $self->parent->family->SEED;
        $msa->write_msa($SEED);
	
        # Values used in the filtering of the SEED and inclusion cutoff
        my $fractionMotifs   = 0.1;
        my $minNumberHits    = 2; 
        my $idf              = 0.9;

        # Perform filtering, calculate weights & generate an array of seq ids that pass filter
        my ($filtSEED, $weights, $sum)=stockholm2filteredSTK($SEED,$idf,$results_loc);
        my $filtSeqIDs = "$results_loc/filtSeqIDs";
        my @filtSeqIDs=stockholm2SeqIDs($filtSeqIDs, $filtSEED);
       
        # Create an array and a hash containing the accession and label respectivly of each motif in the database
        my (@allMotifs, %motifLabels, %taken);
        my @completeMotResultSet = $rfamdb->resultset('Motif')->all();
        foreach my $mot_rs (@completeMotResultSet) {
          my $motif_acc   = $mot_rs->get_column('motif_acc');
          my $motif_id    = $mot_rs->get_column('motif_id');
          $motifLabels{$motif_acc} = assign_motif_label($motif_id,\%taken) if not defined $motifLabels{$motif_acc};
          $taken{$motifLabels{$motif_acc}}=1;
          push(@allMotifs, $motif_acc);
        }
        my %revMotifLabels = reverse %motifLabels;

        # Run cmscan on the SEED
        my $cmscanOpts  = " --cpu 4 --max --toponly --verbose --cut_ga --tblout $results_loc/TBL -o $results_loc/cmscan "; 
        my $infernal    = $self->parent->config->infernalPath;
        my $command     = $infernal . "cmscan";
        my $arg = $command . $cmscanOpts .  $CMdb ." ". $SEED;
        system($arg) == 0 
          or die "System $arg failed: $?";
        
        # Parse the TBL, and calculate statistics on those seq ids which make up the filtered SEED
        my $TBL = "$results_loc/TBL";
        my @filteredMoitfMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, \@allMotifs, \@filtSeqIDs,$rfamdb);

        # Perform Calculations on filtered matches and print the results to filtered outlist
        my %filtMotifHash = motifMatchCalcs(\@filteredMoitfMatches, \@completeMotResultSet, $filtSEED);
        printMotifOutlist(\%filtMotifHash, "filt.outlist", $results_loc, $rfam_acc);
        
        # Create a list of motifs that fufill the heuristic criteria for accepting matches
        my @acceptedMotifs;
        foreach my $motif (keys %filtMotifHash) {
          if ($filtMotifHash{$motif}->{NUM_HITS} >= $minNumberHits && $filtMotifHash{$motif}->{FREQ_HITS} >= $fractionMotifs) {
           push (@acceptedMotifs, $motif);
          }
        }

        # Parse the TBL, and calculate statisitcs of accepted motif matched in the full SEED
        my $allSeqIDs = "$results_loc/allSeqIDs";
        my @allSeqIDs = stockholm2SeqIDs($allSeqIDs, $SEED); 
        unlink($allSeqIDs);
        my @acceptedMotifMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, \@acceptedMotifs, \@allSeqIDs, $rfamdb);

        # Calculate statistics, print summary and send allowed matches into RfamLive
        my %acceptedMotifHash = motifMatchCalcs(\@acceptedMotifMatches, \@completeMotResultSet, $SEED);
        printMotifOutlist(\%acceptedMotifHash, "allow.outlist", $results_loc, $rfam_acc);

        foreach my $motifMatchObj (@acceptedMotifMatches) {
          my $guard = $rfamdb->txn_scope_guard;
          $rfamdb->resultset('MotifMatch')->find_or_createFromMotifMatchObj($motifMatchObj);
          $guard->commit;
        }
 
        # Calculate statistics, print summary of ALL matches (no filters/min matches etc - EVERY motif vs EVERY seq)
        my @completeMoitfMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, \@allMotifs,\@allSeqIDs, $rfamdb);
        my %completeMotifHash = motifMatchCalcs(\@completeMoitfMatches, \@completeMotResultSet, $SEED);
        printMotifOutlist(\%completeMotifHash, "complete.outlist", $results_loc, $rfam_acc);
 
        # Create an array of hashes for the markup
        my %f2=matchObjects2hash(\@acceptedMotifMatches, \%motifLabels);

        # Markup the seed alignment with the accepted motifs
        markup($SEED, \%f2, \@acceptedMotifs, \%motifLabels, \%completeMotifHash, $results_loc);

        my $anno_msa = Bio::Easel::MSA->new({ fileLocation => "$results_loc/annotated.SEED" });
        $anno_msa->remove_rf_gap_columns();

        $anno_msa->write_msa("$results_loc/gapless.annotation");

        system("esl-alimanip --outformat pfam $results_loc/gapless.annotation > $results_loc/gaplessAnnotationPfam");

        my $alnLength = $anno_msa->alen(); 
     
        # Create a hash of the positions where each motif matches in the gapless SEED
        my %motifCoords;
        my @annotationArray; 
        foreach my $motifID (@acceptedMotifs) {
          my $motifLabelFromID = $motifLabels{$motifID};
          @annotationArray = ();
          @annotationArray = (0) x $alnLength;
          $motifCoords{$motifLabelFromID}=[@annotationArray];
        }

        

        open(F, "$results_loc/gaplessAnnotationPfam") or die "FATAL: could not open $results_loc/gaplessAnnotationPfam \n[$!]";
        while (my $w = <F>) {
          if($w=~/(#=GR\s[A-Za-z0-9\-.\/]+\s+MT.\d+\s+)([0-9a-zA-Z.]+)/i) {
            my $annotationString = $2;
            my $counter = 0;
            my @annotation_chars = split //, $annotationString;
            foreach my $annotation_char (@annotation_chars) {
              unless ($annotation_char eq '.') {
                $motifCoords{$annotation_char}->[$counter] += 1;
              } 
            $counter++;
            }
          }
        } 

        # Generate the unannotated SS image
        my $MIS = $anno_msa->most_informative_sequence();
        my $SScons = $anno_msa->get_ss_cons_dot_parantheses();
        my $RNAplot = "$results_loc/rnaplot";
        my $RNAplot_img = "$results_loc/$rfam_acc"."_ss.svg"; 
        open(my $fh, ">", "$RNAplot" ) or die "$RNAplot";
        print $fh (">$rfam_acc\n$MIS\n$SScons\n");
        close $fh;

        chdir($results_loc);

        use IPC::Run qw(run);
        my @cmd2 = ("/nfs/production/xfam/rfam/software/bin/RNAplot", "-o", "svg");
        run \@cmd2, '<', $RNAplot;

        unless(-e $RNAplot_img) {
          die ("Error in creating original RNA SVG image \n")
        }

        # Assign a colour for each motif at each position which represents the fraction 
        # of sequences at that postion which contain the motif
        
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
        
        my $numSeqs = $anno_msa->nseq();
        my $annotationArrayRef;
        my $fractionValue;
        my $colourValue;
        foreach my $coords (keys %motifCoords) {
          my $motifAccession = $revMotifLabels{$coords};
          my $counter = 0;
          $annotationArrayRef = $motifCoords{$coords};
          my @annotationAR = @$annotationArrayRef;
          my %fractionSeqPosWithMotif;
          my %colourSeqPosWithMotif;  
          foreach my $posValue (@annotationAR) {
            $fractionValue = ($posValue/$numSeqs);
            if ($fractionValue > 1.0) {$fractionValue = 1.0};
            if ($fractionValue < 0.0) {$fractionValue = 0.0}; 
            $colourValue = $colours{int((($fractionValue*36.0)+1)+0.5)};
            $fractionSeqPosWithMotif{$counter}=$fractionValue;
            if ($fractionValue == 0) {
              $colourSeqPosWithMotif{$counter}='-';
            }
            else {
              $colourSeqPosWithMotif{$counter}=$colourValue;
            }
            $counter++;
          }
          # Create the individual motif images, zip them and store them in the database
          my $motifLegendLabel = $acceptedMotifHash{$motifAccession}->{MOTIF_ID};
          my $motifSVGlegend = SVGLegend("Frac. Seed Seq. with Motif", 0, 1, \%colours,$motifLegendLabel,$rfam_acc);
          my $motifSVGobj = annotateSVG($RNAplot_img, \%colourSeqPosWithMotif,$motifSVGlegend);
          my $motifHandle;
          open ($motifHandle, '>', "$results_loc/$rfam_acc.$motifAccession.svg") or die ("Unable to open $motifHandle");
          print $motifHandle $motifSVGobj;
          close ($motifHandle) or die ("Unable to close $motifHandle");
          my $fileGzipped;
          
          open ($motifHandle, '<', "$results_loc/$rfam_acc.$motifAccession.svg") or die ("Unable to open $motifHandle");
          gzip $motifHandle => \$fileGzipped;
          my $imageResultset= $rfamdb->resultset('MotifSecondaryStructureImage')->update_or_create(
                                                                    {rfam_acc => $rfam_acc,
                                                                     motif_acc => $motifAccession,
                                                                     image => $fileGzipped});

          close ($motifHandle) or die ("Unable to close $motifHandle");
      }

      # Add the summary statistics (allow.outlist) to the database
      foreach my $motifHashKey (keys %acceptedMotifHash) {
        my $numHits   = $acceptedMotifHash{$motifHashKey}{NUM_HITS};
        my $freqHits  = sprintf("%.3f", $acceptedMotifHash{$motifHashKey}{FREQ_HITS});
        my $sumBits   = sprintf("%.3f", $acceptedMotifHash{$motifHashKey}{SUM_BITS});
        my $avgWBits  = sprintf("%.3f", $acceptedMotifHash{$motifHashKey}{AVG_W_BITS});
        my $motifAcc  = $motifHashKey;
        if ($numHits > 0) {
          my $resultset = $rfamdb->resultset('MotifFamilyStat')->update_or_create(
			 					{rfam_acc => $rfam_acc,
                                                                 motif_acc => $motifAcc,
                                                                 num_hits => $numHits,
                                                                 frac_hits => $freqHits,
                                                                 sum_bits => $sumBits,
                                                                 avg_weight_bits => $avgWBits});

        }
      }

      # Unlink the tmp directory after results have been inserted into the database
     #chdir $originalCWD;
     #File::Path::remove_tree($location);
}


##############

sub SVGLegend {
  my ($legend_name,$min_value,$max_value,$colours,$motif_id,$family_id) = @_;

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
                                                            'text-anchor'  => "left",
                                                            'fill'         => "dimgrey" } )->cdata($max_value);
  my $legendBox = $legendElementsGroup->group(id => 'legendBoxElements',
                                              x  =>  0,
                                              y  =>  0 );


  my $motifID = $legendElementsGroup->text(
                                                id     => 'motifID',
                                                x      => 550,
                                                y      => 15,
                                                style  => { 'font-family'  => "Arial,Helvetica",
                                                            'font-size'    => "18px",
                                                            'text-anchor'  => "middle",
                                                            'fill'         => "dimgrey" } )->cdata($motif_id);
  
  my $familyID = $legendElementsGroup->text(
                                                id     => 'familyID',
                                                x      => 550,
                                                y      => -10,
                                                style  => { 'font-family'  => "Arial,Helvetica",
                                                            'font-size'    => "18px",
                                                            'text-anchor'  => "middle",
                                                            'fill'         => "dimgrey" } )->cdata($family_id);


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
  # The default from RNAplot is 452 x 452  so we increase it to 700 x 550
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
                                             'text-anchor'  => "middle",
                                             'fill'         => "dimgrey" } )->cdata("5'");

  my $threePrime = $seqElements->text(
                                 id     => 'threePrime',
                                 x      => $threeCoordx,
                                 y      => $threeCoordy,
                                 style  => { 'font-family'  => "Arial,Helvetica",
                                             'font-size'    => "10px",
                                             'text-anchor'  => "middle",
                                             'fill'         => "dimgrey" } )->cdata("3'");

  # Create a new SVG group object which will contain the annotation circles
  my $tmpSVGObj = SVG->new;
  my $circleElementsGroup=$tmpSVGObj->group(id => 'circleElements');

  # Add the annotation element for each position
  my $tag;
  foreach my $key (sort {$a<=>$b} keys %seqPosHash) {
    unless ($annotationColour{$key}  eq  '-') {
      $tag = $circleElementsGroup->circle(   cx     =>   $seqPosHash{$key}->{'X'},
                                             cy     =>   $seqPosHash{$key}->{'Y'},
                                             r      =>   7,
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

  my $svgXML = $svg->xmlify();

return $svgXML;
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


#---------------------------------------------------------------------------------------
# Perform calculations on an array of match objects, return a hash containing the results
sub motifMatchCalcs {
  my ($motifMatchObjects, $completeMotifResultSet, $stockholm) = @_; 

  my ($weights, $sum) = stockholm2weights($stockholm);
  my $num_seq = stockholmSeqStats($stockholm); 

  my %motifHash;
  my %seqIDHash;
  foreach my $mot (@$completeMotifResultSet) {
    my $motif_acc = $mot->get_column('motif_acc');
    $motifHash{$motif_acc} = { NUM_HITS => 0, FREQ_HITS => 0, SUM_BITS => 0, AVG_W_BITS => 0, MOTIF_ID => $mot->get_column('motif_id')};
    $seqIDHash{$motif_acc} = {};
  }
  

  foreach my $motifMatchObj (@$motifMatchObjects) {
    my $seq_id = $motifMatchObj->RFAMSEQ_ID;
    my $motif_acc = $motifMatchObj->MOTIF_ACC;
    print "Match between $motif_acc and $seq_id \n";
    unless (defined $seqIDHash{$motif_acc}{$seq_id}) {$seqIDHash{$motif_acc}{$seq_id}=0;}
    if (!$seqIDHash{$motif_acc}{$seq_id}==1) {
      $motifHash{$motif_acc}->{NUM_HITS}+=1;
      $seqIDHash{$motif_acc}{$seq_id}=1;
    }

    $motifHash{$motif_acc}->{SUM_BITS}+=sprintf("%.3f",$motifMatchObj->BIT_SCORE);
    my $seq_weight;
    if (defined $weights->{$seq_id}) {
      $seq_weight = $weights->{$seq_id};
    }
    else { $seq_weight = 1.0 }
    if ($sum == 0) { $sum = 1.0}
    my $avg_w_bits=sprintf("%.3f",(($seq_weight*$motifMatchObj->BIT_SCORE)/$sum));
    $motifHash{$motif_acc}->{AVG_W_BITS}+=sprintf("%.3f",$avg_w_bits);
  }
  
  foreach my $motif (keys %motifHash) {
    my $numHits = ($motifHash{$motif}->{NUM_HITS}); 
    my $freq=$numHits/$num_seq;
    $motifHash{$motif}->{FREQ_HITS}=sprintf("%.3f",$freq);
  }
  return %motifHash;
}
#----------------------------------------------------------------------------------------
# Create a file containing a table with statistics of the motif matches 
sub printMotifOutlist {
  my ($motifsHash, $outlistFname, $outlistDir,$rfam_acc) = @_;

  my $outlist = "$outlistDir/$outlistFname";

  open (my $outlistHandle, '>', $outlist) or die "Could not open file $outlist $!";
 
  printf $outlistHandle (    "%-12s %-12s %12s %15s %20s %25s\n", 
                             "rfam acc", 
                             "motif acc", 
                             "number hits", 
                             "fraction hits", 
                             "sum bits", 
                             "average weighted bits");
  
  foreach my $motif (keys %$motifsHash) {
    if ($motifsHash->{$motif}->{NUM_HITS} >= 1) {
      printf $outlistHandle (  "%-12s %-12s %12s %15s %20s %25s\n", 
                               $rfam_acc, $motif, 
                               $motifsHash->{$motif}->{NUM_HITS}, 
                               $motifsHash->{$motif}->{FREQ_HITS}, 
                               $motifsHash->{$motif}->{SUM_BITS}, 
                               $motifsHash->{$motif}->{AVG_W_BITS});
    }
  }
close $outlistHandle;
}
  

#----------------------------------------------------------------------------------------
# Parse the TBL out to an array of match objects
sub parseTBL2MotifMatchObj{
  my ($TBL, $rfam_acc, $motifs, $seqids, $rfamlive) = @_;
  my @MotifMatchObjs;
  
  open(TBL, "grep -v ^'#' $TBL | sort -nrk 15 | ") or die "FATAL: could not open pipe for reading $TBL\n[$!]";

  while (my $tblline = <TBL>) {
    my @tblA = split(/\s+/, $tblline);
    my (   $motif_name, 
           $rfamseq_id, 
           $CMstart, 
           $CMend, 
           $qStart, 
           $qEnd, 
           $strand, 
           $trunc, 
           $bits, 
           $evalue ) = ($tblA[0], $tblA[2], $tblA[5], $tblA[6], $tblA[7], $tblA[8], $tblA[9], $tblA[10], $tblA[14], $tblA[15]); 
  
    my $motif_acc_rs = $rfamlive->resultset('Motif')->search({ motif_id => $motif_name });
    my $new_motif_acc = $motif_acc_rs->get_column('motif_acc')->single(); 

    if (grep {$_ eq $new_motif_acc} @$motifs) {
     if (grep {$_ eq $rfamseq_id} @$seqids) {
    
      # Split rfamseq id into acc, start and stop
      my ($rfamseq_acc, $rfamseq_start, $rfamseq_stop);
      if ($rfamseq_id =~ /(\S+.\d)\/(\d+)-(\d+)/) {
        $rfamseq_acc    = $1; 
        $rfamseq_start  = $2;
        $rfamseq_stop   = $3;
      }

      # Create the match object
      my $motifMatchObj = Bio::Rfam::MotifMatch->new;
      $motifMatchObj->MOTIF_ACC($new_motif_acc);
      $motifMatchObj->RFAM_ACC($rfam_acc);
      $motifMatchObj->RFAMSEQ_ID($rfamseq_id);
      $motifMatchObj->RFAMSEQ_ACC($rfamseq_acc);
      $motifMatchObj->RFAMSEQ_START($rfamseq_start);
      $motifMatchObj->RFAMSEQ_STOP($rfamseq_stop);
      $motifMatchObj->QUERY_START($qStart);
      $motifMatchObj->QUERY_STOP($qEnd);
      $motifMatchObj->MOTIF_START($CMstart);
      $motifMatchObj->MOTIF_STOP($CMend);
      $motifMatchObj->E_VALUE($evalue);
      $motifMatchObj->BIT_SCORE($bits);

      push(@MotifMatchObjs, $motifMatchObj);
      }
    }
  }
  return @MotifMatchObjs;
}

#-------------------------------------------------------------------------------------------
# Assign labels to each motif to be used in markup of the SEED
sub assign_motif_label {
  my ($motif_id, $taken)=@_;
  my @chars = ("A".."Z","a".."z","0".."9");
  my @motif_ids = split(//,$motif_id);

  foreach my $c (@motif_ids,@chars) {
    next if ($c !~ /^[a-zA-Z0-9]*$/);
    return $c if (not defined $taken->{$c});
    }
}

#-----------------------------------------------------------------------------------------
# Creates an array of hashes from motif match objects to be used in the markup sub.
# This is not elegant and is only done to enable easy intergration of Paul's markup code
# which is not easy to decifer, however it does function.

sub matchObjects2hash {
  my ($arrayMatchObjs, $motifLabels) = @_; 
 
  my %f2;
  foreach my $motifMatchObj (@$arrayMatchObjs) {
    my $motifMatchSeqId = $motifMatchObj->RFAMSEQ_ID;
    
    my $seqid       = $motifMatchSeqId;
    my $start       = $motifMatchObj->QUERY_START;
    my $end         = $motifMatchObj->QUERY_STOP;
    my $strand      = 1;
    my $score       = $motifMatchObj->BIT_SCORE;
    my $evalue      = $motifMatchObj->E_VALUE;
    my $f_motif_acc = $motifMatchObj->MOTIF_ACC;
    my $label       = $motifLabels->{$f_motif_acc};

    if( $end < $start ) {
      ( $start, $end ) = ( $end, $start );
      $strand = -1;
    }

    my %f = (  seqid           => $seqid,
               start           => $start,
               end             => $end,
               strand          => $strand,
               score           => $score,
               evalue          => $evalue,
               motif_acc       => $f_motif_acc,
               label           => $label );

  push( @{ $f2{$seqid} }, \%f );
  }
  return %f2;
}

#----------------------------------------------------------------------------------------
# Markup the seed with the accepted motifs - adapted from Pauls markup in rmfam_scan.pl
sub markup {
  my ($seed, $features, $acceptedMotifs, $motifLabels, $motifHash, $dir) = @_;
  
  my (@seqCoords2alnCoords, %positions2seqid, %seqid2positions, %motifLines, %motiffedSeqLineNumbers, $firstSeqLine);
  my $cnt = 0;

  my $alnLength = compute_length_of_alignment($seed);
  open (F, "esl-reformat -ru --mingap pfam $seed | ") or die "FATAL: could not open pipe for reading $seed\n[$!]"; 
  my @stk =<F>;

  foreach my $stk (@stk){
    if($stk=~/^(\S+)\s+\S+$/){
      $positions2seqid{$cnt}=$1;
      $seqid2positions{$1}=$cnt;
      $firstSeqLine=$cnt if not defined $firstSeqLine;
    }
    $cnt++;
  }
  
  foreach my $seqid (keys %seqid2positions) {
    if(defined $features->{$seqid}){   
      my $fpos=0;
      foreach my $f ( @{$features->{$seqid}} ){
        my $alnSeq=$stk[$seqid2positions{$seqid}];
        if($alnSeq=~/^(\S+)\s+(\S+)$/){
          my ($lid,$lseq)=($1,$2);  
          if($lid ne $seqid){
	    print "WARNING: seqId:[$seqid] and alnSeq:[$alnSeq] don't match!\n";
            next;
	  }
          $alnSeq=$lseq; 
          my @alnSeq = split(//, $alnSeq);
          if(scalar(@alnSeq) != $alnLength){
	    printf "WARNING: the lengths [$alnLength]!=[%d] computed from seqId [$seqid] and alnSeq:\n[$alnSeq]\ndon't match! [CHECK: could be a gap-only column]\n", scalar(@alnSeq);
          }
          my ($aCnt,$sCnt)=(0,0); 
          foreach my $as (@alnSeq){
            if(is_nucleotide($as)){
              $seqCoords2alnCoords[$sCnt]=$aCnt;
              $sCnt++;
            }
            $aCnt++;
          }
        } 
        else { printf "WARNING: line number [%d] is meant to correspond to $seqid! Check the formatting.\n", $seqid2positions{$seqid};
        }
        my$mtCnt=0;
        my ($start,$end, $char, $rmfamid) = ($f->{'start'}, $f->{'end'}, $f->{'label'}, $f->{'motif_acc'});
        for (my $gpos=0; $gpos<$fpos; $gpos++){
          my $g = ${ $features->{$seqid} }[$gpos];
          $mtCnt++ if ( overlap($start,$end, $g->{'start'},$g->{'end'}) );
        }
        $motifLines{$seqid}[$mtCnt] = '.' x $alnLength if not defined $motifLines{$seqid}[$mtCnt];
        for(my $mpos=$start-1; $mpos<$end; $mpos++){
          my $aCoord = $seqCoords2alnCoords[$mpos];
         substr($motifLines{$seqid}[$mtCnt],$aCoord,1)=$char;
        }
        $fpos++;
      }
      $motiffedSeqLineNumbers{$seqid2positions{$seqid}}=$seqid if defined($motifLines{$seqid}[0]);
    }
  }

  my $outFile="$dir/tmp.annotated.SEED";
  my $fileh = IO::File->new();
  $fileh->open("> $outFile");
  for(my $ii=0; $ii<scalar(@stk); $ii++){
    if ($ii == $firstSeqLine-1){
      foreach my $l (sort {$motifLabels->{$a} cmp $motifLabels->{$b}} keys %{$motifLabels}){
        if (grep { $l eq $_} @$acceptedMotifs){
          my $labMotifID = $motifHash->{$l}->{MOTIF_ID};
          printf $fileh "#=GF MT.%s   %s   %s\n", $motifLabels->{$l}, $l, $labMotifID; 
        }
      }
    }
    print $fileh $stk[$ii];
    if (defined $motiffedSeqLineNumbers{$ii} && defined $positions2seqid{$ii}){
      my $mCnt=0;
      foreach my $mt (@{$motifLines{$positions2seqid{$ii}}}){
        printf $fileh "#=GR %s MT.$mCnt %s\n", $motiffedSeqLineNumbers{$ii}, $mt if (defined $mt);
        $mCnt++
      }
    }
  }
  my $annotatedSEED="$dir/annotated.SEED";
  system "esl-reformat stockholm $outFile > $annotatedSEED";
  unlink($outFile);
}

 
#-----------------------------------------------------------------------------------------
sub overlap {
  my($x1, $y1, $x2, $y2) = @_;
  if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
    return 1;
  }
  else {
    return 0;
  }
}
#-----------------------------------------------------------------------------------------
sub stockholm2filteredSTK {
    my ($infile,$idf,$fam_dir) = @_;
    my $filtSTK = "$fam_dir"."/filtered.SEED";
    system ("esl-weight -f --idf $idf $infile > $filtSTK") == 0 or die "FATAL: failed to run [esl-weight -f --idf $idf $infile > $filtSTK]";
    my ($weights,$sum) = stockholm2weights($filtSTK); 
    return ($filtSTK,$weights,$sum);
}
#-----------------------------------------------------------------------------------------
sub stockholm2weights {
    my $infile = shift;
    my $sum=0;
    my %weights;
    open(F, "esl-weight -g $infile | ") or die "FATAL: could not open pipe for [esl-weight -g $infile]\n[$!]";
    while(my $w = <F>){
	if($w=~/^#=GS\s+(\S+)\s+WT\s+(\S+)/){
	    $weights{$1}=$2; 
	    $sum+=$2;
	}
    }
return \%weights, $sum;
}
#-----------------------------------------------------------------------------------------
sub stockholmSeqStats{
  my $infile = shift;
  my $seq_num;
  open(F, "esl-seqstat $infile | ") or die "FATAL: could not open pipe [esl-seqstat $infile]\n[$!]";
  while (my $w = <F>) {
    if($w=~/(^Number of sequences:)(\s+)(\d+)/) {
      $seq_num = $3;
     }
  }
  return $seq_num;
}
#----------------------------------------------------------------------------------------
sub stockholm2SeqIDs{
  my ($seqIDsList, $seed) = @_;  
  my @seqIDs;
  system("esl-alistat --list $seqIDsList $seed") == 0 or die "System esl-alistat failed: $?";
  open my $seqIDsHandle, $seqIDsList or die "Could not open $seqIDsList: $!";
  while (  my $line = <$seqIDsHandle>) {
           chomp($line);
           push(@seqIDs, $line);
  }
  return @seqIDs; 
}
#-----------------------------------------------------------------------------------------
sub compute_length_of_alignment {    
  my $file = shift;
  my $alnLength;
  open(ALI,"esl-alistat --rna $file |") or die "FATAL: could not open [esl-seqalistat $file] pipe:[$!]";
  while(<ALI>) {
    if (/^Alignment length:\s+(\d+)/){ 
      $alnLength=$1;
    }
  }
  close(ALI);
  print "WARNING: alnLength is undefined for [esl-alistat $file]!" if not defined $alnLength;
  return $alnLength;
}

#-----------------------------------------------------------------------------------------
sub is_nucleotide {
  my $a = shift;
  if (defined($a)){
    $a =~ tr/a-z/A-Z/;
  }  
  if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
    return 1;}
  else {return 0;}
}
#-----------------------------------------------------------------------------------------

1;
