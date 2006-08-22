
# Simap.pm
# jt6 20060503 WTSI
#
# Controller to build a set of graphics for a given UniProt entry.
#
# $Id: Simap.pm,v 1.5 2006-08-22 13:36:43 rdf Exp $

package PfamWeb::Controller::Protein::Simap;

use strict;
use warnings;
use Data::Dumper;
use Storable qw(thaw);
use Time::HiRes qw( gettimeofday );

use Bio::Pfam::AlignPfam;
use Bio::Pfam::WebServices::Client::Simap;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

# extend the Protein class. This way we should get hold of the pfamseq
# data by default, via the "begin" method on Protein
use base "PfamWeb::Controller::Protein";

#-------------------------------------------------------------------------------
# do something with the list of DAS sources that were specified by the
# user through the list of checkboxes

# pick up a URL like http://localhost:3000/proteingraphics?acc=P00179

sub getSimapData : Path('/getsimapdata') {
  my( $this, $c ) = @_;

  $c->log->debug( "Protein::Simap::getSimapData listing parameters:" );

  my $seqAcc = $c->stash->{pfamseq}->pfamseq_acc;
  my $length = $c->stash->{pfamseq}->length;
  my $seqStorable = $c->stash->{pfamseq}->annseq;
  my @seqs;
  push(@seqs, thaw($seqStorable->annseq_storable));
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layoutPfam->scale_x(1);
  my %regionsAndFeatures = ( "PfamA"      => 1,
                             "PfamB"      => 1,
                             "noFeatures" => 0 );
  $layoutPfam->layout_sequences_with_regions_and_features(\@seqs, \%regionsAndFeatures);
  my $drawingXML = $layoutPfam->layout_to_XMLDOM;

  $c->log->debug( "\n\n\n proxy is set to: |".$this->{simapProxy}."| \n\n\n\n" );
#	if defined $this->{simapProxy};

  my $simap = Bio::Pfam::WebServices::Client::Simap->new(
							 '-proxy'      => $this->{simapProxy},
							 '-md5'        => $c->stash->{pfamseq}->md5,
							 '-maxHits'    => 50,
							 '-minSWscore' => 1,
							 '-maxEvalues' => 0.001,
							 '-database'   => [qw/313 314/],
							 '-showSeq'    => 1,
							 '-showAli'    => 0);
  $simap->queryService;
  $simap->processResponse4Website($drawingXML, $c->stash->{pfamseq});
  my $pfamaln = $simap->hits2Ali;
 
  
  #Now Parse the alignment
  #my $pfamaln = new Bio::Pfam::AlignPfam->new;
  #$pfamaln->read_msf($ali);
  #open(ALIGN, "fa.sto") || die "Could not open ALIGN:[$1]\n";
  #$pfamaln->read_stockholm(\*ALIGN);


  #Remove any gaps in the alignment
  my $pos=0;
  foreach my $seq ( $pfamaln->each_seq() ){
    $pos++;
    last if($seqAcc eq $seq->id);
  }
  $pfamaln->splice_by_seq_pos($pos);

  die "Length of alignment duff\n" unless ($pfamaln->length == $c->stash->{pfamseq}->length);


  #Calulate the average ID per column.
  my ($idPerCol, $averageId, $residuesPerCol) = $pfamaln->average_percentage_identity_per_column;
  my $imageNode = $drawingXML->documentElement;
  my $graph = $drawingXML->createElement( "graph" );
  $imageNode->appendChild($graph);
  $graph->setAttribute("length",  $c->stash->{pfamseq}->length);
  $graph->setAttribute("height",  150);
  $graph->setAttribute("graphType","mixed");
  if($length < 100){
    $graph->setAttribute("showLegend", "0");
  }else{
    $graph->setAttribute("showLegend", "1");
  }
  $graph->setAttribute("title", "% Identity of homologs");
  #Set up the y-axis
  my $axisY = $drawingXML->createElement( "axisY" );
  $graph->appendChild($axisY);
  $axisY->setAttribute("label", "% identity");
  #$axisY->setAttribute("colour", "offblack1");

  #Set up the x-axis 
  my $axisX = $drawingXML->createElement( "axisX" );
  $graph->appendChild($axisX);
  $axisX->setAttribute("label", "residue");
  #$axisX->setAttribute("colour", "offblack1");
  
  #Set up the data
  my $graphData = $drawingXML->createElement( "graphData" );
  $graph->appendChild($graphData);
  my $dataSeries1 = $drawingXML->createElement( "dataSeries" );
  $graphData->appendChild($dataSeries1);
  $dataSeries1->setAttribute("label", "% Residue Identity");
  $dataSeries1->setAttribute("colour", "green");
  $dataSeries1->setAttribute("type", "area");
  
  my $dataSeries2 = $drawingXML->createElement( "dataSeries" );
  $graphData->appendChild($dataSeries2);
  $dataSeries2->setAttribute("label", "Average % Residue Identity");
  $dataSeries2->setAttribute("colour", "blue");
  $dataSeries2->setAttribute("type", "lines");
 
  my $dataSeries3 = $drawingXML->createElement( "dataSeries" );
  $graphData->appendChild($dataSeries3);
  $dataSeries3->setAttribute("label", "% Residue Aligned");
  $dataSeries3->setAttribute("colour", "red");
  $dataSeries3->setAttribute("type", "lines");
 
  
  for( my $r = 1; $r < @$idPerCol; $r++){
    my $d1 = $drawingXML->createElement( "data" );
    $dataSeries1->appendChild($d1);
    $d1->setAttribute("x",$r);
    #Smooth this data over a window of four aa
    my @smooth = @$idPerCol[($r-2)..($r+2)];
    my $smoothTotal = 0;
    my $n = 0;
    foreach (@smooth){
      if($_){
	$smoothTotal += $_;
	$n++;
      }
    }

    my $smoothedId = 0;
    if($n > 0){
      $smoothedId = $smoothTotal/$n;
    }

    $d1->setAttribute("y", $smoothedId);

    my $d2 = $drawingXML->createElement( "data" );
    $dataSeries2->appendChild($d2);
    $d2->setAttribute("x",$r);
    $d2->setAttribute("y", $averageId);
    my $d3 = $drawingXML->createElement( "data" );
    $dataSeries3->appendChild($d3);
    $d3->setAttribute("x",$r);
    $d3->setAttribute("y", $$residuesPerCol[($r-1)]);
  }
  my $pfamImageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $pfamImageset->create_images($drawingXML);
  $c->stash->{imageset} = $pfamImageset;
  $c->log->debug( "|" . $seqAcc ."|" );

}


#-------------------------------------------------------------------------------
# override the end method from Protein.pm, so that we now redirect to
# a template that doesn't require the wrapper

sub end : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Protein::Simap::end: handing off to wrapper-less template..." );

  $c->stash->{template} = "components/blocks/protein/simapGraphics.tt";

  # forward to the class that's got the WRAPPER set to null
  $c->forward( "PfamWeb::View::TTBlock" );

}

1;

