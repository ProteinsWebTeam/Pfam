
# Simap.pm
# jt6 20060503 WTSI
#
# Controller to build a set of graphics for a given UniProt entry.
#
# $Id: Simap.pm,v 1.3 2006-08-14 10:44:47 jt6 Exp $

package PfamWeb::Controller::Protein::Simap;

use strict;
use warnings;
use Data::Dumper;
use Storable qw(thaw);
use Time::HiRes qw( gettimeofday );

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

  my $seqStorable = $c->stash->{pfamseq}->annseq;
  my @seqs;
  push(@seqs, thaw($seqStorable->annseq_storable));
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;

  my %regionsAndFeatures = ( "PfamA"      => 1,
                             "PfamB"      => 1,
                             "noFeatures" => 1 );
  $layoutPfam->layout_sequences_with_regions_and_features(\@seqs, \%regionsAndFeatures);
  my $drawingXML = $layoutPfam->layout_to_XMLDOM;

  $c->log->debug( "proxy is set to: |".$this->{simapProxy}."|" )
	if defined $this->{simapProxy};

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

