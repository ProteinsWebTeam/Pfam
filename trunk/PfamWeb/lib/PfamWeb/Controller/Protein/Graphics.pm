
# Graphics.pm
# jt6 20060503 WTSI
#
# Controller to build a set of graphics for a given UniProt entry.
#
# $Id: Graphics.pm,v 1.3 2006-05-22 16:47:22 jt6 Exp $

package PfamWeb::Controller::Protein::Graphics;

use strict;
use warnings;

use Data::Dumper;

use Bio::Pfam::Drawing::Layout::DasLayoutManager;

# extend the Protein class. This way we should get hold of the pfamseq
# data by default, via the "begin" method on Protein

use base "PfamWeb::Controller::Protein";

#-------------------------------------------------------------------------------
# do something with the list of DAS sources that were specified by the
# user through the list of checkboxes

# pick up a URL like http://localhost:3000/protein/graphics?acc=P00179

sub updateSources : Path( "/updatesources" ) {
  my( $this, $c ) = @_;

  $c->log->debug( "Protein::Graphics::updateSources: listing parameters:" );

  my $seqAcc = $c->stash->{pfamseq}->pfamseq_acc;

  # get a sequence first...
  my @dsnList = ( "http://das.sanger.ac.uk/das/pfam" );

#  my $dl = PfamWeb::Model::Das_sources->getDasLite;

  my $dl = Bio::DasLite->new( { dsn     => PfamWeb->config->{dasDsn},
								timeout => PfamWeb->config->{dasTo},
								proxy   => PfamWeb->config->{dasProxy}
							  } );

  $dl->dsn( \@dsnList );

  my $sequence = $dl->sequence( $seqAcc );

  # if we don't get a sequence from the Pfam source, all hope is lost...
  return unless $sequence;

  # if we do get a sequence, we can now add the user-specified sources
  # and call those registries to get features.

  # first, reset the sources list
  @dsnList = ();

  # get the sources from the parameter list
  foreach ( sort keys %{$c->req->parameters} ) {

	# we want only the server IDs
	next unless /^DS_\d+$/ and $c->req->param( $_ ) eq "on";
	$c->log->debug( "Protein::Graphics::updateSources: param: |$_|"
					. $c->req->param($_) . "|" );
	my $ds = PfamWeb::Model::Das_sources->find( server_id => $_ );
	push @dsnList, $ds->url;
  }

  $dl->dsn( \@dsnList );
  #my $result = $dl->features( $seqAcc );

   #$c->log->debug( "Protein::Graphics::updateSources: result for |"
 	#			  . $seqAcc . "|: " . Dumper( $result ) );

  my $features = $dl->features( $seqAcc );

  my $layout = Bio::Pfam::Drawing::Layout::DasLayoutManager->new;
  $layout->layout_DAS_sequences_and_features( $sequence, $features );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;

}


#-------------------------------------------------------------------------------
# override the end method from Protein.pm, so that we now redirect to
# a template that doesn't require the wrapper

sub end : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Protein::Graphics::end: handing off to wrapper-less template..." );

  $c->stash->{template} = "components/blocks/protein/loadGraphics.tt";

  # forward to the class that's got the WRAPPER set to null
  $c->forward( "PfamWeb::View::TTBlock" );

}

1;

