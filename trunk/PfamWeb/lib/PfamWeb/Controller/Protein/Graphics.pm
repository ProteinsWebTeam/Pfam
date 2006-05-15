
# Graphics.pm
# jt6 20060503 WTSI
#
# Controller to build a set of graphics for a given UniProt entry.
#
# $Id: Graphics.pm,v 1.1 2006-05-15 12:14:46 jt6 Exp $

package PfamWeb::Controller::Protein::Graphics;

use strict;
use warnings;

use Bio::DasLite;
use Data::Dumper;

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

  my @dsnList;
  foreach ( sort keys %{$c->req->parameters} ) {

	# we want only the server IDs
	next unless( /^DS_\d+$/ and $c->req->param( $_ ) eq "on" );
	$c->log->debug( "Protein::Graphics::updateSources: param: |$_|"
					. $c->req->param($_) . "|" );
	my $ds = PfamWeb::Model::Das_sources->find( server_id => $_ );
	push @dsnList, $ds->url;
  }

  my $dl = PfamWeb::Model::Das_sources->getDasLite;
  $dl->dsn( \@dsnList );
  my $result = $dl->features( $c->stash->{pfamseq_acc} );

  $c->log->debug( "Protein::Graphics::updateSources: result for |" . $c->stash->{pfamseq_acc}
 				  . "|: " . Dumper( $result ) );

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

