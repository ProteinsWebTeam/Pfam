
# Graphics.pm
# jt6 20060503 WTSI
#
# Controller to build a set of graphics for a given UniProt entry.
#
# $Id: Graphics.pm,v 1.6 2006-05-26 15:44:37 jt6 Exp $

package PfamWeb::Controller::Protein::Graphics;

use strict;
use warnings;
use Data::Dumper;
use Storable qw(thaw);
use Time::HiRes qw( gettimeofday );

use Bio::Pfam::Drawing::Layout::DasLayoutManager;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
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

  my $dl = PfamWeb::Model::Das_sources->getDasLite;
  $dl->dsn( \@dsnList );
  my $sequence = $dl->sequence( $seqAcc );

  # if we don't get a sequence from the Pfam source, all hope is lost...
  return unless $sequence;

  my @seqs;
  my $seqStorable = $c->stash->{pfamseq}->annseq;
  #$c->log->debug("raw storable = $seqStorable");
  #my $seqThaw = thaw($seqStorable->annseq_storable);
  #$c->log->debug("Storable|".Dumper($seqThaw)."|");
  push(@seqs, thaw($seqStorable->annseq_storable) );
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layoutPfam->scale_x(1);
  my %regionsAndFeatures = ( "PfamA"      => 1,
			     "PfamB"      => 1,
                             "noFeatures" => 1 );
  $layoutPfam->layout_sequences_with_regions_and_features(\@seqs, \%regionsAndFeatures);

  my $pfamImageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $pfamImageset->create_images($layoutPfam->layout_to_XMLDOM);
  my( %maps, $handle, $server, $serverName );
  foreach my $image ( $pfamImageset->each_image ) {
      $image->print_image;
      push(@{$maps{"pfam"} }, { image => $image->file_location,
				map   => $image->image_map,
				server=> "Pfam",
				info  => $image->image_info }); 
  }

  # if we do get a sequence, we can now add the user-specified sources
  # and call those registries to get features.

  # first, reset the sources list
  @dsnList = ();

  # get the default servers from the database table
  unless( $c->req->param( "reload" ) ) {
	my @defaultServers = PfamWeb::Model::Das_sources->search( default_server => 1 );
	$c->log->debug( "found " . scalar @defaultServers . " default servers" );
	foreach ( @defaultServers ) {
	  push @dsnList, $_->url;
	}
  }

  # get the user-specified sources from the parameter list
  foreach ( sort keys %{$c->req->parameters} ) {

	# we want only the server IDs
	next unless /^DS_\d+$/ and $c->req->param( $_ ) eq "on";
	$c->log->debug( "Protein::Graphics::updateSources: param: |$_|"
					. $c->req->param($_) . "|" );
	my $ds = PfamWeb::Model::Das_sources->find( server_id => $_ );
	push @dsnList, $ds->url if defined $ds;
  }

  # give the DasLite object our list of servers and retrieve the
  # features for them
  $dl->dsn( \@dsnList );
  my $features = $dl->features( $seqAcc );

  # hand the features to the layout manager and get it to draw the graphics
  my $layout = Bio::Pfam::Drawing::Layout::DasLayoutManager->new;
  $layout->layout_DAS_sequences_and_features( $sequence, $features );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  # process the generated images and convert the URL that comes back
  # from the layout manager into a simple label, via our database
  # table of server information
  my $i = 0;
  
  foreach my $image ( $imageset->each_image ) {

	( $handle = $image->image_info ) =~ s/^(.*?)\/features\?.*$/$1/;
	$server = PfamWeb::Model::Das_sources->find( url => $handle );

	# generate a high-resolution fingerprint for the image
	my $stamp = gettimeofday * 10000;
	$image->{image_name} .= ".$stamp." . $i++;
	$image->print_image;
	
	$serverName = ( defined $server ) ? $server->name : "unknown";
	push @{ $maps{$serverName} }, { image => $image->file_location,
									map   => $image->image_map,
									server=> $serverName,
									info  => $image->image_info };
  }

  # sort the maps according to server name
  my @maps;
  foreach ( sort keys %maps ) {
	push @maps, @{ $maps{$_} };
  }

  # stash the maps and we're done
  $c->stash->{maps} = \@maps;
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

