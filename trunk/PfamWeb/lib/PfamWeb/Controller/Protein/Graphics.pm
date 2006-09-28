
# Graphics.pm
# jt6 20060503 WTSI
#
# Controller to build a set of graphics for a given UniProt entry.
#
# $Id: Graphics.pm,v 1.13 2006-09-28 10:46:52 jt6 Exp $

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

sub updateSources : Path {
  my( $this, $c ) = @_;

  $c->log->debug( "Protein::Graphics::updateSources: listing parameters:" );

  my $seqAcc = $c->stash->{pfamseq}->pfamseq_acc;

  # get a sequence first...
  my @dsnList = ( "http://das.sanger.ac.uk/das/pfam" );

  # retrieve the DasLite client from the base model class and hand it
  # the list of DSNs
  my $dl = $c->model("PfamDB")->getDasLite;
  $dl->dsn( \@dsnList );

  # if we don't get a sequence from the Pfam source, all hope is lost...
  my $sequence = $dl->sequence( $seqAcc );
  return unless $sequence;

  #$c->log->debug( "Protein::Graphics::updateSources: sequence: |".Dumper($sequence)."|" );

  # retrieve the image map for the Pfam graphic from the stash and add
  # it to the array of images that we're going to generate here.
  my( %maps, $handle, $server, $serverName, @pfam );
  foreach my $image ( $c->stash->{pfamImageset}->each_image ) {
	$image->image_name( $image->image_name . "0" );
	$image->print_image;
	push @pfam, { image => $image->file_location,
				  map   => $image->image_map,
				  server=> "Pfam",
				  info  => $image->image_info };
  }

  # if we do get a sequence, we can now add the user-specified sources
  # and call those registries to get features.

  # reset the list of DSN URLs
  @dsnList = ();

  # get the server list and update @dsnList in place
  $c->forward( "getServerList", [ \@dsnList ] );

  foreach my $ds ( @{$c->stash->{dsnList}} ) {
	$c->log->debug( "Protein::Graphics::updateSources: default server: |$ds|" );
  }

  # give the DasLite object our list of servers and retrieve the
  # features for them
  $dl->dsn( $c->stash->{dsnList} );

  my $features = $dl->features( $seqAcc );
  #$c->log->debug( "Protein::Graphics::updatesources: features: |" . Dumper($features) ."|" );

  # hand the features to the layout manager and get it to draw the graphics
  my $layout = Bio::Pfam::Drawing::Layout::DasLayoutManager->new;
  my $success = $layout->layout_DAS_sequences_and_features( $sequence, $features );

  if($success){
 	my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
 	$imageset->create_images( $layout->layout_to_XMLDOM );

 	# process the generated images and convert the URL that comes back
 	# from the layout manager into a simple label, via our database
 	# table of server information
 	my $i = 1; # start at 1 because the Pfam graphic is 0

 	my ($pfamRef);
 	foreach my $image ( $imageset->each_image ) {

 	  ( $handle = $image->image_info ) =~ s/^(.*?)\/features\?.*$/$1/;
 	  $server = $c->model("PfamDB::Das_sources")->find( { url => $handle } );

 	  # append an image number to the image name, to avoid name clashes
 	  # for multiple images generated in the same run
 	  $image->image_name( $image->image_name . $i++ );
 	  $image->print_image;
 	  $serverName = ( defined $server ) ? $server->name : "unknown";
 	  push @{ $maps{$serverName} }, { image => $image->file_location,
 									  map   => $image->image_map,
 									  server=> $serverName,
 									  info  => $image->image_info };
 	}
   }

   # sort the maps according to server name
   my @maps;
   foreach ( sort keys %maps ) {
 	push @maps, @{ $maps{$_} };
   }
   unshift @maps, @pfam;

   # stash the maps and we're done
   $c->stash->{maps} = \@maps;

   $c->log->debug( "Protein::Graphics::updateSources: generated "
				   . scalar @{$c->stash->{maps}} . " images" );
}

#-------------------------------------------------------------------------------
# retrieve the list of servers from either the request (in the
# parameters), the session or, finally, the database

sub getServerList : Private {
  my( $this, $c, $dsnList ) = @_;

  # keep track of server IDs, so that we can store them in the session
  # later
  my %servers;

  # first, see if there's a list in the request parameters
  if( $c->req->param( "reload" ) ) {

	$c->log->debug( "Protein::Graphics::getServerList: getting DAS server IDs from request" );
	foreach ( sort keys %{$c->req->parameters} ) {

	  # we want only the server IDs
	  next unless /^(DS_\d+)$/ and $c->req->param( $_ ) eq "on";

	  my $ds = $c->model("PfamDB::Das_sources")->find( { server_id => $1 } );

	  if( defined $ds ) {
		push @$dsnList, $ds->url;
		$servers{$1} = 1;
		$c->log->debug( "Protein::Graphics::getServerList:   extracted $1" );
	  }
	}

  # next, see if there's a list of servers set in the session
  } elsif( $c->session->{selectedDASServers} ) {

 	$c->log->debug( "Protein::Graphics::getServerList: getting server IDs from session" );

 	foreach ( keys %{$c->session->{selectedDASServers}} ) {
 	  my $ds = $c->model("PfamDB::Das_sources")->find( { server_id => $_ } );
 	  $c->log->debug( "Protein::Graphics::getServerList:   extracted $_" );
 	  push @$dsnList, $ds->url if defined $ds;
 	}

  } else {

	# finally, if we don't have a list of servers from either the
	# session or the request, get the default list from the DB
	$c->log->debug( "Protein::Graphics::getServerList: getting server IDs from database" );
	my @defaultServers = $c->model("PfamDB::Das_sources")->search( { default_server => 1 } );
 	foreach ( @defaultServers ) {
	  push @$dsnList, $_->url;
 	  $servers{$_->server_id} = 1;
	  $c->log->debug( "Protein::Graphics::getServerList:   extracted " . $_->server_id );
 	}

   }

  # store the list of selected servers in the session
  $c->session->{selectedDASServers} = \%servers if scalar keys %servers;

  $c->stash->{dsnList} = $dsnList;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/protein/loadGraphics.tt";

}

#-------------------------------------------------------------------------------

1;

