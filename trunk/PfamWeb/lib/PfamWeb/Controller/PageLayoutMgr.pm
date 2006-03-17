
# PageLayoutMgr.pm
# jt6 20060317 WTSI
#
# manage the storage (and maybe later access) of page layouts
#
# Right now this is very simplistic. It'll need to get a bit cleverer
# before going live...
#
# $Id: PageLayoutMgr.pm,v 1.2 2006-03-17 13:02:28 jt6 Exp $

package PfamWeb::Controller::PageLayoutMgr;

use strict;
use warnings;

use base "Catalyst::Controller";
use JSON;

sub store : Global {
  my( $this, $c ) = @_;

  # just have a quick look to see if there was a layout set already
  #   if( defined $c->session->{layout} ) {
  # 	my $layout = $c->session->{layout};
  # 	$c->log->debug( "goBlock started: ", $layout->{right}->{goBlock} ? "open" : "closed" );
  #   }

  # retrieve the new layout from our input parameters, defaulting to
  # the empty string if there's no input
  my $layoutString = $c->req->params->{layout};
  $layoutString ||= "";

  # create a JSON object from the layout string
  my $json = new JSON;
  my $layout = $json->jsonToObj( $layoutString );

  # pretty-print the JSON object to the log
  my $js = $json->objToJson($layout, {pretty => 1, indent => 2});
  #$c->log->info( "received a layout string: |$js|" );

  # stash the layout object (a JSON object) in the session object
  $c->session->{layout} = $layout;

  $c->response->status( 200 );
}

1;

