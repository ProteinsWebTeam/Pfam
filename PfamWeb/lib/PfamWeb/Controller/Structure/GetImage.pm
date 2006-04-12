
package PfamWeb::Controller::GetImage;

use strict;
use warnings;
use base 'Catalyst::Controller';

sub default : Private {
  my( $this, $c ) = @_;

  $c->req->param("pdbid") =~ m/^(\d\w{3})$/;
  $c->stash->{pdb} = PfamWeb::Model::Pdb->search( { pdb_id => $1 } )->next;
}

sub end : Private {
  my( $this, $c ) = @_;

  if( defined $c->stash->{pdb} ) {
	# we retrieved a Pdb object, which we can use to get the image
	# itself
	$c->res->content_type( "image/gif" );
	$c->res->write( $c->stash->{pdb}->pdb_image );
  } else {
	$c->res->redirect( "static/images/blank.gif" );
  }
}

1;
