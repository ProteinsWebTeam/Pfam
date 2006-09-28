
# Int.pm
# jt6 20060411 WTSI
#
# Controller to build the main iPfam page.
#
# $Id: Int.pm,v 1.2 2006-09-28 09:35:49 rdf Exp $

package PfamWeb::Controller::Int;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/int

sub generateSummary : Path {
  my( $this, $c ) = @_;

  $c->log->debug("Int::generateSummary: Hello");

}

#-------------------------------------------------------------------------------
sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pfam object in the stash
  #return 0 unless defined $c->stash->{ligand};
  
  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/blocks/ipfam/errors.tt";
  } else {
	$c->stash->{pageType} = "ipfam";
	$c->stash->{template} ||= "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}
1;
