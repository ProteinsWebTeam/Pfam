
# Int.pm
# jt6 20060411 WTSI
#
# Controller to build the main iPfam page.
#
# $Id: Int.pm,v 1.1 2006-04-25 16:45:39 jt6 Exp $

package PfamWeb::Controller::Int;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/int

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # set up the TT view
  $c->stash->{pageType} = "ipfam";
  $c->stash->{template} = "pages/layout.tt";

  # and use it
  $c->forward( "PfamWeb::View::TT" );

}

#-------------------------------------------------------------------------------

1;
