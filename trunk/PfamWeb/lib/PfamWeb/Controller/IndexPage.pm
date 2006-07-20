
# Index.pm
# jt6 20060717 WTSI
#
# Controller to build the main Pfam index page.
#
# $Id: IndexPage.pm,v 1.1 2006-07-20 09:01:09 jt6 Exp $

package PfamWeb::Controller::IndexPage;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

sub begin : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "in the begin method" );
}

sub indexPage : Path {
  my( $this, $c ) = @_;

  $c->log->debug( "in the BLANK indexPage method" );
}

sub indexPage2 : Path( "/homepage" ) {
  my( $this, $c ) = @_;

  $c->log->debug( "in the indexPage2 method" );
}

sub end : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "in the end method" );
  $c->stash->{template} = "pages/indexPage.tt";
  $c->forward( "PfamWeb::View::TT" );

  

}

1;
