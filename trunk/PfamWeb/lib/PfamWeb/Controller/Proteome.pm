
# Proteome.pm
# rdf 20060821 WTSI
#
# Controller to build the main Pfam Proteome page.
#
# $Id: Proteome.pm,v 1.1 2006-08-22 13:24:05 rdf Exp $

package PfamWeb::Controller::Proteome;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param( "ncbiCode" ) ) {

    $c->req->param( "ncbiCode" ) =~ m/^(\d+)$/i;
    $c->log->info( "Proteome::begin: found ncbiCode |$1|" );
    
    #$c->stash->{clan} = $c->model("PfamDB::Clans")->find( { clan_acc => $1 } )
    #if defined $1;
    
  }
}

#-------------------------------------------------------------------------------
sub generateSummary : Path {
  my( $this, $c ) = @_;

  # empty; just here to capture the URL
}

#-------------------------------------------------------------------------------
# default end

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render unless there's a Clan object in the stash
  #return 0 unless defined $c->stash->{clan};

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/blocks/proteome/errors.tt";
  } else {
	$c->stash->{pageType} = "proteome";
	$c->stash->{template} = "pages/layout.tt";
  }

  # and use it
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}

#-------------------------------------------------------------------------------

1;

