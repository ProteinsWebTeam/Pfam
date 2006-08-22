# Domain.pm
# rdf 20060818 WTSI
#
# Controller to build the main Domain page.
#
# $Id: Domain.pm,v 1.1 2006-08-22 13:22:51 rdf Exp $

package PfamWeb::Controller::Int::Domain;

use strict;
use warnings;
use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/int

sub generateDomainIntSum : Path {
  my( $this, $c ) = @_;

  $c->log->debug("Int::Domain::generateDomainIntSum: Hello");


  if( defined $c->req->param("acc") ) {
    $c->req->param("acc") =~ m/^(PF\d{5})$/i;
    #$c->stash->{domain} = $c->model("PfamDB::Interactions")->find( { pfamA_acc_A => $1 } );
    $c->log->debug("Int::Domain::generateDomainIntSum: Got domain data for:");
  }elsif(defined $c->req->param("id") ) {

  }elsif(defined $c->req->param("entry") ) {

  }

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
        $c->log->debug("PfamWeb::Controller::Int::Domain - Handing off to layout");
        $c->stash->{pageType} = "iDomain";
        $c->stash->{template} ||= "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}
1;

