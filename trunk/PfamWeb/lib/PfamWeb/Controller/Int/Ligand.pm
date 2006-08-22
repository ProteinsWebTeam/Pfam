# Ligand.pm
# rdf 20060818 WTSI
#
# Controller to build the main Ligand page.
#
# $Id: Ligand.pm,v 1.1 2006-08-22 13:22:51 rdf Exp $

package PfamWeb::Controller::Int::Ligand;

use strict;
use warnings;
use Data::Dumper;

use base "PfamWeb::Controller::Int";

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/int

sub generateLigandIntSum : Path {
  my( $this, $c ) = @_;
  
  $c->log->debug("Int::Ligand::generateLigandIntSum: Hello");

  
  if( defined $c->req->param("code") ) {
    $c->req->param("code") =~ m/^(\w{3})$/i;
    $c->stash->{ligand} = $c->model("PfamDB::Ligands")->find( { three_letter_code => $1 } );
    $c->log->debug("Int::Ligand::generateLigandIntSum: Got ligand data for:".$c->stash->{ligand}->name.":");
  }



}

#-------------------------------------------------------------------------------
sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pfam object in the stash
  return 0 unless defined $c->stash->{ligand};

  # check for errors
  if ( scalar @{ $c->error } ) {
        $c->stash->{errors}   = $c->error;
        $c->stash->{template} = "components/blocks/ipfam/errors.tt";
  } else {
        $c->stash->{pageType} = "iLigand";
        $c->stash->{template} ||= "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}
 

1;
