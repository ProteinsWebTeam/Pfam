# Ligand.pm
# rdf 20060818 WTSI
#
# Controller to build the main Ligand page.
#
# $Id: Ligand.pm,v 1.3 2006-09-28 14:42:02 jt6 Exp $

package PfamWeb::Controller::Int::Ligand;

use strict;
use warnings;
use Data::Dumper;

use base "PfamWeb::Controller::Section";

__PACKAGE__->config( SECTION => "ligand" );

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

1;
