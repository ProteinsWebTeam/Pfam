
# Structures.pm
# jt6 20060727 WTSI
#
# Controller to build an image of one of the PDB structure for the
# specified family, along with a form for choosing a different one
#
# $Id: Structures.pm,v 1.1 2006-08-14 10:44:16 jt6 Exp $

package PfamWeb::Controller::Family::Structures;

use strict;
use warnings;

use base "PfamWeb::Controller::Family";

# pick up a URL like http://localhost:3000/family/structures?acc=PF00067

sub buildPageFragment : Path {
  my( $this, $c ) = @_;

  my $id;
  ( $id ) = $c->req->param("pdbId") =~ /^(\d\w{3})$/
	if defined $c->req->param( "pdbId" );

  $c->stash->{pdbObj} = $c->model("PfamDB::Pdb")->find( { pdb_id => $id } )
	if defined $id;

}

#-------------------------------------------------------------------------------
# override the end method from the Family class, so that we now hand
# off to a template that doesn't require the wrapper

sub end : Private {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  $c->stash->{template} = "components/blocks/family/familyStructures.tt";

  # forward to the class that's got the WRAPPER set to null
  $c->forward( "PfamWeb::View::TTBlock" );

}

1;
