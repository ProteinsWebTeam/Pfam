
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# Controller to build a species tree.
#
# $Id: SpeciesTree.pm,v 1.2 2006-05-15 12:13:55 jt6 Exp $

package PfamWeb::Controller::SpeciesTree;

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------
# pick up a URL like http://localhost:3000/speciestree?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # the accession should be set by the begin method on the application class
  my $acc = $c->stash->{pfam}->pfamA_acc;
  my $tree = PfamWeb::Model::GetSpeciesTree::getTree( $acc );

  my $js;
  $tree->convert_to_js( \$js );
  $c->stash->{tree} = $js;

}

#-------------------------------------------------------------------------------
# override the default end from Family, so that we hand off to a
# template that doesn't need a wrapper

sub end : Private {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  $c->stash->{template} = "components/blocks/treeContents.tt";

  # forward to the class that's got the WRAPPER set to null
  #$c->forward( "PfamWeb::View::TTBlock" );

  $c->response->body( $c->stash->{tree} );

}

1;
