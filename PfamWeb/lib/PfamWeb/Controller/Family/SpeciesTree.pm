
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# Controller to build a species tree. This is the clickable,
# expandable tree in the "Species" tab.
#
# $Id: SpeciesTree.pm,v 1.3 2006-08-14 10:43:45 jt6 Exp $

package PfamWeb::Controller::Family::SpeciesTree;

use strict;
use warnings;

use Bio::Pfam::Web::Tree;

use Data::Dumper;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------
# pick up a URL like http://localhost:3000/speciestree?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # the accession should be set by the begin method on the application class
  my $acc = $c->stash->{pfam}->pfamA_acc;

  $c->forward( "getTree" );

  my $js;
  $c->stash->{rawTree}->convert_to_js( \$js );
  $c->stash->{tree} = $js;

}

#-------------------------------------------------------------------------------
# override the default end from Family, so that we can return the tree directly

sub end : Private {
  my( $this, $c ) = @_;

  $c->response->body( $c->stash->{tree} );
}

#-------------------------------------------------------------------------------

sub getTree : Private {
  my( $this, $c ) = @_;

  my @regions = $c->model("PfamDB::PfamA_reg_full")->search(
				  { "pfamA.pfamA_acc" => $c->stash->{pfam}->pfamA_acc,
					"in_full"         => 1 },
				  { join              => [ qw/ pfamA pfamseq /] }
				);
	
  my @treeData;
  foreach my $region ( @regions ) {
	push @treeData, $region->taxonomy;
  }

  my $tree = Bio::Pfam::Web::Tree->new();
  $tree->grow_tree ( \@treeData, ';' );

  $c->stash->{rawTree} = $tree->clear_root();

}

#-------------------------------------------------------------------------------

1;
