
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# Controller to build a species tree. This is the clickable,
# expandable tree in the "Species" tab.
#
# $Id: SpeciesTree.pm,v 1.7 2006-10-09 16:21:06 rdf Exp $

package PfamWeb::Controller::Family::SpeciesTree;

use strict;
use warnings;

#use Bio::Pfam::Web::Tree;

use Data::Dump qw( dump );

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------
# pick up a URL like http://localhost:3000/speciestree?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # the accession should be set by the begin method on the application class
  my $acc = $c->stash->{pfam}->pfamA_acc;

  $c->forward( "getTree" );
  $c->log->debug( dump $c->stash->{rawTree} );

  my $js;
#  $c->stash->{rawTree}->convert_to_js( \$js );
#  $c->stash->{tree} = $js;

  $c->stash->{template} = "components/blocks/family/renderTree.tt";

}

#-------------------------------------------------------------------------------
# override the default end from Family, so that we can return the tree directly

#sub end : Private {
#  my( $this, $c ) = @_;
#
#  $c->response->body( $c->stash->{tree} );
#}

#-------------------------------------------------------------------------------

sub getTree : Private {
  my( $this, $c ) = @_;
  #Get the species information for the full alignment
  my @regions = $c->model("PfamDB::PfamA_reg_full")->search(
							    { "pfamA.pfamA_acc" => $c->stash->{pfam}->pfamA_acc,
							      "in_full"         => 1 },
							    { join              => [ qw/ pfamA pfamseq /],
							      prefetch          => [ qw/pfamseq/ ],}
							   );
  #Get the species information for the seed alignment
  my @resultsSeed = $c->model("PfamDB::PfamA_reg_seed")->search(
								 { "pfamA.pfamA_acc" => "PF07065"},
								 { join              => [ qw/ pfamA pfamseq /],
								   prefetch          => [ qw/pfamseq/] }
								);
  #Hash the seed info
  my %seedSeqs;
  foreach my $seedRegion( @resultsSeed){
    $seedSeqs{$seedRegion->pfamseq_acc}++;
  }

  my %tree;
  foreach my $region ( @regions ) {
    my $speciesData = {}; #This probably could be moved out
    #For some reason in the database Taxonomoy is stored up to genus!
    my $tax = $region->taxonomy;
    my $species = $region->species;
    $tax =~ s/\s+//g;
    #Remove ful stop from the end of the species line. Dodgy......I know
    chop($species);
    #As the species has a leading white space.....
    $species =~ s/^(\s+)//g;
    my ($genus) = split(/\s+/, $species);
    #Work out must to remove.
    my $lengthToRemove = length($genus) + 1; #+1 for full stop and one for leading white space;
    my $lengthOfTaxonomy = length($tax);
    my $offSet = $lengthOfTaxonomy-$lengthToRemove;
    substr($tax, $offSet, $lengthToRemove, $species);
    $$speciesData{'acc'} = $region->pfamseq_acc;
    $$speciesData{'species'} = $species;
    $$speciesData{'inSeed'} = 1  if ($seedSeqs{$region->pfamseq_acc});
    $$speciesData{'tax'} = [split(/;/,$tax)];
    &addBranch(\%tree, $speciesData);
  }
  $c->stash->{rawTree} = \%tree;
}

sub addBranch {
  my ($treeRef, $branchRef) = @_;
  my $node = shift @{$$branchRef{'tax'}};
  if($node){
    $treeRef->{branches}->{$node}->{frequency}++; # count the number of regions
    $treeRef->{branches}->{$node}->{sequences}->{$$branchRef{'acc'}}++; #count the number of unique sequences
    $treeRef->{branches}->{$node}->{species}->{$$branchRef{'species'}}++; #count the number of unique species
    if($node eq $$branchRef{'species'}){
      $treeRef->{branches}->{$node}->{inSeed}++ if($$branchRef{'inSeed'});
    }
    addBranch($treeRef->{branches}->{$node}, $branchRef);
  }
}

#-------------------------------------------------------------------------------

1;
