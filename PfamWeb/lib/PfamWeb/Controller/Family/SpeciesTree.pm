
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# $Id: SpeciesTree.pm,v 1.18 2007-06-26 11:52:31 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::SpeciesTree - controller to build the
species tree

=cut

package PfamWeb::Controller::Family::SpeciesTree;

=head1 DESCRIPTION

Builds a species tree as a series of nested hashes, which is handed
off to a template to be rendered as a clickable HTML tree.

Generates a B<page fragment>.

$Id: SpeciesTree.pm,v 1.18 2007-06-26 11:52:31 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
use Data::Dump qw( dump );

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Generates the tree and adds it to the stash.

=cut

sub auto : Private {
  my( $this, $c ) = @_;
  $c->log->debug( 'Family::SpeciesTree::auto: in auto method' );

  # retrieve the tree and stash it
  $c->forward( 'getTree' );

}

# a version of the auto method that uses caching
#sub auto : Private {
#  my( $this, $c ) = @_;
#
#  my $cacheKey = 'speciesTree' . $c->stash->{pfam}->pfamA_acc;
#  my $tree = $c->cache->get( $cacheKey );
#  
#  if( $tree ) {
#    $c->log->debug( 'Family::SpeciesTree::auto: successfully retrieved cached tree for '
#                    . $c->stash->{pfam}->pfamA_acc ); 
#    $c->stash->{rawTree} = $tree;
#  } else {
#    $c->log->debug( 'Family::SpeciesTree::auto: no cached tree; generating one' );
#
#    # retrieve the tree and stash it
#    $c->forward( 'getTree' );
#
#    # and cache it
#    $c->cache->set( $cacheKey, $c->stash->{rawTree}, 1209600 );
#  }
#}

#-------------------------------------------------------------------------------

=head2 renderTree : Path

Just hands off to the template which generates the javascript that will build
the tree objects on the client.

=cut

sub renderTree : Path {
  my( $this, $c ) = @_;

  $c->log->debug( "Family::SpeciesTree::renderTree: rendering..." );
  $c->stash->{template} = "components/blocks/family/renderTree.tt";
}

#-------------------------------------------------------------------------------

=head2 renderSubTree : Path

Renders a tree from the supplied sequence accessions.

=cut

sub renderSubTree : Path( "/family/speciessubtree" ) {
  my( $this, $c ) = @_;

  $c->log->debug( "Family::SpeciesTree: acc:  |" . $c->req->param( "acc" ) . "|" );
  $c->log->debug( "Family::SpeciesTree: seqs: |" . $c->req->param( "seqs" ) . "|" );

  foreach ( split / /, uri_unescape( $c->req->param("seqs") ) ) {
  	$c->log->debug( "  id: |$_|" );
  }

  $c->stash->{selectedSeqs} = [ split / /, uri_unescape( $c->req->param("seqs") ) ];

  $c->stash->{template} = "components/tools/seqView.tt";

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# retrieve the tree data from the database and recurse over it to
# build a set of nested hashes that represent it

sub getTree : Private {
  my( $this, $c ) = @_;
  
  #Get the species information for the full alignment
  my @regions = $c->model("PfamDB::PfamA_reg_full")
	->search( { "pfamA.pfamA_acc" => $c->stash->{pfam}->pfamA_acc,
				"in_full"         => 1 },
			  { join              => [ qw/ pfamA pfamseq /],
				prefetch          => [ qw/pfamseq/ ] } );
  $c->log->debug( "Family::SpeciesTree::getTree:: found " . scalar @regions
                  ." full regions" );

  #Get the species information for the seed alignment
  my @resultsSeed = $c->model("PfamDB::PfamA_reg_seed")
	->search( { "pfamA.pfamA_acc" => $c->stash->{pfam}->pfamA_acc },
			  { join              => [ qw/ pfamA pfamseq /],
				prefetch          => [ qw/pfamseq/] } );
  $c->log->debug( "Family::SpeciesTree::getTree:: found " . scalar @resultsSeed
                  ." seed regions" );
								
  #Hash the seed info
  my %seedSeqs;
  foreach my $seedRegion( @resultsSeed){
    $seedSeqs{$seedRegion->pfamseq_acc}++;
  }

  my %tree;
  my $maxDepth = 0;
  foreach my $region ( @regions ) {
    my $speciesData = {}; #This probably could be moved out
    #For some reason in the database Taxonomoy is stored up to genus!
    my $tax = $region->taxonomy;
    my $species = $region->species;
    $tax =~ s/\s+//g;
    #Remove full stop from the end of the species line. Dodgy......I know
    chop($species);
    #As the species has a leading white space.....
    $species =~ s/^(\s+)//g;
    my @tax = split(/\;/, $tax);

  	$maxDepth = scalar @tax	if scalar @tax > $maxDepth;

    $tax[$#tax] = $species;
    #my ($genus) = split(/\s+/, $species);
    #Work out must to remove.
    #my $lengthToRemove = length($genus) + 1; #+1 for full stop and one for leading white space;
    #my $lengthOfTaxonomy = length($tax);
    #my $offSet = $lengthOfTaxonomy-$lengthToRemove;
    #substr($tax, $offSet, $lengthToRemove, $species);
    $$speciesData{'acc'} = $region->pfamseq_acc;
    $$speciesData{'species'} = $species;
    $$speciesData{'inSeed'} = 1  if ($seedSeqs{$region->pfamseq_acc});
    $$speciesData{'tax'} = \@tax;
    &addBranch(\%tree, $speciesData);
  }
  $tree{maxTreeDepth} = $maxDepth;

  $c->stash->{rawTree} = \%tree;
}

#-------------------------------------------------------------------------------
# recursive subroutine to construct a node in the tree

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

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
