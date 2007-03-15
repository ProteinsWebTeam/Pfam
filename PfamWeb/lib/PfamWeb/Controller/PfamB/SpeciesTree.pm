
# SpeciesTree.pm
# jt6 20061018 WTSI
#
# $Id: SpeciesTree.pm,v 1.2 2007-03-15 14:06:10 jt6 Exp $

=head1 NAME

PfamWeb::Controller::PfamB::SpeciesTree - controller to build the
species tree

=cut

package PfamWeb::Controller::PfamB::SpeciesTree;

=head1 DESCRIPTION

Builds a species tree as a series of nested hashes, which is handed
off to a template to be rendered as a clickable HTML tree.

Generates a B<page fragment>.

$Id: SpeciesTree.pm,v 1.2 2007-03-15 14:06:10 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
use Data::Dump qw( dump );

use base "PfamWeb::Controller::PfamB";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Generates the tree and adds it to the stash.

=cut

sub auto : Private {
  my( $this, $c ) = @_;

  # retrieve the tree and stash it
  $c->forward( "getTree" );

}

#-------------------------------------------------------------------------------

=head2 renderTree : Path

Just hands off to the template that will generate the javascript to
build the tree on the client.

=cut

sub renderTree : Path {
  my( $this, $c ) = @_;

  # point to the template that will generate the javascript that
  # builds the tree in the client
  $c->stash->{template} = "components/blocks/family/renderTree.tt";
}

 #-------------------------------------------------------------------------------

=head2 renderSubTree : Path

Renders a tree from the supplied sequence accessions.

=cut

sub renderSubTree : Path( "/pfamb/speciessubtree" ) {
  my( $this, $c ) = @_;

  $c->log->debug( "acc:  |" . $c->req->param( "acc" ) . "|" );
  $c->log->debug( "seqs: |" . $c->req->param( "seqs" ) . "|" );

  foreach ( split / /, uri_unescape( $c->req->param("seqs") ) ) {
	$c->log->debug( "  id: |$_|" );
  }

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# retrieve the tree data from the database and recurse over it to
# build a set of nested hashes that represent it

sub getTree : Private {
  my( $this, $c ) = @_;
  #Get the species information for the full alignment
  my @regions = $c->model("PfamDB::PfamB_reg")
	->search( { "auto_pfamB" => $c->stash->{pfam}->auto_pfamB },
			  { join         => [ qw/pfamseq/],
				prefetch     => [ qw/pfamseq/ ]} );

  my %tree;
  my $maxDepth = 0;
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
    my @tax = split(/\;/, $tax);

	$maxDepth = scalar @tax	if scalar @tax > $maxDepth;

    $tax[$#tax] = $species;

    $$speciesData{'acc'} = $region->pfamseq_acc;
    $$speciesData{'species'} = $species;
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
