
# Graphics.pm
# jt6 20060710 WTSI
#
# $Id: Graphics.pm,v 1.3 2006-09-22 10:47:46 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::Graphics - controller for generating
domain graphics for a UniProt sequence

=cut

package PfamWeb::Controller::Structure::Graphics;

=head1 DESCRIPTION

Controller to build a set of domain graphics for a given UniProt
sequence - used in the structure section, confusingly.

Generates a B<page component>.

$Id: Graphics.pm,v 1.3 2006-09-22 10:47:46 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw(thaw);

use base "PfamWeb::Controller::Structure";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Overrides the L<begin|/PfamWeb::Controller::Structure> method from the
parent class, and looks for a single parameter with a comma-separated
list of IDs. Those IDs are the UniProt IDs for the sequences that
should be drawn.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  $c->log->warn( "$this: no IDs" ) and return
	unless defined $c->req->param("ids");

  my $ids;
  ( $ids ) = $c->req->param("ids") =~ m/^((\w+\_\w+\,*)+)$/;

  # detaint the IDs
  my @idList;
  foreach ( split /\,/, $ids ) {
	push @idList, $_ if /^(\w+\_\w+)$/;
  }

  $c->stash->{idList} = \@idList;
}

#-------------------------------------------------------------------------------

=head2 default : Path

Picks up a URL like http://localhost:3000/structure/graphics?ids=Q6FRP6

=cut

sub getData : Path {
  my( $this, $c ) = @_;

  my @seqs;
  foreach my $id ( @{ $c->stash->{idList} } ) {

	# retrieve the Storable with the data for this sequence
	my $pfamseq = $c->model("PfamDB::Pfamseq")->find( { pfamseq_id => $id } );

	# thaw it out and stash it
	push @seqs, thaw( $pfamseq->annseq_storable ) if defined $pfamseq;
  }
  $c->log->debug( "found " . scalar @seqs . " storables" );

  # render the sequences
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->scale_x( $this->{scale_x} ); #0.33
  $layout->scale_y( $this->{scale_y} ); #0.45

  my %regionsAndFeatures = ( "PfamA"      => 1,
							 "noFeatures" => 1 );
  $layout->layout_sequences_with_regions_and_features( \@seqs,
													   \%regionsAndFeatures );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;

  # get the PDB chain/uniprot mapping from the cache
  my $chainsMapping = $c->cache->get( "chain_mapping" );

  # build a chains-to-UniProt ID mapping
  my %chainsToUnp;
  my $chains;
  foreach my $unp ( keys %$chainsMapping ) {

	# each key in %chains is a uniprot ID, pointing to an anonymous
	# hash that has the PDB chain ID as keys and "" as values
	my $chains = join ", ", sort keys %{$chainsMapping->{$unp}};
	
	$chainsToUnp{$chains} = $unp;
  }

  # build a UniProt ID-to-image mapping
  my %unpToImage;
  my $unp;
  foreach my $image ( $imageset->each_image ) {
	$unp = $image->image_name;
	$unpToImage{$unp} = $image;

	# while we're iterating over all images, print them...
	$image->print_image;
  }

  # and put both of those mappings into the stash for the template...
  $c->stash->{chainsToUnp} = \%chainsToUnp;
  $c->stash->{unpToImage}  = \%unpToImage;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/structure/loadGraphics.tt";

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
