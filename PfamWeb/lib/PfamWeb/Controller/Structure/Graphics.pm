
# Graphics.pm
# jt6 20060710 WTSI
#
# $Id: Graphics.pm,v 1.7 2007-08-01 14:43:55 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::Graphics - controller for generating
domain graphics for a UniProt sequence

=cut

package PfamWeb::Controller::Structure::Graphics;

=head1 DESCRIPTION

Controller to build a set of domain graphics for a given UniProt
sequence - used in the structure section, confusingly.

Generates a B<page component>.

$Id: Graphics.pm,v 1.7 2007-08-01 14:43:55 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw(thaw);

use base "PfamWeb::Controller::Structure";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Overrides the L<begin|/PfamWeb::Controller::Structure> method from the
parent class and looks for a single parameter with a comma-separated
list of IDs. Those IDs are the UniProt IDs for the sequences that
should be drawn.

=cut

#sub begin : Private {
#  my( $this, $c ) = @_;
#
#	unless( defined $c->req->param('ids') and
#	        $c->req->param('ids') =~ m/^((\w+\_\w+\,*)+)$/ ) {
#    $c->log->warn( 'Structure::Graphics::begin: no IDs found' );
#    return;
#	}
#
#  # detaint the IDs - maybe redundant
#  foreach ( split /\,/, $1 ) {
#  	push @{ $c->stash->{idList} }, $_ if /^(\w+\_\w+)$/;
#  }
#
#  # add the family to sequence to structure mapping
#  $c->forward( 'addMapping' );
#
#}

#-------------------------------------------------------------------------------

=head2 generateGraphics : Path

Generate a Pfam graphic for each of the sequences specified by the parameter
"ids".

=cut

sub generateGraphics : Path {
  my( $this, $c ) = @_;

  unless( defined $c->req->param('seqIds') and
          $c->req->param('seqIds') =~ m/^((\w+\_\w+\,*)+)$/ ) {
    $c->log->warn( 'Structure::Graphics::begin: no IDs found' );
    return;
  }

  # detaint the IDs - maybe redundant
  foreach ( split /\,/, $1 ) {
    push @{ $c->stash->{idList} }, $_ if /^(\w+\_\w+)$/;
  }

  my @seqs;
  foreach my $id ( @{ $c->stash->{idList} } ) {
    $c->log->debug( "Structure::Graphics::generateGraphics: looking for |$id|" );
  
  	# retrieve the Storable with the data for this sequence
  	my $pfamseq = $c->model('PfamDB::Pfamseq')
  	                ->find( { pfamseq_id => $id } );
  
  	# thaw it out and stash it
  	push @seqs, thaw( $pfamseq->annseq_storable ) if defined $pfamseq;
  }
  $c->log->debug( 'Structure::Graphics::generateGraphics: found '
                  . scalar @seqs . ' storables' );

  # render the sequences
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->scale_x( $this->{scale_x} ); #0.33
  $layout->scale_y( $this->{scale_y} ); #0.45

  my %regionsAndFeatures = ( 'PfamA'      => 1,
              							 'noFeatures' => 1  );
  $layout->layout_sequences_with_regions_and_features( \@seqs,
													   \%regionsAndFeatures );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;

  # get the PDB chain/uniprot mapping from the cache
  my $chainsMapping = $c->stash->{chainsMapping};

  # build a chains-to-UniProt ID mapping
  my %chainsToUnp;
  my $chains;
  foreach my $unp ( keys %$chainsMapping ) {

  	# each key in %chains is a uniprot ID, pointing to an anonymous
  	# hash that has the PDB chain ID as keys and '' as values
  	my $chains = join ', ', sort keys %{$chainsMapping->{$unp}};
  	
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

  # set up the view and rely on 'end' from the parent class to render it
  $c->stash->{template} = 'components/blocks/structure/loadGraphics.tt';

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
