
# Graphics.pm
# jt6 20060710 WTSI
#
# $Id: Graphics.pm,v 1.10 2009-10-07 12:07:19 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::Graphics - controller for generating
domain graphics for a UniProt sequence

=cut

package PfamWeb::Controller::Structure::Graphics;

=head1 DESCRIPTION

Controller to build a set of domain graphics for a given UniProt
sequence - used in the structure section, confusingly.

Generates a B<page component>.

$Id: Graphics.pm,v 1.10 2009-10-07 12:07:19 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw(thaw);
use JSON qw( -convert_blessed_universally );

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

=head2 graphics : Path

Generate a Pfam graphic for each of the sequences specified by the parameter
"ids".

=cut

sub graphics : Path {
  my ( $this, $c ) = @_;

  unless ( defined $c->req->param('seqIds') and
           $c->req->param('seqIds') =~ m/^((\w+\_\w+\,*)+)$/ ) {
    $c->log->warn( 'Structure::Graphics::begin: no IDs found' )
      if $c->debug;
    return;
  }

  # detaint the IDs - maybe redundant
  foreach ( split /\,/, $c->req->param('seqIds') ) {
    push @{ $c->stash->{idList} }, $_ if /^(\w+\_\w+)$/;
  }

  my @seqs;
  foreach my $id ( @{ $c->stash->{idList} } ) {
    $c->log->debug( "Structure::Graphics::graphics: looking for |$id|" )
      if $c->debug;
  
  	# retrieve the Storable with the data for this sequence
  	my $pfamseq = $c->model('PfamDB::Pfamseq')
  	                ->search( { pfamseq_id => $id },
                              { prefetch => [ 'annseqs' ] } )
                    ->first;
   
   	# thaw it out and stash it
   	push @seqs, thaw( $pfamseq->annseqs->annseq_storable ) if defined $pfamseq;
   }
  $c->log->debug( 'Structure::Graphics::graphics: found '
                  . scalar @seqs . ' storables' ) if $c->debug;

  # render the sequences
  my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
  $lm->layoutSequences( \@seqs );

  # TODO add highlights for the various structural regions
  # foreach ( @seqs ) {
  #   $_->highlight( { start => 10, end => 100, text => 'highlight' } );
  # }

  my $json = new JSON;
  #$json->pretty(1);
  $json->allow_blessed;
  $json->convert_blessed;

  $c->stash->{layout} = $json->encode( \@seqs );

  #----------------------------------------

  # build a chains-to-UniProt ID mapping
  my $chains_to_unp = {};
  my $chains;
  foreach my $unp ( keys %{ $c->stash->{chainsMapping} } ) {

  	# each key in %chains_mapping is a uniprot ID, pointing to an anonymous
  	# hash that has the PDB chain ID as keys and '' as values
  	$chains = join ', ', sort keys %{ $c->stash->{chainsMapping}->{$unp} };
  	
  	$chains_to_unp->{$chains} = $unp;
  }

  # build a UniProt ID-to-image mapping
  # my %unpToImage;
  # my $unp;
  # foreach my $image ( $imageset->each_image ) {
  # 	$unp = $image->image_name;
  # 	$unpToImage{$unp} = $image;
  # 
  # 	# while we're iterating over all images, print them...
  # 	$image->print_image;
  # }

  # and put both of those mappings into the stash for the template...
  $c->stash->{chainsToUnp} = $chains_to_unp;
  # $c->stash->{unpToImage}  = \%unpToImage;

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

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
