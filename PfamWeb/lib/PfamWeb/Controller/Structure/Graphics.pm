
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

use utf8;
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

    if (defined $pfamseq) {
      # thaw it out and stash it
      push @seqs, thaw( $pfamseq->annseqs->annseq_storable ) if defined $pfamseq;
    } else {
      @seqs = generate_uniprot_graphic($c, $id);
    }
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

sub generate_uniprot_graphic {
  my ($c, $id) = @_;

  my @seqs;
  my @uniprot = $c->model('PfamDB::UniProt')
                       ->search( { 'me.uniprot_id' => $id});
  my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
  foreach my $uniprot (@uniprot) {
    my (@regions, @markups, %nestingRegions, $containerRegion,
        @topLevelRegions, %regionArrangement);

    my $meta = Bio::Pfam::Sequence::MetaData->new({
        organism    => $uniprot->species,
        taxid       => $uniprot->ncbi_taxid,
        accession   => $uniprot->uniprot_acc,
        identifier  => $uniprot->uniprot_id,
        description => $uniprot->description,
        database    => 'uniprot'
    });

    my @pdb_regions = $c->model("PfamDB::PdbPfamaReg")->search({
        'me.pfamseq_acc' => $uniprot->uniprot_acc
    }, {  prefetch => [qw(auto_uniprot_reg_full)],
          order_by => 'chain, me.seq_start ASC' });

    $c->log->debug( 'Searching for Regions matching: '
                  .$uniprot->uniprot_acc. " got " . scalar @pdb_regions)
                  if $c->debug;

    #1 fetch sorted domains and check for nested domains
    foreach my $region (@pdb_regions) {
      my @nested_domains = $c->model('PfamDB::NestedDomains')
                           ->search( { 'me.pfama_acc' => $region->pfama_acc }, {} );
      foreach my $nested_domain (@nested_domains) {
          $nestingRegions{$nested_domain->nests_pfama_acc} = $region->pfama_acc;
      }
    }

    #2. make a %regionArrangement hash structure of regions which nest other regions
    for(my $i=0; $i < scalar @pdb_regions; $i++) {
      my $currRegion = $pdb_regions[$i];
      if ($i > 0 && !defined $containerRegion) {
        $containerRegion = $pdb_regions[$i-1];
      }

      #previous region is a container region and current is nested
      if (defined $containerRegion
          && defined $nestingRegions{$currRegion->pfama_acc}
          && $nestingRegions{$currRegion->pfama_acc}
          eq $containerRegion->pfama_acc
          && $currRegion->seq_start >= $containerRegion->seq_start
          && $currRegion->seq_end <= $containerRegion->seq_end) {
            push(@{$regionArrangement{$containerRegion->pfama_acc}}, $currRegion);
      } else {
        #current region is either a self contained region or a container
        $regionArrangement{$currRegion->pfama_acc} = [];
        $containerRegion = undef;
        push(@topLevelRegions, $currRegion);
      }
    }

    #3. create objects for web display
    foreach my $region (@topLevelRegions) {
      if (scalar @{$regionArrangement{$region->pfama_acc}} > 0) {
        my $seq_start = $region->seq_start;
        my $seq_end = $region->seq_end;
        my $ali_start = $region->auto_uniprot_reg_full->ali_start;
        my $ali_end = $region->auto_uniprot_reg_full->ali_end;

        foreach my $nestedRegion (@{$regionArrangement{$region->pfama_acc->pfama_acc}}) {
          $seq_end = $nestedRegion->seq_start-1;
          $ali_end = $nestedRegion->seq_start-1;

          my $containerRegionObj = _drawRegion($region,
                                      $seq_start,
                                      $seq_end,
                                      $ali_start,
                                      $ali_end);
          push(@regions, $containerRegionObj);

          my $regionObj = _drawRegion($nestedRegion,
                                      $nestedRegion->seq_start,
                                      $nestedRegion->seq_end,
                                      $nestedRegion->auto_uniprot_reg_full->ali_start,
                                      $nestedRegion->auto_uniprot_reg_full->ali_end);
          push(@regions, $regionObj);

          $seq_start = $nestedRegion->ali_end+1;
          $ali_start = $nestedRegion->auto_uniprot_reg_full->ali_end+1;

          my $markup = Bio::Pfam::Sequence::Markup->new( {
              start    => $nestedRegion->seq_start - 1,
              end      => $nestedRegion->seq_end + 1,
              type     => 'Nested',
              colour   => '#00ffff',
              lineColour => '#ff0000',
              metadata => Bio::Pfam::Sequence::MetaData->new({
                  database => 'pfam',
                  start    => $nestedRegion->seq_start - 1,
                  end      => $nestedRegion->seq_end + 1,
                  type     => 'Link between discontinous regions'
              }
            ),
          });
          push(@markups, $markup);
        }

        $seq_end = $region->seq_end;
        $ali_end = $region->auto_uniprot_reg_full->ali_end;
        my $containerRegionObj = _drawRegion($region,
                                    $seq_start,
                                    $seq_end,
                                    $ali_start,
                                    $ali_end);
        push(@regions, $containerRegionObj);

      } else {
        my $regionObj = _drawRegion($region,
                                    $region->seq_start,
                                    $region->seq_end,
                                    $region->auto_uniprot_reg_full->ali_start,
                                    $region->auto_uniprot_reg_full->ali_end);
        push(@regions, $regionObj);
      }
    }
    my $seqObj = Bio::Pfam::Sequence->new({
        metadata => $meta,
        length   => $uniprot->length,
        regions  => \@regions,
        motifs   => [],
        markups  => \@markups,
    });

    push(@seqs, $seqObj);
  }
  $c->log->debug( 'get_summary_data_uniprot '.$id. " got " . scalar @seqs) if $c->debug;
  $lm->layoutSequences(\@seqs);
  return @seqs;
}

sub _drawRegion {
  my ($region, $start, $end, $ali_start, $ali_end) = @_;
  my $regionObj = Bio::Pfam::Sequence::Region->new( {
      start       => $start,
      end         => $end,
      aliStart    => $ali_start,
      aliEnd      => $ali_end,
      modelStart  => $region->pdb_res_start,
      modelEnd    => $region->pdb_res_end,
      modelLength => $region->pfama_acc->model_length,
      metadata    => Bio::Pfam::Sequence::MetaData->new( {
          accession   => $region->pfama_acc->pfama_acc,
          identifier  => $region->pfama_acc->pfama_id,
          type        => $region->pfama_acc->type->type,
          description => $region->pfama_acc->description,
          start       => $start,
          end         => $end,
          aliStart    => $ali_start,
          aliEnd      => $ali_end,
          database    => 'pfam'
        }
      ),
      type => 'pfama'
    }
  );
  return $regionObj
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
