
# ClanGraphics.pm
# jt6 20060718 WTSI
#
# Controller to build the post-loaded clan graphics
#
# $Id

package PfamWeb::Controller::Clan::Graphics;

use strict;
use warnings;

use Data::Dumper;
use Storable qw(thaw);

use base "PfamWeb::Controller::Clan";


sub clanGraphics : Path {
  my( $this, $c ) = @_;

  my $autoClan = $c->stash->{clan}->auto_clan;

  my @archs;

  if( defined $c->req->param( "all" ) ) {

	@archs = $c->model("PfamDB::Architecture")->search( { auto_clan => $autoClan },
												   {
													join     => [qw/clan_arch storable type_example/],
													prefetch => [qw/storable type_example/],
													order_by => "no_seqs DESC"
												   } )->all;
  } else {
	
	@archs = $c->model("PfamDB::Architecture")->search( { auto_clan => $autoClan },
												   {
													join     => [qw/clan_arch storable type_example/],
													prefetch => [qw/storable type_example/],
													order_by => "no_seqs DESC",
													rows     => 50,
													page     => 1
												   } )->all;
  }

  my( @seqs, %seqInfo );
  foreach my $arch ( @archs ) {
	push @seqs, thaw( $arch->annseq_storable );
	my @domains = split /\~/, $arch->architecture;
	$seqInfo{$arch->pfamseq_id}{arch} = \@domains;
	$seqInfo{$arch->pfamseq_id}{num} = $arch->no_seqs;
  }
  $c->log->debug( "found " . scalar @seqs . " storables" );

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->scale_x( $this->{scale_x} ); #0.33
  $layout->scale_y( $this->{scale_y} ); #0.45

  $layout->layout_sequences_with_regions_and_features( \@seqs,
													   { PfamA      => 1,
														 noFeatures => 0 } );
														
  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{imageset} = $imageset;
  $c->stash->{seqInfo}  = \%seqInfo;

}

#-------------------------------------------------------------------------------
# override the end method from the Clan class, so that we now hand
# off to a template that doesn't require the wrapper

sub end : Private {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{clan};

  $c->stash->{template} = "components/blocks/clan/loadGraphics.tt";

  # forward to the class that's got the WRAPPER set to null
  $c->forward( "PfamWeb::View::TTBlock" );

}

1;
