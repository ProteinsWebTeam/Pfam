
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# Controller to build a set of domain graphics for a given Pfam.
#
# $Id: DomainGraphics.pm,v 1.1 2006-04-12 16:24:50 jt6 Exp $

package PfamWeb::Controller::DomainGraphics;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";


# pick up a URL like http://localhost:3000/domaingraphics?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  my @autoPfams = PfamWeb::Model::PfamA_architecture->search(
      { auto_pfamA => $c->stash->{pfam}->auto_pfamA },
	  { join       => [ qw/arch/ ],
	     prefetch  => [ qw/arch/ ],
	     order_by  => "arch.no_seqs DESC"
      }
    );

  my @accs;
  foreach my $arch ( @autoPfams) {
	push @accs, $arch->pfamseq_acc;
  }

  my $seqs = PfamWeb::Model::GetBioObjects::getAnnseq( \@accs, { pfama => 1 } );

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->scale_x( $this->{scale_x} ); #0.33
  $layout->scale_y( $this->{scale_y} ); #0.45
  $layout->layout_sequences( @$seqs );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;

}

sub end : Private {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  $c->stash->{template} = "components/blocks/domainSummary.tt";

  # forward to the class that's got the WRAPPER set to null
  $c->forward( "PfamWeb::View::TTBlock" );

}

1;
