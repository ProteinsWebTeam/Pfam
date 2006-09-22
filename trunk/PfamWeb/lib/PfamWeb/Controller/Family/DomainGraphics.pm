
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# Controller to build a set of domain graphics for a given Pfam.
#
# $Id: DomainGraphics.pm,v 1.6 2006-09-22 10:46:00 jt6 Exp $

package PfamWeb::Controller::Family::DomainGraphics;

use strict;
use warnings;

use Storable qw(thaw);

use base "PfamWeb::Controller::Family";


# pick up a URL like http://localhost:3000/family/domaingraphics?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

# the old way of doing things... this bit of code extracts the
# regions, etc. from the database directly and builds the various Bio
# objects that it needs. The new bit uses the Storable objects that have
# all the Bio objects pre-formed.

#   my @autoPfams = PfamWeb::Model::PfamA_architecture->search(
#       { auto_pfamA => $c->stash->{pfam}->auto_pfamA },
# 	  { join       => [ qw/arch/ ],
# 	     prefetch  => [ qw/arch/ ],
# 	     order_by  => "arch.no_seqs DESC"
#       }
#     );

#   my @accs;
#   foreach my $arch ( @autoPfams) {
# 	push @accs, $arch->pfamseq_acc;
#   }

#   my $seqs = PfamWeb::Model::GetBioObjects::getAnnseq( \@accs, { pfama => 1 } );

  my $acc = $c->stash->{pfam}->pfamA_acc;

#	PfamWeb::Model::PfamA_architecture->search( { pfamA_acc => $acc },
  my @architectures =
	$c->model("PfamDB::PfamA_architecture")->search( { pfamA_acc => $acc },
													 { join      => [ qw/ arch pfam /],
													   order_by  => "arch.no_seqs DESC" }
												   );
  $c->log->debug( "found " . scalar @architectures . " architectures" );

  my @seqs;
  foreach my $arch ( @architectures ) {
	push @seqs, thaw( $arch->annseq_storable );
  }
  $c->log->debug( "found " . scalar @seqs . " storables" );

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  #$layout->scale_x( $this->{scale_x} ); #0.33
  #$layout->scale_y( $this->{scale_y} ); #0.45

  my %regionsAndFeatures = ( "PfamA"      => 1,
							 "noFeatures" => 1 );
  $layout->layout_sequences_with_regions_and_features( \@seqs, \%regionsAndFeatures );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";

}

#-------------------------------------------------------------------------------

1;
