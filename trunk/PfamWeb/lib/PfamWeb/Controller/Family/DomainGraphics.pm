
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# Controller to build a set of domain graphics for a given Pfam.
#
# $Id: DomainGraphics.pm,v 1.7 2006-10-27 08:52:07 rdf Exp $

package PfamWeb::Controller::Family::DomainGraphics;

use strict;
use warnings;

use Storable qw(thaw);

use base "PfamWeb::Controller::Family";


# pick up a URL like http://localhost:3000/family/domaingraphics?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  my $acc = $c->stash->{pfam}->pfamA_acc;

  my( $arch ) = $c->req->param( "arch" ) =~ /^(\d+)$/;
  

  my @architectures;
  my %regionsAndFeatures = ( "PfamA"      => 1,
			     "noFeatures" => 1 );
  if($arch){
    @architectures = $c->model("PfamDB::Pfamseq_architecture")
      ->search( { "arch.auto_architecture" => $arch },
		{ join      => [ qw/ arch annseq /],
		  prefetch  => [ qw/ arch annseq/],
		}
	      );
    $regionsAndFeatures{PfamB} = 1;
  }else{
    @architectures =
	$c->model("PfamDB::PfamA_architecture")
	  ->search( { pfamA_acc => $acc },
		    { join      => [ qw/ arch pfam /],
		      prefetch  => [ qw/ arch pfam /],
		      order_by  => "arch.no_seqs DESC" });
  }
  $c->log->debug( "found " . scalar @architectures . " architectures" );

  my @seqs;
  foreach my $arch ( @architectures ) {
	push @seqs, thaw( $arch->annseq_storable );
  }
  $c->log->debug( "found " . scalar @seqs . " storables" );

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;


  $layout->layout_sequences_with_regions_and_features( \@seqs, \%regionsAndFeatures );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";

}

#-------------------------------------------------------------------------------

1;
