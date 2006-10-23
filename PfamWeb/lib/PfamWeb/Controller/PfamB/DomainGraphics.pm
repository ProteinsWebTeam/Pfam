
# DomainGraphics.pm
# jt6 20061023 WTSI
#
# Controller to build a set of domain graphics for a given PfamB.
#
# $Id: DomainGraphics.pm,v 1.1 2006-10-23 12:23:06 jt6 Exp $

package PfamWeb::Controller::PfamB::DomainGraphics;

use strict;
use warnings;

use Storable qw(thaw);

use base "PfamWeb::Controller::PfamB";


# pick up a URL like http://localhost:3000/family/domaingraphics?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

#   my $auto = $c->stash->{pfam}->auto_pfamB;

#   my @architectures =
# 	$c->model("PfamDB::PfamB_reg")->search( { auto_pfamB => $auto },
# 											{ join      => [ qw/ pfamseq_architecture  /],
# 											  select    => [ { count => "auto_architecture" }, x ],
# 											  as        => [ qw/ count x / ],
# 											  group_by  => [ qw/ auto_architecture / ],
# 											  order_by  => "count DESC" }
# 										  );
#   $c->log->debug( "found " . scalar @architectures . " architectures" );

#   my @seqs;
#   foreach my $arch ( @architectures ) {
# 	push @seqs, thaw( $arch->annseq_storable );
#   }
#   $c->log->debug( "found " . scalar @seqs . " storables" );

#   my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
#   #$layout->scale_x( $this->{scale_x} ); #0.33
#   #$layout->scale_y( $this->{scale_y} ); #0.45

#   my %regionsAndFeatures = ( "PfamA"      => 1,
# 							 "noFeatures" => 1 );
#   $layout->layout_sequences_with_regions_and_features( \@seqs, \%regionsAndFeatures );

#   my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
#   $imageset->create_images( $layout->layout_to_XMLDOM );

#   $c->stash->{images} = $imageset;

#   # set up the view and rely on "end" from the parent class to render it
#   $c->stash->{template} = "components/blocks/family/domainSummary.tt";

}

#-------------------------------------------------------------------------------

1;
