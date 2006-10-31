
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# Controller to build a set of domain graphics for a given Pfam.
#
# $Id: DomainGraphics.pm,v 1.8 2006-10-31 15:16:24 jt6 Exp $

package PfamWeb::Controller::Family::DomainGraphics;

use strict;
use warnings;

use Storable qw(thaw);

use base "PfamWeb::Controller::Family";

# pick up a URL like http://localhost:3000/family/domaingraphics?acc=PF00067

sub default : Path {
  my( $this, $c ) = @_;

  # if we were supplied with an auto_architecture, we need to put a
  # note in the stash to show that this is a post-load, so that we can
  # adjust what gets generated in the TT and stuffed into the existing
  # page
  ( $c->stash->{auto_arch} ) = $c->req->param( "arch" ) =~ /^(\d+)$/
	if $c->req->param( "arch" );


  my @architectures;
  my %regionsAndFeatures = ( PfamA      => 1,
							 PfamB      => 1,
							 noFeatures => 1 );

  if( $c->stash->{auto_arch} ) {
	# we want to see all of the sequences with a given architecture

    @architectures = $c->model("PfamDB::Pfamseq_architecture")
      ->search( { "arch.auto_architecture" => $c->stash->{auto_arch} },
				{ join      => [ qw/ arch annseq /],
				  prefetch  => [ qw/ arch annseq/] } );
    $regionsAndFeatures{PfamB} = 1;
	
  } else {
	# we want to see the unique architectures containing this domain

    @architectures = $c->model("PfamDB::PfamA_architecture")
 	  ->search( { pfamA_acc => $c->stash->{acc} },
				{ join      => [ qw/ arch pfam /],
				  prefetch  => [ qw/ arch pfam /],
				  order_by  => "arch.no_seqs DESC" } );
	
  }
  $c->log->debug( "found " . scalar @architectures . " architectures" );

  # build the mappings that we'll need to interpret all this...
  my( @seqs, %seqInfo );
  foreach my $arch ( @architectures ) {

	# thaw out the sequence object for this architecture
	push @seqs, thaw( $arch->annseq_storable );

	# work out which domains are present on this sequence
	my @domains = split /\~/, $arch->architecture;
	$seqInfo{$arch->pfamseq_id}{arch} = \@domains;

	# store a mapping between the sequence and the auto_architecture
	$seqInfo{$arch->pfamseq_id}{auto_arch} = $arch->auto_architecture;

	# if this is a call to retrieve all of the architectures, we don't
	# have an auto_architecture, so this won't work
	$seqInfo{$arch->pfamseq_id}{num} = $arch->no_seqs unless $c->stash->{auto_arch};
  }
  $c->log->debug( "found " . scalar @seqs . " storables" );

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;

  $layout->layout_sequences_with_regions_and_features( \@seqs, \%regionsAndFeatures );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;
  $c->stash->{seqInfo}  = \%seqInfo;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";

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
