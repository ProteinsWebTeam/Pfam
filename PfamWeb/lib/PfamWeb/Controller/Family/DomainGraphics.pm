
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# $Id: DomainGraphics.pm,v 1.10 2007-03-15 14:06:10 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::DomainGraphics

=cut

package PfamWeb::Controller::Family::DomainGraphics;

=head1 DESCRIPTION

Controller to build a set of domain graphics for a given Pfam.

$Id: DomainGraphics.pm,v 1.10 2007-03-15 14:06:10 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw(thaw);

use base "PfamWeb::Controller::Section";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Looks for a Pfam accession and populates the stash with the
appropriate object.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(P([FB])\d{5,6})$/i;
	$c->log->info( "Family::begin: found accession |$1|, family A / B ? |$2|" );

	if( defined $1 ) {
	  $c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );
	  $c->stash->{acc}  = $c->stash->{pfam}->pfamA_acc;
	}

  }

}

#-------------------------------------------------------------------------------

=head2 default : Path

Builds the domain graphics for the specified entry.

=cut

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

  my $sum = 0;
  foreach my $arch ( @architectures ) {
	$sum += $arch->no_seqs;
  }

  $c->log->debug( "SeqSearch::domain: found " . scalar @architectures
				  . " rows, with a total of $sum sequences" );

  $c->stash->{numRows} = scalar @architectures;
  $c->stash->{numSeqs} = $sum;

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

  if( scalar @seqs ) {
	my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;

	$layout->layout_sequences_with_regions_and_features( \@seqs, \%regionsAndFeatures );

	my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
	$imageset->create_images( $layout->layout_to_XMLDOM );

	$c->stash->{images} = $imageset;
	$c->stash->{seqInfo}  = \%seqInfo;
  }

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";

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
