
# SeqSearch.pm
# jt6 20061108 WTSI
#
# $Id: SeqSearch.pm,v 1.5 2007-01-31 14:07:41 jt6 Exp $

=head1 NAME

PfamWeb::Controller::SeqSearch - perform various sequence searches

=cut

package PfamWeb::Controller::SeqSearch;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: SeqSearch.pm,v 1.5 2007-01-31 14:07:41 jt6 Exp $

=cut

use strict;
use warnings;
use HTML::Widget;

use Storable qw(thaw);

# set the default container to be the one we've defined, which makes
# the markup a little easier to style with CSS
BEGIN {
  HTML::Widget::Element->container_class( "PfamWeb::CustomContainer" );
}

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "seqsearch" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract the query terms from the URL and de-taint them.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  return unless $c->req->param( "query" );

  # get the query
  my $terms;
  ( $terms ) = $c->req->param( "query" ) =~ /^([\w\:\;\-\.\s]+)/;

  # we're done here unless there's a query specified
  $c->log->warn( "Search::begin: no query terms supplied" ) and return
	unless defined $terms;

  # stash the de-tainted terms so we can safely display them later
  $c->stash->{rawQueryTerms} = $terms;

  # somewhere for the results of this search
  $c->stash->{results} = {};

}

#-------------------------------------------------------------------------------

=head2 index : Private

Generates the default search page.

=cut

sub default : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::default: captured a URL" );

  # build the widgets and stash them

  # search by protein name 
  #  $c->stash->{proteinForm} = $c->forward( "buildProteinNameForm" )->result;

}

#-------------------------------------------------------------------------------

=head2 unp : Local

Executes a protein search.

=cut

sub unp : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a protein search" );
}

#-------------------------------------------------------------------------------

=head2 batch : Local

Executes a batch search.

=cut

sub batch : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a batch search" );
}

#-------------------------------------------------------------------------------

=head2 seq : Local

Executes a protein sequence search.

=cut

sub seq : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a protein sequence search" );
}

#-------------------------------------------------------------------------------

=head2 domain : Local

Executes a domain query.

=cut

sub domain : Local {
  my( $this, $c ) = @_;

  $c->log->debug( "SeqSearch::domain: executing a domain search" );

  $c->log->debug( "SeqSearch::domain: |" . $c->req->param( "have" ) . "|" );

  # point at the template right away
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";

  my $list;
  if( defined $c->req->param( "have" ) ) {
	foreach ( split /\s+/, $c->req->param( "have" ) ) {
	  next unless /(PF\d{5})/;
	  $list .= "+$1 ";
	}
  }
  if( defined $c->req->param( "not" ) ) {
	foreach ( split /\s+/, $c->req->param( "not" ) ) {
	  next unless /(PF\d{5})/;
	  $list .= "-$1 ";
	}
  }

  $c->log->debug( "SeqSearch::domain: list: |$list|" );

  return unless $list;

  my @architectures = $c->model("PfamDB::Architecture")
	->search( {},
			  { join     => [ qw/ annseq / ],
				prefetch => [ qw/ annseq / ],
				order_by => "no_seqs DESC" } )
	  ->search_literal( "MATCH( architecture_acc ) " .
						"AGAINST( ? IN BOOLEAN MODE )",
						$list );

  my $sum = 0;
  foreach my $arch ( @architectures ) {
	$sum += $arch->no_seqs;
  }

  $c->log->debug( "SeqSearch::domain: found " . scalar @architectures
				  . " rows, with a total of $sum sequences" );

  $c->stash->{numRows} = scalar @architectures;
  $c->stash->{numSeqs} = $sum;

  # if there are too many results, bail here and let the TT just
  # display the text summary, plus an admonition to the user to
  # restrict their search a bit
  return if scalar @architectures > 500;

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
	
	$layout->layout_sequences_with_regions_and_features( \@seqs, { PfamA      => 1,
																   PfamB      => 1,
																   noFeatures => 1 } );
	
	my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
	$imageset->create_images( $layout->layout_to_XMLDOM );

	$c->stash->{images} = $imageset;
	$c->stash->{seqInfo}  = \%seqInfo;
  }

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
