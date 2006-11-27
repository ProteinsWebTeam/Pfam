
# SeqSearch.pm
# jt6 20061108 WTSI
#
# $Id: SeqSearch.pm,v 1.2 2006-11-27 16:31:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::SeqSearch - perform various sequence searches

=cut

package PfamWeb::Controller::SeqSearch;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: SeqSearch.pm,v 1.2 2006-11-27 16:31:28 jt6 Exp $

=cut

use strict;
use warnings;

use PfamWeb::CustomContainer;
use HTML::Widget::Element;
use IO::All;
use Storable qw(thaw);
use Data::Dump qw( dump );

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

  $c->log->debug( "SeqSearch::domain: |" . $c->req->parameters->{have} . "|" );

  my $list;
  if( exists $c->req->parameters->{have} ) {
	foreach ( split /\s+/, $c->req->parameters->{have} ) {
	  next unless /(PF\d{5})/;
	  $list .= "+$1 ";
	}
  }
  if( exists $c->req->parameters->{not} ) {
	foreach ( split /\s+/, $c->req->parameters->{not} ) {
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
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";

  # if there are too many results, bail here and let the TT just
  # display the text summary, plus an admonition to the user to
  # restrict their search a bit
  if( scalar @architectures > 500 ) {
	$c->stash->{template} = "components/blocks/seqsearch/domainResults.tt";
	return;
  }

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

  $layout->layout_sequences_with_regions_and_features( \@seqs, { PfamA      => 1,
																 PfamB      => 1,
																 noFeatures => 1 } );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );

  $c->stash->{images} = $imageset;
  $c->stash->{seqInfo}  = \%seqInfo;



}

#-------------------------------------------------------------------------------

=head2 default : Path

Actually run a search...

=cut

#sub default : Private {
#  my( $this, $c ) = @_;
#
#  $c->log->debug( "SiteSearch::default: caught a URL; running a search" );
#
#  unless( $c->stash->{rawQueryTerms} ) {
#	$c->stash->{errorMsg} = "You did not supply any valid search terms";
#	return;
#  }
#
#}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 buildProteinNameForm : Private

Builds the "search by protein name" form.

=cut

sub buildProteinNameForm : Private {
  my( $this, $c ) = @_;

  my $w = $c->widget( "proteinNameForm" );
  $w->method( "post") ;
  $w->action( $c->uri_for( "checkProteinNameForm" ) );

  # the single text field
  $w->element( "Textfield", "unp" )
	->label( "Enter a UniProt ID or accession" )
	->size( 30 )
	->maxlength( 40 );

  # a submit button
  $w->element( "Submit", "submit" )
	->value( "Submit" );

  # and a reset button
  $w->element( "Reset", "reset" );

  # constraints...
  $w->constraint( Length => "unp" )
	->max( 40 );
  $w->constraint( Regex => "unp" )
	->regex( qr/^[\w\d_-]+$/ );

  return $w;
}

#-------------------------------------------------------------------------------

=head2 buildProteinSeqForm : Private

Builds the "search by protein sequence" form.

=cut

sub buildProteinSeqForm : Private {
  my( $this, $c ) = @_;

  my $w = $c->widget( "proteinSeqForm" );
  $w->method( "post") ;
  $w->action( $c->uri_for( "checkProteinSeqForm" ) );

  # the sequence box
  $w->element( "Textarea", "unp" )
	->label( "Your sequence" )
	->cols( 50 )
	->rows( 20 );

  # search type dropdown
  $w = $c->widget( "Select", "type" )
    ->comment( "(Required)" )
	->label( "Pfam search type" )
	->options( both => "Both global and fragment",
			   ls   => "Global (ls)",
			   fs   => "Fragment (fs)" )
	->selected( "both" );

  # a submit button
  $w->element( "Submit", "submit" )
	->value( "Submit" );

  # and a reset button
  $w->element( "Reset", "reset" );

  # constraints...
  $w->constraint( Length => "unp" )
	->max( 40 );
  $w->constraint( Regex => "unp" )
	->regex( qr/^[\w\d_-]+$/ );

  return $w;
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
