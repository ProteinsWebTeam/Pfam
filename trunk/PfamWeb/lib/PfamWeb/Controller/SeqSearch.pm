
# SeqSearch.pm
# jt6 20061108 WTSI
#
# $Id: SeqSearch.pm,v 1.1 2006-11-13 14:31:59 jt6 Exp $

=head1 NAME

PfamWeb::Controller::SeqSearch - perform various sequence searches

=cut

package PfamWeb::Controller::SeqSearch;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: SeqSearch.pm,v 1.1 2006-11-13 14:31:59 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "seqsearch" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 index : Private

The default search page.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # empty; just capture the base URL

}

#-------------------------------------------------------------------------------

=head2 begin : Private

Extracts the query terms from the URL and de-taints them.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

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

=head2 default : Path

Actually run a search...

=cut

sub default : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "SiteSearch::default: caught a URL; running a search" );

  unless( $c->stash->{rawQueryTerms} ) {
	$c->stash->{errorMsg} = "You did not supply any valid search terms";
	return;
  }

  # clever search code goes here

  # set the page template and we're done
  $c->stash->{template} = "pages/seqsearch.tt";
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# none yet

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
