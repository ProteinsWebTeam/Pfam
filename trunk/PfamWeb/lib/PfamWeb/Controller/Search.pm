
# Search.pm
# jt6 20060807 WTSI
#
# $Id: Search.pm,v 1.1 2006-08-14 10:41:20 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search - a search engine for the site

=cut

package PfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller reads a list of search plugins from the application
configuration and forwards to each of them in turn, collects the
results and hands off to a template to format them as a results page.

$Id: Search.pm,v 1.1 2006-08-14 10:41:20 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Extracts the query terms from the URL and de-taints them.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # get the query
  my $terms;
  ( $terms ) = $c->req->params->{query} =~ /^([\w\:\;\-\.\s]+)/;

  # we're done here unless there's a query specified
  $c->log->warn( "Search::begin: no query terms supplied" ) and return
	unless defined $terms;

  $c->stash->{rawQueryTerms} = $terms;

  # somewhere for the results of this search
  $c->stash->{results} = {};

}

#-------------------------------------------------------------------------------

=head2 default : Path

Captures URLs like

=over

=item http://localhost:3000/search?query=accessory

=back

=cut

sub default : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::default: caught a URL; running a search" );

  $c->error( "You did not supply any valid search terms" )
	unless $c->stash->{rawQueryTerms};

  foreach my $query ( @{$this->{queries}} ) {
	$c->log->debug( "Search::begin: executing query: |$query|" );

	# format the terms - add wildcard characters, etc.
	$c->forward( "Search::$query", "formatTerms" );

	# execute the query
	$c->forward( "Search::$query" ); # calls "process()" by default

	# add the results to our growing set of results
	$c->forward( "addResults" );
  }	

  $c->log->debug( "Search::default: found a total of " .
				  scalar( keys %{$c->stash->{results}} ) . " rows" );

}

#-------------------------------------------------------------------------------

sub addResults : Private {
  my( $this, $c ) = @_;

  # the new set of results:			$c->stash->{queryResults}
  # all results from this search:	$c->stash->{results}

  $c->log->debug( "Search::addResults: checking " . $c->stash->{queryResults}->count . " rows" );

  while( my $row = $c->stash->{queryResults}->next ) {
	my $auto_pfamA = $row->auto_pfamA;
	$c->log->debug( "Search::addResults: found auto_pfamA, row: |$auto_pfamA|$row|" );
	$c->stash->{results}->{$auto_pfamA} = $row;
  }

  return 1;
}


#-------------------------------------------------------------------------------

=head2 formatTerms : Private

Formats the query terms to add wildcard and fulltext operators to each
word in the list. This base implementation just prepends a "+"
(require that the word is present in every returned row; necessary for
"IN BOOLEAN MODE" queries) and appends a "*" (wildcard) to each term.

This method should be over-ridden by plugin search classes, if they
need some other processing to be performed on the search terms.

=cut

sub formatTerms : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::formatTerms: in base \"formatTerms\"" );
  $c->stash->{terms} =
	join " ",
	  map { $_ = "+$_*" }
		split /\s+|\W/,
		  $c->stash->{rawQueryTerms};
  $c->log->debug( "Search::formatTerms: query terms: |" . $c->stash->{terms} ."|" );

}

#-------------------------------------------------------------------------------

=head2 end : Private

Hands off to the full page template or catches any errors that were
generated earlier

=cut

sub end : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::end: working out what to render..." );

  # don't try to render a page unless there's a Pdb object in the stash
  return 0 unless defined $c->stash->{results};

  # set up the TT view
  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "pages/errors.tt";
  } else {
	$c->stash->{pageType} = "family"; # THIS IS WRONG ! FIX ME !
	$c->stash->{fullPage} = 1;
	$c->stash->{template} = "pages/search.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

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
