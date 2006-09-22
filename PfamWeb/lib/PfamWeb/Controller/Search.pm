
# Search.pm
# jt6 20060807 WTSI
#
# $Id: Search.pm,v 1.3 2006-09-22 10:44:23 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search - a search engine for the site

=cut

package PfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller reads a list of search plugins from the application
configuration and forwards to each of them in turn, collects the
results and hands off to a template to format them as a results page.

$Id: Search.pm,v 1.3 2006-09-22 10:44:23 jt6 Exp $

=cut

use strict;
use warnings;

use Module::Pluggable;
use Data::Dumper;

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

Captures URLs like

=over

=item http://localhost:3000/search?query=accessory

=back

This method walks the list of search plugins and for each one that is
enabled in the configuration, it calls the following methods:

=over

=item o C<Plugin::formatTerms>

Intended to convert the simple user input string from the URL into a
suitable query term for the DB. There's a basic version of this method
on this class but plugins can override that with their own version if
they need to do some other processing.

=item o C<Plugin::process>

Executes the database query and returns the results as a L<DBIC
ResultSet|DBIx::Class::ResultSet>.

=item o C<Search::mergeResults>

Walks the L<ResultSet|DBIx::Class::ResultSet> and adds the results to
the results of the whole query.

=back

It might be necessary, in the future, to modify this process so that
the plugins can return an array of
L<ResultSets|DBIx::Class::ResultSet>, allowing each one to execute
multiple queries.

=cut

sub default : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::default: caught a URL; running a search" );

  $c->error( "You did not supply any valid search terms" )
	unless $c->stash->{rawQueryTerms};

  #----------------------------------------
  # build a list of the enabled queries. The YAML stores this
  # information as an array of hashes, each of which has a single
  # key/value pair, e.g.
  #   - { Pfam:     1 } # enabled
  #   - { Pdb:      0 } # DISABLED
  #   - { Seq_info: 1 } # enabled
  # The key gives the name of the plugin and the value (taken as true
  # or false) determines whether that particular search plugin will be
  # called. The order of the queries is determined by their order in
  # the array.

  my( @enabledPlugins, @enabledPluginsReversed, $pluginName, $pluginDesc );
  foreach my $row ( @{ $this->{plugins} } ) {

	# we could use "each" to get the single key/value pair from the hash,
	# but it does strange things when called multiple times, so this
	# is safer, if uglier
	next unless( $pluginDesc = (values %$row)[0] );

	$pluginName = (keys %$row)[0];

	# keep track of the order of the enabled plugins. Store the list
	# forwards and backwards, since we'll use it both ways
	push    @enabledPlugins,         $pluginName;
	unshift @enabledPluginsReversed, $pluginName;

	# and drop them into a hash too, for easy look-up
	$c->stash->{enabledPluginsHash}->{$pluginName} = $pluginDesc;
  }

  # store the (reversed) list of query names
  $c->stash->{enabledPluginsArray} =         \@enabledPlugins;
  $c->stash->{enabledPluginsArrayReversed} = \@enabledPluginsReversed;

  #----------------------------------------

  # keep track of the number of hits for each query
  $c->stash->{pluginHits} = {};

  # walk the plugins and run each query in turn
  foreach my $plugin ( $this->plugins ) {
	my $pluginName = ( split /\:\:/, $plugin )[-1];

	# check that the plugin is properly formed and is enabled
	next unless( $plugin->can( "process" ) and
				 $c->stash->{enabledPluginsHash}->{$pluginName} );

	# firkle with the user input if necessary and build a string that
	# we can pass straight to the DB
	$c->forward( "formatTerms" );

	# and run the query
	$c->log->debug( "Search::default: running query for plugin $pluginName" );
	my $results = $c->forward( $plugin );

	# merge results from the individual query
	$c->forward( "mergeResults", [ $pluginName, $results ] );

  }

  $c->log->debug( "Search::default: found a total of " .
				  scalar( keys %{$c->stash->{results}} ) . " rows" );


  #----------------------------------------

  # if there are no results, redirect to the error page
  my $numHits = scalar keys %{$c->stash->{results}};
  if( $numHits < 1 ) {
	$c->stash->{template} = "pages/searchError.tt";
	return 0;
  }

  #----------------------------------------

  # if there's only one result, redirect straight to it

  if( $numHits == 1 ) {
	my( $acc ) = keys %{$c->stash->{results}};
	$c->log->debug( "Search::default: found a single hit: |$acc|; redirecting" );
	$c->res->redirect( $c->uri_for( "/family", { acc => $acc } ) );
	return 1;
  }

  #----------------------------------------

  # sort the results according to the score and pfam accession
  my @results;
  my $results = $c->stash->{results};
  foreach my $acc ( sort { $results->{$b}->{score} <=> $results->{$a}->{score}
							 || $a cmp $b }
					keys %{$c->stash->{results}} ) {
	push @results, $c->stash->{results}->{$acc};
  }

  $c->stash->{results} = \@results;

  #----------------------------------------

  # do a quick look-up to see if the search term matches a Pfam ID
  # or accession

  # first, check if there are multiple "words" in the query term,
  # because if there are, this can't be a unique ID or accession
  unless( $c->stash->{rawQueryTerms} =~ /![A-Za-z0-9_-]/ ) {
	
	my $rs = $c->model("PfamDB::Pfam")->search(
											   [
												{ pfamA_acc => $c->stash->{rawQueryTerms} },
												{ pfamA_id  => $c->stash->{rawQueryTerms} }
											   ]
											  );
	
	# we're going to assume that there's only one hit here... we're in
	# trouble if there's more than one, certainly
	my $hit = $rs->next;
	$c->stash->{lookupHit} = $hit if $hit;

  }

}

#-------------------------------------------------------------------------------

=head2 formatTerms : Private

Formats the query terms to add wildcard and fulltext operators to each
word in the list. This base implementation just prepends a "+"
(require that the word is present in every returned row; necessary for
"IN BOOLEAN MODE" queries) and appends a "*" (wildcard) to each term.

This method should be over-ridden by plugin search classes if they
need some other processing to be performed on the search terms.

=cut

sub formatTerms : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
	join " ", map { $_ = "+$_*" } split /\s+|\W|\:|\-|\_/, $c->stash->{rawQueryTerms};

}

#-------------------------------------------------------------------------------

=head2 mergeResults : Private

Merges the results of an individual query into the set of results for
the whole search. The PfamA accession is used as the hash key, so it
needs to be present in the results of the plugin queries. Also keeps
track of the number of hits for each plugin query.

=cut

sub mergeResults : Private {
  my( $this, $c, $pluginName, $rs ) = @_;

  # walk the query results and merge them into the overall results
  my( $acc, $row );
  while( my $dbObj = $rs->next ) {
	$acc = $dbObj->pfamA_acc;

	$row = $c->stash->{results}->{$acc} ||= {};

	$row->{dbObj} = $dbObj;
    $row->{query}->{$pluginName} = 1;

	$c->stash->{pluginHits}->{$pluginName} += 1;

	# score this hit
	$this->_computeScore( $c, $row );
  }
}

#-------------------------------------------------------------------------------

=head2 _computeScore

Calculates a simple score for each hit, based on which plugin
generated the hit.

The score is calculated according to the order of the plugins from the
config, treating the first one in the list - probably the PfamA table
search - as the most significant, e.g.

=over

if the order of the plugins is Pfam -> Pdb -> Seq_info -> GO,
the score is 8 + 4 + 2 + 1 = 15

if the Seq_info plugin doesn't generate a hit, the score is 8 + 4 + 0
+ 1 = 13, and that hit will sort lower in the results than the one above.

=back

Since it's called once per hit, this method is implemented as a
regular perl function, rather than a Catalyst action, so it's called
directly instead of using C<< $c->forward >>.

=cut

sub _computeScore {
  my( $this, $c, $row ) = @_;

  my $score = 0;
  my $factor = 1;
  foreach my $pluginName ( @{ $c->stash->{enabledPluginsArrayReversed} } ) {
	$score  += $factor if $row->{query}->{$pluginName};
	$factor *= 2;
  }
  $row->{score} = $score;
}

#-------------------------------------------------------------------------------

=head2 end : Private

Hands off to the full page template or catches any errors that were
generated earlier

=cut

sub end : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::end: working out what to render..." );

  # set up the TT view
  # check for errors
  if ( scalar @{ $c->error } ) {
	foreach ( @{$c->error} ) {
	  $c->log->debug( "error message: |$_|" );
	}
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "pages/errors.tt";
  } else {
	$c->stash->{pageType} = "family"; # THIS IS WRONG ! FIX ME !
	$c->stash->{fullPage} = 1;
	$c->stash->{template} ||= "pages/search.tt";
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
