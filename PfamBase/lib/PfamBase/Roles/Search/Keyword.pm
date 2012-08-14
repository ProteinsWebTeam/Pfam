
# Keyword.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

PfamWeb::Roles::Search::Keyword - a keyword search engine for the site

=cut

package PfamBase::Roles::Search::Keyword;

=head1 DESCRIPTION

A role to add keyword search functionality to the search controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 keyword : Path

Forwards immediately to the L<run_searches> action to run the text searches.

=cut

sub keyword : Local {
  my ( $this, $c ) = @_;

  # if there's no query parameter, we're done here; drop straight to the 
  # template that will render the search forms
  unless ( $c->req->param('query') ) {
    $c->stash->{kwSearchError} = 'You did not supply a query term.';

    $c->log->debug( 'Roles::Search::Keyword::text_search: no query terms supplied' )
      if $c->debug;

    return;
  }

  # get the query
  my ( $terms ) = $c->req->param('query') =~ m/^([\w:.\-\s]+$)/;

  # we're done here unless there's a query specified
  unless ( defined $terms ) {
    $c->stash->{kwSearchError} = 'You did not supply any valid query terms.';

    $c->log->debug( 'Roles::Search::Keyword::text_search: no *valid* query terms supplied' )
      if $c->debug;

    return;
  }

  # stop Prasad submitting single character searches...
  unless ( length $terms > 1 ) {
    $c->stash->{kwSearchError} = 'You cannot use a single character as a query term.';

    $c->log->debug( 'Roles::Search::Keyword::text_search: single character query term' )
      if $c->debug;

    return;
  }

  $c->log->debug( 'Roles::Search::Keyword::text_search: running query with: |' 
                  . $terms . '|' ) if $c->debug;

  # stash the de-tainted terms so we can safely display them later
  $c->stash->{rawQueryTerms} = $terms;

  # somewhere for the results of this search
  $c->stash->{results} = {};

  # hand off to the method which will run the queries
  $c->forward( 'run_searches', [ 'textSearches' ] );
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 run_searches : Private

This method is handed the name of a set of searches as its first
argument. It retrieves the list of search methods for that search set
and runs each one in turn. 

=cut

sub run_searches : Private {
  my ( $this, $c, $searchSet ) = @_;

  $c->log->debug( 'Roles::Search::Keyword::run_searches: running a search' )
    if $c->debug;

  my $pluginDesc;

  # get the list of text search plugins from the configuration
  foreach my $pluginName ( @{ $this->{searchSets}->{$searchSet} } ) {

    next unless ( $pluginDesc = $this->{plugins}->{$pluginName} );

    $c->log->debug( 'Roles::Search::Keyword::run_searches: adding keyword search method ' .
                    "|$searchSet|$pluginName|$pluginDesc|" ) if $c->debug;

    # keep track of the order of the configured plugins. Store the
    # list forwards and backwards, since we'll use it both ways,
    # and drop them (and their descriptions) into a hash too, for 
    # easy look-up
    push    @{ $c->stash->{pluginsArray} },         $pluginName;
    unshift @{ $c->stash->{pluginsArrayReversed} }, $pluginName;
    $c->stash->{pluginsHash}->{$pluginName} = $pluginDesc;
  }

  #----------------------------------------

  # keep track of the number of hits for each query
  $c->stash->{pluginHits} = {};

  # walk the plugins and run each query in turn. The list of plugins comes
  # from Module::Pluggable, via our parent class, Search. The plugin object
  # stringifies to the fully qualified class name, e.g. Search::Plugin::Pfam
  foreach my $plugin ( @{ $c->stash->{pluginsArray} } ) {

    # check that the plugin is switched on in the config
    next unless $c->stash->{pluginsHash}->{$plugin};

    # build the method name
    my $method = 'process_' . lc $plugin;

    # and run the query
    $c->log->debug( "Roles::Search::Keyword::run_searches: running query method $method" )
      if $c->debug;
    my $results = $c->forward( $method );

    # merge results from the individual query
    $c->forward( 'merge_results', [ $plugin, $results ] );
  }

  $c->log->debug( 'Roles::Search::Keyword::run_searches: found a total of '
                  . scalar( keys %{$c->stash->{results}} ) . ' rows' )
    if $c->debug;

  #----------------------------------------

  # if there are no results, redirect to the error page
  my $numHits = scalar keys %{$c->stash->{results}};
  if ( $numHits < 1 ) {
    $c->stash->{template} = 'pages/search/keyword/error.tt';
    return 0;
  }

  #----------------------------------------

  # if there's only one result, redirect straight to it
  if ( $numHits == 1 ) {
    my ( $acc ) = keys %{$c->stash->{results}};
    $c->log->debug( "Roles::Search::Keyword::run_searches: found a single hit: |$acc|; redirecting" )
      if $c->debug;
    $c->res->redirect( $c->uri_for( '/family', { acc => $acc } ) );
    return 1;
  }

  #----------------------------------------

  # sort the results according to the score and pfam accession
  my @results;
  my $results = $c->stash->{results};
  foreach my $acc ( sort { $results->{$b}->{score} <=> $results->{$a}->{score} ||
                                                $a cmp $b }
                    keys %{$c->stash->{results}} ) {
    push @results, $c->stash->{results}->{$acc};
  }

  $c->stash->{results} = \@results;

  #----------------------------------------

  # do a quick look-up to see if the search term matches a Pfam ID or 
  # accession, but first check if there are multiple "words" in the query 
  # term, because if there are, this can't be a unique ID or accession
  $c->forward( 'lookup_term' )
    unless $c->stash->{rawQueryTerms} =~ /![A-Za-z0-9_-]/;

  # set the page template and we're done
  $c->stash->{template} = 'pages/search/keyword/results.tt';

}

#-------------------------------------------------------------------------------

=head2 merge_results : Private

Merges the results of an individual query into the set of results for
the whole search. The Pfam-A accession is used as the hash key, so it
needs to be present in the results of the plugin queries. Also keeps
track of the number of hits for each plugin query.

If the plugin returns an array of ResultSets, walks down it and merges in each
one in turn.

=cut

sub merge_results : Private {
  my ( $this, $c, $pluginName, $results ) = @_;

  if ( ref $results eq 'ARRAY' ) {
    foreach my $rs ( @$results ) {
      $c->forward( 'merge', [ $pluginName, $rs ] );
    }
  }
  else {
    $c->forward( 'merge', [ $pluginName, $results ] );
  }
}

#-------------------------------------------------------------------------------

=head1 NON-ACTION METHODS

=head2 _computeScore

Calculates a simple score for each hit, based on which plugin
generated the hit.

The score is calculated according to the order of the plugins from
the config, treating the first one in the list - probably the PfamA
table search - as the most significant, e.g.

 if the order of the plugins is Pfam -> Pdb -> Seq_info -> GO,
 the score is 8 + 4 + 2 + 1 = 15

 if the Seq_info plugin doesn't generate a hit, the score is 8 + 4 +
 0 + 1 = 13, and that hit will sort lower in the results than the
 one above.

Since it's called once per hit, this method is implemented as a
regular perl function, rather than a Catalyst action, so it's called
directly instead of using $c->forward.

=cut

sub _computeScore {
  my ( $this, $c, $row ) = @_;

  my $score = 0;
  my $factor = 1;
  foreach my $pluginName ( @{ $c->stash->{pluginsArrayReversed} } ) {
    $score  += $factor if $row->{query}->{$pluginName};
    $factor *= 2;
  }
  $row->{score} = $score;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
