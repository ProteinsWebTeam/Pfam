
# Keyword.pm
# jt6 20060807 WTSI
#
# $Id: Keyword.pm,v 1.5 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Keyword - a keyword search engine for the site

=cut

package PfamWeb::Controller::Search::Keyword;

=head1 DESCRIPTION

This controller reads a list of search plugins from the application
configuration and forwards to each of them in turn, collects the
results and hands off to a template to format them as a results page.

$Id: Keyword.pm,v 1.5 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 textSearch : Path

Forwards immediately to the L<runSearches> action to run the text searches.

=cut

sub textSearch : Path {
  my( $this, $c ) = @_;

  # if there's no query parameter, we're done here; drop straight to the 
  # template that will render the search forms
  return unless $c->req->param('query');

  # get the query
  my( $terms ) = $c->req->param('query') =~ m/^([\w:.\-\s]+)/;

  # we're done here unless there's a query specified
  $c->log->warn( 'Search::Keyword::textSearch: no query terms supplied' ) and return
    unless defined $terms;

  $c->log->debug( 'Search::Keyword::textSearch: running query with: |' 
                  . $terms . '|' ) if $c->debug;

  # stash the de-tainted terms so we can safely display them later
  $c->stash->{rawQueryTerms} = $terms;

  # somewhere for the results of this search
  $c->stash->{results} = {};

  # hand off to the method which will run the queries
  $c->forward( 'runSearches', [ 'textSearches' ] );
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 runSearches : Private

This method is handed the name of a set of searches as its first
argument. It retrieves the list of search plugins for that search set
and runs each one in turn. For each plugin it calls the following methods:

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

Plugins can run as many queries as necessary, returning the results as
an array of references to DBIC L<ResultSets|DBIx::Class::ResultSet>.
The results from multiple C<ResultSets> are merged in the order that
they were received, so that results from later queries will override
those from earlier ones.

=cut

sub runSearches : Private {
  my( $this, $c, $searchSet ) = @_;

  $c->log->debug( 'Search::Keyword::runSearches: running a search' )
    if $c->debug;

  # TODO this is a bit sloppy...
  $c->error( 'You did not supply any valid search terms' )
    unless $c->stash->{rawQueryTerms};

  my( @plugins, @pluginsReversed, $pluginName, $pluginDesc );

  # get the list of text search plugins from the configuration
  foreach my $pluginName ( @{ $this->{searchSets}->{$searchSet} } ) {

    next unless( $pluginDesc = $this->{plugins}->{$pluginName} );

    $c->log->debug( 'Search::Keyword::runSearches: adding plugin ' .
                    "|$searchSet|$pluginName|" ) if $c->debug;

    # keep track of the order of the configured plugins. Store the
    # list forwards and backwards, since we'll use it both ways
    push    @plugins,         $pluginName;
    unshift @pluginsReversed, $pluginName;

    # and drop them (and their descriptions) into a hash too, for easy look-up
    $c->stash->{pluginsHash}->{$pluginName} = $pluginDesc;
  }

  # store the (reversed) list of query names
  $c->stash->{pluginsArray} =         \@plugins;
  $c->stash->{pluginsArrayReversed} = \@pluginsReversed;

  #----------------------------------------

  # keep track of the number of hits for each query
  $c->stash->{pluginHits} = {};

  # walk the plugins and run each query in turn. The list of plugins comes
  # from Module::Pluggable, via our parent class, Search. The plugin object
  # stringifies to the fully qualified class name, e.g. Search::Plugin::Pfam
  foreach my $plugin ( $this->plugins ) {
    my $pluginName = ( split m/\:\:/, $plugin )[-1];

    # check that the plugin is switched on in the config
    next unless $c->stash->{pluginsHash}->{$pluginName};

    # check that the plugin is properly formed
    next unless( $plugin->can( 'process' ) and
                 $plugin->can( 'formatTerms' ) );

    # firkle with the user input if necessary and build a string that
    # we can pass straight to the DB
    $c->forward( $plugin, 'formatTerms' );

    # and run the query
    $c->log->debug( "Search::Keyword::runSearches: running query for plugin $pluginName" )
      if $c->debug;
    my $results = $c->forward( $plugin );

    # merge results from the individual query
    $c->forward( 'mergeResults', [ $pluginName, $results ] );
  }

  $c->log->debug( 'Search::Keyword::runSearches: found a total of ' .
          scalar( keys %{$c->stash->{results}} ) . ' rows' ) if $c->debug;

  #----------------------------------------

  # if there are no results, redirect to the error page
  my $numHits = scalar keys %{$c->stash->{results}};
  if( $numHits < 1 ) {
    $c->stash->{template} = 'pages/search/keyword/error.tt';
    return 0;
  }

  #----------------------------------------

  # if there's only one result, redirect straight to it
  if( $numHits == 1 ) {
    my( $acc ) = keys %{$c->stash->{results}};
    $c->log->debug( "Search::Keyword::runSearches: found a single hit: |$acc|; redirecting" )
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

  # do a quick look-up to see if the search term matches a Pfam ID
  # or accession

  # first, check if there are multiple "words" in the query term,
  # because if there are, this can't be a unique ID or accession
  unless( $c->stash->{rawQueryTerms} =~ /![A-Za-z0-9_-]/ ) {

    my $rs = $c->model('PfamDB::Pfam')
               ->search( [ { pfamA_acc => $c->stash->{rawQueryTerms} },
                           { pfamA_id  => $c->stash->{rawQueryTerms} } ] );
  
    # we're going to assume that there's only one hit here... we're in
    # trouble if there's more than one, certainly
    my $hit = $rs->next;
    $c->stash->{lookupHit} = $hit if $hit;

  }

  # set the page template and we're done
  $c->stash->{template} = 'pages/search/keyword/results.tt';

}

#-------------------------------------------------------------------------------

=head2 mergeResults : Private

Merges the results of an individual query into the set of results for
the whole search. The PfamA accession is used as the hash key, so it
needs to be present in the results of the plugin queries. Also keeps
track of the number of hits for each plugin query.

=cut

sub mergeResults : Private {
  my( $this, $c, $pluginName, $results ) = @_;

  if( ref $results eq 'ARRAY' ) {
    foreach my $rs ( @$results ) {
      $c->forward( '_mergeResults', [ $pluginName, $rs ] );
    }
  } else {
    $c->forward( '_mergeResults', [ $pluginName, $results ] );
  }
}

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _mergeResults

Merges results from plugins. If the plugin returns an array of
ResultSets, walks down it and merges in each one in turn.

=cut

sub _mergeResults : Private {
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
  my( $this, $c, $row ) = @_;

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
