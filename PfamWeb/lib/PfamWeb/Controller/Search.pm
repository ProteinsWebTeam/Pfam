
# Search.pm
# jt6 20060807 WTSI
#
# $Id: Search.pm,v 1.12 2007-04-27 16:22:22 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search - a search engine for the site

=cut

package PfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller reads a list of search plugins from the application
configuration and forwards to each of them in turn, collects the
results and hands off to a template to format them as a results page.

$Id: Search.pm,v 1.12 2007-04-27 16:22:22 jt6 Exp $

=cut

use strict;
use warnings;

use Module::Pluggable;

# inherit from Section, so we get a default end method
use base "PfamWeb::Controller::Section";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 index : Private

Forwards immediately to the L<runSearches> action to run the text searches.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # get the query terms
  my $terms;
  ( $terms ) = $c->req->param( "query" ) =~ /^([\w\:\;\-\.\s]+)/;

  # we're done here unless there's a query specified
  $c->log->warn( "Search::begin: no query terms supplied" ) and return
  unless defined $terms;

  # stash the de-tainted terms so we can safely display them later
  $c->stash->{rawQueryTerms} = $terms;

  # somewhere for the results of this search
  $c->stash->{results} = {};

  # hand off to the method which will run the queries
  $c->forward( "runSearches", [ "textSearches" ] );
}

#-------------------------------------------------------------------------------

=head2 jump : Local

Re-directs to one of the main page type controllers, such as Family, Clan, etc.,
which can handle directly the entry ID or accession.

=cut

sub jump : Local {
  my( $this, $c ) = @_;

  # de-taint the entry ID or accession
  my $entry;
  ( $entry ) = $c->req->param("entry") =~ /^([\w\-_]+)/;
  $c->log->debug( "Search::jump: called with entry |$entry|" );

  # bail immediately if there's no entry given
  unless( $entry ) {
    $c->stash->{error} = "You did not specify an accession or ID.";
    if( defined $c->req->referer )  {
      $c->res->redirect( $c->req->referer );
    } else {
      $c->res->redirect( $c->uri_for( "/" ) );
    }
    return 1;
  }

  # now we know we have an entry. De-taint the ID type
  my $entryType;
  ( $entryType ) = $c->req->param("type") || ""  =~ /^(\w+)/;

  # see if that entry type is valid and, if so, redirect to it
  if( $entryType and defined $this->{jumpTargets}->{$entryType} ) {
    my $action = "/" . $this->{jumpTargets}->{$entryType};
    $c->log->debug( "Search::jump: called on entry type |$entryType|; redirecting to |$action|" );
    $c->res->redirect( $c->uri_for( $action, $c->req->params ) );
    return 1;
  }
  
  # if we get to here we have an entry accession or ID, but we have no idea what 
  # kind of entry it refers to

  # let's guess !
  $c->log->debug( "Search::jump: no valid entry type specified; guessing instead" );
  my $action = $c->forward( "guess", [ $entry ] );

  if( $action ) {
    $c->log->debug( "Search::jump: we've made a guess; redirecting to |$action|" );
    $c->res->redirect( $c->uri_for( "/$action", $c->req->params ) );
  } else {
    $c->log->debug( "Search::jump: couldn't guess entry type..." );
    if( defined $c->req->referer ) {
      $c->log->debug( "Search::guess: redirecting to referer (".$c->req->referer.")" );
      $c->res->redirect( $c->req->referer );
    } else {
      $c->log->debug( "Search::guess: couldn't guess entry type and no referer; redirecting to home page" );
      $c->res->redirect( $c->uri_for( "/" ) );
    }
  }
  return 1;
}

#-------------------------------------------------------------------------------

=head2 guess : Private

Tries to guess what kind of entity is referred to by the argument. It's assumed
that the entry is already de-tainted before it's passed in here.

Returns the action to which the entry maps, or undef if guessing fails.

=cut

sub guess : Private {
  my( $this, $c, $entryUnknownCase ) = @_;
  
  # make sure we know the entry is upper case
  my $entry = uc( $entryUnknownCase );

  $c->log->debug( "Search::guess: called with entry |$entry|" );

  # see if we can figure out what kind if ID or accession we've been handed
  my( $action, $found );  

  if( $entry =~ /^(P([FB])\d{5,6})$/i ) {
    # first, see if it's a Pfam family

    if( $2 eq "F" ) {
      $found = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );
    } elsif ( $2 eq "B" ) {
      $found = $c->model("PfamDB::PfamB")->find( { pfamB_acc => $1 } );      
    }
  
    if( defined $found ) {
      $c->log->debug( "Search::guess: found a Pfam family (from accession)" );
      $action = "family";            
    }
  
  } elsif( $entry =~ /^(CL\d{4})$/i ) {
    # next, could it be a clan ?

    $found = $c->model("PfamDB::Clans")->find( { clan_acc => $1 } );

    if( $found ) {
      $c->log->debug( "Search::guess: found a clan" );
      $action = "clan";
    }
  
  } elsif( $entry =~ /^([OPQ]\d[A-Z0-9]{3}\d)$/i ) {
    # how about a sequence entry ?
  
    $found = $c->model("PfamDB::Pfamseq")->find( { pfamseq_acc => $1 } );
  
    if( defined $found ) {
      $c->log->debug( "Search::guess: found a sequence entry" );
      $action = "protein";
    } else {
      # see if it's a secondary accession
      $found = $c->model("PfamDB::Secondary_pfamseq_acc")
                 ->find( { secondary_acc => $1 },
                         { join =>     [ qw/pfamseq/ ],
                           prefetch => [ qw/pfamseq/ ] } );
      if ( defined $found ) {
        $c->log->debug( "Search::guess: found a secondary sequence entry" );
        $action = "protein";
      }
    }
  } elsif( $entry =~ /^([A-Z0-9]+\_[A-Z0-9]+)$/i ) {
    # see if it's a protein sequence ID (e.g. CANX_CHICK)
  
    $found = $c->model("PfamDB::Pfamseq")->find( { pfamseq_id => $1 } );
  
    if( $found ) {
      $c->log->debug( "Search::guess: found a sequence entry (from ID)" );
      $action = "protein";
    }
  
  } elsif( $entry =~ /^([0-9][A-Za-z0-9]{3})$/ ) {
    # maybe a structure ?
  
    $found = $c->model("PfamDB::Pdb")->find( { pdb_id => $1 } );
  
    if( defined $found ) {
      $c->log->debug( "Search::guess: found a structure" );
      $action = "structure";
    }
  
  } else {
    # finally, see if it's a Pfam family ID or a clan ID
  
    # a Pfam family ID ?    
    $found = $c->model("PfamDB::Pfam")->find( { pfamA_id => $entry } );
  
    if( $found ) {
      $c->log->debug( "Search::guess: found a Pfam family (from ID)" );
      $action = "family";
    } else {
      $found = $c->model("PfamDB::Clans")->find( { clan_id => $entry } );
      if( $found ) {
        $c->log->debug( "Search::guess: found a clan (from ID)" );
        $action = "clan";
      }
    }

  }

  return $action;
}

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

  $c->log->debug( "Search::default: caught a URL; running a search" );

  $c->error( "You did not supply any valid search terms" )
    unless $c->stash->{rawQueryTerms};

  my( @plugins, @pluginsReversed, $pluginName, $pluginDesc );

  # get the list of text search plugins from the configuration
  foreach my $pluginName ( @{ $this->{searchSets}->{$searchSet} } ) {

    next unless( $pluginDesc = $this->{plugins}->{$pluginName} );

    # keep track of the order of the configured plugins. Store the
    # list forwards and backwards, since we'll use it both ways
    push    @plugins,         $pluginName;
    unshift @pluginsReversed, $pluginName;

    # and drop them into a hash too, for easy look-up
    $c->stash->{pluginsHash}->{$pluginName} = $pluginDesc;
  }

  # store the (reversed) list of query names
  $c->stash->{pluginsArray} =         \@plugins;
  $c->stash->{pluginsArrayReversed} = \@pluginsReversed;

  #----------------------------------------

  # keep track of the number of hits for each query
  $c->stash->{pluginHits} = {};

  # firkle with the user input if necessary and build a string that
  # we can pass straight to the DB
  $c->forward( "formatTerms" );

  # walk the plugins and run each query in turn
  foreach my $plugin ( $this->plugins ) {
    my $pluginName = ( split /\:\:/, $plugin )[-1];

    # check that the plugin is properly formed and is enabled
    next unless $plugin->can( "process" );

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

    my $rs = $c->model("PfamDB::Pfam")
               ->search( [ { pfamA_acc => $c->stash->{rawQueryTerms} },
                         { pfamA_id  => $c->stash->{rawQueryTerms} } ] );
  
    # we're going to assume that there's only one hit here... we're in
    # trouble if there's more than one, certainly
    my $hit = $rs->next;
    $c->stash->{lookupHit} = $hit if $hit;

  }

  # set the page template and we're done
  $c->stash->{template} = "pages/search.tt";

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
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
  my( $this, $c, $pluginName, $results ) = @_;

  if( ref $results eq "ARRAY" ) {
    foreach my $rs ( @$results ) {
      $c->forward( "_mergeResults", [ $pluginName, $rs ] );
    }
  } else {
    $c->forward( "_mergeResults", [ $pluginName, $results ] );
  }
}

#-------------------------------------------------------------------------------
# merge results from plugins. If the plugin returns an array of
# ResultSets, walk down it and merge in each one in turn

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

# Calculates a simple score for each hit, based on which plugin
# generated the hit.
#
# The score is calculated according to the order of the plugins from
# the config, treating the first one in the list - probably the PfamA
# table search - as the most significant, e.g.
#
#  if the order of the plugins is Pfam -> Pdb -> Seq_info -> GO,
#  the score is 8 + 4 + 2 + 1 = 15
#
#  if the Seq_info plugin doesn't generate a hit, the score is 8 + 4 +
#  0 + 1 = 13, and that hit will sort lower in the results than the
#  one above.
#
# Since it's called once per hit, this method is implemented as a
# regular perl function, rather than a Catalyst action, so it's called
# directly instead of using $c->forward.

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
