
# News.pm
# jt 20061207 WTSI
#
# $Id: NewsFeed.pm,v 1.4 2007-03-15 14:06:12 jt6 Exp $

=head1 NAME

PfamWeb::Controller::News - generate the Pfam news feed RSS

=cut

package PfamWeb::Controller::NewsFeed;

=head1 DESCRIPTION

Generates the Pfam news feed RSS.

$Id: NewsFeed.pm,v 1.4 2007-03-15 14:06:12 jt6 Exp $

=cut

use strict;
use warnings;

use XML::Feed;
use DateTime::Format::MySQL;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 index : Private

Shows all Pfam news feed items in full.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # set the page to be cached for an hour
  $c->cache_page( 3600 );

  # retrieve the news items from the feed
  my @entries = $c->model("WebUser::News")
    ->search( {}, { order_by => "pubDate DESC" } );
  $c->stash->{newsItems} = \@entries;

  $c->stash->{template} = "pages/news.tt";
}

#-------------------------------------------------------------------------------

=head2 rss : Global

Generates an RSS feed.

=cut

sub rss : Global {
  my( $this, $c ) = @_;

  # start a feed
  my $feed = XML::Feed->new("RSS");

  # build the channel info
  $feed->title("Pfam RSS Feed");
  $feed->link( $c->req->base );
  $feed->description("Pfam News");
  $feed->language("en-GB");

  # see if the entries can be extracted from the cache rather than the DB
  my $entries= $c->cache->get( "newsFeedEntries" );
  $c->log->debug( "NewsFeed::RSS: extracted news entries from cache" ) if $entries;

  unless( $entries ) {
    $c->log->debug( "NewsFeed::RSS: retrieving news entries from database" );

    # query the DB for news items, in reverse chronological order
    my @entries = $c->model("WebUser::News")->search( {}, { order_by => "pubDate DESC" } );
    $entries = \@entries;

    # cache the news items for later
    $c->cache->set( "newsFeedEntries", $entries, "1 hour" );
  }

  # add each item to the feed
  foreach my $entry (@$entries) {
    my $feedEntry = XML::Feed::Entry->new("RSS");
    $feedEntry->title( $entry->title );
    $feedEntry->link( $c->uri_for( "/newsfeed#" . $entry->auto_news ) );
    $feedEntry->issued(
                   DateTime::Format::MySQL->parse_datetime( $entry->pubDate ) );
    $feed->add_entry($feedEntry);
  }

  # just dump the raw XML to the body
  $c->res->body( $feed->as_xml );
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
