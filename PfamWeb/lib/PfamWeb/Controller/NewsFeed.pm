
# News.pm
# jt 20061207 WTSI
#
# $Id: NewsFeed.pm,v 1.8 2007-07-10 19:47:47 jt6 Exp $

=head1 NAME

PfamWeb::Controller::News - generate the Pfam news feed RSS

=cut

package PfamWeb::Controller::NewsFeed;

=head1 DESCRIPTION

Generates the Pfam news feed RSS.

$Id: NewsFeed.pm,v 1.8 2007-07-10 19:47:47 jt6 Exp $

=cut

use strict;
use warnings;

use XML::Feed;
use DateTime::Format::MySQL;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 newsFeed : Path

Shows all Pfam news feed items in full.

=cut

sub newsFeed : Path {
  my( $this, $c ) = @_;

  # retrieve the news items from the feed
  my @entries = $c->model('WebUser::News')
                  ->search( { },
                            { order_by => 'pubDate DESC' } );
  $c->stash->{newsItems} = \@entries;

  $c->stash->{template} = 'pages/news.tt';
}

#-------------------------------------------------------------------------------

=head2 end : ActionClass

An empty C<end> to hand off to L<RenderView>.

=cut

sub end : ActionClass( 'RenderView' ) {}

#-------------------------------------------------------------------------------

=head2 rss : Global

Generates an RSS feed.

=cut

sub rss : Global {
  my( $this, $c ) = @_;

  my $cacheKey = 'newsFeed';
  my $feedXML = $c->cache->get( $cacheKey );
  
  if( $feedXML ) {
    $c->log->debug( 'NewsFeed::rss: retrieved feed from cache' );
  } else { 

    # start a feed
    my $feed = XML::Feed->new("RSS");
  
    # build the channel info
    $feed->title("Pfam RSS Feed");
    $feed->link( $c->req->base );
    $feed->description("Pfam News");
    $feed->language("en-GB");
  
    # query the DB for news items, in reverse chronological order
    my @entries = $c->model("WebUser::News")->search( {}, { order_by => "pubDate DESC" } );
  
    # add each item to the feed
    foreach my $entry (@entries) {
      my $feedEntry = XML::Feed::Entry->new('RSS');
      $feedEntry->title( $entry->title );
      $feedEntry->link( $c->uri_for( "/newsfeed#" . $entry->auto_news ) );
      $feedEntry->issued(
                     DateTime::Format::MySQL->parse_datetime( $entry->pubDate ) );
      $feed->add_entry($feedEntry);
    }

    # convert the feed to XML and cache it for a day 
    $feedXML = $feed->as_xml;
    $c->cache->set( $cacheKey, $feedXML, 86400 );
    $c->log->debug( 'NewsFeed::rss: cached feed XML for one day' );
  }

  # set the headers and dump the raw XML to the body
  $c->res->content_type('application/rss+xml');
  $c->res->body( $feedXML );
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
