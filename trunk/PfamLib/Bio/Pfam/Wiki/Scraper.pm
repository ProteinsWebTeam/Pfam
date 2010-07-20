
# Scraper.pm
# jt6 20100720 WTSI
# 
# $Id$

=head1 NAME

Bio::Pfam::Wiki::Scraper - scrape wikipedia content

=cut

package Bio::Pfam::Wiki::Scraper;

=head1 SYNOPSIS

  use Bio::Pfam::Wiki::Scraper;

  my $scraper = new Bio::Pfam::Wiki::Scraper;

  my $main_page_content = $scraper->scrape('Main_Page');

=head1 DESCRIPTION

A simple Moose class for scraping wikipedia content. Given an article title, 
the C<scrape> method retrieves the article content and returns it. The content
is "sanitised" using L<HTML::Scrubber> before being returned. In particular, 
scripts and anything looking like a scriptable attribute is stripped.

=cut

use Moose;
use Log::Log4perl;
use HTML::Scrubber;
use MediaWiki::Bot;
use LWP::UserAgent;

our $VERSION = '0.1';

#-------------------------------------------------------------------------------
#- configure logging -----------------------------------------------------------
#-------------------------------------------------------------------------------

BEGIN {
  my $logger_conf = q(
    log4perl.logger                   = WARN, Screen
    log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
  );

  Log::Log4perl->init( \$logger_conf );
}

has 'logger' => (
  is      => 'ro',
  isa     => 'Log::Log4perl::Logger',
  lazy    => 1,
  default => sub { 
    my $this = shift;
    return Log::Log4perl->get_logger( ref $this );
  }
);

#-------------------------------------------------------------------------------
#- public accessors ------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 wiki_root

Sets/gets the root of the URL for wiki pages. Defaults to 
C<http://en.wikipedia.org/w/index.php?action=render&amp;title=>.

=cut

has 'wiki_root' => (
  is => 'rw',
  isa => 'Str',
  default => 'http://en.wikipedia.org/w/index.php?action=render&title='
);

#---------------------------------------

# HTML::Scrubber instance
has 'scrubber' => (
  is      => 'ro',
  isa     => 'HTML::Scrubber',
  lazy    => 1,
  default => sub {
    return HTML::Scrubber->new(
      default => [ 1,  # allow all tags
                   {
                     '*'            => 1, # allow all attributes...
                     'alink'        => 0, # then override that for some
                     'background'   => 0,
                     'bgcolor'      => 0,
                     'color'        => 0,
                     'face'         => 0,
                     'href'         => qr{^(?!(?:java)?script)}i,
                     'link'         => 1,
                     'text'         => 0,
                     'onblur'       => 0,
                     'onchange'     => 0,
                     'onclick'      => 0,
                     'ondblclick'   => 0,
                     'onfocus'      => 0,
                     'onkeydown'    => 0,
                     'onkeyup'      => 0,
                     'onload'       => 0,
                     'onmousedown'  => 0,
                     'onmousemove'  => 0,
                     'onmouseout'   => 0,
                     'onmouseover'  => 0,
                     'onmouseup'    => 0,
                     'onreset'      => 0,
                     'onselect'     => 0,
                     'onunload'     => 0,
                     'src'          => qr{^(?!(?:java)?script)}i,
                     'vlink'        => 0,
                   } ],
      rules   => [ big    => 0, # remove these tags
                   font   => 0,
                   i      => 0,
                   script => 0,
                   small  => 0,
                   style  => 0, ],
      comment => 0, # no comments
      process => 0, # no process instructions
    );
  }
);

#---------------------------------------

# MediaWiki::Bot instance
has 'mw_bot' => (
  is      => 'ro',
  isa     => 'MediaWiki::Bot',
  lazy    => 1,
  default => sub { return MediaWiki::Bot->new; }
);

#---------------------------------------

# LWP::UserAgent instance
has 'ua' => (
  is      => 'ro',
  isa     => 'LWP::UserAgent',
  lazy    => 1,
  default => sub {
    my $ua = LWP::UserAgent->new;
    $ua->env_proxy;
    $ua->agent( 'Mozilla/5.001 (windows; U; NT4.0; en-us) Gecko/25250101' );
    return $ua;
  }
);

#-------------------------------------------------------------------------------
#- public methods --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 scrape

Downloads the wikipedia content for the specified article.

=cut

sub scrape {
  my ( $this, $title ) = @_;

  return unless $title;

  # build the URL
  my $url = $this->wiki_root . $title;
  $this->logger->info( "retrieving wikipedia content for '$title'" );

  # build a new request object and retrieve a response from it
  my $req = HTTP::Request->new( GET => $url );
  my $res = $this->ua->request( $req );

  # see if the request worked...
  my $content;
  if ( $res->is_success ) {
    $content = $this->scrubber->scrub( $res->content );
  }
  else {
	  $this->logger->logwarn( "couldn't retrieve content for '$title': "
	             . $res->status_line );
  }

  return $content;
}

#-------------------------------------------------------------------------------

no Moose;
__PACKAGE__->meta->make_immutable;

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), 
         Paul Gardner, (pg5@sanger.ac.uk),
         John Tate (jt6@sanger.ac.uk)

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

