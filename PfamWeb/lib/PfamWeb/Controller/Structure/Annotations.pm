
# Annotations.pm
# jt6 20090820 WTSI
#
# $Id: Annotations.pm,v 1.2 2009-10-14 14:53:41 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::Annotations - controller for 
handling various types of annotation for structures

=cut

package PfamWeb::Controller::Structure::Annotations;

=head1 DESCRIPTION

A collection of actions, mostly intended to be called via AJAX, which return
annotation data for the structure in question.

$Id: Annotations.pm,v 1.2 2009-10-14 14:53:41 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::Lit;
use LWP::UserAgent;
use XML::Simple;
use HTML::Scrubber;
use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Structure';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 references : Local

Returns any references for this structure that can be found in BioLit.

=cut

sub references : Local {
  my ( $this, $c ) = @_;
  
  my $biolit = Bio::Lit->new( entry_type => 'pdb', 
                              id         => $c->stash->{pdbId} );
  
  my $cache_key = 'articles_' . $c->stash->{pdbId};
  my $articles = $c->cache->get( $cache_key ); 
  
  if ( defined $articles ) {
    $c->log->debug( 'Structure::references: retrieved articles list from cache' )
      if $c->debug;  
  }
  else {
    $c->log->debug( 'Structure::references: no articles in cache; retrieving from BioLit' )
      if $c->debug;  

    $articles = $biolit->articles;
    $c->cache->set( $cache_key, $articles ) unless $ENV{NO_CACHE};
  }
  
  $c->stash->{articles} = $articles;
  
  $c->log->debug( 'Structure::references: found ' . scalar @{ $c->stash->{articles} } )
      if $c->debug;

  $c->stash->{template} = 'components/blocks/structure/biolit.tt';
}

#-------------------------------------------------------------------------------

=head2 topsan : Local

Returns any TOPSAN annotations for this entry.

=cut

package ExtScrubber;

use base 'HTML::Scrubber';

sub _validate {
  my ( $self, $t, $r, $a, $as ) = @_;

  if ( $t eq 'a' ) {
    $a->{class} = 'ext';
    push @$as, 'class' unless grep { /class/ } @$as;
  }
  $self->SUPER::_validate( $t, $r, $a, $as );
}

1;

package PfamWeb::Controller::Structure::Annotations;

sub topsan : Local {
  my ( $this, $c ) = @_;

  my $cache_key = 'topsan' . $c->stash->{pdbId};

  my $topsan_data = $c->cache->get( $cache_key ),
  my $topsan_error;

  if ( defined $topsan_data ) {
    $c->log->debug( 'Structure::Annotations::topsan: retrieved data from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Structure::topsan: failed to retrieve data from cache; going to DB' )
      if $c->debug;

    # my $scrubber = new HTML::Scrubber( 
    my $scrubber = new ExtScrubber( 
      allow   => [ qw( div span a p br hr table tbody tr td strong img em
                       ul ol dl li dt dd ) ],
      comment => 1,
      default => [
                   0,
                   {
                     '*'           => 1, # default rule, allow all attributes
                     'href'        => qr{^(?!(?:java)?script)}i,
                     'src'         => qr{^(?!(?:java)?script)}i,
                     'cite'        => '(?i-xsm:^(?!(?:java)?script))',
                     'language'    => 0,
                     'style'       => 0,
                     'name'        => 1,
                     'src'         => 0,
                     'type'        => 0,
                   }
                 ],
      rules   => [
                   script => 0,
                   img    => { src   => 1,
                               alt   => 1,
                               style => 1,
                               class => 1,
                               '*'   => 0 },
                 ],
    );

    my $ua = LWP::UserAgent->new;
    $ua->timeout(10);
    $ua->env_proxy;
    $c->log->debug( 'Structure::Annotations::topsan: built a user agent' )
      if $c->debug;

    TEST: {

      # build the URI
      my $topsan_url = new URI( $this->{topsanUrl} );
      $topsan_url->query_form( pdbId => $c->stash->{pdbId} );

      $c->log->debug( "Structure::Annotations::topsan: query URL: $topsan_url" )
        if $c->debug;

      # get the server response
      my $response = $ua->get( $topsan_url );
      $c->log->debug( 'Structure::Annotations::topsan: got a response from topsan' )
        if $c->debug;

      # was the query successful ?
      my $topsan_xml;
      if ( $response->is_success ) {
        $topsan_xml = $response->decoded_content;
        $c->log->debug( 'Structure::Annotations::topsan: query was successful' )
          if $c->debug;
      }
      else {
        $c->log->debug( 'Structure::Annotations::topsan: query failed: ' . $response->status_line )
          if $c->debug;

        $topsan_error = 'We could not retrieve TOPSAN annotations for ' . $c->stash->{pdbId} . '.';

        last TEST;
      } 

      $c->log->debug( 'Structure::Annotations::topsan: topsan XML: ', $topsan_xml )
        if $c->debug;

      # can we parse the XML into a perl data structure ?
      my $data;
      eval {
        $data = XMLin( $topsan_xml );
      };
      if ( $@ ) {
        $c->log->debug( 'Structure::Annotations::topsan: error parsing XML: ' . $@ )
          if $c->debug;

        $topsan_error = 'We could not interpret the TOPSAN annotations';

        last TEST;
      }

      $c->log->debug( 'Structure::Annotations::topsan: data structure from XML: ', dump( $data ) )
        if $c->debug;

      # scrub the HTML portion of that data structure
      my $text;
      if ( ref $data ) {
        $text = $scrubber->scrub( $data->{protein}->{content}->{content} );
        $c->log->debug( "Structure::Annotations::topsan: topsan data: |$text|" )
          if $c->debug;
      }
      else {
        $c->log->debug( "Structure::Annotations::topsan: couldn't parse a data structure out of the XML" )
          if $c->debug;

        $topsan_error = 'There are no TOPSAN annotations';

        last TEST;
      } 
        
      # did scrubbing the HTML yield anything at all ?
      unless ( defined $text ) {
        $c->log->debug( 'Structure::Annotations::topsan: failed to find the text annotation in the XML data structure' )
          if $c->debug;

        $topsan_error = 'We could not parse the response from the TOPSAN servers';

        last TEST;
      }

      # by this point, we really should have some text...
      $topsan_data->{text} = $text;
      
      # see if we can get an image URL from the perl data structure too
      my $img = $data->{protein}->{image}->{href};
      if ( defined $img ) {
        $c->log->debug( 'Structure::Annotations::topsan: got an image URL from the XML data structure' )
          if $c->debug;

        $topsan_data->{img} = $img;
      }
      else {
        $c->log->debug( 'Structure::Annotations::topsan: failed to find an image URL' )
          if $c->debug;
      }

      # look for the URL for the annotation
      my $href = $data->{protein}->{href};
      if ( defined $href ) {
        $c->log->debug( 'Structure::Annotations::topsan: got a URL for the annotation' )
          if $c->debug;

        $topsan_data->{href} = $href;
      }
      else {
        $c->log->debug( 'Structure::Annotations::topsan: failed to find an annotation URL' )
          if $c->debug;
      }

      $c->log->debug( 'Structure::Annotations::topsan: caching topsan data: ', dump( $topsan_data ) )
        if $c->debug;

      $c->cache->set( $cache_key, $topsan_data ) unless $ENV{NO_CACHE};
    }
    
  }
  
  $c->stash->{topsanError} = $topsan_error;
  $c->stash->{topsanData}  = $topsan_data;
  $c->stash->{template}    = 'components/blocks/structure/topsan.tt';
}

# TODO check 2h1q for examples of non-UTF8 characters
# example structures:
# 3gag, 3gwq, 3f44, 3f9s, 3fcr, 3due

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
