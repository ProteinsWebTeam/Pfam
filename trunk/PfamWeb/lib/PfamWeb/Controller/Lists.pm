
# Family.pm
# jt6 20060411 WTSI
#
# $Id: Lists.pm,v 1.2 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family - controller to build the main Pfam family
page

=cut

package PfamWeb::Controller::Lists;

=head1 DESCRIPTION

Generates lists of Pfam-A families (currently). We support HTML, XML and plain
text formats for the output.

Generates a B<tabbed page>.

$Id: Lists.pm,v 1.2 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Just decides if the user wants HTML, plain text, XML, etc.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') ) {
    if( $c->req->param('output') eq 'xml' ) {
      $c->stash->{output_xml} = 1;
    }
    elsif ( $c->req->param('output') eq 'text' ) {
      $c->stash->{output_text} = 1;
    }
  }
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 families : Global
  
Lists all Pfam-A families.

=cut

sub families : Global {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Lists::families: showing list of families' ) if $c->debug;

  my ( $output, $format, $template );
 
  if ( $c->stash->{output_xml} ) {
    $output = $c->forward( 'get_families_list', 
                           [ 'xml', 'rest/family/families.tt' ] );
    $c->res->content_type('text/xml');
  }
  elsif ( $c->stash->{output_text} ) {
    $output = $c->forward( 'get_families_list', 
                           [ 'text', 'rest/family/families_text.tt' ] );
    $c->res->content_type('text/plain');
  }
  else {
    $output = $c->forward( 'get_families_list', 
                           [ 'html', 'rest/family/families_list.tt' ] );
    $c->res->content_type('text/html');
  }

  $c->res->output( $output );

}

#-------------------------------------------------------------------------------

=head2 get_families_list : Private

Retrieves the list of families either from cache or from the DB. Caches the 
result of transforming the list through the specified template. Since the list
can be quite big, in whatever format we choose, and since the caching will 
silently fail to store something too large, we also try to compress the template
output before we cache it. 

=cut

sub get_families_list : Private {
  my ( $this, $c, $format, $template ) = @_;
  
  my $output;

  my $cache_key = "families_list_$format";
  my $cache_output = $c->cache->get( $cache_key );
 
  # we got a cache hit 
  if ( defined $cache_output ) {
    $c->log->debug( 'Lists::get_families_list: retrieved list from cache' )
      if $c->debug;
    
    # try to uncompress the thing that we got back from the cache
    $output = Compress::Zlib::memGunzip( $cache_output );
    if ( not defined $output ) {
      $c->log->debug( "Lists::get_families_list: couldn't uncompress cache output; assuming uncompressed already" )
        if $c->debug;
      $output = $cache_output;
    }
  }
  
  # couldn't find the list in the cache; go generate it
  else {
    $c->log->debug( 'Lists::get_families_list: getting new list from DB' )
      if $c->debug;

    my @rs = $c->model('PfamDB::Pfam')
               ->search( {},
                         { order_by => [ 'pfamA_acc' ] } );
    
    $c->log->debug( 'Lists::get_families_list: found |' . scalar @rs . '| families in DB' )
      if $c->debug;
    
    $c->stash->{entries} = \@rs;

    # and use the specified template to generate the list. Capture the output
    # of the template though, so that we can cache it
    $output = $c->view('TT')->render( $c, $template );

    # the template output might be large, so, to give us the best chance of 
    # being able to cache it (memcached, for example, will silently refuse to 
    # cache anything larger than its hardcoded limit), we compress the template 
    # output
    my $compressed_output = Compress::Zlib::memGzip( $output );

    # the compression code will return undef if there's any problem with the
    # gzipping, in which case we'll try caching the uncompressed output anyway 
    if ( not defined $output ) {
      $c->log->warn( "Lists::get_families_list: couldn't compress formatted list" )
        if $c->debug;
      $compressed_output = $output;
    }
    
    # cache the (hopefully) compressed output
    $c->cache->set( $cache_key, $compressed_output );
  }
  
  return $output;
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
