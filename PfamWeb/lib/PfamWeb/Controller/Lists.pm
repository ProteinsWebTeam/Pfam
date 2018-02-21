
# Family.pm
# jt6 20060411 WTSI
#
# $Id: Lists.pm,v 1.4 2009-10-28 11:55:34 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family - controller to build the main Pfam family
page

=cut

package PfamWeb::Controller::Lists;

=head1 DESCRIPTION

Generates lists of Pfam-A families (currently). We support HTML, XML and plain
text formats for the output.

Generates a B<tabbed page>.

$Id: Lists.pm,v 1.4 2009-10-28 11:55:34 jt6 Exp $

=cut

use utf8;
use strict;
use warnings;

use Compress::Zlib;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------
#- public actions --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 families : Global

Lists all Pfam-A families. The list can be requested in various formats.  We
try to cache the result. Since the list can be quite big, in whatever format we
choose, and since the caching will silently fail to store something too large,
we also try to compress the template output before we cache it.

=cut

sub families : Global {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Lists::families: showing list of families' ) if $c->debug;

  my $args;

  if ( $c->stash->{output_xml} ) {
    $args = [ 'family', 'xml', 'rest/family/families.tt' ];
    $c->res->content_type('text/xml');
  }
  elsif ( $c->stash->{output_text} ) {
    $args = [ 'family', 'text', 'rest/family/families_text.tt' ];
    $c->res->content_type('text/plain');
  }
  elsif ( $c->stash->{output_pfamalyzer} ) {
    $args = [ 'family', 'pfamalyzer', 'rest/family/families_pfamalyzer.tt' ];
    $c->res->content_type('text/plain');
  }
  elsif ( $c->stash->{output_interpro} ) {
    $args = [ 'family', 'interpro', 'rest/family/families_interpro.tt' ];
    $c->res->content_type('text/xml');
  }
  else {
    $args = [ 'family', 'html', 'rest/family/families_list.tt' ];
    $c->res->content_type('text/html');
  }

  my $output = $c->forward( 'get_list', [ $args ] );
  $c->res->output( $output );
}

#-------------------------------------------------------------------------------

=head2 clans : Global

Lists all clans.

=cut

sub clans : Global {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Lists::clans: showing list of clans' ) if $c->debug;

  my $args;

  # if ( $c->stash->{output_xml} ) {
  #   $args = [ 'clan', 'xml', 'rest/clan/clans.tt' ];
  #   $c->res->content_type('text/xml');
  # }
  if ( $c->stash->{output_text} or
       $c->stash->{output_pfamalyzer}) {
    $args = [ 'clan', 'text', 'rest/clan/clans_text.tt' ];
    $c->res->content_type('text/plain');
  }
  else {
    $args = [ 'clan', 'html', 'rest/clan/clans_list.tt' ];
    $c->res->content_type('text/html');
  }

  my $output = $c->forward( 'get_list', [ $args ] );
  $c->res->output( $output );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

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
    elsif ( $c->req->param('output') eq 'interpro' ) {
      $c->stash->{output_interpro} = 1;
    }
    elsif ( $c->req->param('output') eq 'pfamalyzer' ) {
      # PfamAlyzer won't handle the text output with a comment header, so if we
      # see the appropriate parameter, we'll tell the template not to add the
      # header. This is only applicable to the clans list so far.
      $c->stash->{output_pfamalyzer} = 1;
    }
  }

  return 1;
}

#-------------------------------------------------------------------------------

sub get_list : Private {
  my ( $this, $c, $args ) = @_;

  my $cache_key;
  ( $cache_key = join '', @$args ) =~ s/[\W_]//g;
  my $cache_output = $c->cache->get( $cache_key );
  my $output;

  # we got a cache hit
  if ( defined $cache_output ) {
    $c->log->debug( "Lists::get_list: retrieved family list from cache" )
      if $c->debug;

    # see if the cached output is actually compressed
    $output = Compress::Zlib::memGunzip( $cache_output );

    # the compression code will return undef if there's any problem with the
    # gunzipping
    if ( not defined $output ) {
      $c->log->warn( "Lists::get_list: couldn't uncompress formatted list" )
        if $c->debug;
      $output = $cache_output;
    }
  }

  # couldn't find the list in the cache; go generate it
  else {
    $c->log->debug( 'Lists::get_list: getting new list from DB' )
      if $c->debug;

    $output = $c->forward( 'retrieve_list_from_db', $args );

    # the template output might be large, so, to give us the best chance of
    # being able to cache it (memcached, for example, will silently refuse to
    # cache anything larger than its hardcoded limit), we compress the template
    # output
    my $compressed_output = Compress::Zlib::memGzip( $output );

    # the compression code will return undef if there's any problem with the
    # gzipping, in which case we'll try caching the uncompressed output anyway
    if ( not defined $compressed_output ) {
      $c->log->warn( "Lists::get_list: couldn't compress formatted list" )
        if $c->debug;
      $compressed_output = $output;
    }

    # cache the (hopefully) compressed output
    $c->cache->set( $cache_key, $compressed_output ) unless $ENV{NO_CACHE};
  }

  return $output;
}

#-------------------------------------------------------------------------------

=head2 get_list : Private

Retrieves the list of families or clans.

=cut

sub retrieve_list_from_db : Private {
  my ( $this, $c, $entity, $format, $template ) = @_;

  my @rs;
  if ( $entity eq 'family' ) {

    if ( $c->stash->{output_pfamalyzer} ) {
      $c->log->debug( 'Lists::retrieve_list_from_db: getting pfamalyzer-specific list of families' )
        if $c->debug;

      # we should, in principle, be able to join from pfamA to clan_membership to
      # clans, but although the join works in terms of the SQL, the resulting ResultSource
      # doesn't seem to work. It seems to be a bug in the DBIC version that we're running
      # in the VMs but seems fixed in the latest version in CPAN (0.08192). In lieu of
      # that bug, we join to clan_membership here and then let the template trigger
      # a look-up of clan_acc in the clans table. Nasty, but hey, at least we cache the
      # result...
      print STDERR "HERE\n";
      @rs = $c->model('PfamDB::Pfama')
              ->search( {},
                        { columns  => [ qw( pfama_id pfama_acc ) ],
                          join     => [ { clan_memberships => 'clan' } ],
                          prefetch => [ { clan_memberships => 'clan' } ],
                          order_by => [ 'pfama_id' ] } );
    }
    else {
      @rs = $c->model('PfamDB::Pfama')
              ->search( {},
                        { order_by => [ 'pfama_acc' ] } );
    }
  }
  elsif ( $entity eq 'clan' ) {

    # pfamalyzer needs the clans list ordered by clan ID, but we order
    # the families list by accession by default
    my $order = $c->stash->{output_pfamalyzer} ? 'clan_id' : 'clan_acc';

    @rs = $c->model('PfamDB::Clan')
            ->search( {},
                      { order_by => [ $order ] } );
  }
  else {
    $c->log->debug( 'Lists::retrieve_list_from_db: not a family or a clan; bailing' )
      if $c->debug;

    return;
  }

  $c->log->debug( 'Lists::retrieve_list_from_db: found |' . scalar @rs . "| $entity rows in DB" )
    if $c->debug;

  $c->stash->{entries} = \@rs;

  # and use the specified template to generate the list
  return $c->view('TT')->render( $c, $template );
}

# sub get_list : Private {
#   my ( $this, $c, $entity, $format, $template ) = @_;
#
#   my $output;
#
#   my $cache_key = $entity . '_list_' . $format;
#   my $cache_output = $c->cache->get( $cache_key );
#
#   # we got a cache hit
#   if ( defined $cache_output ) {
#     $c->log->debug( "Lists::get_list: retrieved $entity list from cache" )
#       if $c->debug;
#
#     # try to uncompress the thing that we got back from the cache
#     $output = Compress::Zlib::memGunzip( $cache_output );
#     if ( not defined $output ) {
#       $c->log->debug( "Lists::get_list: couldn't uncompress cache output; assuming uncompressed already" )
#         if $c->debug;
#       $output = $cache_output;
#     }
#   }
#
#   # couldn't find the list in the cache; go generate it
#   else {
#     $c->log->debug( 'Lists::get_list: getting new list from DB' )
#       if $c->debug;
#
#     my @rs;
#     if ( $entity eq 'family' ) {
#
#       if ( $c->stash->{output_pfamalyzer} ) {
#         $c->log->debug( 'Lists::get_list: getting pfamalyzer-specific list of families' )
#           if $c->debug;
#
#         # we should, in principle, be able to join from pfamA to clan_membership to
#         # clans, but although the join works in terms of the SQL, the resulting ResultSource
#         # doesn't seem to work. It seems to be a bug in the DBIC version that we're running
#         # in the VMs but seems fixed in the latest version in CPAN (0.08192). In lieu of
#         # that bug, we join to clan_membership here and then let the template trigger
#         # a look-up of clan_acc in the clans table. Nasty, but hey, at least we cache the
#         # result...
#
#         @rs = $c->model('PfamDB::Pfama')
#                 ->search( {},
#                           { columns  => [ qw( pfama_id pfama_acc ) ],
#                             prefetch => [ { member => 'clan' } ],
#                             order_by => [ 'pfama_id' ] } );
#       }
#       else {
#         @rs = $c->model('PfamDB::Pfama')
#                 ->search( {},
#                           { order_by => [ 'pfama_acc' ] } );
#       }
#     }
#     elsif ( $entity eq 'clan' ) {
#
#       # pfamalyzer needs the clans list ordered by clan ID, but we order
#       # the families list by accession by default
#       my $order = $c->stash->{output_pfamalyzer} ? 'clan_id' : 'clan_acc';
#
#       @rs = $c->model('PfamDB::Clans')
#               ->search( {},
#                         { order_by => [ $order ] } );
#     }
#     else {
#       $c->log->debug( 'Lists::get_list: not a family or a clan; bailing' )
#         if $c->debug;
#
#       return;
#     }
#
#     $c->log->debug( 'Lists::get_list: found |' . scalar @rs . "| $entity rows in DB" )
#       if $c->debug;
#
#     $c->stash->{entries} = \@rs;
#
#     # and use the specified template to generate the list. Capture the output
#     # of the template though, so that we can cache it
#     $output = $c->view('TT')->render( $c, $template );
#
#     # the template output might be large, so, to give us the best chance of
#     # being able to cache it (memcached, for example, will silently refuse to
#     # cache anything larger than its hardcoded limit), we compress the template
#     # output
#     my $compressed_output = Compress::Zlib::memGzip( $output );
#
#     # the compression code will return undef if there's any problem with the
#     # gzipping, in which case we'll try caching the uncompressed output anyway
#     if ( not defined $compressed_output ) {
#       $c->log->warn( "Lists::get_list: couldn't compress formatted list" )
#         if $c->debug;
#       $compressed_output = $output;
#     }
#
#     # cache the (hopefully) compressed output
#     $c->cache->set( $cache_key, $compressed_output ) unless $ENV{NO_CACHE};
#   }
#
#   return $output;
# }

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
