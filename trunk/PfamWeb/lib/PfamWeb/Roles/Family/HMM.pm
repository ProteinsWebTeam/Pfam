# HMM.pm
# jt6 20120920 WTSI
#
# $Id$

=head1 NAME

PfamWeb::Roles::Family::HMM - role to add HMM-related methods to the family
controller

=cut

package PfamWeb::Roles::Family::HMM;

=head1 DESCRIPTION

This is a Moose role that adds functionality to the L<Family> controller. It
contains methods related to HMMs specifically.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use Image::Size;

#-------------------------------------------------------------------------------
#- HMM logos -------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 HMM LOGO ACTIONS

=head2 logo : Chained

Returns the HMM logo image for this family. This is subject to a check on the 
size of the image and the type of browser that is requesting it. Since there
are known problems with firefox and large PNGs, we don't return the image 
immediately in that case. The template takes care of showing a bit of text
and providing a link to load the image anyway.

=cut

sub logo : Chained( 'family' )
           PathPart( 'logo' )
           Args( 0 ) {
  my ( $this, $c ) = @_;
  
  my $logo = $c->forward( 'get_logo' );    
  return if $c->stash->{errorMsg};
  
  my ( $logo_x, $logo_y ) = imgsize( \$logo );

  if ( defined $logo_x and defined $logo_y and  
       ( $logo_x > $this->{image_size_limit} or
         $logo_y > $this->{image_size_limit} ) and
           $c->req->user_agent =~ m/Gecko/ and
       not $c->req->user_agent =~ m/WebKit/ ) {
  
    $c->log->debug( 'Family::FamilyActions::logo: browser is Gecko-based and image is large'
                    . " ($logo_x x $logo_y)" )
      if $c->debug;

    $c->stash->{logo_x} = $logo_x;
    $c->stash->{logo_y} = $logo_y;
    
    $c->stash->{large_logo} = 1;
  }

  $c->stash->{template} = 'components/logo.tt';
}

#---------------------------------------

=head2 old_logo : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_logo : Path( '/family/logo' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family::FamilyActions::old_logo: redirecting to "logo"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'logo', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 logo_image : Chained

Returns the HMM logo image for this family.

=cut

sub logo_image : Chained( 'family' )
                 PathPart( 'logo_image' ) 
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  $c->log->debug( 'Family::FamilyActions::logo_image: returning raw image' )
    if $c->debug;
  
  my $logo = $c->forward( 'get_logo' );
    
  return if $c->stash->{errorMsg};
  
  if ( $c->req->param('dl') ) {
    my $filename = $c->stash->{acc} . '_logo.png';

    $c->log->debug( 'Family::FamilyActions::logo_image: forcing download of logo as '
                    . $filename ) if $c->debug;
    
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $logo );
}

#---------------------------------------

=head2 old_logo_image : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_logo_image : Path( '/family/logo_image' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_logo_image: redirecting to "logo_image"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'logo_image', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- RESTful utility actions -----------------------------------------------------
#-------------------------------------------------------------------------------

=head1 RESTFUL UTILITY ACTIONS

=head2 hmm : Chained

Serve the contents of the HMM for a Pfam-A entry from the database. Requires 
the "mode" parameter to be set either to "ls" or "fs".

=cut

sub hmm : Chained( 'family' )
          PathPart( 'hmm' ) 
          Args( 0 ) {
  my ( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  my $cacheKey = 'hmm' . $c->stash->{acc};
  my $hmm      = $c->cache->get( $cacheKey );

  if ( defined $hmm ) {
    $c->log->debug( 'Family::gethmm: extracted HMM from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::gethmm: failed to extract HMM from cache; going to DB' )
      if $c->debug;
     
    my $rs = $c->model('PfamDB::PfamaHmm')
               ->find( $c->stash->{pfam}->pfama_acc );

    unless ( $rs ) {
      $c->log->warn( 'Family::FamilyActions::hmm: failed to find row' )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not find the HMM for ' 
                              . $c->stash->{acc};
      $c->res->status( 500 );
      return;
    }

    $hmm = $rs->hmm;
    
    unless ( $hmm ) {
      $c->log->warn( 'Family::FamilyActions::hmm: failed to retrieve HMM from row' )
        if $c->debug;
        
      $c->stash->{errorMsg} = 'We could not retrieve the HMM for ' 
                              . $c->stash->{acc};
                              
      $c->res->status( 500 );
      return;
    }

    # cache the raw HMM
    $c->cache->set( $cacheKey, $hmm ) unless $ENV{NO_CACHE};
  }

  # build a name for the file that will be downloaded
  my $filename = $c->stash->{pfam}->pfama_id . '.hmm';

  # set the response headers
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # at this point we should have the HMM in hand, so spit it out to the 
  # response and we're done
  $c->res->body( $hmm );

  # the RenderView action on the end method in Section.pm will spot that there's
  # content in the response and return without trying to render any templates
}

#---------------------------------------

=head2 old_hmm : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_hmm : Path( '/family/hmm' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_hmm: redirecting to "hmm"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'hmm', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_logo : Private

Retrieves the HMM logo for this family, either from cache or the DB. Returns
the logo, if found.

=cut

sub get_logo : Private {
  my ( $this, $c ) = @_;
  
  my $cache_key = 'logo' . $c->stash->{acc};
  my $logo = $c->cache->get( $cache_key );
  
  if ( defined $logo ) {
    $c->log->debug( 'Family::FamilyActions::logo: extracted logo from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::FamilyActions::logo: failed to extract logo from cache; going to DB' )
      if $c->debug;
    
    my $rs = $c->model('PfamDB::PfamaHmm')
               ->find( $c->stash->{pfam}->pfama_acc );
    $logo = $rs->logo;

    unless ( defined $logo ) {
      $c->log->debug( 'Family::FamilyActions::logo: failed to retrieve logo from DB' )
        if $c->debug;

      $c->res->status( 204 );

      return;
    }
  
    # cache the LOGO
    $c->cache->set( $cache_key, $logo ) unless $ENV{NO_CACHE};
  }

  return $logo;  
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

