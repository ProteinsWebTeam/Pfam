# Methods.pm
# jt6 20120920 WTSI
#
# $Id$

=head1 NAME

PfamWeb::Roles::Family::Methods - role to add various utility methods to the
family controller

=cut

package PfamWeb::Roles::Family::Methods;

=head1 DESCRIPTION

This role is a collection of methods related to the L<Family> controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------

=head1 ACTIONS

=head2 id : Chained

Returns the ID for this family as a single, plain text string. Returns 404 if
there's no family to work on.

=cut

sub id : Chained( 'family' )
         PathPart( 'id' )
         Args( 0 ) {
  my ( $this, $c ) = @_;
  
  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  if ( defined $c->stash->{pfam} ) {    
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfama_id );
    }
  }
  else { 
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#---------------------------------------

=head2 old_id : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_id : Path( '/family/id' ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family:FamilyActions::old_id: redirecting to "id"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'id', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 acc : Chained

Returns the accession for this family as a single, plain text string. Returns 
404 if there's no family to work on.

=cut

sub acc : Chained( 'family' )
          PathPart( 'acc' )
          Args( 0 ) {
  my ( $this, $c ) = @_;
  
  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  if ( defined $c->stash->{pfam} ) {    
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfama_acc );
    }
  }
  else { 
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#---------------------------------------

=head2 old_acc : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_acc : Path( '/family/acc' ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family:FamilyActions::old_acc: redirecting to "acc"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'acc', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 desc : Chained

Returns the description string for a family. If the "output_pfamalyzer"
parameter is set, the output returns more family information.

=cut

sub desc : Chained( 'family' )
           PathPart( 'desc' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  if ( defined $c->stash->{pfam} ) {

    $c->res->content_type( 'text/plain' );

    if ( $c->stash->{output_pfamalyzer} ) {
      $c->res->body(
        $c->stash->{pfam}->pfama_acc   . "\t" .
        $c->stash->{pfam}->author      . "\t" .
        $c->stash->{pfam}->type        . "\t" .
        $c->stash->{pfam}->num_seed    . "\t" .
        $c->stash->{pfam}->num_full    . "\t" .
        $c->stash->{pfam}->description . "\t" .
        $c->stash->{pfam}->comment
      );
    }
    else {
      $c->res->body( $c->stash->{pfam}->description );
    }
  }
  else {
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
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

