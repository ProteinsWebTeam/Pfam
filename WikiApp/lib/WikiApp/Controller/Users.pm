
# Users.pm
# jt6 20100426 WTSI
#
# $Id$

=head1 NAME

WikiApp::Controller::Users - shows wikipedia users list

=cut

package WikiApp::Controller::Users;

=head1 DESCRIPTION

This controller handles the display and approval of wikipedia users.

=cut

use Moose;
use namespace::autoclean;
use MediaWiki::Bot;
use Data::Dump qw(dump);

BEGIN {extends 'Catalyst::Controller'; }

#-------------------------------------------------------------------------------

=head1 ACTIONS

=head2 users : Global

Shows the list of wikipedia users.

=cut

sub users : Global Args(0) {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'users.tt';

  my @users = $c->model( 'WikiAppDB::Wikiuser' )
                ->search( {}, {} );

  $c->log->debug( 'Users::users: found ' . scalar @users . ' users' )
    if $c->debug;
  
  $c->stash->{users} = \@users;
}

#-------------------------------------------------------------------------------

=head2 user : Chained('/') PathPart('user')

Retrieves the details of the specified user.

=cut

sub user : Chained('/') PathPart('user') CaptureArgs(1) {
  my ( $this, $c, $user_name ) = @_;

  if ( $user_name =~ m/^[\#\.\/\\]+$/ ) {
    $c->res->body( qq[invalid user name ("$user_name")] );
    $c->detach;
    return;
  }

  $c->log->debug( "Users::user: user_name: |$user_name|" )
    if $c->debug;

  my $user_row = $c->model( 'WikiAppDB::Wikiuser' )
                   ->find_or_create( { user_name => $user_name },
                                     { key => 'primary' } );

  $c->stash->{user} = $user_row;
}

#-------------------------------------------------------------------------------

=head2 is_approved : Chained('user') PathPart('isapproved')

Returns the approval status (0 or 1) of the specified user.

=cut

sub is_approved : Chained('user') PathPart('isapproved') Args(0) {
  my ( $this, $c ) = @_;

  my $approval = $c->stash->{user}->approved ? 1 : 0;

  $c->res->body( $approval );
}

#-------------------------------------------------------------------------------

=head2 add_single_edit : Chained('user') PathPart('addedit')

Increments the edit count for the specified user by 1. Returns the new number
of edits for the user.

=cut

sub add_single_edit : Chained('user') PathPart('addedit') Args(0) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'User::add_single_action: adding a single edit for the user' )
    if $c->debug;
  $c->forward( $c->controller('Users')->action_for('add_edits'), [ 1 ] );
}

#-------------------------------------------------------------------------------

=head2 add_edits : Chained('user') PathPart('addedit')

Increments the edit count for the specified user by the specified number.
Returns the new number of edits for the user.

=cut

sub add_edits : Chained('user') PathPart('addedit') Args(1) {
  my ( $this, $c, $new_edits ) = @_;

  unless ( $new_edits =~ m/^\d+$/ ) {
    $c->log->debug( 'Users::add_edits: not a valid "new_edits" count' )
      if $c->debug;
    $c->res->status( 400 ); # Bad request
    $c->res->body( 'New edit count was not a valid integer' );
    return;
  }

  eval {
    $c->stash->{user}->add_edits( $new_edits );
  };
  if ( $@ ) {
    $c->log->debug( 'Users::add_edits: failed to add new edits in model' )
      if $c->debug;
    $c->res->status( 500 ); # Internal server error
    $c->res->body( "Failed to set new edit count: $@" );
    return;
  }

  $c->res->status( 200 ); # OK
  $c->res->body( $c->stash->{user}->number_edits );
}

#-------------------------------------------------------------------------------

=head2 before qw( toggle_approval approve unapprove )

Checks that the user has authorisation to change user approval status. Redirects
to the home page and displays an appropriate message if not.

=cut

before qw( toggle_approval approve unapprove ) => sub {
  my ( $this, $c ) = @_;

  if ( $c->user_exists ) {
    $c->log->debug( 'Users::before approval actions: user is logged in; allowing request' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Users::before approval actions: user is NOT logged in; denying request' )
      if $c->debug;
    $c->stash->{message} = 'You are not authorised to change user statuses. Please login first.';
    $c->res->redirect( $c->uri_for('/') );
    $c->detach();
  }
};

#-------------------------------------------------------------------------------

=head2 toggle_approval : Chained('user') PathPart('toggleapproval')

Toggles the approval status of a user. Returns the approval status (0 or 1) in
the response body.

=cut

sub toggle_approval : Chained('user') PathPart('toggleapproval') Args(0) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Users::approve: toggling approval status for user_name: |'
                  . $c->stash->{user}->user_name . '|; currently '
                  . $c->stash->{user}->approved )
    if $c->debug;

  my $approval = $c->stash->{user}->approved ? 0 : 1;

  $c->stash->{user}->update( { approved => $approval } );
  $c->res->body( $approval );
}

#-------------------------------------------------------------------------------

=head2 approve : Chained('user') PathPart('approve')

Marks a user as approved. Returns the approval status (0 or 1) in the response 
body.

=cut

sub approve : Chained('user') PathPart('approve') Args(0) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Users::approve: approving user_name: |'
                  . $c->stash->{user}->user_name . '|' )
    if $c->debug;

  $c->stash->{user}->update( { approved => 1 } );
  $c->res->body( 1 );
}

#-------------------------------------------------------------------------------

=head2 unapprove : Chained('user') PathPart('unapprove')

Marks a user as unapproved. Returns the approval status (0 or 1) in the 
response body.

=cut

sub unapprove : Chained('user') PathPart('unapprove') Args(0) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Users::approve: UNapproving user_name: |'
                  . $c->stash->{user}->user_name . '|' )
    if $c->debug;

  $c->stash->{user}->update( { approved => 0 } );
  $c->res->body( 0 );
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>
Paul Gardner, C<pg5@sanger.ac.uk>
Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
         Paul Gardner (pg5@sanger.ac.uk)

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

__PACKAGE__->meta->make_immutable;

1;
