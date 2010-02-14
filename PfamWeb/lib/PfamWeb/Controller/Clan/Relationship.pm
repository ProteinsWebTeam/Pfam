
# Relationship.pm
# jt6 20060411 WTSI
#
# Controller to return the clan relationship image.
#
# $Id: Relationship.pm,v 1.4 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Clan::Relationship - clan relationship image server

=cut

package PfamWeb::Controller::Clan::Relationship;

=head1 DESCRIPTION

A simple controller with just one action, which returns the clan relationship
image file.

$Id: Relationship.pm,v 1.4 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Clan';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 image : Local

Serves the image file for the clan relationship diagram.

=cut

sub image : Local {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Clan::Relationship::image: serving clan relationship image' )
    if $c->debug;
  
  if( defined $c->stash->{relationshipImage} ) {
    $c->res->content_type( 'image/gif' );
    $c->res->body( $c->stash->{relationshipImage} );
  } else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
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
