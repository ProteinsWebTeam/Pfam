
# Jump.pm
# jt6 20060807 WTSI
#
# $Id: Jump.pm,v 1.2 2008-05-16 14:58:22 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Jump - guess the type of entry that the user wants to see

=cut

package PfamBase::Controller::Search::Jump;

=head1 DESCRIPTION

This is the base class for "jump" searching classes in specific applications. 
The sub-classes must provide, at a minimum, a private C<guess> method, which 
should:

=over 4

=item determine the C<Action> to which the request should be forwarded

=item forward to the guessed action, and

=item return the action

If the L<jump> method finds that the L<forward> returns undef, it returns
an error message saying that the guess failed. 

$Id: Jump.pm,v 1.2 2008-05-16 14:58:22 jt6 Exp $

=cut

use strict;
use warnings;

use URI;

use base 'PfamBase::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 jump : Path

Tries to find the URL for the specified entry accession or ID. If the 
"entryType" parameter is specified and if the value is found in the 
configuration, we look only for that particualr type of entry. If no "entryType"
is specified, we'll look for any type.

=cut

sub jump : Path {
  my( $this, $c ) = @_;
  
  # de-taint the entry ID or accession
  my $entry = '';
  ( $entry ) = $c->req->param('entry') =~ /^([\w\-_\s()\.]+)$/;
  $c->log->debug( "Search::Jump::jump: called with entry |$entry|" );

  # strip off leading and trailing whitespace
  $entry =~ s/^\s*(.*?)\s*$/$1/;
  $c->log->debug( "Search::Jump::jump: trimmed entry to |$entry|" );

  # bail immediately if there's no valid entry given
  unless( $entry ) {
    $c->stash->{error} = 'No valid accession or ID';
    return;
  }

  # now we know we have an entry. See if the caller specified the type of that
  # entry. If it did, we don't bother trying to guess the type, but just
  # redirect straight to the appropriate URL 
  my $entry_type;
  if( $c->req->param('type') ) {
    $entry_type = $this->{jumpTargets}->{ $c->req->param('type') };
    $c->log->debug( 'Search::Jump::jump: looking for entry type: |'
                    . ( $entry_type || '' ) . '|' )
  }
  
  # let's guess !
  my $action = $c->forward( 'guess', [ $entry, $entry_type ] );

  if( $action ) {
    $c->log->debug( "Search::Jump::jump: we've made a guess; redirecting to |$action|" );
    $c->stash->{url} = $c->uri_for( "/$action", { entry => $entry } );
  } else {
    $c->log->debug( "Search::Jump::jump: couldn't guess entry type..." );

    # set the error message differently according to whether we were trying to
    # "look up" and entry or guess the type
    $c->stash->{error} = $entry_type ? 'Entry not found' : "Couldn't guess entry";
  }
  
}

#-------------------------------------------------------------------------------

=head2 end : Private

Return either a text/plain response with the guessed URL in the body, or an
error response (status 400) with the error message in the body.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  $c->res->content_type( 'text/plain' );
  
  if( $c->stash->{error} ) {
    $c->res->body( $c->stash->{error} );
    $c->res->status( 400 );
  } else {
    $c->res->body( $c->stash->{url} );
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
