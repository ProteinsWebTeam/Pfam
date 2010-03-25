
# Type.pm
# jt6 20081121 WTSI
#
# $Id: Type.pm,v 1.1 2009-01-06 11:33:06 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Search::Type - perform searches on family type

=cut

package RfamWeb::Controller::Search::Type;

=head1 DESCRIPTION

This controller is responsible for running searches based on the Rfam family
type.

$Id: Type.pm,v 1.1 2009-01-06 11:33:06 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base qw( RfamWeb::Controller::Search );
             
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 results : Local

Returns the result of the type search

=cut

sub type : Path {
  my ( $this, $c ) = @_;

  my @paths;
  if ( defined $c->req->param('paths') ) {
    $c->log->debug( 'Search::Type::type: paths parameters: |' 
                    . $c->req->param('paths') . '|' ) if $c->debug;

    if ( $c->req->param('paths') =~ m/^([\s\w\;\-\,]+)$/ ) {
      @paths = split ",", $1;         
    }
    else {
      $c->stash->{typeSearchError} = 'You did not supply a valid list of family types.';

      $c->log->debug( 'Search::Type::type: bad paths list' )
        if $c->debug;

      return;
    }
    
  }
  else {
    $c->stash->{typeSearchError} = 'You did not supply a list of family types.';

    $c->log->debug( 'Search::Type::type: no paths list' )
      if $c->debug;

    return;
  }

  unless ( scalar @paths ) {
    $c->stash->{typeSearchError} = 'We did not find a list of family types.';

    $c->log->debug( 'Search::Type::type: empty paths list' )
      if $c->debug;

    return;
  }

  my @hits = $c->model('RfamDB::Rfam')
               ->search( { type => { 'IN', \@paths } },
                         {} );

  $c->log->debug( 'Search::Type::type: found ' . scalar @hits . ' hits' )
    if $c->debug;

  $c->stash->{paths}    = \@paths;
  $c->stash->{results}  = \@hits;
  $c->stash->{template} = 'pages/search/type/results.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

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
