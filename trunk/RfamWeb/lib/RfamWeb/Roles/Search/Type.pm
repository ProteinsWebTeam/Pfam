
# Type.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Search::Type - role containing actions related to
family type searching

=cut

package RfamWeb::Roles::Search::Type;

=head1 DESCRIPTION

A role to add family type search-related methods to the main search controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 type_search : Chained('search') PathPart('type') Args(0)

Search based on family type.

=cut

sub type_search : Chained( 'search' )
                  PathPart( 'type' )
                  Args( 0 ) {
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

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012 Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk)

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

