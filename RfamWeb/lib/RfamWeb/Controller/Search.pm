
# Search.pm
# jt6 20061108 WTSI
#
# $Id: Search.pm,v 1.1 2008-09-12 09:13:10 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Search - top-level platform for performing various searches

=cut

package RfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller is responsible for running searches. This is actually just an
empty wrapper around a shared base class. See
L<PfamBase::Controller::Search> for more details.

$Id: Search.pm,v 1.1 2008-09-12 09:13:10 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

use Data::UUID;
use Data::Dump qw( dump );

BEGIN {
  extends 'Catalyst::Controller::REST';
}

with 'RfamWeb::Roles::Search::Keyword',
     'RfamWeb::Roles::Search::SingleSequence',
     'RfamWeb::Roles::Search::Batch',
     'RfamWeb::Roles::Search::Taxonomy',
     'RfamWeb::Roles::Search::Type';

# set up the list of content-types that we handle via the RESTful interface
__PACKAGE__->config(
  'default' => 'text/html',
  'map'     => {
    'text/html'          => [ 'View', 'TT' ],
    'text/xml'           => [ 'View', 'TT' ],
    'text/plain'         => [ 'SeqSearchResultsFormatter', 'tsv' ],
    'application/x-gff3' => [ 'SeqSearchResultsFormatter', 'gff' ],
    'application/json'   => 'JSON',
  }
);

# set the name of the section
__PACKAGE__->config( SECTION => 'search' );

#-------------------------------------------------------------------------------

=head2 METHODS

=head2 begin : Private

Sets up the stash for all types of search.

=cut

after 'begin' => sub {
  my( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'search';
  
  # tell the layout template to disable the summary icons
  $c->stash->{iconsDisabled} = 1;
};

#-------------------------------------------------------------------------------

=head2 search : Chained('/') PathPart('search') CaptureArgs(0)

Start of a chain for handling search actions.

=cut

sub search : Chained( '/' )
             PathPart( 'search' )
             CaptureArgs( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::search: start of chain' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 search : Chained('search') PathPart('') Args(0)

Shows the search page.

=cut

sub search_page : Chained( 'search' )
                  PathPart( '' )
                  Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::search_page: end of chain; showing search page' )
    if $c->debug;

  $c->stash->{pageType} = 'search';
  $c->stash->{template} = 'pages/layout.tt';
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
