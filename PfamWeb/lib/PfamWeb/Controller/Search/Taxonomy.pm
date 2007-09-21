
# Taxonomy.pm
# jt6 20070918 WTSI
#
# $Id: Taxonomy.pm,v 1.1 2007-09-21 11:21:50 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Taxonomy - controller for taxonomy searches

=cut

package PfamWeb::Controller::Search::Taxonomy;

=head1 DESCRIPTION

A search controller for performing taxonomy searches

$Id: Taxonomy.pm,v 1.1 2007-09-21 11:21:50 jt6 Exp $

=cut

use strict;
use warnings;

use Search::QueryParser;
use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Path

Perform a taxonomy search

=cut

sub process : Path {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Search::Taxonomy::process: starting a taxonomy search' );

  # make sure we got *something*
  unless( defined $c->req->param('q') ) {
    $c->stash->{taxSearchError} = 'You did not supply a query string.';
    return;
  }

  # make sure it was plain text
  unless( $c->req->param('q') =~ m/^([\w().\s]+)$/ ) {
    $c->stash->{taxSearchError} = 'You did not supply a valid query string.';
    return;
  }
  $c->log->debug( "Search::Taxonomy::process: found query string: |$1|" );

  # we got a valid string, but does it parse ?
  my $qp = new Search::QueryParser;
  my $pq = $qp->parse( $1 );
  $c->log->debug( 'Search::Taxonomy::process: parsed query: ', dump $pq );

  
  $c->stash->{template} = 'pages/search/taxonomy/results.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

# none

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;