
# Search.pm
# jt6 20061108 WTSI
#
# $Id: Search.pm,v 1.26 2008-10-22 15:41:20 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search - top-level platform for performing various searches

=cut

package PfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller is responsible for running searches. This is actually just an
empty wrapper around a shared base class. See
L<PfamBase::Controller::Search> for more details.

$Id: Search.pm,v 1.26 2008-10-22 15:41:20 jt6 Exp $

=cut

use utf8;
use strict;
use warnings;

# need to use Module::Pluggable here, otherwise the Search plugins don't
# get correctly registered
use Module::Pluggable;

use base qw( PfamBase::Controller::Search
             PfamWeb::Controller::Section );

# set the name of the section
__PACKAGE__->config( { SECTION => 'search' } );

#-------------------------------------------------------------------------------

=head2 METHODS

=head2 auto : Private

Checks for a sequence that should be used to pre-fill the search form.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  # see if we should pre-fill the sequence field, based on the sequence that
  # was handed to us by the referring site

  # detaint it, naturally, and accept only sequences that are shorter than some
  # limit, which is set in the config
  if ( defined $c->req->param('preseq') and
       $c->req->param('preseq') =~ m/^([A-Za-z]+)$/ and
       length( $1 ) < $this->{maxPrefillLength} ) {

    $c->log->debug( 'Search::auto: found a sequence with which to pre-fill the search form' )
      if $c->debug;

    # stash it and let the template stuff the value into the form
    $c->stash->{preseq} = $1;
  }

  # check if the preseq should be handled as DNA rather than protein
  if ( defined $c->req->param('dnapreseq') and
       $c->req->param('dnapreseq') =~ m/^([A-Za-z\>\_\-\s\d]+)$/ and
       length( $1 ) < $this->{maxPrefillLength} ) {

    $c->log->debug( 'Search::auto: found a DNA sequence with which to pre-fill the search form' )
      if $c->debug;

    # stash it and let the template stuff the value into the form
    $c->stash->{dnapreseq} = $1;
  }

  # copy searchable sequence limits from config to stash
  $c->stash->{$_} = $this->{$_} for qw( maxProteinSeqLength maxDnaSeqLength minProteinSeqLength minDnaSeqLength );

  return 1;
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
