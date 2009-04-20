
# BatchSearch.pm
# jt6 20061108 WTSI
#
# $Id: InteractiveSearch.pm,v 1.4 2009-04-20 09:06:00 jt6 Exp $

=head1 NAME

PfamBase::Controller::Search::InteractiveSearch - parent class for interactive 
searches

=cut

package PfamBase::Controller::Search::InteractiveSearch;

=head1 DESCRIPTION

This is the parent class for interactive search operations.

$Id: InteractiveSearch.pm,v 1.4 2009-04-20 09:06:00 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Queues a sequence search job and returns a page that polls the server for
results.

=cut

sub search : Path {
  my ( $this, $c ) = @_;

  # validate the input
  unless ( $c->forward('validate_input') ) {

    # copy the error message into the slot in the stash where the templates
    # expect to find it
    $c->stash->{seqSearchError} = $c->stash->{searchError}
      || 'There was an unknown problem when validating your sequence.';

    # if we're returning XML, we need to set a template to render the error
    # message. If we're emitting HTML, the end action (ultimately on Section)
    # will take of us and return us to the HTML page containing search form
    # and show the error message
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
    }

    return;
  }

  # no errors with the input; try to submit the search

  # success !
  if ( $c->forward('queue_seq_search') ) {

    $c->log->debug(
      'Search::Sequence::search: sequence search submitted; polling')
      if $c->debug;

    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/poll_xml.tt';
      $c->res->content_type('text/xml');
    }
    else {
      $c->stash->{template} = 'pages/search/sequence/polling.tt';
    }

  }

  # failure...
  else {

    $c->stash->{seqSearchError} = $c->stash->{searchError}
      || 'There was an unknown problem when submitting your search.';

    $c->log->debug(
      'Search::Sequence::search: problem with submission; re-rendering form')
      if $c->debug;

    # point to the XML error template if emitting XML, otherwise, we're just
    # done here
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
    }

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
