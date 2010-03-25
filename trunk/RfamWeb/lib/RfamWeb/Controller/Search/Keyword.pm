
# Keyword.pm
# jt6 20060807 WTSI
#
# $Id: Keyword.pm,v 1.3 2009-02-11 10:51:50 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Search::Keyword - a keyword search engine for the site

=cut

package RfamWeb::Controller::Search::Keyword;

=head1 DESCRIPTION

This controller reads a list of search plugins from the application
configuration and forwards to each of them in turn, collects the
results and hands off to a template to format them as a results page.

$Id: Keyword.pm,v 1.3 2009-02-11 10:51:50 jt6 Exp $

=cut

use strict;
use warnings;

use base 'RfamWeb::Controller::Search';
use base 'PfamBase::Controller::Search::Keyword';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 lookup_term : Private

Does a quick look-up to see if the search term matches a Pfam ID
or accession.

=cut

sub lookup_term : Private {
  my ( $this, $c ) = @_;

   my $rs = $c->model('RfamDB::Rfam')
              ->search( [ { rfam_acc => $c->stash->{rawQueryTerms} },
                          { rfam_id  => $c->stash->{rawQueryTerms} } ] );

  # we're going to assume that there's only one hit here... we're in
  # trouble if there's more than one, certainly
  my $hit = $rs->next;
  $c->stash->{lookupHit} = $hit if $hit;
}

#-------------------------------------------------------------------------------

=head2 merge

Merges results from plugins. Merging requires that each row of the C<ResultSet>
has access to an Rfam accession. We'll try to get it using

  $rs->rfam_acc

or

  $rs->auto_rfam->rfam_acc

but if neither method works, the row is skipped.

=cut

sub merge : Private {
  my ( $this, $c, $pluginName, $rs ) = @_;

  ROW: while ( my $row = $rs->next ) {

    my ( $acc, $hit );

    TRY: {

      # first try accessing the accession on the table row directly
      eval {
        $acc = $row->rfam_acc;
      };
      if ( $@ ) {
        $c->log->debug( 'Search::Keyword::merge: caught an exception when trying '
                       . " \$row->rfam_acc for plugin |$pluginName|: $@" )
          if $c->debug;
      }

      if ( defined $acc ) {
        $hit = $c->stash->{results}->{$acc} ||= {};
        $hit->{dbObj} = $row;
        last TRY;
      }

      # we couldn't find the accession on the row itself, so try walking down one
      # possible relationship, "auto_rfam", and see if that gets us to another
      # table, which hopefully does store the details of the Rfam family
      #
      # This is all a bit convoluted, but it means that we shouldn't need to add
      # proxy columns indiscriminately throughout the model, just so that text
      # searching can work
      eval {
        $acc = $row->auto_rfam->rfam_acc;
      };
      if ( $@ ) {
        $c->log->debug( 'Search::Keyword::merge: caught an exception when trying '
                       . " \$row->auto_rfam->rfam_acc for plugin |$pluginName|: $@" )
          if $c->debug;
      }

      if ( defined $acc ) {
        $hit = $c->stash->{results}->{$acc} ||= {};
        $hit->{dbObj} = $row->auto_rfam;
        # last TRY;
      }

    }

    $hit->{query}->{$pluginName} = 1;
    $c->stash->{pluginHits}->{$pluginName} += 1;

    # score this hit
    $this->_computeScore( $c, $hit );
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
