
# Search.pm
# jt6 20061108 WTSI
#
# $Id: Search.pm,v 1.23 2008-09-03 15:39:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search - top-level platform for performing various searches

=cut

package PfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller is responsible for running searches. This is actually just an
empty wrapper around a shared base class. See
L<PfamBase::Controller::Search> for more details.

$Id: Search.pm,v 1.23 2008-09-03 15:39:58 jt6 Exp $

=cut

use strict;
use warnings;

# need to use Module::Pluggable here, otherwise the Search plugins don't 
# get correctly registered
use Module::Pluggable;

use base 'PfamBase::Controller::Search';

#-------------------------------------------------------------------------------

=head2 formatTerms : Private

Inherited by the search plugins, this action formats the query terms to add
wildcard and fulltext operators to each word in the list. This base
implementation just prepends a "+" (require that the word is present in every
returned row; necessary for "IN BOOLEAN MODE" queries) and appends a "*"
(wildcard) to each term.

This method should be over-ridden by plugin search classes if they
need some other processing to be performed on the search terms.

=cut

sub formatTerms : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
    join " ", map { $_ = "+$_*" } split /\s+|\W|\_/, $c->stash->{rawQueryTerms};

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
