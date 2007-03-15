
# Browse.pm
# jt6 20060821 WTSI
#
# $Id: Browse.pm,v 1.4 2007-03-15 14:06:12 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure - controller for the clan "browse"

=cut

package PfamWeb::Controller::Clan::Browse;

use strict;
use warnings;

use base "Catalyst::Controller";

=head1 DESCRIPTION

Just a simple wrapper around a template that displays a "browse" page
for clans. Since there are relatively few clans we can put them all on
one page, rather than splitting them up like the families.

Generates a B<full page>.

$Id: Browse.pm,v 1.4 2007-03-15 14:06:12 jt6 Exp $

=cut

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 browse : Path

Picks up a URL like C<http://localhost:3000/clan/browse/>. Retrieves a
list of all clan IDs and hands off to the template.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # override the begin method from Family, to avoid error messages
  # when there's no Pfam accession or ID specified in the
  # parameters. Which there won't ever be for the browse pages.

}

sub browse : Path {
  my( $this, $c ) = @_;

  # set the page to be cached for one week
  $c->cache_page( 604800 );

  my @res = $c->model("PfamDB::Clans")
    ->search( {},
			  { order_by => "clan_id ASC" } );

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

}

#-------------------------------------------------------------------------------

=head2 end : Private

Renders the clan browse pages.

=cut

# render the page

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if ( scalar @{ $c->error } ) {
  	$c->stash->{template} = "pages/error.tt";
  } else {
  	$c->stash->{pageType} = "clan";
  	$c->stash->{template} = "pages/browseClans.tt";
  }

  # and use it
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->clear_errors;

}

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
