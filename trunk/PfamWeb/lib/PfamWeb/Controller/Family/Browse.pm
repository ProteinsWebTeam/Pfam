
# Browse.pm
# jt6 20060717 WTSI
#
# $Id: Browse.pm,v 1.12 2007-04-27 16:18:46 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Browse - controller to build the "browse
by family" pages

=cut

package PfamWeb::Controller::Family::Browse;

=head1 DESCRIPTION

Retrieves the data for the "browse by family" pages.

Generates a B<full page>.

$Id: Browse.pm,v 1.12 2007-04-27 16:18:46 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "family" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Just overrides the default begin method from Section.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # override the begin method from Family, to avoid error messages
  # when there's no Pfam accession or ID specified in the
  # parameters. Which there won't ever be for the browse pages.

  # tell the navbar where we are
  $c->stash->{nav} = "browse";
}

#-------------------------------------------------------------------------------

=head2 browse : Path

Retrieves data for the specified browse page.

=cut

sub browse : Path {
  my( $this, $c ) = @_;

  # set the page to be cached for one week
#  $c->cache_page( 604800 );

  my @res;

  if( lc $c->req->param("browse") eq "numbers" ) {
    $c->log->debug( "Family::Browse::browse: browsing \"numbers\"..." );
    $c->stash->{char} = "numbers";

    # run the query to get back all families starting with a number
    @res = $c->model("PfamDB::Pfam")
  	  ->search( { pfamA_id => { "REGEXP", "^[0-9]" } },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  order_by => "pfamA_id ASC" } );

  } elsif( lc $c->req->param("browse") eq "top twenty" ) {
    $c->log->debug( "Family::Browse::browse: browsing \"top twenty\"..." );
    $c->stash->{char} = "top twenty";

  	# retrieve the top twenty largest families, ordered by number of
  	# sequences in the family
    @res = $c->model("PfamDB::Pfam")
  	  ->search( { },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  rows     => 20,
				  page     => 1,
				  order_by => "num_full DESC" }
			  )->all;

  } else {

    $c->log->debug( "Family::Browse::browse: not a number, not \"top twenty\"..." );

    # see if we should load the page for families starting with a given letter
    my $char;
    ( $char ) = $c->req->param( "browse" ) =~ /^(\w{1})$/;

    if( defined $char ) {
      $c->log->debug( "Family::Browse::browse: browsing for a character: |$char|" );
      $c->stash->{char} = uc $char;
  
    	# run the query to get back all families starting with the
    	# specified letter, ordered by ID
    	@res = $c->model("PfamDB::Pfam")
    	  ->search( { pfamA_id => { "LIKE", "$char%" } },
  				{ join     => [ qw/pfamA_web/ ],
  				  prefetch => [ qw/pfamA_web/ ],
  				  order_by => "pfamA_id" } );

    } else {

      # either "new" specified, or no starting letter specified, so default 
      # to new families anyway
      $c->log->debug( "Family::Browse::browse: browsing new entries" );
      $c->stash->{char} = "new";
  
      @res = $c->model("PfamDB::Pfam")
    	  ->search( { change_status => "NEW" },
  				{ join     => [ qw/pfamA_web/ ],
  				  prefetch => [ qw/pfamA_web/ ],
  				  order_by => "pfamA_id ASC" } );

    }

  }

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # set the template and let the default end action from Section
  # render it for us
  if( $c->req->param("list") ) {
  	# we want to return just the list of Pfam IDs, as a snippet of HTML.
  	# This is used in the domain query search form
  	$c->stash->{template} = "pages/browseIds.tt";
  } else {
  	# just render as a regular "browse" page
  	$c->stash->{template} = "pages/browse.tt";
  }
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
