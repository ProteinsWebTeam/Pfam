
# Browse.pm
# jt6 20060821 WTSI
#
# Controller for the "browse" pages for clans.
#
# $Id: Browse.pm,v 1.1 2006-08-22 15:12:51 jt6 Exp $

package PfamWeb::Controller::Clan::Browse;

use strict;
use warnings;

use base "Catalyst::Controller";

sub begin : Private {}

# pick up a URL like http://localhost:3000/clan/browse/

sub browse : Path {
  my( $this, $c ) = @_;

  my @res = $c->model("PfamDB::Clans")->search( {},
												{ order_by => "clan_id ASC" }
											  );

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

}


# render the page

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/errors.tt";
  } else {
	$c->stash->{pageType} = "clan";
	$c->stash->{template} = "pages/browseClans.tt";
  }

  # and use it
  $c->stash->{fullPage} = 1;
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}

1;
