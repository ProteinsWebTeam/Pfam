
# Browse.pm
# jt6 20060717 WTSI
#
# Controller for the "browse" pages. Originally by RDF.
#
# $Id: Browse.pm,v 1.3 2006-08-22 15:13:29 jt6 Exp $

package PfamWeb::Controller::Family::Browse;

use strict;
use warnings;

use base "Catalyst::Controller";

sub begin : Private {}

# pick up a URL like http://localhost:3000/family/browse?browse=a

sub browse : Path {
  my( $this, $c ) = @_;

  return unless defined $c->req->param( "browse" );

  my @res;

  if( lc $c->req->param("browse") eq "numbers" ) {

	$c->stash->{char} = "numbers";
	# run the query to get back all families starting with a number
	@res = $c->model("PfamDB::Pfam")->search( { pfamA_id => { "REGEXP", "^[0-9]" } },
											  { order_by => "pfamA_id ASC",
												join     => [ qw/pfamA_web/ ],
												prefetch => [ qw/pfamA_web/ ] }
											);
  } else {

	my $char;
	( $char ) = $c->req->param( "browse" ) =~ /^(\w)/;

	return unless defined $char;

	$c->stash->{char} = uc $char;
	$c->log->debug( "Browse::browse: looking for letter \"$char\"" );

	# run the query to get back all families starting with the specified letter
	@res = $c->model("PfamDB::Pfam")->search( { pfamA_id => { "LIKE", "$char%" } },
											  { join     => [ qw/pfamA_web/ ],
												prefetch => [ qw/pfamA_web/ ] }
											);
  }

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

}


# pick up a URL like http://localhost:3000/browse/top_twenty

# sub browseTopTwenty : Path( "top_twenty" ) {
#   my( $this, $c ) = @_;

#   # run the query to get back all families starting with a number
#   my @res = $c->model("PfamDB::Pfam")->search( { },
# 										  { order_by => "pfamA_id ASC",
# 											limit    => 20,
# 											join     => [ qw/pfamA_web/ ],
# 											prefetch => [ qw/pfamA_web/ ] }
# 										);

#   # and stash them for the template
#   $c->stash->{browse} = \@res if scalar @res;

# }


# render the page

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/errors.tt";
  } else {
	$c->stash->{pageType} = "family";
	$c->stash->{template} = "pages/browse.tt";
  }

  # and use it
  $c->stash->{fullPage} = 1;
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}

1;
