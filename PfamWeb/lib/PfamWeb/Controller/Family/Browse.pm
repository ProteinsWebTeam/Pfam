
# Browse.pm
# jt6 20060717 WTSI
#
# Controller for the "browse" pages. Originally by RDF.
#
# $Id: Browse.pm,v 1.6 2006-11-16 13:25:19 jt6 Exp $

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
    @res = $c->model("PfamDB::Pfam")
	  ->search( { pfamA_id => { "REGEXP", "^[0-9]" } },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  order_by => "pfamA_id ASC" }
			  );

  } elsif( lc $c->req->param("browse") eq "new" ) {
    $c->stash->{char} = "new";

    # run the query to retrieve new families
    @res = $c->model("PfamDB::Pfam")
	  ->search( { change_status => "NEW" },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  order_by => "pfamA_id ASC" }
			  );

  } elsif( lc $c->req->param("browse") eq "top twenty" ) {
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

	my $char;
	( $char ) = $c->req->param( "browse" ) =~ /^(\w)/;

	return unless defined $char;

	$c->stash->{char} = uc $char;

	# run the query to get back all families starting with the
	# specified letter, ordered by ID
	@res = $c->model("PfamDB::Pfam")
	  ->search( { pfamA_id => { "LIKE", "$char%" } },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  order_by => "pfamA_id" }
			  );
  }

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

}


# render the page

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{template} = "pages/error.tt";
  } else {
	$c->stash->{pageType} = "family";
	$c->stash->{template} ||= "pages/browse.tt";
  }

  # and use it
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->clear_errors;

}

1;
