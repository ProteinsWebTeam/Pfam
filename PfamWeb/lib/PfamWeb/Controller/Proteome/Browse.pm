
# Browse.pm
# jt6 20060717 WTSI
#
# Controller for the "browse" pages. Originally by RDF.
#
# $Id: Browse.pm,v 1.1 2006-11-13 15:36:59 rdf Exp $

package PfamWeb::Controller::Proteome::Browse;

use strict;
use warnings;

use base "Catalyst::Controller";

sub begin : Private {}

# pick up a URL like http://localhost:3000/proteome/browse

sub browse : Path {
  my( $this, $c ) = @_;

  #return unless defined $c->req->param( "browse" );

  my @res;
  @res = $c->model("PfamDB::Genome_species")->search( { },
					      { order_by => "species ASC"}
					    );
  $c->log->debug("*** Got |".scalar(@res)."| results ***");
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
	$c->stash->{pageType} = "proteome";
	$c->stash->{template} ||= "pages/browseProteomes.tt";
  }

  # and use it
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->clear_errors;

}

1;
