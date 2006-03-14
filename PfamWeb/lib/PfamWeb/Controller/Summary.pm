
package PfamWeb::Controller::Summary;

use strict;
use warnings;
use base "Catalyst::Controller";


# pick up a URL like http://localhost:3000/summary/PF00067

sub getacc : LocalRegex( '^(PF\d{5})' ) {
  my( $this, $c ) = @_;

  my $acc = $c->req->snippets->[0];
  $c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $acc } );

  $c->stash->{template} = "pages/error.tt"
	unless defined $c->stash->{pfam};

  #$c->log->info( "getacc: Pfam object: |", $c->stash->{pfam}, "|" );
}


# pick up URLs like
#   http://localhost:3000/summary?acc=PF00067
# or
#   http://localhost:3000/summary?id=p450

sub default : Private {
  my( $this, $c ) = @_;

#   if( defined $c->session->{layout} ) {
# 	$c->stash->{layout} = $c->session->{layout};
# 	$c->log->debug( "added layout object to stash" );
#   }

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(PF\d{5})$/;
	$c->log->info( "found accession |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param("id") ) {

	$c->req->param("id") =~ m/(^\w+$)/;
	$c->log->info( "found ID |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_id => $1 } )
	  if defined $1;

  }

  $c->stash->{template} = "pages/error.tt"
	unless defined $c->stash->{pfam};

  $c->log->info( "default: Pfam object: |", $c->stash->{pfam}, "|" );

}


# hand off to TT to render the page. Default to "view.tt", which is
# the standard summary page right now. If actions have had problems,
# they'll have switched to point to "error.tt"

sub end : Private {
  my( $this, $c ) = @_;

  # see if a view was specified
  my $view;
  if( $c->req->param( "view" ) ) {
	( $view = $c->req->param( "view" ) ) =~ s/^([A-Za-z_]+).*$/$1/;

	# set the view
	$c->stash->{template} ||= "pages/" . $this->{views}->{$view}
  }

  # pick a tab
  if( $c->req->param( "tab" ) ) {
	$c->req->param( "tab" ) =~ /^([A-Za-z]+).*$/;
	$c->stash->{selectedTab} = $1;
  }

  # make sure there's a template defined ultimately
  $c->stash->{template} ||= "pages/" . $this->{views}->{default};

  # and use it
  $c->forward( "PfamWeb::View::TT" );
}

1;
