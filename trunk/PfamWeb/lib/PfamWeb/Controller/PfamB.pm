
# PfamB.pm
# jt6 20060809 WTSI
#
# Controller to build a PfamB  page.
#
# $Id: PfamB.pm,v 1.1 2006-08-14 10:41:05 jt6 Exp $

=head1 NAME

PfamWeb::Controller::PfamB - controller for PfamB pages

=cut

package PfamWeb::Controller::PfamB;

=head1 DESCRIPTION

A C<Controller> to handle pages for Pfam B entries.

Generates a B<full page>.

$Id: PfamB.pm,v 1.1 2006-08-14 10:41:05 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Extract the PfamB accession from the URL and load the appropriate
Model objects into the hash.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(PB\d{6})$/i;
	$c->log->info( "PfamB::begin: found a PfamB, accession |$1|" );

	$c->stash->{pfam} = $c->model("PfamDB::PfamB")->find( { pfamB_acc => $1 } )
	  if defined $1;

  }

  # we're done here unless there's an entry specified
  unless( defined $c->stash->{pfam} ) {
	$c->log->warn( "Family::begin: no ID or accession" );
	$c->error( "No valid Pfam family accession or ID" );
	return;
  }

}

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/pfamb?acc=PF000001

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # the accession should have been dropped into the stash by the begin
  # method

  $c->log->debug( "PfamB::generateSummary: generating a page for a PfamB" );
}

#-------------------------------------------------------------------------------
# hand off to the full page template

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pfam object in the stash
  return 0 unless defined $c->stash->{pfam};

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/blocks/pfamb/errors.tt";
  } else {
	$c->stash->{pageType} = "pfamb";
	$c->stash->{template} ||= "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}

#-------------------------------------------------------------------------------

1;
