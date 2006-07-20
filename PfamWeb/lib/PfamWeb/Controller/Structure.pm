
# Structure.pm
# jt6 20060706 WTSI
#
# Controller to build the structure page.
#
# $Id: Structure.pm,v 1.2 2006-07-20 09:00:20 jt6 Exp $

package PfamWeb::Controller::Structure;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------
# get the row in the Pdb table for this entry

sub begin : Private {
  my( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  my $pdb;
  if( defined $c->req->param("id") ) {

	$c->req->param("id") =~ m/^([0-9][A-Z0-9]{3})$/i;
	$pdb = PfamWeb::Model::Pdb->find( { pdb_id => $1 } ) if defined $1;

	$c->log->info( "Structure::begin: found ID |$1|" );

  } elsif( defined $c->req->param("entry") ) {

	$c->req->param("entry") =~ m/^([0-9][A-Z0-9]{3})$/i;
	$c->log->debug( "Structure::begin: looks like an ID ($1); redirecting" );
	$c->res->redirect( $c->uri_for( "/structure", { id => $1 } ) );
  }

  # we're done here unless there's an entry specified
  $c->log->warn( "Structure::begin: no valid PDB ID supplied" ) and return
	unless defined $pdb;

  # stash the PDB object
  $c->stash->{pdb} = $pdb;

  # get the authors list
  my @authors = PfamWeb::Model::PdbAuthor->search( { auto_pdb => $pdb->auto_pdb },
												   { order_by => "author_order ASC" } );

  $c->stash->{authors} = \@authors;

  # and the UniProt mapping
  my @mapping = PfamWeb::Model::PdbMap->search( { auto_pdb    => $pdb->auto_pdb,
												  pfam_region => 1 },
												{ join     => [ qw/pfamA pfamseq/ ],
												  prefetch => [ qw/pfamA pfamseq/ ],
												  order_by => "chain ASC" } );
  # stash the mapping
  $c->stash->{mapping} = \@mapping;

  # build a little data structure to map PDB chains to uniprot IDs and
  # then cache that for the post-loaded graphics component
  my %chains;
  foreach my $row ( @mapping ) {
	$chains{$row->pfamseq_id}->{$row->chain} = "";
  }
  $c->cache->set( "chain_mapping", \%chains );

}

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/structure

sub default : Path {
  my( $this, $c ) = @_;

  $c->log->debug( "Structure::default: starting..." );

}

#-------------------------------------------------------------------------------
# hand off to the full page template

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pdb object in the stash
  return 0 unless defined $c->stash->{pdb};

  # set up the TT view
  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/blocks/structure/errors.tt";
  } else {
	$c->stash->{pageType} = "structure";
	$c->stash->{template} = "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}

#-------------------------------------------------------------------------------

1;
