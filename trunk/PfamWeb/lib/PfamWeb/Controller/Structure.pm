
# Structure.pm
# jt6 20060706 WTSI
#
# Controller to build the structure page.
#
# $Id: Structure.pm,v 1.1 2006-07-14 13:08:06 jt6 Exp $

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
	$c->req->param("id") =~ m/(^\d\w{3}$)/;
	$pdb = PfamWeb::Model::Pdb->find( { pdb_id => $1 } ) if defined $1;

	$c->log->info( "$this: found ID |$1|" );
  }

  # we're done here unless there's an entry specified
  $c->log->warn( "$this: no valid PDB ID supplied" ) and return
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

  # set up the TT view
  $c->stash->{pageType} = "structure";
  $c->stash->{template} = "pages/layout.tt";

  # and use it
  $c->forward( "PfamWeb::View::TT" );

}

#-------------------------------------------------------------------------------

1;
