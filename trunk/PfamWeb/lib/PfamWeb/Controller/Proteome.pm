
# Proteome.pm
# rdf 20060821 WTSI
#
# Controller to build the main Pfam Proteome page.
#
# $Id: Proteome.pm,v 1.2 2006-11-01 13:12:22 rdf Exp $

package PfamWeb::Controller::Proteome;

use strict;
use warnings;

use Data::Dumper;
use Data::Dumper;

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "proteome" );

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param( "ncbiCode" ) ) {

    my ($ncbiCode) = $c->req->param( "ncbiCode" ) =~ m/^(\d+)$/i;
    $c->log->info( "Proteome::begin: found ncbiCode |$ncbiCode|" );

    $c->stash->{proteomeSpecies} = $c->model("PfamDB::Proteome_species")->find( { ncbi_code => $ncbiCode});
  }
}


#-------------------------------------------------------------------------------

1;

