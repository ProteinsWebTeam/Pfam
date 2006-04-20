
# Family.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam family page.
#
# $Id: Family.pm,v 1.2 2006-04-20 16:32:53 jt6 Exp $

package PfamWeb::Controller::Family;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/summary

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # the accession should have been dropped into the stash by the
  # begin method from the PfamWeb.pm class
  my $acc = $c->stash->{pfam}->pfamA_acc;

  # add the cross-references to the stash
  $c->forward( "getDbXrefs" );

  # set up the TT view
  $c->stash->{pageType} = "family";
  $c->stash->{template} = "pages/layout.tt";

  # and use it
  $c->forward( "PfamWeb::View::TT" );

}

#-------------------------------------------------------------------------------
# mimc the old "getacc" URL - doesn't work right now...

#sub getacc : Regex( "/getacc\?(PF\d{5})$" ) {
#  my( $this, $c ) = @_;

#  my $acc = $c->req->snippets->[0];
#  $c->log->debug( "acc: $acc" );
				 
#  $c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $acc } );
#
#  $c->forward( "family" );

#}

#-------------------------------------------------------------------------------
# get the database cross-references from various places and stuff them
# into the stash

sub getDbXrefs : Private {
  my( $this, $c ) = @_;

  my %xRefs;

  # Interpro
  push @{ $xRefs{interpro} }, $c->stash->{pfam}->interpro_id
	if $c->stash->{pfam}->interpro_id;

  # PDB
  $xRefs{pdb} = keys %{ $c->stash->{pdb} }
	if $c->stash->{summaryData}{numStructures};

  # PfamA to PfamB links based on PRODOM
  my %atobPRODOM;
  foreach my $xref ($c->stash->{pfam}->pfamA_database_links ) {

	if( $xref->db_id eq "PFAMB" ) {
	  $atobPRODOM{$xref->db_link} = $xref;
	} else {
	  push @{ $xRefs{$xref->db_link} }, $xref;
	}
  }

  # PfamA to PfamA links based on PRC
  my @atoaPRC = PfamWeb::Model::PfamA2pfamA_PRC_results->search(
    { "pfamA1.pfamA_acc" => $c->stash->{pfam}->pfamA_acc,
	  "evalue"           => { "<=", "0.01"} },
	{ join               => [ qw/pfamA1 pfamA2/ ],
	  select             => [ "pfamA1.pfamA_id", "pfamA2.pfamA_id" ],
	  as                 => [ qw/l_pfamA_id r_pfamA_id/ ],
	  prefetch           => [ qw/pfamA2 pfamA1/ ] } );

  $xRefs{atobPRC} = \@atoaPRC
	if scalar @atoaPRC;

  # PfamB to PfamA links based on PRC
  my @atobPRC = PfamWeb::Model::PfamB2pfamA_PRC_results->search(
    { pfamA_acc => $c->stash->{pfam}->pfamA_acc },
	{ join      => [ qw/pfamA pfamB/ ],
	  prefetch  => [ qw/pfamA pfamB/ ] } );

  # find the union between PRC and PRODOM PfamB links
  my %atobPRC = map { $_->pfamB_acc => $_ } @atobPRC;

  my %atobBOTH;
  foreach ( keys %atobPRC, keys %atobPRODOM ) {
	$atobBOTH{$_} = $atobPRC{$_}
	  if( exists( $atobPRC{$_} ) and exists( $atobPRODOM{$_} ) );
  }

  # and then prune out those accessions that are in both lists
  foreach ( keys %atobPRC ) {
      delete $atobPRC{$_} if exists $atobBOTH{$_};
  }

  foreach ( keys %atobPRODOM ) {
      delete $atobPRODOM{$_} if exists $atobBOTH{$_};
  }

  # now populate the hash of xRefs;
  my @atobPRC_pruned = values %atobPRC;
  $xRefs{atobPRC} = \@atobPRC_pruned if scalar @atobPRC_pruned;

  my @atobPRODOM = values %atobPRODOM;
  $xRefs{atobPRODOM} = \@atobPRODOM if scalar @atobPRODOM;

  my @atobBOTH = values %atobBOTH;
  $xRefs{atobBOTH} = \@atobBOTH if scalar @atobBOTH;

  $c->stash->{xrefs} = \%xRefs;

}


1;
