
# Family.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam family page.
#
# $Id: Family.pm,v 1.5 2006-07-20 08:46:52 jt6 Exp $

package PfamWeb::Controller::Family;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

# this is the base class for dealing with Pfam family-related
# data. The begin method will try to extract data from the database
# for the family that's specified either with an ID or accession. The
# end method will hand off to the tab-layout template.
#
# Subclasses can override the end method to, for example, remove the
# wrapper from the templates.

#-------------------------------------------------------------------------------
# get the row in the Pfam table for this entry

sub begin : Private {
  my( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(PF\d{5})$/i;
	$c->log->info( "Family::begin: found accession |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param("id") ) {

	$c->req->param("id") =~ m/^(\w+)$/;
	$c->log->info( "Family::begin: found ID |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_id => $1 } )
	  if defined $1;

  }	elsif( defined $c->req->param( "entry" ) ) {

	if( $c->req->param( "entry" ) =~ /^(PF\d{5})$/i ) {

	  # looks like an accession; redirect to this action, appending the accession
	  $c->log->debug( "Family::begin: looks like a Pfam accession ($1); redirecting" );
	  $c->res->redirect( $c->uri_for( "/family", { acc => $1 } ) );
	  return 1;

	} elsif( $c->req->param( "entry" ) =~ /^([\w_-]+)$/ ) {

	  # looks like an ID; redirect to this action, appending the ID
	  $c->log->debug( "Family::begin: might be a Pfam ID; redirecting" );
	  $c->res->redirect( $c->uri_for( "/family", { id => $1 } ) );
	  return 1;
	}

  }

  # we're done here unless there's an entry specified
  unless( defined $c->stash->{pfam} ) {
	$c->log->warn( "$this: no ID or accession" );
	$c->error( "No valid Pfam family accession or ID" );
	return;
  }

  #----------------------------------------
  # get the data items for the overview bar

  my %summaryData;

  # make things easier by getting hold of the auto_pfamA
  my $auto_pfam = $c->stash->{pfam}->auto_pfamA;

  # get the PDB details
  my @maps = PfamWeb::Model::PdbMap->search(
    { auto_pfam   => $auto_pfam,
	  pfam_region => 1 },
	{ join        => [qw/ pdb / ] } );
  $c->stash->{pfamMaps} = \@maps;


  # count the number of architectures
  my $rs = PfamWeb::Model::PfamA_architecture->find(
    { auto_pfamA => $auto_pfam },
    {
      select => [
        { count => "auto_pfamA" }
      ],
      as => [ 'count' ]
    }
  );

  # number of architectures....
  $summaryData{numArchitectures} = $rs->get_column( "count" );

  # number of sequences in full alignment
  $summaryData{numSequences} = $c->stash->{pfam}->num_full;

  # number of structures known for the domain
  my %pdb_unique = map {$_->pdb_id => 1} @maps;
  $summaryData{numStructures} = scalar(keys %pdb_unique);
  $c->stash->{pdbUnique} = \%pdb_unique;

  # number of species
  my @species = PfamWeb::Model::PfamA_reg_full->search(
    { auto_pfamA => $auto_pfam,
	  in_full    => 1 },
    { join       => [ qw/pfamseq/ ],
	  prefetch   => [ qw/pfamseq/ ] } );

  my %species_unique = map {$_->species => 1} @species;
  $summaryData{numSpecies} = scalar(keys %species_unique);

  # number of interactions
  $rs = PfamWeb::Model::Int_pfamAs->find({ auto_pfamA_A => $auto_pfam },
	{ select => [
				 { count => "auto_pfamA_A" }
				],
	  as => [ qw/NumInts/ ]
    }
  );

  $summaryData{numIpfam} = $rs->get_column( "NumInts" );

  $c->stash->{summaryData} = \%summaryData;

}

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/summary

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # the accession should have been dropped into the stash by the begin
  # method

  # add the cross-references to the stash
  $c->forward( "getDbXrefs" ) if defined $c->stash->{pfam};

}

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
  $xRefs{pdb} = keys %{ $c->stash->{pdbUnique} }
	if $c->stash->{summaryData}{numStructures};

  # PfamA to PfamB links based on PRODOM
  my %atobPRODOM;
  foreach my $xref ( $c->stash->{pfam}->pfamA_database_links ) {
	if( $xref->db_id eq "PFAMB" ) {
	  $atobPRODOM{$xref->db_link} = $xref;
	} else {
	  push @{ $xRefs{$xref->db_id} }, $xref;
	}
  }

  # PfamA to PfamA links based on PRC
  my @atoaPRC = PfamWeb::Model::PfamA2pfamA_PRC_results->search(
    { "pfamA1.pfamA_acc" => $c->stash->{pfam}->pfamA_acc,
	  evalue             => { "<=", "0.01"} },
	{ join               => [ qw/pfamA1 pfamA2/ ],
	  select             => [ "pfamA1.pfamA_id", "pfamA2.pfamA_id" ],
	  as                 => [ qw/l_pfamA_id r_pfamA_id/ ],
	  prefetch           => [ qw/pfamA2 pfamA1/ ] } );

  $xRefs{atoaPRC} = \@atoaPRC if scalar @atoaPRC;

  # PfamB to PfamA links based on PRC
  my @atobPRC = PfamWeb::Model::PfamB2pfamA_PRC_results->search(
    { "pfamA.pfamA_acc" => $c->stash->{pfam}->pfamA_acc, },
#	  evalue    => { "<=", "0.01"} },
	{ join      => [ qw/pfamA pfamB/ ],
	  prefetch  => [ qw/pfamA pfamB/ ] } );

  # find the union between PRC and PRODOM PfamB links
  my %atobPRC;
  foreach ( @atobPRC ) {
	$atobPRC{$_->pfamB_acc} = $_ if $_->evalue <= 0.01;
  }
  # we should be able to filter the results of the query according to
  # evalue using a call on the DBIx::Class object, but for some reason
  # it's broken, hence this last loop...
  # my %atobPRC = map { $_->pfamB_acc => $_ } @atobPRC;

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
  my @atobPRC_pruned;
  foreach ( sort keys %atobPRC ) {
	push @atobPRC_pruned, $atobPRC{$_};
  }
  $xRefs{atobPRC} = \@atobPRC_pruned if scalar @atobPRC_pruned;

  my @atobPRODOM;
  foreach ( sort keys %atobPRODOM ) {
	push @atobPRODOM, $atobPRODOM{$_};
  }
  $xRefs{atobPRODOM} = \@atobPRODOM if scalar @atobPRODOM;

  my @atobBOTH;
  foreach ( sort keys %atobBOTH ) {
	push @atobBOTH, $atobBOTH{$_};
  }
  $xRefs{atobBOTH} = \@atobBOTH if scalar @atobBOTH;

  $c->stash->{xrefs} = \%xRefs;

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
	$c->stash->{template} = "components/blocks/family/errors.tt";
  } else {
	$c->stash->{pageType} = "family";
	$c->stash->{template} = "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}

#-------------------------------------------------------------------------------

1;
