
# Family.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam family page.
#
# $Id: Family.pm,v 1.1 2006-04-12 16:25:53 jt6 Exp $

package PfamWeb::Controller::Family;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";


# pick up http://localhost:3000/summary

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # the accession should have been dropped into the stash by the
  # begin method from the PfamWeb.pm class
  my $acc = $c->stash->{pfam}->pfamA_acc;

  # make things easier by getting hold of the auto_pfamA too
  my $auto_pfam = $c->stash->{pfam}->auto_pfamA;

  #----------------------------------------------------------------------
  # get the PDB details

  my @maps = PfamWeb::Model::PdbMap->search(
    { auto_pfam   => $auto_pfam,
	  pfam_region => 1 },
	{ join        => [qw/ pdb / ] } );
  $c->stash->{pfamMaps} = \@maps;

  #----------------------------------------------------------------------

  my $rs = PfamWeb::Model::PfamA_architecture->find(
    { auto_pfamA => $auto_pfam },
    {
      select => [
        { count => "auto_pfamA" }
      ],
      as => [ 'count' ]
    }
  );

  #----------------------------------------------------------------------
  # Build the summary details

  my %summaryData;

  # Number or architectures....
  $summaryData{numArchitectures} = $rs->get_column( "count" );

  # Number of sequences in full alignment
  $summaryData{numSequences} = $c->stash->{pfam}->num_full;

  # Number of structures known for the domain
  my %pdb_unique = map {$_->pdb_id => 1} @maps;
  $summaryData{numStructures} = scalar(keys %pdb_unique);
  $c->stash->{pdbUnique} = \%pdb_unique;

  # Number of species
  my @species = PfamWeb::Model::PfamA_reg_full->search(
    { auto_pfamA => $auto_pfam,
	  in_full    => 1 },
    { join       => [ qw/pfamseq/ ],
	  prefetch   => [ qw/pfamseq/ ] } );

  my %species_unique = map {$_->species => 1} @species;
  $summaryData{numSpecies} = scalar(keys %species_unique);

  # HACK:hardcoded interactions number added here...
  $summaryData{numIpfam} = 7;
  $c->log->warn( "$this: WARNING: number of interactions is hard coded !" );

  $c->stash->{summaryData} = \%summaryData;

  #----------------------------------------------------------------------
  # add the cross-references to the stash
  $c->forward( "getDbXrefs" );

  #----------------------------------------------------------------------
  # make sure there's a template defined ultimately
  $c->stash->{template} ||= "pages/" . $c->config->{views}->{default};

  # and use it
  $c->forward( "PfamWeb::View::TT" );

}

sub getacc : Regex( qw[^getacc/(PF\d{5})$] ) {
  my( $this, $c ) = @_;

  $c->forward( "family?acc=" . $c->req->snippets->[0] );

}


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
