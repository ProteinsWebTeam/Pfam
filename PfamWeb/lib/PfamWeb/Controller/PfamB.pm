
# PfamB.pm
# jt6 20060809 WTSI
#
# Controller to build a PfamB  page.
#
# $Id: PfamB.pm,v 1.3 2007-02-14 11:48:28 rdf Exp $

=head1 NAME

PfamWeb::Controller::PfamB - controller for PfamB pages

=cut

package PfamWeb::Controller::PfamB;

=head1 DESCRIPTION

A C<Controller> to handle pages for Pfam B entries.

Generates a B<full page>.

$Id: PfamB.pm,v 1.3 2007-02-14 11:48:28 rdf Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Section";

__PACKAGE__->config( SECTION => "pfamb" );

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

	  if (defined $1){
		$c->stash->{pfam} = $c->model("PfamDB::PfamB")->find( { pfamB_acc => $1 } );
		$c->stash->{prodom} = $c->model("PfamDB::PfamB_database_links")
		  ->find( { auto_pfamB => $c->stash->{pfam}->auto_pfamB,
					db_id => "PRODOM"} );
	  }
  }

  # we're done here unless there's an entry specified
  unless( defined $c->stash->{pfam} ) {
	$c->log->warn( "PfamB::begin: no ID or accession" );
	$c->error( "No valid Pfam family accession or ID" );
	return;
  }

  # flag this as a PfamB
  $c->stash->{entryType} = "B";
  $c->stash->{acc}  = $c->stash->{pfam}->pfamB_acc;
}

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/pfamb?acc=PB000001

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # the accession should have been dropped into the stash by the begin
  # method

  $c->log->debug( "PfamB::generateSummary: generating a page for a PfamB" );

  $c->forward( "_getSummaryData" );
  $c->forward( "_getDbXrefs" );
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# get the data items for the overview bar

sub _getSummaryData : Private {
  my( $this, $c ) = @_;

  my %summaryData;

  # make things easier by getting hold of the auto_pfamA
  my $auto_pfam = $c->stash->{pfam}->auto_pfamB;

  # get the PDB details
  my @maps = $c->model("PfamDB::PdbMap")->search(
    { auto_pfam   => $auto_pfam,
	  pfam_region => 0 },
	{ join        => [ qw/ pdb / ],
	  prefetch    => [ qw/ pdb / ] } );
  $c->stash->{pfamMaps} = \@maps;

  # count the number of architectures
  my $rs = $c->model("PfamDB::PfamB_reg")->find(
    { auto_pfamB => $auto_pfam },
    {
      select => [
        { count => { distinct => "auto_architecture" } }
      ],
      as => [ 'count' ],
	 join => [ qw/ pfamseq_architecture / ]
    }
  );

  # number of architectures....
  $summaryData{numArchitectures} = $rs->get_column( "count" );

  # count number of sequences in full alignment
  $rs = $c->model("PfamDB::PfamB_reg")->find(
    { auto_pfamB => $auto_pfam },
    {
      select => [
        { count => "auto_pfamB" }
      ],
      as => [ 'count' ]
    }
  );

  # number of sequences in full alignment
  $summaryData{numSequences} = $rs->get_column( "count" );

  # number of structures known for the domain
  my %pdb_unique = map {$_->pdb_id => $_} @maps;
  $summaryData{numStructures} = scalar(keys %pdb_unique);
  $c->stash->{pdbUnique} = \%pdb_unique;

  # number of species
  my @species = $c->model("PfamDB::PfamB_reg")->search(
    { auto_pfamB => $auto_pfam },
    { join       => [ qw/pfamseq/ ],
	  prefetch   => [ qw/pfamseq/ ] } );

  my %species_unique = map {$_->species => 1} @species;
  $summaryData{numSpecies} = scalar(keys %species_unique);

  # number of interactions
#   $rs = $c->model("PfamDB::Int_pfamAs")->find({ auto_pfamA_A => $auto_pfam },
# 	{ select => [
# 				 { count => "auto_pfamA_A" }
# 				],
# 	  as => [ qw/NumInts/ ]
#     }
#   );
#
#  $summaryData{numInt} = $rs->get_column( "NumInts" );

  $summaryData{numInt} = 0;

  $c->stash->{summaryData} = \%summaryData;

}

#-------------------------------------------------------------------------------

sub _getDbXrefs : Private {
  my( $this, $c ) = @_;

  my %xRefs;

  # stuff in the accession and ID for this entry
  $xRefs{entryAcc} = $c->stash->{pfam}->pfamB_acc;
  $xRefs{entryId}  = $c->stash->{pfam}->pfamB_id;

  # PfamB to PfamA links based on PRODOM
  my %btoaPRODOM;
  foreach my $xref ( $c->stash->{pfam}->pfamB_database_links ) {
	if( $xref->db_id eq "PFAMA_PRODOM" ) {
	  $btoaPRODOM{$xref->db_link} = $xref;
	}else {
	  push @{ $xRefs{$xref->db_id} }, $xref;
	}
  }

  # PfamB to PfamB links based on PRC
  my @btobPRC = $c->model("PfamDB::PfamB2pfamB_PRC_results")
	->search(
			 { "pfamB1.pfamB_acc" => $c->stash->{pfam}->pfamB_acc },
			 { join               => [ qw/pfamB1 pfamB2/ ],
			   select             => [ qw/pfamB1.pfamB_acc pfamB2.pfamB_acc evalue/ ],
			   as                 => [ qw/l_pfamB_acc r_pfamB_acc evalue/ ],
			   order_by           => "pfamB2.auto_pfamB ASC" }
			);

  $xRefs{btobPRC} = [];
  foreach ( @btobPRC ) {
	next if $_->get_column( "evalue" ) <= 0.001;
	next if $_->get_column("l_pfamB_acc") eq $_->get_column("r_pfamB_acc");
	push @{$xRefs{btobPRC}}, $_;
  }

#  $xRefs{btobPRC} = \@btobPRC if scalar @btobPRC;

  # PfamB to PfamA links based on PRC
  my @btoaPRC = $c->model("PfamDB::PfamB2pfamA_PRC_results")
	->search(
			 { "pfamB.pfamB_acc" => $c->stash->{pfam}->pfamB_acc, },
			 { join      => [ qw/pfamA pfamB/ ],
			   prefetch  => [ qw/pfamA pfamB/ ] }
			);

  # find the union between PRC and PRODOM PfamB links
  my %btoaPRC;
  foreach ( @btoaPRC ) {
	$btoaPRC{$_->pfamB_acc} = $_ if $_->evalue <= 0.001;
  }

  my %btoaBOTH;
  foreach ( keys %btoaPRC, keys %btoaPRODOM ) {
	$btoaBOTH{$_} = $btoaPRC{$_}
	  if( exists( $btoaPRC{$_} ) and exists( $btoaPRODOM{$_} ) );
  }

  # and then prune out those accessions that are in both lists
  foreach ( keys %btoaPRC ) {
	delete $btoaPRC{$_} if exists $btoaBOTH{$_};
  }
  foreach ( keys %btoaPRODOM ) {
	delete $btoaPRODOM{$_} if exists $btoaBOTH{$_};
  }

  # now populate the hash of xRefs;
  my @btoaPRC_pruned;
  foreach ( sort keys %btoaPRC ) {
	push @btoaPRC_pruned, $btoaPRC{$_};
  }
  $xRefs{btoaPRC} = \@btoaPRC_pruned if scalar @btoaPRC_pruned;

  my @btoaPRODOM;
  foreach ( sort keys %btoaPRODOM ) {
	push @btoaPRODOM, $btoaPRODOM{$_};
  }
  $xRefs{btoaPRODOM} = \@btoaPRODOM if scalar @btoaPRODOM;

  my @btoaBOTH;
  foreach ( sort keys %btoaBOTH ) {
	push @btoaBOTH, $btoaBOTH{$_};
  }
  $xRefs{btoaBOTH} = \@btoaBOTH if scalar @btoaBOTH;

  $c->stash->{xrefs} = \%xRefs;
}

#-------------------------------------------------------------------------------

1;
