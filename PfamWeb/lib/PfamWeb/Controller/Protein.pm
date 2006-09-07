
# Protein.pm
# jt6 20060427 WTSI
#
# Controller to build the main protein page.
#
# $Id: Protein.pm,v 1.8 2006-09-07 11:52:09 jt6 Exp $

package PfamWeb::Controller::Protein;

use strict;
use warnings;

use Data::Dumper;

use Storable qw(thaw);
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------
# get the data from the database for the UniProt entry

sub begin : Private {
  my( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  if( defined $c->req->param( "acc" ) ) {

	$c->req->param( "acc" ) =~ m/^([OPQ]\d[A-Za-z0-9]{3}\d)$/i;
	$c->log->info( "Protein::begin: found a uniprot accession |$1|" );

	# try a lookup in the main pfamseq table first
	my $p = $c->model("PfamDB::Pfamseq")->find( { pfamseq_acc => $1 } );

	# if we got a result there, so much the better...
	if( defined $p ) {
	  $c->stash->{pfamseq} = $p;
	} else {

	  # ... otherwise, see if this is really a secondary accession
	  $p = $c->model("PfamDB::Secondary_pfamseq_acc")->find(
             { secondary_acc => $1 },
			 { join =>     [ qw/pfamseq/ ],
			   prefetch => [ qw/pfamseq/ ] } );

	  $c->stash->{pfamseq} = $p if defined $p;
	}

  } elsif( defined $c->req->param( "id" ) ) {

	$c->req->param( "id" ) =~ m/^(\w+)$/;
	$c->log->info( "Protein::begin: found a uniprot ID |$1|" );
	
	# try a lookup in the main pfamseq table first
	my $p = $c->model("PfamDB::Pfamseq")->find( { pfamseq_id => $1 } );

	$c->stash->{pfamseq} = $p if defined $p;

  } elsif( defined $c->req->param( "entry" ) ) {

	# we don't know if this is an accession or an ID; try both

	if( $c->req->param( "entry" ) =~ m/^([OPQ]\d[A-Z0-9]{3}\d)$/i ) {

	  # looks like an accession; redirect to this action, appending the accession
	  $c->log->debug( "Protein::begin: looks like a uniprot accession ($1); redirecting" );
	  $c->res->redirect( $c->uri_for( "/protein", { acc => $1 } ) );
	  return 1;

	} elsif( $c->req->param( "entry" ) =~ m/^(\w+_\w+)$/ ) {

	  # looks like an ID; redirect to this action, appending the ID
	  $c->log->debug( "Protein::begin: looks like a uniprot ID; redirecting" );
	  $c->res->redirect( $c->uri_for( "/protein", { id => $1 } ) );
	  return 1;
	}

  }

  # we're done here unless there's an entry specified
  unless( defined $c->stash->{pfamseq} ) {
	$c->log->warn( "Protein::begin: no ID or accession" );
	$c->error( "No valid UniProt accession or ID" );
	return;
  }

  $c->log->debug( "Protein::begin: successfully retrieved a pfamseq object" );

  #----------------------------------------
  # generate the Pfam graphic

  # get a layout manager and set the X scale
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layoutPfam->scale_x(1);
  $c->log->debug( "Protein::begin: instantiated a layout manager" );

  # retrieve the Storable containing the annotated sequence, thaw it
  # and hand it off to the layout manager
  my $annseq;
  eval {
	$annseq = thaw( $c->stash->{pfamseq}->annseq->annseq_storable );
  };
  if( $@ ) {
	$c->log->error( "Protein::begin: ERROR: failed to thaw annseq: $@" );
  }

  $layoutPfam->layout_sequences_with_regions_and_features(
	 [ $annseq ],
	 { PfamA      => 1,
	   PfamB      => 1,
	   noFeatures => 0 }
  );

  # and build an imageset
  my $pfamImageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $pfamImageset->create_images( $layoutPfam->layout_to_XMLDOM );
  $c->log->debug( "Protein::begin: created images" );

  $c->stash->{pfamImageset} = $pfamImageset;

  $c->log->debug( "Protein::begin: successfully generated an imageset object" );

  #----------------------------------------
  # add available DAS sources to the stash

  my @dasSources = $c->model("PfamDB::Das_sources")->search();
  $c->stash->{dasSourcesRs} = \@dasSources;

  $c->log->debug( "Protein::begin: added DAS sources to the stash" );

  #----------------------------------------
  # get the structure mapping

  my $autoPfamseq = $c->stash->{pfamseq}->auto_pfamseq;

  my @mapping = $c->model("PfamDB::PdbMap")->search(
						    { "pfamseq.auto_pfamseq" => $autoPfamseq,
						      pfam_region            => 1 },
						    {
						     join     => [ qw/pfamA pfamseq pdb/ ],
						     prefetch => [ qw/pfamA pfamseq pdb/ ]
						    }
						   );

  $c->stash->{pfamMaps} = \@mapping;

  $c->log->debug( "Protein::begin: added the structure mapping to the stash" );

  #----------------------------------------
  # get the data items for the overview bar

  my %summaryData;

  # first, the number of sequences... pretty easy...
  $summaryData{numSequences} = 1;

  # also, the number of architectures
  $summaryData{numArchitectures} = 1;

  # number of species
  $summaryData{numSpecies} = 1;

  # number of structures
  my $rs = $c->model("PfamDB::Pdb_residue")->find(
             { auto_pfamseq => $autoPfamseq },
			 { select => [
						  { count => [
									  { distinct => [ qw/auto_pdb/ ] }
									 ]
						  }
						 ],
			   as => [ qw/numberPdbs/ ]
			 }
		   );
  $summaryData{numStructures} = $rs->get_column( "numberPdbs" );

  # number of interactions
  $rs = $c->model("PfamDB::Interactions")->find(
		  { auto_pfamseq_A => $autoPfamseq },
		  { select => [
					   { count => [
								   { distinct => [ "auto_int_pfamAs" ] }
								  ]
					   }
					  ],
			as => [ qw/numInts/ ]
		  }
	   );
  $summaryData{numInt} = $rs->get_column( "numInts" );

  $c->stash->{summaryData} = \%summaryData;

  $c->log->debug( "Protein::begin: added the summary data to the stash" );

}

#-------------------------------------------------------------------------------
# the hook into the class

# pick up a URL like http://localhost:3000/protein?acc=P00179

sub default : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Protein::default: action caught a URL..." );
}

#-------------------------------------------------------------------------------
# default end; hand off to the whole page layout

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pfamseq object in the stash
  return 0 unless defined $c->stash->{pfamseq};

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/blocks/protein/errors.tt";
  } else {
	$c->stash->{pageType} = "protein";
	$c->stash->{template} = "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);
}

#-------------------------------------------------------------------------------

1;
