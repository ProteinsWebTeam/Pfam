
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Clan.pm,v 1.5 2006-08-14 10:38:50 jt6 Exp $

package PfamWeb::Controller::Clan;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param( "acc" ) ) {

	$c->req->param( "acc" ) =~ m/^(CL\d{4})$/i;
	$c->log->info( "Clan::begin: found accession |$1|" );

	$c->stash->{clan} = $c->model("PfamDB::Clans")->find( { clan_acc => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param( "id" ) ) {

	$c->log->info( "Clan::begin: found param |".$c->req->param("id")."|" );
	$c->req->param( "id" ) =~ m/^([\w-]+)$/;
	$c->log->info( "Clan::begin: found ID |$1|" );
	
	$c->stash->{clan} = $c->model("PfamDB::Clans")->find( { clan_id => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param( "entry" ) ) {

	# see if this is really an accession...
	if( $c->req->param( "entry" ) =~ /^(CL\d{4})$/i ) {

	  $c->log->debug( "Clan::begin: looks like a clan accession ($1); redirecting" );
	  $c->res->redirect( $c->uri_for( "/clan", { acc => $1 } ) );
	  return 1;

	} else {

	  # no; assume it's an ID and see what happens...
	  $c->log->debug( "Clan::begin: doesn't look like a clan accession ($1); redirecting with an ID" );
	  $c->res->redirect( $c->uri_for( "/clan", { id => $c->req->param( "entry" ) } ) );
	  return 1;

	}

  }

  # we're done here unless there's an entry specified
  unless( defined $c->stash->{clan} ) {
	$c->log->warn( "Clan::begin: no ID or accession" );
	return 1;
  }

  #----------------------------------------
  # get the data items for the overview bar

  my %summaryData;

  # get the auto number - should be quicker to use
  my $autoClan = $c->stash->{clan}->auto_clan;

  # number of sequences
  my $rs = $c->model("PfamDB::Clan_membership")->find(
    { auto_clan => $autoClan },
    {
      select => [ { sum => "pfam.num_full" } ],
      as => [ 'total_in_clan' ],
	 join => [qw/pfam/]
    }
  );
  $summaryData{numSequences} = $rs->get_column( "total_in_clan" );

  # count the number of architectures
  $rs = $c->model("PfamDB::ClanArchitecture")->find(
    { auto_clan => $autoClan },
    {
      select => [ { count => "auto_clan" } ],
      as => [ 'count' ]
    }
  );
  $summaryData{numArchitectures} = $rs->get_column( "count" );

  # number of interactions
  $rs = $c->model("PfamDB::Clan_membership")->find(
    { auto_clan => $autoClan },
	{ select => [ { count => "auto_pfamA_A" } ],
	  as     => [ qw/NumInts/ ],
	  join   => [ qw/pfamAInts/ ]
    }
  );
  $summaryData{numInt} = $rs->get_column( "NumInts" );

  # get the PDB details
  my @maps = $c->model("PfamDB::Clan_membership")->search(
    { auto_clan            => $autoClan,
	  "pdbmap.pfam_region" => 1 },
	{ select => [ qw/pdb.pdb_id/ ],
	  as     => [ qw/pdb_id/ ],
	  join   => { pdbmap => "pdb" }
    }
  );

  # number of structures known for the domain
  my %pdb_unique = map {$_->get_column("pdb_id") => 1} @maps;
  $summaryData{numStructures} = scalar(keys %pdb_unique);
  $c->stash->{pdbUnique} = \%pdb_unique;

  # number of species
  my @species = $c->model("PfamDB::Clan_membership")->search(
    { auto_clan              => $autoClan,
 	  "pfamARegFull.in_full" => 1 },
    { select => [ qw/pfamseq.species/ ],
	  as     => [ qw/species/ ],
	   join   => { pfamARegFull => "pfamseq" }
    }
  );

  my %species_unique = map {$_->get_column("species") => 1} @species;
  $summaryData{numSpecies} = scalar(keys %species_unique);

  $c->stash->{summaryData} = \%summaryData;

  #----------------------------------------
  # get the database cross references

  my @refs = $c->model("PfamDB::Clan_database_links")->search( { auto_clan => $autoClan } );

  my %xRefs;
  foreach ( @refs ) {
	$xRefs{$_->db_id} = $_;
  }
  $c->stash->{xrefs} = \%xRefs;

  #----------------------------------------
  # get the structure mapping

  my @mapping = $c->model("PfamDB::Clan_membership")->search( { auto_clan => $autoClan },
														 { select => [ qw/pfamseq.pfamseq_id
																		  pfamA.pfamA_id
																		  pfamA.pfamA_acc
																		  pdbmap.pfam_start_res
																		  pdbmap.pfam_end_res
																		  pdb.pdb_id
																		  pdbmap.chain
																		  pdbmap.pdb_start_res
																		  pdbmap.pdb_end_res/ ],
														   as     => [ qw/pfamseq_id
																		  pfamA_id
																		  pfamA_acc
																		  pfam_start_res
																		  pfam_end_res
																		  pdb_id
																		  chain
																		  pdb_start_res
																		  pdb_end_res/ ],
														   join => { pdbmap => [qw/pfamA
																				   pfamseq
																				   pdb/ ]
																   }
														 }
													   );

  $c->stash->{pfamMaps} = \@mapping;

}

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/clan

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # empty; just here to capture the URL
}

#-------------------------------------------------------------------------------
# default end

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render unless there's a Clan object in the stash
  return 0 unless defined $c->stash->{clan};

  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/blocks/clan/errors.tt";
  } else {
	$c->stash->{pageType} = "clan";
	$c->stash->{template} = "pages/layout.tt";
  }

  # and use it
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}

#-------------------------------------------------------------------------------

1;



#pick up a URL like http://localhost:3000/clans/summary/CL0001
#Relative to the class
# sub summary : LocalRegex('^summary\/(CL\d{4})$') { 
#   my ($this, $c) = @_;
#   my $clan_acc = $c->req->snippets->[0]; #get what was captured in the regex
#   $c->stash->{clan} = $c->model("PfamDB::Clans")->find( {clan_acc=>$clan_acc} ); 
#   if ($c->stash->{clan}){
#      $c->stash->{template} = "pages/clanSum.tt";
#   }else{ 
#      $c->stash->{template} = "pages/error.tt";
#   }
#   $c->log->info( "clan summary: Clan object: |", $c->stash->{clan}, "|" );
# }
