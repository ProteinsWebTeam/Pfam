
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Clan.pm,v 1.7 2006-09-22 13:22:13 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Clan - controller for clan-related
sections of the site

=cut

package PfamWeb::Controller::Clan;

=head1 DESCRIPTION

This is intended to be the base class for everything related to clans
across the site. The L<begin|/"begin : Private"> method will try to
extract a clan ID or accession from the captured URL and then try to
load a Clan object from the model into the stash.

Generates a B<tabbed page>.

$Id: Clan.pm,v 1.7 2006-09-22 13:22:13 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Section";

# define the name of the section...
__PACKAGE__->config( SECTION => "clan" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract a clan ID or accession from the URL and gets the row
in the clan table for that entry.

=cut

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

	# de-taint the accession or ID
	my $input = $c->req->param("acc")
	  || $c->req->param("id")
	  || $c->req->param("entry");
	$input =~ s/^(\w+)/$1/;

	# see if this was an internal link and, if so, report it
	my $b = $c->req->base;
	if( $c->req->referer =~ /^$b/ ) {

	  # this means that the link that got us here was somewhere within
	  # the Pfam site and that the accession or ID which it specified
	  # doesn't actually exist in the DB

	  # report the error as a broken internal link
	  $c->error( "Found a broken internal link; no valid clan accession or ID "
				 . "(\"$input\") in \"" . $c->req->referer . "\"" );
	  $c->forward( "/reportError" );

	  # now reset the errors array so that we can add the message for
	  # public consumption
	  $c->clear_errors;

	}

	# the message that we'll show to the user
	$c->stash->{errorMsg} = "No valid clan accession or ID";

	# log a warning and we're done; drop out to the end method which
	# will put up the standard error page
	$c->log->warn( "Family::begin: no valid clan ID or accession" );

	return;
  }

  $c->log->debug( "Clan::begin: successfully retrieved a clan object" );

  # populate the stash with other data
  $c->forward( "_getSummaryData" );
  $c->forward( "_getXrefs" );
  $c->forward( "_getMapping" );

  # done
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# get the data items for the overview bar

sub _getSummaryData : Private {
  my( $this, $c ) = @_;

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

}

#-------------------------------------------------------------------------------
# get the database cross references

sub _getXrefs : Private {
  my( $this, $c ) = @_;

  my @refs = $c->model("PfamDB::Clan_database_links")
	->search( { auto_clan => $c->stash->{clan}->auto_clan } );

  my %xRefs;
  foreach ( @refs ) {
	$xRefs{$_->db_id} = $_;
  }
  $c->stash->{xrefs} = \%xRefs;

}

#-------------------------------------------------------------------------------
# get the structure mapping

sub _getMapping : Private {
  my( $this, $c ) = @_;

  my @mapping = $c->model("PfamDB::Clan_membership")
	->search( { auto_clan => $c->stash->{clan}->auto_clan },
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

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
