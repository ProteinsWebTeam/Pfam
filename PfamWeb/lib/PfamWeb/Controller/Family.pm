
# Family.pm
# jt6 20060411 WTSI
#
# $Id: Family.pm,v 1.14 2006-11-13 15:31:14 rdf Exp $

=head1 NAME

PfamWeb::Controller::Family - controller to build the main Pfam family
page

=cut

package PfamWeb::Controller::Family;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Pfam
families across the site. The L<begin|/"begin : Private"> method tries
to extract a Pfam ID or accession from the captured URL and tries to
load a Pfam object from the model.

Generates a B<tabbed page>.

$Id: Family.pm,v 1.14 2006-11-13 15:31:14 rdf Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "family" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Extracts the Pfam family ID or accession from the URL and gets the row
in the Pfam table for that entry.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(P([FB])\d{5,6})$/i;
	$c->log->info( "Family::begin: found accession |$1|, family A / B ? |$2|" );

	if( defined $1 ) {

	  # see if this is actually a PfamB...
 	  if( $2 eq 'B' ) {
 		$c->log->debug( "Family::begin: looks like a PfamB; retrieving" );
		$c->stash->{entryType} = "B";
		$c->stash->{pfam} = $c->model("PfamDB::PfamB")->find( { pfamB_acc => $1 } );
		$c->stash->{acc}  = $c->stash->{pfam}->pfamB_acc;

 	  } else {

		# no; must be a PfamA
		$c->log->debug( "Family::begin: family is a PfamA" );
		$c->stash->{entryType} = "A";
		$c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );
		$c->stash->{acc}  = $c->stash->{pfam}->pfamA_acc;
		$c->stash->{alnType} = ( $c->req->param( "alnType" ) eq "seed" ) ? "seed" : "full"
		  if defined $c->req->param( "alnType" );
		
	  }

	}

  } elsif( defined $c->req->param("id") ) {

	$c->req->param("id") =~ /^([\w_-]+)$/;
	$c->log->info( "Family::begin: found ID |$1|" );

	$c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_id => $1 } )
	  if defined $1;

	$c->stash->{entryType} = "A";
	$c->stash->{acc}  = $c->stash->{pfam}->pfamA_acc;
	$c->stash->{alnType} = ( $c->req->param( "alnType" ) eq "seed" ) ? "seed" : "full"
	  if defined $c->req->param( "alnType" );
	
  }	elsif( defined $c->req->param( "entry" ) ) {

	if( $c->req->param( "entry" ) =~ /^(P[FB]\d{5})$/i ) {

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

	# de-taint the accession or ID
	my $input = $c->req->param("acc")
	  || $c->req->param("id")
	  || $c->req->param("entry")
	  || "";
	$input =~ s/^(\w+)/$1/;

	# see if this was an internal link and, if so, report it
	my $b = $c->req->base;
	if( defined $c->req->referer and $c->req->referer =~ /^$b/ ) {

	  # this means that the link that got us here was somewhere within
	  # the Pfam site and that the accession or ID which it specified
	  # doesn't actually exist in the DB

	  # report the error as a broken internal link
	  $c->error( "Found a broken internal link; no valid Pfam family accession or ID "
				 . "(\"$input\") in \"" . $c->req->referer . "\"" );
	  $c->forward( "/reportError" );

	  # now reset the errors array so that we can add the message for
	  # public consumption
	  $c->clear_errors;

	}

	# the message that we'll show to the user
	$c->stash->{errorMsg} = "No valid Pfam family accession or ID";

	# log a warning and we're done; drop out to the end method which
	# will put up the standard error page
	$c->log->warn( "Family::begin: no valid Pfam family ID or accession" );

	return;
  }

  $c->log->debug( "Family::begin: successfully retrieved a pfam object" );

  #----------------------------------------

  # if this is a PfamA then we need to get more stuff out of the
  # database for it...

  if( $c->stash->{entryType} eq "A" ) {

	# add the clan details, if any
	$c->stash->{clan} = $c->model("PfamDB::Clans")
	  ->find( { clan_acc => $c->stash->{pfam}->clan_acc } )
		if defined $c->stash->{pfam}->clan_acc;

	# add extra data to the stash
	$c->forward( "_getSummaryData" );
	$c->forward( "_getDbXrefs" );

  }

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# get the data items for the overview bar

sub _getSummaryData : Private {
  my( $this, $c ) = @_;

  my %summaryData;

  # make things easier by getting hold of the auto_pfamA
  my $auto_pfamA = $c->stash->{pfam}->auto_pfamA;

  # number of architectures....
  $summaryData{numArchitectures} = $c->stash->{pfam}->number_archs;

  # number of sequences in full alignment
  $summaryData{numSequences} = $c->stash->{pfam}->num_full;

  # number of structures known for the domain
  $summaryData{numStructures} = $c->stash->{pfam}->number_structures;

  # Number of species
  $summaryData{numSpecies} = $c->stash->{pfam}->number_species;

  # number of interactions
  #$rs = $c->model("PfamDB::Int_pfamAs")->find({ auto_pfamA_A => $auto_pfamA },
	#{ select => [
	#			 { count => "auto_pfamA_A" }
	#			],
	#  as => [ qw/NumInts/ ]
    #}
  #);

  $summaryData{numInt} = 0;

  $c->stash->{summaryData} = \%summaryData;

}

#-------------------------------------------------------------------------------
# gets the database cross-references

sub _getDbXrefs : Private {
  my( $this, $c ) = @_;

  my %xRefs;

  # stuff in the accession and ID for this entry
  $xRefs{entryAcc} = $c->stash->{pfam}->pfamA_acc;
  $xRefs{entryId}  = $c->stash->{pfam}->pfamA_id;

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
  my @atoaPRC = $c->model("PfamDB::PfamA2pfamA_PRC_results")
	->search(
			 { "pfamA1.pfamA_acc" => $c->stash->{pfam}->pfamA_acc },
			 { join               => [ qw/pfamA1 pfamA2/ ],
			   select             => [ qw/pfamA1.pfamA_id pfamA2.pfamA_id evalue/ ],
			   as                 => [ qw/l_pfamA_id r_pfamA_id evalue/ ],
			   order_by           => "pfamA2.auto_pfamA ASC" }
			);

  $xRefs{atoaPRC} = [];
  foreach ( @atoaPRC ) {
	push @{$xRefs{atoaPRC}}, $_ if $_->get_column( "evalue" ) <= 0.001;
  }

#  $xRefs{atoaPRC} = \@atoaPRC if scalar @atoaPRC;

  # PfamB to PfamA links based on PRC
  my @atobPRC = $c->model("PfamDB::PfamB2pfamA_PRC_results")->search(
    { "pfamA.pfamA_acc" => $c->stash->{pfam}->pfamA_acc, },
	{ join      => [ qw/pfamA pfamB/ ],
	  prefetch  => [ qw/pfamA pfamB/ ] } );

  # find the union between PRC and PRODOM PfamB links
  my %atobPRC;
  foreach ( @atobPRC ) {
	$atobPRC{$_->pfamB_acc} = $_ if $_->evalue <= 0.001;
  }
  # we should be able to filter the results of the query according to
  # evalue using a call on the DBIx::Class object, but for some reason
  # it's broken, hence that last loop rather than this neat map...
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

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
