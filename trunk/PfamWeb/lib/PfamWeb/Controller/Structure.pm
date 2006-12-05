
# Structure.pm
# jt6 20060706 WTSI
#
# $Id: Structure.pm,v 1.9 2006-12-05 10:11:23 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure - controller for structure-related
sections of the site

=cut

package PfamWeb::Controller::Structure;

=head1 DESCRIPTION

This is intended to be the base class for everything related to 3-D
structure across the site. The L<begin|/"begin : Private"> method will
try to extract a PDB ID from the captured URL and then try to load a
Pdb object from the model into the stash.

This is also the controller that handles the structure section of the
site, so it includes an action to capture a URL like

=over

=item http://localhost:3000/structure?id=1abc

=back

Generates a B<tabbed page>.

$Id: Structure.pm,v 1.9 2006-12-05 10:11:23 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Section";

__PACKAGE__->config( SECTION => "structure" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract a PDB ID from the URL and gets the row in the Pdb table
for that entry. Accepts various formats of URL:

=over

=item * http://localhost:3000/structure/I<..>?id=1abc

=item * http://localhost:3000/structure/I<..>?entry=1abc

=item * http://localhost:3000/structure/I<..>/1abc

=item * http://localhost:3000/structure/I<..>/1abc.pdb

=back

=cut

sub begin : Private {
  my( $this, $c, $pdbIdArg ) = @_;

  # get the accession or ID code

  my( $pdb, $pdbId );
  if( defined $c->req->param("id") ) {

	$c->req->param("id") =~ m/^([0-9][A-Z0-9]{3})$/i;
	$pdbId = $1	if defined $1;

  } elsif( defined $c->req->param("entry") ) {

	# handle requests that specify "entry" rather than "id"

	$c->req->param("entry") =~ m/^([0-9][A-Z0-9]{3})$/i;
	$pdbId = $1	if defined $1;

	# previously we were redirecting back to this same action,
	# exchanging "entry" for "id", but it's a bit daft, given really..
	#$c->log->debug( "Structure::begin: looks like an ID ($1); redirecting" );
	#$c->res->redirect( $c->uri_for( "/structure", { id => $1 } ) );

  } elsif( defined $pdbIdArg ) {

	$c->log->debug( "Structure::begin: found an argument ($pdbIdArg); checking..." );
	$pdbIdArg =~ /^(\d\w{3})/;
	$pdbId = $1 if defined $1;

  }

  $pdb = $c->model("PfamDB::Pdb")->find( { pdb_id => $pdbId } );

  # we're done here unless there's an entry specified
  unless( defined $pdb ) {

	# de-taint the ID
	my( $input ) = $c->req->param("id") =~ /^(\w+)/;

	# see if this was an internal link and, if so, report it
	my $b = $c->req->base;
	if( defined $c->req->referer and $c->req->referer =~ /^$b/ ) {

	  # report the error as a broken internal link
	  $c->error( "Found a broken internal link; no valid PDB ID "
				 . "(\"$input\") in \"" . $c->req->referer . "\"" );
	  $c->forward( "/reportError" );

	  $c->clear_errors;
	}

	$c->stash->{errorMsg} = "No valid PDB ID";

	# log a warning and we're done; drop out to the end method which
	# will put up the standard error page
	$c->log->warn( "Structure::begin: couldn't retrieve data for PDB ID |$input|" );

	return;
  }

  $c->log->debug( "Structure::begin: successfully retrieved pdb object for $pdbId" );

  # stash the PDB object and ID
  $c->stash->{pdb}   = $pdb;
  $c->stash->{pdbId} = $pdbId;

  # get the icon summary data
  $c->forward( "_getSummaryData" );

}

#-------------------------------------------------------------------------------

=head2 addMapping : Path

Adds the structure-to-UniProt mapping to the stash. Required by a
couple of subclasses, such as
L<Viewer|/"PfamWeb::Controller::Structure::Viewer">.

Call using a C<forward>, e.g. C<$c->forward( "addMapping" );>

=cut

sub addMapping : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Structure::addMapping: adding mappings for PDB entry "
				  . $c->stash->{pdb}->pdb_id );

  # add the structure-to-UniProt mapping to the stash
  my @unpMap = $c->model("PfamDB::PdbMap")
	->search(
			 { auto_pdb    => $c->stash->{pdb}->auto_pdb,
			   pfam_region => 1 },
			 { join     => [ qw/pfamA pfamseq/ ],
			   prefetch => [ qw/pfamA pfamseq/ ],
			   order_by => "chain ASC" }
			);

  $c->log->debug( "Structure::addMapping: found " . scalar @unpMap . " mappings" );
  $c->stash->{mapping} = \@unpMap;

  # build a little data structure to map PDB chains to uniprot IDs and
  # then cache that for the post-loaded graphics component
  my( %chains, $chain );
  foreach my $row ( @unpMap ) {
	$chain = ( defined $row->chain ) ? $row->chain : " ";
	# N.B. Need to think more about the consequences of setting null
	# chain ID to " "...

	$chains{$row->pfamseq_id}->{$chain} = "";
  }
  $c->cache->set( "chain_mapping", \%chains );

}

#-------------------------------------------------------------------------------

=head2 default : Path

Picks up a URL like

=over

=item http://localhost:3000/structure?id=1abc

=back

Also adds the structure-sequence-pfam mapping to the stash, since
that's used by various subclasses, such as Viewer.

=cut

sub default : Path {
  my( $this, $c ) = @_;

  return 0 unless defined $c->stash->{pdb};

  $c->forward( "addMapping" );

  # get the authors list
  my @authors = $c->model("PfamDB::PdbAuthor")->search(
                  { auto_pdb => $c->stash->{pdb}->auto_pdb },
				  { order_by => "author_order ASC" }
                );

  $c->stash->{authors} = \@authors;

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# get the data items for the overview bar

sub _getSummaryData : Private {
  my( $this, $c ) = @_;

  my %summaryData;

  my $autoPdb = $c->stash->{pdb}->auto_pdb;

  # number of sequences in the structure - count the number of chains.
  my $rs = $c->model("PfamDB::Pdb_residue")->find({auto_pdb => $autoPdb},
						  {select => [ { count => [ {distinct => ["chain"] } ]}],
						     as   => [ qw/numChains/]});
  $summaryData{numSequences} = $rs->get_column( "numChains" );

  # number of species should be one, but get the species for the sequences
  $rs = $c->model("PfamDB::Pdb_residue")->find({auto_pdb => $autoPdb},
						{  join => [ qw/pfamseq/],
						   select => [ { count => [ {distinct => ["pfamseq.species"] } ]}],
						    as => [ qw/numSpecies/]} );
  $summaryData{numSpecies} = $rs->get_column( "numSpecies" );

  # number architectures
  $rs = $c->model("PfamDB::Pdb_residue")->find({auto_pdb => $autoPdb},
						{  join => [ qw/pfamseq_arch/],
						   select => [ { count => [ {distinct => ["pfamseq_arch.auto_architecture"] } ]}],
						     as => [ qw/numArch/]} );
  $summaryData{numArchitectures} = $rs->get_column( "numArch" );;

  # number of interactions.
  $rs = $c->model("PfamDB::Interactions")->find(
						{ auto_pdb => $autoPdb },
						{ select => [
							     { count => [
									 { distinct => [ "auto_int_pfamAs" ] }
									]
							     }
							    ],
						  as => [ qw/numInts/ ]
						}
					       );
  $summaryData{numInt} = $rs->get_column("numInts");

  # number of structures is one
  $summaryData{numStructures} = 1;

  $c->stash->{summaryData} = \%summaryData;

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
