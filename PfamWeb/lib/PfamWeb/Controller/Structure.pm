
# Structure.pm
# jt6 20060706 WTSI
#
# $Id: Structure.pm,v 1.6 2006-08-23 14:35:09 jt6 Exp $

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

Generates a B<full page>.

$Id: Structure.pm,v 1.6 2006-08-23 14:35:09 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

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

  $c->log->debug( "Structure::begin: found a valid ID ($pdbId)" );
  $pdb = $c->model("PfamDB::Pdb")->find( { pdb_id => $pdbId } );

  # we're done here unless there's an entry specified
  $c->log->warn( "Structure::begin: couldn't retrieve data for PDB ID |$pdbId|" )
	and return
	  unless defined $pdb;

  # stash the PDB object and ID
  $c->stash->{pdb}   = $pdb;
  $c->stash->{pdbId} = $pdbId;
  my $autoPdb = $c->stash->{pdb}->auto_pdb;

  # Now get the Icon information for the structure;
  my %summaryData;

  # Number of sequences in the structure - count the number of chains.
  my $rs = $c->model("PfamDB::Pdb_residue")->find({auto_pdb => $autoPdb},
						  {select => [ { count => [ {distinct => ["chain"] } ]}],
						     as   => [ qw/numChains/]});
  $summaryData{numSequences} = $rs->get_column( "numChains" );

  # Number of species should be one, but get the species for the sequences
  $rs = $c->model("PfamDB::Pdb_residue")->find({auto_pdb => $autoPdb},
						{  join => [ qw/pfamseq/],
						   select => [ { count => [ {distinct => ["pfamseq.species"] } ]}],
						    as => [ qw/numSpecies/]} );
  $summaryData{numSpecies} = $rs->get_column( "numSpecies" );

  # Number Architectures
  $rs = $c->model("PfamDB::Pdb_residue")->find({auto_pdb => $autoPdb},
						{  join => [ qw/pfamseq_arch/],
						   select => [ { count => [ {distinct => ["pfamseq_arch.auto_architecture"] } ]}],
						     as => [ qw/numArch/]} );
  $summaryData{numArchitectures} = $rs->get_column( "numArch" );;

  # Number of Interactions.
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

  # Structures is one
  $summaryData{numStructures} = 1;

  $c->stash->{summaryData} = \%summaryData;

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

  # add the structure-to-UniProt mapping to the stash
  my @unpMap = $c->model("PfamDB::PdbMap")->search(
                  { auto_pdb    => $c->stash->{pdb}->auto_pdb,
					pfam_region => 1 },
				  { join     => [ qw/pfamA pfamseq/ ],
					prefetch => [ qw/pfamA pfamseq/ ],
					order_by => "chain ASC" }
				);
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

Picks up a URL like http://localhost:3000/structure?id=1abc

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

=head2 end : Private

Hands off to the full page template or catches any errors that were
generated earlier

=cut

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pdb object in the stash
  return 0 unless defined $c->stash->{pdb};

  # set up the TT view
  # check for errors
  if ( scalar @{ $c->error } ) {
	$c->stash->{errors}   = $c->error;
	$c->stash->{template} = "components/blocks/structure/errors.tt";
  } else {
	$c->stash->{pageType} = "structure";
	$c->stash->{template} = "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

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
