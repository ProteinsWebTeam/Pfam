
# PfamWeb.pm
# jt 20060316 WTSI
#
# This is the main class for the Pfam website catalyst
# application. Configuration is all done through the pfamweb.yml
# config file and there's (currently) not much else in here.
#
# $Id: PfamWeb.pm,v 1.4 2006-04-20 16:30:26 jt6 Exp $

package PfamWeb;

use strict;
use warnings;

#
# Set flags and add plugins for the application
#
#         -Debug: activates the debug mode for very useful log messages
# Static::Simple: will serve static files from the application's root
# directory
#
use Catalyst qw/ -Debug
				 ConfigLoader
				 Static::Simple
				 Session
				 Session::Store::FastMmap
				 Session::State::Cookie /;

use PfamConfig qw( pfamlib );

our $VERSION = '0.01';

#
# Configure the application
#

#__PACKAGE__->config( file => "../PfamConfig/pfamweb.yml" );

# use the default location for the config file, i.e. the root of the
# catalyst application

#
# Start the application
#
__PACKAGE__->setup;

=head1 NAME

PfamWeb - Catalyst based application

=head1 SYNOPSIS

    script/pfamweb_server.pl

=head1 DESCRIPTION

Catalyst based application.

=head1 METHODS

=cut



# get the row in the Pfam table for this entry

sub begin : Private {
  my( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(PF\d{5})$/;
	$c->log->info( "$this: found accession |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param("id") ) {

	$c->req->param("id") =~ m/(^\w+$)/;
	$c->log->info( "$this: found ID |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_id => $1 } )
	  if defined $1;

  }	

  # we're done here unless there's an entry specified
  $c->log->warn( "$this: no ID or accession" ) and return
	unless defined $c->stash->{pfam};

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

  # number or architectures....
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

  # HACK: hardcoded interactions number added here...
  $summaryData{numIpfam} = 7;
  $c->log->warn( "$this: WARNING: number of interactions is hard coded !" );

  $c->stash->{summaryData} = \%summaryData;

}


#
# Output a friendly welcome message
#
=head2 default

=cut

sub default : Private {
    my ( $self, $c ) = @_;

    # Hello World
    $c->response->body( $c->welcome_message );
}

#
# Uncomment and modify this end action after adding a View component
#
#=head2 end
#
#=cut
#
#sub end : Private {
#    my ( $self, $c ) = @_;
#
#    # Forward to View unless response body is already defined
#    $c->forward( $c->view('') ) unless $c->response->body;
#}

=head1 AUTHOR

John Tate,,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
