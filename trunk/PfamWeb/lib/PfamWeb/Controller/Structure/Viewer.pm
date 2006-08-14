
# Viewer.pm
# jt6 20060728 WTSI
#
# $Id: Viewer.pm,v 1.1 2006-08-14 10:46:35 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::Viewer - controller for the structure
viewers

=cut

package PfamWeb::Controller::Structure::Viewer;

=head1 DESCRIPTION

A simple controller to hand off to (currently) either Jmol or
AstexViewer.

Generates a B<full page>.

$Id: Viewer.pm,v 1.1 2006-08-14 10:46:35 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Structure";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Retrieves any annotations for the chosen structure and drops them into
the stash

=cut

sub auto : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Viewer::auto: is there a PDB object in the stash ?" );

  return 0 unless defined $c->stash->{pdb};

  # we need the mapping from structure-to-UniProt
  $c->forward( "addMapping" );

  # get the markup for this entry
  my %seenChainAutoPfamseq;
  my @allMarkups;
  foreach my $map ( @{$c->stash->{mapping}}  ) {
	
	next if $seenChainAutoPfamseq{ $map->auto_pfamseq.$map->chain };
 	#my @markups = PfamWeb::Model::Pfamseq_markup->search(
 	my @markups = $c->model("PfamDB::Pfamseq_markup")->search(
                    { "pdbResidue.auto_pfamseq" => $map->auto_pfamseq,
					  "pdbResidue.chain"        => $map->chain,
					  "pdbResidue.auto_pdb"     => $c->stash->{pdb}->auto_pdb },
				    { join     => [qw/pdbResidue/],
					  prefetch => [qw/pdbResidue/] }
				  );
	$seenChainAutoPfamseq{ $map->auto_pfamseq.$map->chain}++;
	push @allMarkups, @markups;
  }

  $c->stash->{markups} = \@allMarkups;

  if( defined $c->req->param("viewer") ) {
	$c->req->param("viewer") =~ m/^(av|jmol)$/i;
	$c->stash->{viewer} = $1 if defined $1;
  }

  # default to jmol
  $c->stash->{viewer} ||= "jmol";

  return 1;
}

#-------------------------------------------------------------------------------

=head2 default : Path

Picks up a URL like http://localhost:3000/structure/viewer?id=1abc

=cut

sub default : Path {
  my( $this, $c ) = @_;

  $c->log->debug( "showing " . $c->stash->{viewer}
				  . " for entry " . $c->stash->{pdbId} );

}

#-------------------------------------------------------------------------------

=head2 end : Private

Overrides the basic "end" method from the Structure base class and
forwards either to the Jmol or AstexViewer full-page templates, as
appropriate. Renders an error page if problems were encountered
previously

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
	$c->stash->{fullPage} = 1;
	$c->stash->{template} = "pages/" . $c->stash->{viewer} . ".tt";
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
