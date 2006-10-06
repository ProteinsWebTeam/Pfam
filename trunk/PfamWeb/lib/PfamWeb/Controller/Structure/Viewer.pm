
# Viewer.pm
# jt6 20060728 WTSI
#
# $Id: Viewer.pm,v 1.5 2006-10-06 10:22:33 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::Viewer - controller for the structure
viewers

=cut

package PfamWeb::Controller::Structure::Viewer;

=head1 DESCRIPTION

A simple controller to hand off to (currently) either Jmol or
AstexViewer.

Generates a B<full page>.

$Id: Viewer.pm,v 1.5 2006-10-06 10:22:33 jt6 Exp $

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
  my( @allMarkups, $ap, $chain );
  foreach my $map ( @{$c->stash->{mapping}}  ) {

	# all this crap is to avoid warnings when we try to build a hash
	# key using a chain ID that is not defined...
	$ap    = ( defined $map->auto_pfamseq ) ? $map->auto_pfamseq : "";
	$chain = ( defined $map->chain ) ? $map->chain : "";

	$c->log->debug( "Viewer::auto: auto_pfamseq, chain: |$ap|$chain|" );

	next if $seenChainAutoPfamseq{$ap.$chain};
 	my @markups = $c->model("PfamDB::Pfamseq_markup")->search(
                    { "pdbResidue.auto_pfamseq" => $map->auto_pfamseq,
					  "pdbResidue.chain"        => $map->chain,
					  "pdbResidue.auto_pdb"     => $c->stash->{pdb}->auto_pdb },
				    { join     => [qw/pdbResidue/],
					  prefetch => [qw/pdbResidue/] }
				  );
	$seenChainAutoPfamseq{$ap.$chain}++;
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

  $c->stash->{template} = "components/tools/" . $c->stash->{viewer} . ".tt";
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
