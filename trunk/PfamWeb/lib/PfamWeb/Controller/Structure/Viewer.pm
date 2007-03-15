
# Viewer.pm
# jt6 20060728 WTSI
#
# $Id: Viewer.pm,v 1.7 2007-03-15 14:06:15 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::Viewer - controller for the structure
viewers

=cut

package PfamWeb::Controller::Structure::Viewer;

=head1 DESCRIPTION

A simple controller to hand off to (currently) either Jmol or
AstexViewer.

Generates a B<full page>.

$Id: Viewer.pm,v 1.7 2007-03-15 14:06:15 jt6 Exp $

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
#  	my @markups = $c->model("PfamDB::Pfamseq_markup")
# 	  ->search(
# 			   { "pdbResidue.auto_pfamseq" => $map->auto_pfamseq,
# 				 "pdbResidue.chain"        => $map->chain,
# 				 "pdbResidue.auto_pdb"     => $c->stash->{pdb}->auto_pdb },
# 			   { join     => [qw/pdbResidue/],
# 				 prefetch => [qw/pdbResidue/] }
# 			  );

	$c->log->debug( "Viewer::auto: about to do query; DBIC_TRACE = |".$ENV{DBIC_TRACE}."|" );

 	my @markups = $c->model("PfamDB::Pdb_residue")
	  ->search( { "pfamseqMarkup.auto_pfamseq" => $map->auto_pfamseq,
				  chain                        => $map->chain,
				  auto_pdb                     => $c->stash->{pdb}->auto_pdb },
				{ join                         => [ qw/pfamseqMarkup/ ],
				 prefetch                      => [ qw/pfamseqMarkup/ ] } );
	$c->log->debug( "Viewer::auto: found " . scalar @markups . " markups for mapping to "
					. $map->auto_pfamseq );

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

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
