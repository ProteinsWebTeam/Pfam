
# Viewer.pm
# jt6 20060728 WTSI
#
# $Id: Viewer.pm,v 1.1 2008-10-23 10:56:26 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Structure::Viewer - controller for the structure
viewers

=cut

package RfamWeb::Controller::Structure::Viewer;

=head1 DESCRIPTION

A simple controller to hand off to (currently) either Jmol or
AstexViewer.

Generates a B<full page>.

$Id: Viewer.pm,v 1.1 2008-10-23 10:56:26 jt6 Exp $

=cut

use strict;
use warnings;

use base 'RfamWeb::Controller::Structure';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 viewer : Path

Show a structure viewer. Which viewer is specified by the "viewer" parameter.

=cut

sub viewer : Path {
  my( $this, $c ) = @_;

  # get the markup for this entry
  my %seenChainAutoRfamseq;
  my( @allMarkups, $ar, $chain );
#  foreach my $map ( @{$c->stash->{mapping}} ) {
#
#    # all this crap is to avoid warnings when we try to build a hash
#    # key using a chain ID that is not defined...
#    $ar    = ( defined $map->auto_rfamseq ) ? $map->auto_rfamseq : '';
#    $chain = ( defined $map->chain ) ? $map->chain : '';
#  
#    $c->log->debug( "Structure::Viewer::viewer: auto_rfamseq, chain: |$ar|$chain|" );
#  
#    next if $seenChainAutoRfamseq{$ar.$chain};
#
#    my @markups = $c->model('RfamDB:PdbRfamReg')
#                    ->search( { 'pfamseqMarkup.auto_pfamseq' => $ar,
#                                chain                        => $chain,
#                                auto_pdb                     => $c->stash->{pdb}->auto_pdb },
#                              { join                         => [ qw( pfamseqMarkup ) ],
#                                prefetch                     => [ qw( pfamseqMarkup ) ] } );
#    $c->log->debug( 'Structure::Viewer::viewer: found ' . scalar @markups
#                    . ' markups for mapping to ' . $ar );
#  
#    $seenChainAutoRfamseq{$ar.$chain}++;
#    push @allMarkups, @markups;
#  }
#
#  $c->stash->{markups} = \@allMarkups;

  if( defined $c->req->param('viewer') ) {
    $c->req->param('viewer') =~ m/^(av|jmol)$/i;
    $c->stash->{viewer} = $1 if defined $1;
  }

  # default to jmol
  $c->stash->{viewer} ||= 'jmol';

  $c->log->debug( 'Structure::Viewer::viewer: showing ' . $c->stash->{viewer}
                  . ' for entry ' . $c->stash->{pdbId} );

  $c->stash->{template} = 'components/tools/' . $c->stash->{viewer} . '.tt';
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
