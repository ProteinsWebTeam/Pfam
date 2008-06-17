
# Jump.pm
# jt6 20080314 WTSI
#
# $Id: Jump.pm,v 1.2 2008-06-17 09:17:15 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Jump - guess the type of entry that the user wants to see

=cut

package RfamWeb::Controller::Search::Jump;

=head1 DESCRIPTION

$Id: Jump.pm,v 1.2 2008-06-17 09:17:15 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::Search::Jump';

#-------------------------------------------------------------------------------

=head1 METHODS

This is a sub-class of the L<Jump> controller, with a L<guess> method that is
specific to Rfam.

=cut

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 guess : Private

Coordinates the guessing. All this action does is forward to one or more other
actions, which actually try to figure out what the user-specified entry actually
refers to. If the C<$entry_type> argument is not null, guesses are restricted
to that particular type of entry, e.g. "family", "protein", etc.

=cut

sub guess : Private {
  my ( $this, $c, $entry, $entry_type ) = @_;
  
  $c->log->debug( "Search::Jump::guess: guessing target for |$entry|" )
    if $c->debug;
    
  my %action_types= ( family    => 'guess_family', );

  my @available_actions = qw( guess_family );

  my $guess_actions;
  if ( $entry_type and $action_types{$entry_type} ) {
    $c->log->debug( "Search::Jump::guess: guessing only for type: |$entry_type|" )
      if $c->debug;
    push @$guess_actions, $action_types{$entry_type};
  }
  else {
    $c->log->debug( 'Search::Jump::guess: guessing for any type' ) if $c->debug;
    $guess_actions = \@available_actions;
  }

  my $action;
  foreach my $guess_action( @$guess_actions ) {
    last if $action = $c->forward( $guess_action, [ uc $entry ] );
  }

  return $action;
}

#-------------------------------------------------------------------------------

=head2 guess_family : Private

Look for a Pfam family (A or B) with the specified accession or ID.

=cut

sub guess_family : Private {
  my ( $this, $c, $entry ) = @_;

  $c->log->debug( 'Search::Jump::guess_family: looking for a family...' )
    if $c->debug;
  
  my @rs = $c->model('RfamDB::Rfam')
             ->search( [ { rfam_acc => $entry },
                         { rfam_id  => $entry } ] );

  return 'family' if scalar @rs;
  
  # see if this could be a dead family
  my $dead = $c->model('RfamDB::DeadFamilies')
               ->find( { rfam_acc => $entry } );
  return 'family' if defined $dead;
  
}

#-------------------------------------------------------------------------------
#- cargo area ------------------------------------------------------------------
#-------------------------------------------------------------------------------

#=head2 guess_sequence : Private
#
#Look for a sequence with the specified accession or ID. We check for UniProt
#and metaseq accessions/IDs, as well as NCBI GIs.
#
#=cut
#
#sub guess_sequence : Private {
#  my( $this, $c, $entry ) = @_;
#  
#  $c->log->debug( 'Search::Jump::guess_sequence: looking for a sequence...' )
#    if $c->debug;
#    
#  # how about a sequence entry ?
#  my $found;
#  if( $entry =~ m/^([AOPQ]\d[A-Z0-9]{3}\d)(\.\d+)?$/i ) {
#  
#    return 'protein' if $c->model('PfamDB::Pfamseq')
#                          ->find( { pfamseq_acc => $1 } );
#  }
#
#  # see if it's a protein sequence ID (e.g. CANX_CHICK)
#  if( $entry =~ m/^([A-Z0-9]+\_[A-Z0-9]+)$/ ) {
#  
#    return 'protein' if $c->model('PfamDB::Pfamseq')
#                          ->find( { pfamseq_id => $1 } );
#  }
#  
#  # see if it's a secondary accession; a bit gnarly...
#  return 'protein' if $c->model('PfamDB::Secondary_pfamseq_acc')
#                        ->find( { secondary_acc => $1 },
#                                { join =>     [ qw( pfamseq ) ],
#                                  prefetch => [ qw( pfamseq ) ] } );
#  
#  # an NCBI GI number ?
#  if( $entry =~ m/^(gi)?(\d+)$/ ) {
#  
#    return 'ncbiseq' if $c->model('PfamDB::Ncbi_seq')
#                          ->find( { gi => $2 } );
#  }
#  
#  # an NCBI secondary accession ?
#  my @rs = $c->model('PfamDB::Ncbi_seq')
#             ->search( { secondary_acc => { 'like', "$entry%" } } );
#  return 'ncbiseq' if scalar @rs;
#
#  # a metaseq ID or accession ?
#  @rs = $c->model('PfamDB::Metaseq')
#          ->search( [ { metaseq_acc => $entry }, 
#                      { metaseq_id  => $entry } ] );
#  return 'metaseq' if scalar @rs;
#
#}
#
##-------------------------------------------------------------------------------
#
#=head2 guess_clan : Private
#
#Look for a Pfam clan with the specified accession or ID.
#
#=cut
#
#sub guess_clan : Private {
#  my( $this, $c, $entry ) = @_;
#  
#  $c->log->debug( 'Search::Jump::guess_clan: looking for a clan...' )
#    if $c->debug;
#    
#  # no point worrying about whether we can match to a regex for clan accession,
#  # since we'd end up doing essentially this query whether $entry looks like
#  # an accession or not
#  my @rs = $c->model('PfamDB::Clans')
#             ->search( [ { clan_acc => $entry },
#                         { clan_id  => $entry } ] );
#  return 'clan' if scalar @rs;
#
#}  
#
##-------------------------------------------------------------------------------
#
#=head2 guess_structure : Private
#
#Look for a PDB structure with the specified ID.
#
#=cut
#
#sub guess_structure : Private {
#  my( $this, $c, $entry ) = @_;
#  
#  $c->log->debug( 'Search::Jump::guess_structure: looking for a structure...' )
#    if $c->debug;
#    
#  # maybe a structure ?
#  if( $entry =~ m/^([0-9][A-Za-z0-9]{3})$/ ) {
#    return 'structure' if $c->model('PfamDB::Pdb')
#                            ->find( { pdb_id => $1 } );
#  }
#}
#
##-------------------------------------------------------------------------------
#
#=head2 guess_structure : Private
#
#Look for a proteome with the specified species name.
#
#=cut
#
#sub guess_proteome : Private {
#  my( $this, $c, $entry ) = @_;
#  
#  $c->log->debug( 'Search::Jump::guess_proteome: looking for a proteome...' )
#    if $c->debug;
#    
#  # a proteome ID ?
#  return 'proteome' if $c->model('PfamDB::Proteome_species')
#                         ->find( { species => $entry } );
#}
#
#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
