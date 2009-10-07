
# Jump.pm
# jt6 20060807 WTSI
#
# $Id: Jump.pm,v 1.19 2009-10-07 12:04:31 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Jump - guess the type of entry that the user wants to see

=cut

package PfamWeb::Controller::Search::Jump;

=head1 DESCRIPTION

$Id: Jump.pm,v 1.19 2009-10-07 12:04:31 jt6 Exp $

=cut

use strict;
use warnings;

use URI;

use base 'PfamBase::Controller::Search::Jump';

#-------------------------------------------------------------------------------

=head1 METHODS

This is a concrete sub-class of the base C<Jump> controller, specifically for
the PfamWeb application.

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
  my( $this, $c, $entry, $entry_type ) = @_;
  
  $c->log->debug( "Search::Jump::guess: guessing target for |$entry|" )
    if $c->debug;
    
  my %action_types= ( family    => 'guess_family',
                      protein   => 'guess_sequence',
                      clan      => 'guess_clan',
                      structure => 'guess_structure', );
#                      proteome  => 'guess_proteome' );

  my @available_actions = qw( guess_family
                              guess_clan
                              guess_structure
                              guess_sequence );
#                              guess_proteome

  my $guess_actions;
  if( $entry_type and $action_types{$entry_type} ) {
    $c->log->debug( "Search::Jump::guess: guessing only for type: |$entry_type|" )
      if $c->debug;
    push @$guess_actions, $action_types{$entry_type};
  } else {
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
  my( $this, $c, $entry ) = @_;

  $c->log->debug( 'Search::Jump::guess_family: looking for a family...' )
    if $c->debug;
  
  # first, see if it's a Pfam-A family accession
  my $found;
  if( $entry =~ m/^(PF\d{5})(\.\d+)?$/ ) {
    
    return 'family' if $c->model('PfamDB::Pfama')
                         ->find( { pfama_acc => $1 } );
  } else {
    # a Pfam family ID ?
    return 'family' if $c->model('PfamDB::Pfama')
                         ->find( { pfama_id => $entry } );
  }

  # see if this could be a dead family
  my @rs = $c->model('PfamDB::DeadFamilies')
             ->search( [ { pfama_acc => $entry },
                         { pfama_id  => $entry } ] );
  return 'family' if scalar @rs;
  
  # or a Pfam-B accession ?
  if( $entry =~ m/^(PB\d{6})$/ ) {

    return 'pfamb' if $c->model('PfamDB::Pfamb')
                        ->find( { pfamb_acc => $1 } );      
  }
    
  # maybe a Pfam-B ID ?
  if( $entry =~ m/^(PFAM-B_\d+)$/ ) {

    return 'pfamb' if $c->model('PfamDB::Pfamb')
                        ->find( { pfamb_id => $1 } );
  }
  
}

#-------------------------------------------------------------------------------

=head2 guess_sequence : Private

Look for a sequence with the specified accession or ID. We check for UniProt
and metaseq accessions/IDs, as well as NCBI GIs.

=cut

sub guess_sequence : Private {
  my( $this, $c, $entry ) = @_;
  
  $c->log->debug( 'Search::Jump::guess_sequence: looking for a sequence...' )
    if $c->debug;
    
  # how about a sequence entry ?
  my $found;
  if( $entry =~ m/^([A-Z]\d[A-Z0-9]{3}\d)(\.\d+)?$/i ) {
  
    return 'protein' if $c->model('PfamDB::Pfamseq')
                          ->find( { pfamseq_acc => $1 } );
  }
  # TODO why wouldn't we just combine these two (^ and v) queries ?

  # see if it's a protein sequence ID (e.g. CANX_CHICK)
  if( $entry =~ m/^([A-Z0-9]+\_[A-Z0-9]+)$/ ) {
  
    return 'protein' if $c->model('PfamDB::Pfamseq')
                          ->find( { pfamseq_id => $1 } );
  }
  
  # see if it's a secondary accession; a bit gnarly...
  return 'protein' if $c->model('PfamDB::SecondaryPfamseqAcc')
                        ->search( { secondary_acc => $1 },
                                  { join =>     [ qw( auto_pfamseq ) ],
                                    prefetch => [ qw( auto_pfamseq ) ] } )
                        ->first;
  
  # an NCBI GI number ?
  if( $entry =~ m/^(gi)?(\d+)$/ ) {
  
    return 'ncbiseq' if $c->model('PfamDB::NcbiSeq')
                          ->find( { gi => $2 } );
  }
  
  # a metaseq ID or accession ?
  my @rs = $c->model('PfamDB::Metaseq')
             ->search( [ { metaseq_acc => $entry }, 
                         { metaseq_id  => $entry } ] );
  return 'metaseq' if scalar @rs;

  # an NCBI secondary accession ? Note: this was a really slow query when we
  # were being helpful and using "like", so it's the very last thing that
  # will be attempted and we're doing only an exact lookup
#   @rs = $c->model('PfamDB::NcbiSeq')
#           ->search( { secondary_acc => $entry } );
# #         ->search( { secondary_acc => { 'like', "$entry%" } } );
#   return 'ncbiseq' if scalar @rs;

}

#-------------------------------------------------------------------------------

=head2 guess_clan : Private

Look for a Pfam clan with the specified accession or ID.

=cut

sub guess_clan : Private {
  my( $this, $c, $entry ) = @_;
  
  $c->log->debug( 'Search::Jump::guess_clan: looking for a clan...' )
    if $c->debug;
    
  # no point worrying about whether we can match to a regex for clan accession,
  # since we'd end up doing essentially this query whether $entry looks like
  # an accession or not
  my @rs = $c->model('PfamDB::Clans')
             ->search( [ { clan_acc => $entry },
                         { clan_id  => $entry } ] );
  return 'clan' if scalar @rs;

}  

#-------------------------------------------------------------------------------

=head2 guess_structure : Private

Look for a PDB structure with the specified ID.

=cut

sub guess_structure : Private {
  my( $this, $c, $entry ) = @_;
  
  $c->log->debug( 'Search::Jump::guess_structure: looking for a structure...' )
    if $c->debug;
    
  # maybe a structure ?
  if( $entry =~ m/^([0-9][A-Za-z0-9]{3})$/ ) {
    return 'structure' if $c->model('PfamDB::Pdb')
                            ->find( { pdb_id => $1 } );
  }
}

#-------------------------------------------------------------------------------

=head2 guess_structure : Private

Look for a proteome with the specified species name.

=cut

sub guess_proteome : Private {
  my( $this, $c, $entry ) = @_;
  
  $c->log->debug( 'Search::Jump::guess_proteome: looking for a proteome...' )
    if $c->debug;

  my @rs = $c->model( 'PfamDB::ProteomeSpecies' )
             ->search( [ { species   => $entry },
                         { ncbi_code => $entry } ] );
  
  # a proteome ID ?
  return 'proteome' if scalar @rs;
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
