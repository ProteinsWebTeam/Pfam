
# Jump.pm
# jt6 20080314 WTSI
#
# $Id: Jump.pm,v 1.3 2009-01-06 11:53:54 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Jump - guess the type of entry that the user wants to see

=cut

package RfamWeb::Controller::Jump;

=head1 DESCRIPTION

$Id: Jump.pm,v 1.3 2009-01-06 11:53:54 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::Search::Jump';
    
# these are the available guesses, as a hash and an array, for convenience
my %action_types= (
  family   => 'guess_family',
  clan     => 'guess_clan',
  genome   => 'guess_genome',
  sequence => 'guess_sequence',
);
my @available_actions = qw( 
  guess_family
  guess_clan
  guess_genome
  guess_sequence
);

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

Look for an Rfam family with the specified accession or ID.

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
               ->find( { rfam_acc => $entry },
                       { columns => [ 'rfam_acc' ] } );
  # (We define the list of columns specifically, so that we avoid a mismatch 
  # between the table structures for "dead_family" between the live and dev
  # databases.)
  
  return 'family' if defined $dead;
}

#-------------------------------------------------------------------------------

=head2 guess_clan : Private

Look for an Rfam clan with the specified accession or ID.

=cut

sub guess_clan : Private {
  my ( $this, $c, $entry ) = @_;

  $c->log->debug( 'Search::Jump::guess_clan: looking for a clan...' )
    if $c->debug;
  
  my @rs = $c->model('RfamDB::Clans')
             ->search( [ { clan_acc => $entry },
                         { clan_id  => $entry } ] );

  return 'clan' if scalar @rs;
}

#-------------------------------------------------------------------------------

=head2 guess_sequence : Private

Look for a sequence entry.

=cut

sub guess_sequence : Private {
  my ( $this, $c, $entry ) = @_;

  $c->log->debug( 'Search::Jump::guess_sequence: looking for a sequence...' )
    if $c->debug;
  
  my $seq = $c->model('RfamDB::Rfamseq')
              ->find( { rfamseq_acc => $entry } );

  return 'sequence' if defined $seq;
}

#-------------------------------------------------------------------------------

=head2 guess_genome : Private

Look for a genome entry.

=cut

sub guess_genome : Private {
  my ( $this, $c, $entry ) = @_;

  $c->log->debug( 'Search::Jump::guess_genome: looking for a genome...' )
    if $c->debug;
  
  my @rs = $c->model('RfamDB::GenomeEntry')
             ->search( [ { genome_acc => $entry },
                         { ensembl_id => $entry },
                         { ncbi_id    => $entry } ] );

  return 'genome' if scalar @rs;
}

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
