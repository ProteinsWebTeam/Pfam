
# Jump.pm
# jt6 20060807 WTSI
#
# $Id: Jump.pm,v 1.9 2008-01-14 17:03:12 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Jump - guess the type of entry that the user wants to see

=cut

package PfamWeb::Controller::Search::Jump;

=head1 DESCRIPTION

$Id: Jump.pm,v 1.9 2008-01-14 17:03:12 jt6 Exp $

=cut

use strict;
use warnings;

use URI;

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 jump : Path

Tries to find the URL for the specified entry accession or ID. If the 
"entryType" parameter is specified and if the value is found in the 
configuration, we look only for that particualr type of entry. If no "entryType"
is specified, we'll look for any type.

=cut

sub jump : Path {
  my( $this, $c ) = @_;
  
  # de-taint the entry ID or accession
  my $entry = '';
  ( $entry ) = $c->req->param('entry') =~ /^([\w\-_\s()\.]+)$/;
  $c->log->debug( "Search::Jump::jump: called with entry |$entry|" );

  # strip off leading and trailing whitespace
  $entry =~ s/^\s*(.*?)\s*$/$1/;
  $c->log->debug( "Search::Jump::jump: trimmed entry to |$entry|" );

  # bail immediately if there's no valid entry given
  unless( $entry ) {
    $c->stash->{error} = 'No valid accession or ID';
    return;
  }

  # now we know we have an entry. See if the caller specified the type of that
  # entry. If it did, we don't bother trying to guess the type, but just
  # redirect straight to the appropriate URL 
  my $entry_type;
  if( $c->req->param('type') ) {
    $entry_type = $this->{jumpTargets}->{ $c->req->param('type') };
    $c->log->debug( 'Search::Jump::jump: looking for entry type: |'
                    . ( $entry_type || '' ) . '|' )
  }
  
  # let's guess !
  my $action = $c->forward( 'guess', [ $entry, $entry_type ] );

  if( $action ) {
    $c->log->debug( "Search::Jump::jump: we've made a guess; redirecting to |$action|" );
    $c->stash->{url} = $c->uri_for( "/$action", { entry => $entry } );
  } else {
    $c->log->debug( "Search::Jump::jump: couldn't guess entry type..." );
    $c->stash->{error} = 'Entry not found';
  }
  
}

#-------------------------------------------------------------------------------

=head2 end : Private

Return either a text/plain response with the guessed URL in the body, or an
error response (status 400) with the error message in the body.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  $c->res->content_type( 'text/plain' );
  
  if( $c->stash->{error} ) {
    $c->res->body( $c->stash->{error} );
    $c->res->status( 400 );
  } else {
    $c->res->body( $c->stash->{url} );
  }
    
}

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
                      structure => 'guess_structure',
                      proteome  => 'guess_proteome' );

  my @available_actions = qw( guess_family
                              guess_sequence
                              guess_clan
                              guess_structure
                              guess_proteome );

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
    
    return 'family' if $c->model('PfamDB::Pfam')
                         ->find( { pfamA_acc => $1 } );
  } else {
    # a Pfam family ID ?
    return 'family' if $c->model('PfamDB::Pfam')
                         ->find( { pfamA_id => $entry } );
  }

  # see if this could be a dead family
  my @rs = $c->model('PfamDB::Dead_families')
             ->search( [ { pfamA_acc => $entry },
                         { pfamA_id  => $entry } ] );
  return 'family' if scalar @rs;
  
  # or a Pfam-B accession ?
  if( $entry =~ m/^(PB\d{6})$/ ) {

    return 'pfamb' if $c->model('PfamDB::PfamB')
                        ->find( { pfamB_acc => $1 } );      
  }
    
  # maybe a Pfam-B ID ?
  if( $entry =~ m/^(Pfam-B_\d+)$/ ) {

    return 'pfamb' if $c->model('PfamDB::PfamB')
                        ->find( { pfamB_id => $1 } );
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
  if( $entry =~ m/^([AOPQ]\d[A-Z0-9]{3}\d)(\.\d+)?$/i ) {
  
    return 'protein' if $c->model('PfamDB::Pfamseq')
                          ->find( { pfamseq_acc => $1 } );
  }

  # see if it's a protein sequence ID (e.g. CANX_CHICK)
  if( $entry =~ m/^([A-Z0-9]+\_[A-Z0-9]+)$/ ) {
  
    return 'protein' if $c->model('PfamDB::Pfamseq')
                          ->find( { pfamseq_id => $1 } );
  }
  
  # see if it's a secondary accession; a bit gnarly...
  return 'protein' if $c->model('PfamDB::Secondary_pfamseq_acc')
                        ->find( { secondary_acc => $1 },
                                { join =>     [ qw( pfamseq ) ],
                                  prefetch => [ qw( pfamseq ) ] } );
  
  # an NCBI GI number ?
  if( $entry =~ m/^(gi)?(\d+)$/ ) {
  
    return 'ncbiseq' if $c->model('PfamDB::Ncbi_seq')
                          ->find( { gi => $2 } );
  }
  
  # an NCBI secondary accession ?
  my @rs = $c->model('PfamDB::Ncbi_seq')
             ->search( { secondary_acc => { 'like', "$entry%" } } );
  return 'ncbiseq' if scalar @rs;

  # a metaseq ID or accession ?
  @rs = $c->model('PfamDB::Metaseq')
          ->search( [ { metaseq_acc => $entry }, 
                      { metaseq_id  => $entry } ] );
  return 'metaseq' if scalar @rs;

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
    
  # a proteome ID ?
  return 'proteome' if $c->model('PfamDB::Proteome_species')
                         ->find( { species => $entry } );
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
