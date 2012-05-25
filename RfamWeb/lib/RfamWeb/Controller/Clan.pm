
# Clan.pm
# jt6 20100302 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Controller::Clan - controller to build the main Rfam clan page

=cut

package RfamWeb::Controller::Clan;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Rfam
clans.

Generates a B<tabbed page>.

$Id$

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::Section' => { -excludes => 'section' };

# set the name of the section
__PACKAGE__->config( SECTION => 'clan' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Extracts values from the parameters. Accepts "acc", "id" and "entry", in lieu
of having them as path components.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;
  
  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') and
       $c->req->param('output') eq 'xml' ) {
    $c->stash->{output_xml} = 1;
    $c->res->content_type('text/xml');
  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      '';

  if ( $tainted_entry ) {
    $c->log->debug( 'Clan::begin: got a tainted entry' )
      if $c->debug;
    ( $c->stash->{param_entry} ) = $tainted_entry =~ m/^([\w\._-]+)$/;
  }
}

#-------------------------------------------------------------------------------

=head2 clan : Chained('/') PathPart('clan') CaptureArgs(1)

Starting point of a chain handling clan-related data. Retrieves clan information
from the DB. This is the way in if the clan acc/ID is given as an argument.

=cut

sub clan : Chained( '/' )
           PathPart( 'clan' )
           CaptureArgs( 1 ) {
  my ( $this, $c, $entry_arg ) = @_;
  
  my $tainted_entry = $c->stash->{param_entry} ||
                      $entry_arg               ||
                      '';

  $c->log->debug( "Clan::clan: tainted_entry: |$tainted_entry|" )
    if $c->debug;
  
  my ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;

  unless ( defined $entry ) {
    $c->log->debug( 'Clan::begin: no valid Rfam clan accession or ID' )
      if $c->debug;

    $c->stash->{errorMsg} = 'No valid Rfam clan accession or ID';

    return;
  }
  
  # retrieve data for the clan
  $c->forward( 'get_data', [ $entry ] );
}

#-------------------------------------------------------------------------------

sub clan_page : Chained( 'clan' )
                PathPart( '' )
                Args( 0 ) {
  my ( $this, $c ) = @_;

  # there was a problem retrieving clan data
  unless ( $c->stash->{clan} ) {
    $c->log->debug( 'Clan::begin: problem retrieving clan data' )
      if $c->debug;

    $c->stash->{errorMsg} ||= 'We could not find the data for the Rfam clan ' . $c->stash->{acc} .'.';
  }

  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Clan::begin: emitting XML' ) if $c->debug;

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Clan::begin: there was an error: |' .  $c->stash->{errorMsg} . '|' )
        if $c->debug;

      $c->stash->{template} = 'rest/clan/error_xml.tt';

      return;
    }
    else {
      $c->stash->{template} = 'rest/clan/clan.tt'
    }
  
  }
  else {
    $c->log->debug( 'Clan::begin: emitting HTML; retrieving summary data' )
      if $c->debug;

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Clan::begin: there was an error: |' .  $c->stash->{errorMsg} . '|' )
        if $c->debug;

      $c->stash->{template} = 'components/blocks/clan/error.tt';

      return;

    }
  }

  $c->forward( 'get_summary_data' );

  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_clan : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_clan : Path( '/clan' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'clan::old_clan: redirecting to "clan"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/clan', $c->stash->{param_entry}, $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 structures : Local

Adds the structure-to-sequence-to-family mapping to the stash and hands off
to the template that generates a table showing that mapping.

=cut

sub structures : Chained( 'clan' )
                 PathPart( 'structures' )
                 Args( 0 ) {
  my ( $this, $c) = @_;

  $c->cache_page( 604800 );
  
  # all we need to do extra for this action is retrieve the mapping between
  # structure, sequence and family
  $c->forward( 'get_mapping' );

  $c->stash->{template} = 'components/blocks/clan/structureTab.tt';
}

#---------------------------------------

=head2 old_structures : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_clan_structures : Path( '/clan/structures' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Clan::old_clan_structures: redirecting to "structures"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/clan', $c->stash->{param_entry}, 'structures', 
                     $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves family data for the given entry. Accepts the entry ID or accession
as the first argument. Does not return any value but drops the L<ResultSet>
for the relevant row into the stash.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # check for a family
  my $rs = $c->model('RfamDB::Clans')
             ->search( [ { clan_acc => $entry },
                         { clan_id  => $entry } ] );
  
  my $clan = $rs->first if defined $rs;
  
  unless ( defined $clan ) {
    $c->log->debug( 'Clan::get_data: no row for that accession/ID' )
      if $c->debug;
  
    $c->stash->{errorMsg} = 'No valid Rfam clan accession or ID';

    return;
  }  

  $c->log->debug( 'Clan::get_data: got a clan' ) if $c->debug;

  $c->stash->{clan} = $clan;
  $c->stash->{acc}  = $clan->clan_acc;
  $c->stash->{entryType}  = 'C';

  # set up the pointers to the clan data in the stash
#  my @rs = $c->model('RfamDB::ClanMembership')
#             ->search( { auto_clan => $clan->auto_clan },
#                       { join      => [ 'auto_rfam' ],
#                         prefetch  => [ 'auto_rfam' ] } );
#  $c->stash->{clanMembers} = \@rs;
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the Rfam object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my $summary_data = {};

  # number of sequences in full alignment
#  $summary_data->{numSequences} = $c->stash->{rfam}->num_full;
  $summary_data->{numSequences} = 0;

  # number of structures known for the domain
  my $rs = $c->model('RfamDB::ClanMembership')
             ->search( { auto_clan => $c->stash->{clan}->auto_clan },
                       { join => 'pdb_rfam_reg' } );
  $summary_data->{numStructures} = $rs->count;

  # Number of species
#  $summary_data->{numSpecies} = $c->stash->{rfam}->number_of_species;
  $summary_data->{numSpecies} = 0;

  # number of interactions
  $summary_data->{numInt} = 0;

  $c->stash->{summaryData} = $summary_data;
}

#-------------------------------------------------------------------------------

=head2 get_mapping : Private

Retrieves the structure mappings for this clan. 

=cut

sub get_mapping : Private {
  my( $this, $c ) = @_;

   my @mapping = $c->model('RfamDB::ClanMembership')
                   ->search( { auto_clan => $c->stash->{clan}->auto_clan },
                             { select   => [ qw( rfamseq.rfamseq_acc
                                                 auto_rfam.rfam_id
                                                 auto_rfam.rfam_acc
                                                 pdb_rfam_reg.seq_start
                                                 pdb_rfam_reg.seq_end
                                                 pdb_rfam_reg.pdb_id
                                                 pdb_rfam_reg.chain
                                                 pdb_rfam_reg.pdb_res_start
                                                 pdb_rfam_reg.pdb_res_end ) ],
                               as       => [ qw( rfamseq_acc
                                                 rfam_id
                                                 rfam_acc
                                                 rfam_start_res
                                                 rfam_end_res
                                                 pdb_id
                                                 chain
                                                 pdb_start_res
                                                 pdb_end_res ) ],
                               prefetch => { pdb_rfam_reg => [ qw( auto_rfam
                                                                   rfamseq ) ] }
                             }
                           );

  $c->stash->{rfamMaps} = \@mapping;
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
