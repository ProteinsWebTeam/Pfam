
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

use strict;
use warnings;

use base 'RfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'clan' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

This is the guts of this controller. It's function is to extract
the Rfam clan ID or accession from the URL and get the row
in the clans table for that entry. Expects one of three parameters:

=over

=item acc

a valid Rfam accession

=item id

a valid Rfam accession

=item entry

either an ID or accession

=back

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;
  
  $c->cache_page( 604800 );
  
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
                      $entry_arg              ||
                      '';
  
  my ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;

  unless ( defined $entry ) {
    $c->log->debug( 'Clan::begin: no valid Rfam clan accession or ID' )
      if $c->debug;

    $c->stash->{errorMsg} = 'Invalid Rfam clan accession or ID';

    return;
  }
  
  #----------------------------------------

  # retrieve data for the clan
  $c->forward( 'get_data', [ $entry ] );
  
  #----------------------------------------

  # if we're outputting HTML, we're done here
  unless ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Clan::begin: emitting HTML' ) if $c->debug;
    return;
  }

  # from here on we're handling XML output
  $c->log->debug( 'Clan::begin: emitting XML' ) if $c->debug;

  # if there was an error...
  if ( $c->stash->{errorMsg} ) {
    $c->log->debug( 'Clan::begin: there was an error: |' .
                    $c->stash->{errorMsg} . '|' ) if $c->debug;
    $c->stash->{template} = 'rest/clan/error_xml.tt';
    return;
  }
}

#-------------------------------------------------------------------------------

=head2 structures : Local

Adds the structure-to-sequence-to-family mapping to the stash and hands off
to the template that generates a table showing that mapping.

=cut

sub structures : Local {
  my( $this, $c) = @_;

  $c->cache_page( 604800 );
  
  # all we need to do extra for this action is retrieve the mapping between
  # structure, sequence and family
  $c->forward( 'get_mapping' );

  $c->stash->{template} = 'components/blocks/clan/structureTab.tt';
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
  
  # if we're returning XML, we don't need the extra summary data etc.  
  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Clan::get_data: returning XML; NOT adding extra info' ) 
      if $c->debug;
    return;
  }
    
  # unless this request originates at the top level of the object hierarchy,
  # we don't need the extra summary data
  unless ( ref $this eq 'RfamWeb::Controller::Clan' ) {
    $c->log->debug( 'Clan::get_data: not the root Clan controller; NOT adding extra family info' )
      if $c->debug;
    return;
  }

  # finally, having decided that we need it...
  $c->forward( 'get_summary_data' );
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
