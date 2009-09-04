
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Clan.pm,v 1.24 2009-09-04 09:50:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Clan - controller for clan-related
sections of the site

=cut

package PfamWeb::Controller::Clan;

=head1 DESCRIPTION

This is intended to be the base class for everything related to clans
across the site. The L<begin|/"begin : Private"> method will try to
extract a clan ID or accession from the captured URL and then try to
load a Clan object from the model into the stash.

Generates a B<tabbed page>.

$Id: Clan.pm,v 1.24 2009-09-04 09:50:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

# define the name of the section...
__PACKAGE__->config( SECTION => 'clan' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract a clan ID or accession from the URL and gets the row
in the clan table for that entry.

=cut

sub begin : Private {
  my( $this, $c, $entry_arg ) = @_;

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';
  
  # although these next checks might fail and end up putting an error message
  # into the stash, we don't "return", because we might want to process the 
  # error message using a template that returns XML rather than simply HTML
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\.-]+)$/;
    $c->stash->{errorMsg} = 'Invalid Pfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Pfam family accession or ID specified';
  }
  
  # retrieve data for this entry
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 alignment : Local

Serves the clan alignment. We first try to retrieve the alignment from cache
before falling back to the DB.

=cut

sub alignment : Local {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Clan::alignment: serving clan alignment' );
  
  my $cacheKey = 'clanAlignment' . $c->stash->{acc};
  my $alignment = $c->cache->get( $cacheKey );
  
  if( defined $alignment ) {
    $c->log->debug( 'Clan::alignment: extracted clan alignment from cache' );
  } else {
    $c->log->debug( 'Clan::alignment: failed to extract clan alignment from '
                    . 'cache; going to DB' );
    
    # try to retrieve the appropriate row of the alignment table
    my $row = $c->model('PfamDB::ClanRelationship')
                ->find( $c->stash->{clan}->auto_clan );

    # see if we succeeded in at least retrieving it
    unless( defined $row->alignment ) {
      $c->stash->{errorMsg} = 'We were unable to retrieve the clan alignment for '
                              . $c->stash->{acc};
      return;
    }
    
    # and see if we can uncompress the alignment
    $alignment = Compress::Zlib::memGunzip( $row->alignment );
    unless( defined $alignment ) {
      $c->stash->{errorMsg} = 'We were unable to extract the clan alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->cache->set( $cacheKey, $alignment ) unless $ENV{NO_CACHE};    
  }
  
  $c->res->body( $alignment )

}

#-------------------------------------------------------------------------------

=head2 structures : Local

Adds the structure-to-sequence-to-family mapping to the stash and hands off
to the template that generates a table showing that mapping.

=cut

sub structures : Local {
  my( $this, $c) = @_;

  # all we need to do extra for this action is retrieve the mapping between
  # structure, sequence and family
  $c->forward( 'get_mapping' );

  $c->stash->{template} = 'components/blocks/clan/structureTab.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieve data for the clan, based on the detainted entry that's passed in as the
first argument.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  my $rs = $c->model('PfamDB::Clans')
             ->search( [ { clan_acc => $entry }, 
                         { clan_id  => $entry } ] );

  my $clan = $rs->first if defined $rs;
  
  unless ( defined $clan ) {
    $c->stash->{errorMsg} = 'No valid clan accession or ID';
    return;
  }
  
  $c->log->debug( 'Clan::get_data: got a clan' ) if $c->debug;
  $c->stash->{clan}      = $clan;
  $c->stash->{entryType} = 'C';
  $c->stash->{acc}       = $clan->clan_acc;
  
  # set up the pointers to the clan data in the stash
  my @rs = $c->model('PfamDB::ClanMembership')
             ->search( { auto_clan => $clan->auto_clan },
                       { join      => [ 'auto_pfama' ],
                         prefetch  => [ 'auto_pfama' ] } );
  $c->stash->{clanMembers} = \@rs;
  
  # only add extra data to the stash if we're actually going to use it later
  if ( not $c->stash->{output_xml} and 
       ref $this eq 'PfamWeb::Controller::Clan' ) {
    
    $c->forward( 'get_summary_data' );
    $c->forward( 'get_xrefs' );
  }
   
  $c->forward( 'get_diagram' );
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Populates the stash with data for the summary icons.

=cut

sub get_summary_data : Private {
  my( $this, $c ) = @_;

  my %summaryData;

  # get the auto number - should be quicker to use
  my $autoClan = $c->stash->{clan}->auto_clan;

  # number of sequences
  $summaryData{numSequences} = $c->stash->{clan}->number_sequences;

  # number of architectures
  $summaryData{numArchitectures} = $c->stash->{clan}->number_archs;

  # number of interactions
  my @interactions = $c->model('PfamDB::PfamaInteractions')
                       ->search( { 'clan_membership.auto_clan' => $c->stash->{clan}->auto_clan },
                                 { join     => [ qw( auto_pfama_a 
                                                     auto_pfama_b 
                                                     clan_membership ) ],
                                   select   => [ qw( auto_pfama_a.pfama_id 
                                                     auto_pfama_a.pfama_acc
                                                     auto_pfama_b.pfama_id 
                                                     auto_pfama_b.pfama_acc ) ],
                                   as       => [ qw( pfamA_A_id 
                                                     pfamA_A_acc
                                                     pfamA_B_id 
                                                     pfamA_B_acc ) ] } );
  # stash this for later...
  $c->stash->{interactions} = \@interactions;

  $summaryData{numInt} = scalar @interactions;
  $c->log->debug( 'Clan::get_summary_data: got ' . $summaryData{numInt} . ' interactions' );
  
  # number of structures known for the domain
  $summaryData{numStructures} = $c->stash->{clan}->number_structures;

  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { 'clan_members.auto_clan' => $c->stash->{clan}->auto_clan },
                            { join      => [ qw( auto_pdb clan_members ) ],
                              prefetch  => [ qw( auto_pdb ) ] } );

  my %pdb_unique = map {$_->pdb_id => 1} @mapping;
  $c->log->debug( 'Clan::get_summary_data: got ' . scalar(@mapping) . ' pdb mappings' );
  $c->stash->{pdbUnique} = \%pdb_unique;

  # number of species
  $summaryData{numSpecies} = $c->stash->{clan}->number_species;

  $c->stash->{summaryData} = \%summaryData;

}

#-------------------------------------------------------------------------------

=head2 get_xrefs : Private

Retrieves database cross-references. 

=cut

sub get_xrefs : Private {
  my( $this, $c ) = @_;

  my @refs = $c->model('PfamDB::ClanDatabaseLinks')
              ->search( { auto_clan => $c->stash->{clan}->auto_clan } );

  my %xRefs;
  foreach ( @refs ) {
    $xRefs{$_->db_id} = $_;
  }
  $c->stash->{xrefs} = \%xRefs;

}

#-------------------------------------------------------------------------------

=head2 get_mapping : Private

Retrieves the structure mappings for this clan. 

=cut

sub get_mapping : Private {
  my( $this, $c ) = @_;

   my @mapping = $c->model('PfamDB::ClanMembership')
                   ->search( { auto_clan => $c->stash->{clan}->auto_clan },
                             { select => [ qw( auto_pfamseq.pfamseq_id
                                               auto_pfama.pfama_id
                                               auto_pfama.pfama_acc
                                               pdb_pfama_reg.seq_start
                                               pdb_pfama_reg.seq_end
                                               auto_pdb.pdb_id
                                               pdb_pfama_reg.chain
                                               pdb_pfama_reg.pdb_res_start
                                               pdb_pfama_reg.pdb_res_end ) ],
                               as     => [ qw( pfamseq_id
                                               pfamA_id
                                               pfamA_acc
                                               pfam_start_res
                                               pfam_end_res
                                               pdb_id
                                               chain
                                               pdb_start_res
                                               pdb_end_res ) ],
                               join   => { pdb_pfama_reg => [ qw( auto_pfama
                                                                  auto_pfamseq
                                                                  auto_pdb ) ] }
                             }
                           );

  $c->stash->{pfamMaps} = \@mapping;
}

#-------------------------------------------------------------------------------

=head2 get_diagram : Private

Retrieves the two components of the clan relationship diagram from the DB, 
namely the image showing the relationship and the HTML snippet with the 
image map.

=cut

sub get_diagram : Private {
  my( $this, $c ) = @_;

  my $cacheKeyRoot = 'clanRelationship' . $c->stash->{acc};

  my $image = $c->cache->get( $cacheKeyRoot . 'image' );
  my $map   = $c->cache->get( $cacheKeyRoot . 'map' );

  if ( defined $image and defined $map ) { 
    $c->log->debug( 'Clan::Relationship::get_diagram: extracted image and map from cache' );
  }
  else {
    $c->log->debug( 'Clan::Relationship::get_diagram: failed to extract both image '
                    . 'and map from cache; going to DB' );

    my $row = $c->model('PfamDB::ClanAlignmentsAndRelationships')
                ->search( { auto_clan => $c->stash->{clan}->auto_clan }, {} )
                ->single;
  
    # check we actually retrieved a row
    unless ( defined $row and defined $row->relationship ) {
      $c->log->warn( 'Clan::get_diagram: could not retrieve the relationship data for '
                     . $c->stash->{acc} );
      return;
    }
  
    # we'll need both the image and the image map HTML uncompressed
    $image = Compress::Zlib::memGunzip( $row->relationship );
    unless ( defined $image ) {  
      $c->log->warn( 'Clan::get_diagram: could not extract the relationship image for '
                     . $c->stash->{acc} );
      return;
    }
  
    $map = Compress::Zlib::memGunzip( $row->image_map );
    unless ( defined $map ) {
      $c->log->warn( 'Clan::get-diagram: could not extract the relationship image map for '
                     . $c->stash->{acc} );
      return;
    }

    unless ( $ENV{NO_CACHE} ) {
      $c->cache->set( $cacheKeyRoot . 'image', $image );
      $c->cache->set( $cacheKeyRoot . 'map',   $map );
    }

  }

  $c->stash->{relationshipImage}    = $image;
  $c->stash->{relationshipImageMap} = $map;
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
