
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Clan.pm,v 1.26 2009-10-07 10:18:44 jt6 Exp $

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

$Id: Clan.pm,v 1.26 2009-10-07 10:18:44 jt6 Exp $

=cut

use utf8;
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

  # cache page for 12 hours
  $c->cache_page( 43200 );

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') ) {
    if ( $c->req->param('output') eq 'xml' ) {
      $c->stash->{output_xml} = 1;
      $c->res->content_type('text/xml');

      # enable CORS (see http://www.w3.org/wiki/CORS_Enabled)
      $c->res->header( 'Access-Control-Allow-Origin' => '*' );
    }
    elsif ( $c->req->param( 'output' ) eq 'pfamalyzer' ) {
      $c->stash->{output_pfamalyzer} = 1;
      $c->res->content_type('text/plain');
    }
    elsif ( $c->req->param( 'output' ) eq 'json' ) {
      $c->stash->{output_json} = 1;
      $c->res->content_type('application/json');
    }
  }

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';

  if ( $tainted_entry ) {
    $c->log->debug( 'Clan::begin: got a tainted entry' )
      if $c->debug;
    ( $c->stash->{param_entry} ) = $tainted_entry =~ m/^([\w\.-]+)$/
  }

}

#-------------------------------------------------------------------------------

=head2 clan : Chained

Main action in the chain. Takes the clan ID or accession and gets the row
in the clan table.

=cut

sub clan : Chained( '/' )
           PathPart( 'clan' )
           CaptureArgs( 1 ) {
  my ( $this, $c, $entry_arg ) = @_;

  my $tainted_entry = $c->stash->{param_entry} ||
                      $entry_arg               ||
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

  return unless defined $entry;

  # retrieve data for the entry
  $c->forward( 'get_data', [ $entry ] );
}

#---------------------------------------

=head2 old_clan : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_clan : Path( '/clan' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Clan::old_clan: redirecting to "clan"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/clan', $c->stash->{param_entry}, $c->req->params ) );
}


#-------------------------------------------------------------------------------

=head2 clan_end : Chained

Stub action forming the end of a chain that catches the "/clan" URL.

=cut

sub clan_end : Chained( 'clan' )
               PathPart( '' )
               Args( 0 ) {
  my ( $this, $c ) = @_;

  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Clan::clan_end: emitting XML' )
      if $c->debug;

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Clan::clan_end: there was an error: |' .
                      $c->stash->{errorMsg} . '|' ) if $c->debug;
      $c->stash->{template} = 'rest/clan/error_xml.tt';
      return;
    }
    else {
      $c->stash->{template} = 'rest/clan/entry_xml.tt';
    }
  }
  elsif( $c->stash->{output_pfamalyzer} ) {
    $c->log->debug( 'Clan::clan_end: emitting text for PfamAlyzer' )
      if $c->debug;

    $c->stash->{template} = 'rest/clan/entry_pfamalyzer.tt';
  }
  elsif( $c->stash->{output_json} ) {
    $c->log->debug( 'Clan::clan_end: emitting JSON' )
      if $c->debug;
      # if there was an error...
      if ( $c->stash->{errorMsg} ) {
        $c->log->debug( 'Clan::clan_end: there was an error: |' .
                        $c->stash->{errorMsg} . '|' ) if $c->debug;
        $c->stash->{template} = 'rest/clan/error_json.tt';
        return;
      }
      else {
        $c->stash->{template} = 'rest/clan/entry_json.tt';
      }

  }
  else {
    $c->log->debug( 'Clan::clan_end: emitting HTML' )
      if $c->debug;
  }
}

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 alignment : Local

Retrieves the HTML alignment and dumps it to the response. We first try to
extract the HTML from the cache or, if that fails, we retrieve it from the DB.

=cut

sub alignment : Chained( 'clan' )
                PathPart( 'alignment' )
                Args( 0 ) {
  my ( $this, $c ) = @_;

  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';

  my $cache_key = 'clanjtml' . $c->stash->{acc};

  my $jtml = $c->cache->get( $cache_key );
  if ( defined $jtml ) {
    $c->log->debug( 'Clan::alignment: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Clan::alignment: failed to extract HTML from cache; going to DB' )
      if $c->debug;

    # retrieve the HTML from the DB
    my $row = $c->model('PfamDB::ClanAlignmentAndRelationship')
                ->search( { clan_acc => $c->stash->{clan}->clan_acc } )
                ->single;

    # final check...
    unless ( defined $row->alignment ) {
      $c->log->debug( 'Clan::alignment: failed to retrieve JTML' )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress the row to get the raw HTML
    $jtml = Compress::Zlib::memGunzip( $row->alignment );
    unless ( defined $jtml ) {
      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Clan::alignment: retrieved HTML from DB' )
      if $c->debug;
    $c->cache->set( $cache_key, $jtml ) unless $ENV{NO_CACHE};
  }

  # stash the HTML
  $c->stash->{html_alignment} = $jtml;

}

#---------------------------------------

=head2 old_alignment : Local

Deprecated. Stub to redirect to the chained action.

=cut

sub old_alignment : Path( '/clan/alignment' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Clan::old_alignment redirecting to "alignment"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/clan', $c->stash->{param_entry}, '/alignment', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 structures : Local

Adds the structure-to-sequence-to-family mapping to the stash and hands off
to the template that generates a table showing that mapping.

=cut

sub structures : Chained( 'clan' )
                 PathPart( 'structures' )
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  # all we need to do extra for this action is retrieve the mapping between
  # structure, sequence and family
  $c->forward( 'get_mapping' );

  $c->stash->{template} = 'components/blocks/clan/structureTab.tt';
}

#---------------------------------------

=head2 old_structures : Local

Deprecated. Stub to redirect to the chained action.

=cut

sub old_structures : Path( '/clan/structures' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Clan::old_structures: redirecting to "structures"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/clan', $c->stash->{param_entry}, 'structures', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 desc : Chained

Returns the description of the clan. If the "output=pfamalyzer" parameter is
set, returns a longer string specifically for the PfamAlyzer applet.

=cut

sub desc : Chained( 'clan' )
           PathPart( 'desc' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  if ( defined $c->stash->{clan} ) {

    $c->res->content_type( 'text/plain' );

    if ( $c->stash->{output_pfamalyzer} ) {
      $c->res->body(
        $c->stash->{clan}->clan_acc         . "\t" .
        $c->stash->{clan}->clan_author      . "\t" .
        $c->stash->{clan}->clan_description . "\t" .
        $c->stash->{clan}->clan_comment
      );
    }
    else {
      $c->res->body( $c->stash->{clan}->clan_description );
    }
  }
  else {
    $c->res->status( 404 );
    $c->res->body( 'No such clan' );
  }
}

#-------------------------------------------------------------------------------

=head2 image : Local

Serves the image file for the clan relationship diagram.

=cut

sub image : Chained('clan')
            PathPart('relationship_image')
            Args(0) {
  my( $this, $c ) = @_;

  $c->log->debug( 'Clan::image: serving clan relationship image' )
    if $c->debug;

  if( defined $c->stash->{relationshipImage} ) {
    $c->res->content_type( 'image/gif' );
    $c->res->body( $c->stash->{relationshipImage} );
  } else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
  }

}

#---------------------------------------

=head2 old_image : Local

Deprecated. Stub to redirect to the chained action.

=cut

sub old_image : Path( '/clan/relationship/image' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Clan::old_image redirecting to "relationship_image"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/clan', $c->stash->{param_entry}, 'relationship_image', $c->req->params ) );
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

  my $rs = $c->model('PfamDB::Clan')
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
             ->search( { clan_acc => $clan->clan_acc },
                       { join      => [ 'pfama_acc' ],
                         prefetch  => [ 'pfama_acc' ] } );
  $c->stash->{clanMembers} = \@rs;

  my $subquery = $c->model('PfamDB::ClanMembership')
                   ->search( { clan_acc => $clan->clan_acc },
                             { join      => [ 'pfama_acc' ],
                               prefetch  => [ 'pfama_acc' ] } );
  # set up the pointers to the relationships between clan members
  my @rs2 = $c->model('PfamDB::Pfama2pfamaHhsearch')
              ->search(
                -and => [
                  { 'pfama_acc_1' => { -in => $subquery->get_column('pfama_acc')->as_query } },
                  { 'pfama_acc_2' => { -in => $subquery->get_column('pfama_acc')->as_query } }
                ],
              );
  $c->stash->{clanRelationships} = \@rs2;

  # only add extra data to the stash if we're actually going to use it later
  unless ( $c->stash->{output_xml} or $c->stash->{output_json} or
           $c->stash->{output_pfamalyzer} ) {
    if ( ref $this eq 'PfamWeb::Controller::Clan' ) {
      $c->forward( 'get_summary_data' );
      $c->forward( 'get_xrefs' );
    }
    $c->forward( 'get_diagram' );
  }
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Populates the stash with data for the summary icons.

=cut

sub get_summary_data : Private {
  my( $this, $c ) = @_;

  my %summaryData;


  # number of sequences
  $summaryData{numSequences} = $c->stash->{clan}->number_sequences;

  # number of architectures
  $summaryData{numArchitectures} = $c->stash->{clan}->number_archs;

  # number of interactions
  my @interactions = $c->model('PfamDB::PfamaInteractions')
                       ->search( [{'clan_membership_a.clan_acc' => $c->stash->{clan}->clan_acc },
                                  {'clan_membership_b.clan_acc' => $c->stash->{clan}->clan_acc }],
                                 { join     => [ qw( pfama_acc_a
                                                     pfama_acc_b
                                                     clan_membership_a
                                                     clan_membership_b) ],
                                   select   => [ qw( pfama_acc_a.pfama_id
                                                     pfama_acc_a.pfama_acc
                                                     pfama_acc_b.pfama_id
                                                     pfama_acc_b.pfama_acc ) ],
                                   as       => [ qw( pfamA_A_id
                                                     pfamA_A_acc
                                                     pfamA_B_id
                                                     pfamA_B_acc ) ],
                                  order_by  => [qw(pfama_acc_a.pfama_id pfama_acc_b.pfama_id)] } );

  # stash this for later...
  $c->stash->{interactions} = \@interactions;

  $summaryData{numInt} = scalar @interactions;
  $c->log->debug( 'Clan::get_summary_data: got ' . $summaryData{numInt} . ' interactions' )
    if $c->debug;

  # number of structures known for the domain
  $summaryData{numStructures} = $c->stash->{clan}->number_structures;

  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { 'clan_members.clan_acc' => $c->stash->{clan}->clan_acc },
                            { join      => [ qw( clan_members pdb_id pfama_acc ) ],
                              prefetch => [ qw( pdb_id pfama_acc) ] } );

  my %pdb_unique = map {$_->pdb_id->pdb_id => 1} @mapping;
  $c->log->debug( 'Clan::get_summary_data: got ' . scalar(@mapping) . ' pdb mappings' )
    if $c->debug;
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
              ->search( { clan_acc => $c->stash->{clan}->clan_acc} );

  my %xRefs;
  foreach ( @refs ) {
    push @{ $xRefs{$_->db_id} }, $_;
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
                   ->search( { clan_acc => $c->stash->{clan}->clan_acc },
                             { select => [ qw( pfamseq_acc.pfamseq_id
                                               pfama_acc.pfama_id
                                               pfama_acc.pfama_acc
                                               pdb_pfama_reg.seq_start
                                               pdb_pfama_reg.seq_end
                                               pdb_pfama_reg.pdb_id
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
                               join   => { pdb_pfama_reg => [ qw( pfama_acc
                                                                  pfamseq_acc ) ] }
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
    $c->log->debug( 'Clan::Relationship::get_diagram: extracted image and map from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Clan::Relationship::get_diagram: failed to extract both image '
                    . 'and map from cache; going to DB' )
      if $c->debug;

    my $row = $c->model('PfamDB::ClanAlignmentAndRelationship')
                ->search( { clan_acc => $c->stash->{clan}->clan_acc }, {} )
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
      $c->log->warn( 'Clan::get_diagram: could not extract the relationship image map for '
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
