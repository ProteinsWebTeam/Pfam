
# FamilyActions.pm
# jt6 20070418 WTSI
#
# $Id: FamilyActions.pm,v 1.6 2009-10-22 10:10:08 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::FamilyActions - controller with various actions
related to families.

=cut

package PfamWeb::Controller::Family;

=head1 DESCRIPTION

This controller holds a collection of actions that are related to Pfam-A
families.

$Id: FamilyActions.pm,v 1.6 2009-10-22 10:10:08 jt6 Exp $

=cut

use strict;
use warnings;

use Image::Size;

# use base 'PfamWeb::Controller::Family';
use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 hmm : Chained

Serve the contents of the HMM for a Pfam-A entry from the database. Requires 
the "mode" parameter to be set either to "ls" or "fs".

=cut

sub hmm : Chained( 'family' )
          PathPart( 'hmm' ) 
          Args( 0 ) {
  my ( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  my $cacheKey = 'hmm' . $c->stash->{acc};
  my $hmm      = $c->cache->get( $cacheKey );

  if ( defined $hmm ) {
    $c->log->debug( 'Family::gethmm: extracted HMM from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::gethmm: failed to extract HMM from cache; going to DB' )
      if $c->debug;
     
    my $rs = $c->model('PfamDB::PfamaHmm')
               ->find( $c->stash->{pfam}->auto_pfama );

    unless ( $rs ) {
      $c->log->warn( 'Family::FamilyActions::hmm: failed to find row' )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not find the HMM for ' 
                              . $c->stash->{acc};
      $c->res->status( 500 );
      return;
    }

    $hmm = $rs->hmm;
    
    unless ( $hmm ) {
      $c->log->warn( 'Family::FamilyActions::hmm: failed to retrieve HMM from row' )
        if $c->debug;
        
      $c->stash->{errorMsg} = 'We could not retrieve the HMM for ' 
                              . $c->stash->{acc};
                              
      $c->res->status( 500 );
      return;
    }

    # cache the raw HMM
    $c->cache->set( $cacheKey, $hmm ) unless $ENV{NO_CACHE};
  }

  # build a name for the file that will be downloaded
  my $filename = $c->stash->{pfam}->pfama_id . '.hmm';

  # set the response headers
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # at this point we should have the HMM in hand, so spit it out to the 
  # response and we're done
  $c->res->body( $hmm );

  # the RenderView action on the end method in Section.pm will spot that there's
  # content in the response and return without trying to render any templates
}

#---------------------------------------

=head2 old_hmm : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_hmm : Path( '/family/hmm' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_hmm: redirecting to "hmm"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} . '/hmm' ) );
}

#-------------------------------------------------------------------------------

=head2 logo : Chained

Returns the HMM logo image for this family. This is subject to a check on the 
size of the image and the type of browser that is requesting it. Since there
are known problems with firefox and large PNGs, we don't return the image 
immediately in that case. The template takes care of showing a bit of text
and providing a link to load the image anyway.

=cut

sub logo : Chained( 'family' )
           PathPart( 'logo' )
           Args( 0 ) {
  my ( $this, $c ) = @_;
  
  my $logo = $c->forward( 'get_logo' );    
  return if $c->stash->{errorMsg};
  
  my ( $logo_x, $logo_y ) = imgsize( \$logo );

  if ( defined $logo_x and defined $logo_y and  
       ( $logo_x > $this->{image_size_limit} or
         $logo_y > $this->{image_size_limit} ) and
           $c->req->user_agent =~ m/Gecko/ and
       not $c->req->user_agent =~ m/WebKit/ ) {
  
    $c->log->debug( 'Family::FamilyActions::logo: browser is Gecko-based and image is large'
                    . " ($logo_x x $logo_y)" )
      if $c->debug;

    $c->stash->{logo_x} = $logo_x;
    $c->stash->{logo_y} = $logo_y;
    
    $c->stash->{large_logo} = 1;
  }

  $c->stash->{template} = 'components/logo.tt';
}

#---------------------------------------

=head2 old_logo : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_logo : Path( '/family/logo' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family::FamilyActions::old_logo: redirecting to "logo"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} . '/logo' ) );
}

#-------------------------------------------------------------------------------

=head2 logo_image : Chained

Returns the HMM logo image for this family.

=cut

sub logo_image : Chained( 'family' )
                 PathPart( 'logo_image' ) 
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::FamilyActions::logo_image: returning raw image' )
    if $c->debug;
  
  my $logo = $c->forward( 'get_logo' );
    
  return if $c->stash->{errorMsg};
  
  if ( $c->req->param('dl') ) {
    my $filename = $c->stash->{acc} . '_logo.png';

    $c->log->debug( 'Family::FamilyActions::logo_image: forcing download of logo as '
                    . $filename ) if $c->debug;
    
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $logo );
}

#---------------------------------------

=head2 old_logo_image : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_logo_image : Path( '/family/logo_image' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_logo_image: redirecting to "logo_image"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} . '/logo_image' ) );
}

#-------------------------------------------------------------------------------

=head2 id : Chained

Returns the ID for this family as a single, plain text string. Returns 404 if
there's no family to work on.

=cut

sub id : Chained( 'family' )
         PathPart( 'id' )
         Args( 0 ) {
  my ( $this, $c ) = @_;
  
  if ( defined $c->stash->{pfam} ) {    
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfama_id );
    }
  }
  else { 
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#---------------------------------------

=head2 old_id : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_id : Path( '/family/id' ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family:FamilyActions::old_id: redirecting to "id"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} . '/id' ) );
}

#-------------------------------------------------------------------------------

=head2 acc : Chained

Returns the accession for this family as a single, plain text string. Returns 
404 if there's no family to work on.

=cut

sub acc : Chained( 'family' )
          PathPart( 'acc' )
          Args( 0 ) {
  my ( $this, $c ) = @_;
  
  if ( defined $c->stash->{pfam} ) {    
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfama_acc );
    }
  }
  else { 
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#---------------------------------------

=head2 old_acc : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_acc : Path( '/family/acc' ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family:FamilyActions::old_acc: redirecting to "acc"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} . '/acc' ) );
}

#-------------------------------------------------------------------------------

=head2 desc : Chained

Returns the description string for a family. If the "output_pfamalyzer"
parameter is set, the output returns more family information.

=cut

sub desc : Chained( 'family' )
           PathPart( 'desc' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  if ( defined $c->stash->{pfam} ) {

    $c->res->content_type( 'text/plain' );

    if ( $c->stash->{output_pfamalyzer} ) {
      $c->res->body(
        $c->stash->{pfam}->pfama_acc   . "\t" .
        $c->stash->{pfam}->author      . "\t" .
        $c->stash->{pfam}->type        . "\t" .
        $c->stash->{pfam}->num_seed    . "\t" .
        $c->stash->{pfam}->num_full    . "\t" .
        $c->stash->{pfam}->description . "\t" .
        $c->stash->{pfam}->comment
      );
    }
    else {
      $c->res->body( $c->stash->{pfam}->description );
    }
  }
  else {
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#-------------------------------------------------------------------------------

=head2 structures : Chained

Retrieves the list of PDB entries for this family. If a PDB ID is specified,
the method also retrieves the row of the "pdb" table for that entry.

=cut

sub structures : Chained( 'family' )
                 PathPart( 'structures' )
								 Args( 0 ) {
  my ( $this, $c ) = @_;

  # see if we were handed a valid PDB ID and, if so, just stash it
  if ( defined $c->req->param('pdbId') and
       $c->req->param('pdbId') =~ /^(\d\w{3})$/ ) {

    $c->log->debug( "Family::Structures::structures: got PDB ID: |$1|" )
      if $c->debug;

    $c->stash->{pdb_id} = $1;
  }

  # retrieve the PDB entries for this family
  my @regions;
  if ( defined $c->stash->{pfam}->auto_pfama ) {
    $c->log->debug( 'Family::Structures::structures: got an auto_pfama: '
                    . $c->stash->{pfam}->auto_pfama ) if $c->debug;
    @regions = $c->model('PfamDB::PdbPfamaReg')
                 ->search( { 'me.auto_pfama' => $c->stash->{pfam}->auto_pfama },
                           { prefetch => [ qw( pdb_id pdb_image auto_pfama ) ] } );
    $c->log->debug( 'Family::Structures::structures: got ' 
                    . scalar @regions . ' regions' ) if $c->debug;
  }

  # don't render the template unless we need to
  unless ( scalar @regions ) {
    $c->log->debug( 'Family::Structures::structures: no structure image; not rendering template' )
      if $c->debug;
    $c->res->status( 204 );
    return;
  }

  my $pdb_unique = {};
  my $colours = {};
  foreach my $region ( @regions ) {
    my $id = $region->pdb_id->pdb_id;
    $pdb_unique->{$id} = $region;
    $colours->{$id}->{$region->hex_colour} = $region->auto_pfama->pfama_id;
  }

  $c->stash->{pdb_unique} = $pdb_unique;
  $c->stash->{colours}    = $colours;

  # my %pdb_unique = map{ $_->pdb_id->pdb_id => $_ } @regions;
  # $c->stash->{pdb_unique} = \%pdb_unique;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = 'components/blocks/family/familyStructures.tt';

  # cache the template output for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_structures : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_structures : Path( '/family/structures' ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::Structures::old_structures: redirecting to "structures"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} . '/structures' ) );
}

#-------------------------------------------------------------------------------

=head2 mapping : Chained

Renders a table showing the mapping between Pfam family, UniProt region and
PDB residues.

=cut

sub mapping : Chained( 'family' )
              PathPart( 'mapping' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::FamilyActions::mapping: acc: |'
                  . $c->stash->{acc}  . '|' .  $c->stash->{entryType}. '|' )
    if $c->debug;

  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama },
                            { join       => [ qw( pdb_id auto_pfamseq ) ],
                              columns    => [ qw( auto_pfamseq.pfamseq_id
                                                  seq_start
                                                  seq_end
                                                  pdb_id.pdb_id
                                                  chain
                                                  pdb_res_start
                                                  pdb_res_end ) ] } );

  $c->stash->{pfamMaps} = \@mapping;
  $c->log->debug( 'Family::FamilyActions::mapping: found |' . scalar @mapping . '| rows' )
    if $c->debug;

  unless ( scalar @mapping ) {
    $c->log->debug( 'Family::FamilyActions::mapping: no rows; returning 204' )
      if $c->debug;
      
    $c->res->status( 204 );
    
    return;
  }
  
  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Family::FamilyActions::mapping: emitting XML' ) if $c->debug;
    $c->stash->{template} = 'rest/family/structures_xml.tt';
  }
  else {
    $c->log->debug( 'Family::FamilyActions::mapping: emitting HTML' ) if $c->debug;
    $c->stash->{template} = 'components/blocks/family/structureTab.tt';
  }

  # cache the template output for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_mapping : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_mapping : Path( '/family/structures/mapping' ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::FamilyActions::old_mapping: redirecting to "mapping"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} . '/mapping' ) );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_logo : Private

Retrieves the HMM logo for this family, either from cache or the DB. Returns
the logo, if found.

=cut

sub get_logo : Private {
  my ( $this, $c ) = @_;
  
  my $cache_key = 'logo' . $c->stash->{acc};
  my $logo = $c->cache->get( $cache_key );
  
  if ( defined $logo ) {
    $c->log->debug( 'Family::FamilyActions::logo: extracted logo from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::FamilyActions::logo: failed to extract logo from cache; going to DB' )
      if $c->debug;
    
#    my $rs = $c->model('PfamDB::PfamaHmm')
#               ->find( $c->stash->{pfam}->auto_pfama );
#    $logo = $rs->logo;
    if ( defined $c->stash->{pfam} and 
         defined $c->stash->{pfam}->pfama_hmms ) {
      $logo = $c->stash->{pfam}->pfama_hmms->logo;
    }

    unless ( defined $logo ) {
      $c->log->debug( 'Family::FamilyActions::logo: failed to retrieve logo from DB' )
        if $c->debug;

      $c->res->status( 204 );

      return;
    }
  
    # cache the LOGO
    $c->cache->set( $cache_key, $logo ) unless $ENV{NO_CACHE};
  }

  return $logo;  
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
