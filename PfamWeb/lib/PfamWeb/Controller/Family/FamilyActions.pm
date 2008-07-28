
# FamilyActions.pm
# jt6 20070418 WTSI
#
# $Id: FamilyActions.pm,v 1.3 2008-07-28 13:54:18 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::FamilyActions - controller with various actions
related to families.

=cut

package PfamWeb::Controller::Family::FamilyActions;

=head1 DESCRIPTION

This controller holds a collection of actions that are related to Pfam-A
families.

$Id: FamilyActions.pm,v 1.3 2008-07-28 13:54:18 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 hmm : Private

Serve the contents of the HMM for a Pfam-A entry from the database. Requires 
the "mode" parameter to be set either to "ls" or "fs".

=cut

sub hmm : Path( '/family/hmm' ) {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # find out which HMM we want...
  my( $mode ) = $c->req->param( 'mode' ) =~ /^(fs|ls)$/;
  if( not defined $mode ) {
    $c->stash->{errorMsg} = 'There was no HMM type specified. &quot;mode&quot; '
                            . 'should be either &quot;ls&quot; or &quot;fs&quot;';
    return;
  }

  my $cacheKey = 'hmm' . $c->stash->{acc} . $mode;
  my $hmm      = $c->cache->get( $cacheKey );

  if( defined $hmm ) {
    $c->log->debug( 'Family::gethmm: extracted HMM from cache' )
      if $c->debug;
  } else {
    $c->log->debug( 'Family::gethmm: failed to extract HMM from cache; going to DB' )
      if $c->debug;
     
    # retrieve the HMM from the appropriate table
    if( $mode eq 'ls' ) {
      my $rs = $c->model('PfamDB::PfamA_HMM_ls')
                 ->find( $c->stash->{pfam}->auto_pfamA );
  
      unless( $rs ) {
        $c->stash->{errorMsg} = "We could not find the &quot;ls&quot; HMM file for " 
                                . $c->stash->{acc};
        return;
      }
  
      $hmm = $rs->hmm_ls;
  
    } elsif( $mode eq 'fs' ) {
      my $rs = $c->model('PfamDB::PfamA_HMM_fs')
                 ->find( $c->stash->{pfam}->auto_pfamA );
  
      unless( $rs ) {
        $c->stash->{errorMsg} = "We could not find the &quot;fs&quot; HMM file for " 
                                . $c->stash->{acc};
        return;
      }    
  
      $hmm = $rs->hmm_fs;
    }  

    unless( $hmm ) {
      $c->stash->{errorMsg} = "We could not find the $mode HMM file for " 
                              . $c->stash->{acc};
      return;
    }

    # cache the raw HMM
    $c->cache->set( $cacheKey, $hmm ) unless $ENV{NO_CACHE};
  }

  # build a name for the file that will be downloaded
  my $filename = $c->stash->{pfam}->pfamA_id."_$mode.hmm";

  # set the response headers
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # at this point we should have the HMM in hand, so spit it out to the 
  # response and we're done
  $c->res->body( $hmm );

  # the RenderView action on the end method in Section.pm will spot that there's
  # content in the response and return without trying to render any templates
}

#-------------------------------------------------------------------------------

=head2 logo : Local

Returns the HMM logo image for this family.

=cut

sub logo : Path( '/family/logo' ) {
  my ( $this, $c ) = @_;
  
  return unless defined $c->stash->{pfam};

  my $cache_key = 'logo' . $c->stash->{acc};
  my $logo = $c->cache->get( $cache_key );
  
  if ( defined $logo ) {
    $c->log->debug( 'Family::getlogo: extracted logo from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::getlogo: failed to extract logo from cache; going to DB' )
      if $c->debug;
    
    my $rs = $c->model('PfamDB::PfamA_HMM_logo')
               ->find( $c->stash->{pfam}->auto_pfamA );

    $logo = $rs->logo;

    unless ( $logo ) {
      $c->log->debug( 'Family::getlogo: failed to retrieve logo from DB' )
        if $c->debug;
      $c->stash->{errorMsg} = 'We could not find the HMM logo for ' 
                              . $c->stash->{acc};
      return;
    }
  
    # cache the LOGO
    $c->cache->set( $cache_key, $logo ) unless $ENV{NO_CACHE};
  }

  $c->res->content_type( 'image/png' );
  $c->res->body( $logo );
  
}

#-------------------------------------------------------------------------------

=head2 id : Local

Returns the ID for this family as a single, plain text string. Returns 404 if
there's no family to work on.

=cut

sub id : Path( '/family/id' ) {
  my ( $this, $c ) = @_;
  
  if ( defined $c->stash->{pfam} ) {    
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfamA_id );
    }
  }
  else { 
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#-------------------------------------------------------------------------------

=head2 acc : Local

Returns the accession for this family as a single, plain text string. Returns 
404 if there's no family to work on.

=cut

sub acc : Path( '/family/acc' ) {
  my ( $this, $c ) = @_;
  
  if ( defined $c->stash->{pfam} ) {    
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfamA_acc );
    }
  }
  else { 
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
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
