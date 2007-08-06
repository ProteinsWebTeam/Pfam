
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Clan.pm,v 1.15 2007-08-06 16:09:19 jt6 Exp $

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

$Id: Clan.pm,v 1.15 2007-08-06 16:09:19 jt6 Exp $

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
  my( $this, $c ) = @_;

  my $co;
  if( defined $c->req->param( 'acc' ) ) {

    $c->req->param( 'acc' ) =~ m/^(CL\d{4})$/i;
    $c->log->debug( "Clan::begin: found accession |$1|" );
  
    $co = $c->model('PfamDB::Clans')->find( { clan_acc => $1 } )
      if defined $1;

  } elsif( defined $c->req->param('id') ) {

    $c->log->debug( 'Clan::begin: found param |' . $c->req->param('id') . '|' );
    $c->req->param( 'id' ) =~ m/^([\w-]+)$/;
    $c->log->debug( "Clan::begin: found ID |$1|" );
    $co = $c->model('PfamDB::Clans')->find( { clan_id => $1 } )
      if defined $1;

  } elsif( defined $c->req->param('entry') ) {

    # see if this is really an accession...
    if( $c->req->param('entry') =~ /^(CL\d{4})$/i ) {
  
      $c->log->debug( "Clan::begin: looks like a clan accession ($1); redirecting" );
      $c->res->redirect( $c->uri_for( '/clan', { acc => $1 } ) );
  
    } else {
  
      # no; assume it's an ID and see what happens...
      $c->log->debug( "Clan::begin: doesn't look like a clan accession ($1); redirecting with an ID" );
      $c->res->redirect( $c->uri_for( '/clan', { id => $c->req->param( "entry" ) } ) );
    }

    return 1;
  }

  # we're done here unless there's an entry specified
  unless( defined $co ) {

    # de-taint the accession or ID
    my $input = $c->req->param('acc')
      || $c->req->param('id')
      || $c->req->param('entry');
    $input =~ s/^(\w+)/$1/;
  
    # see if this was an internal link and, if so, report it
    my $b = $c->req->base;
    if( $c->req->referer =~ /^$b/ ) {
  
      # this means that the link that got us here was somewhere within
      # the Pfam site and that the accession or ID which it specified
      # doesn't actually exist in the DB
  
      # report the error as a broken internal link
      $c->error( 'Found a broken internal link; no valid clan accession or ID '
           . '("$input") in "' . $c->req->referer . '"' );
      $c->forward( '/reportError' );
  
      # now reset the errors array so that we can add the message for
      # public consumption
      $c->clear_errors;
  
    }
  
    # the message that we'll show to the user
    $c->stash->{errorMsg} = 'No valid clan accession or ID';
  
    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn( 'Clan::begin: no valid clan ID or accession' );
  
    return;
  }

  $c->log->debug( 'Clan::begin: successfully retrieved a clan object' );

  # set up the pointers to the clan data in the stash
  $c->stash->{entryType} = 'C';
  $c->stash->{acc} = $co->clan_acc;
  my @rs = $c->model('PfamDB::Clan_membership')
             ->search( { auto_clan => $co->auto_clan },
                       { join      => [ qw( pfam ) ],
                         prefetch  => [ qw( pfam ) ] } );
  $c->stash->{clanMembers} = \@rs;

  $c->stash->{clan} = $co;

  #----------------------------------------
  # populate the stash with other data

  # if this request originates at the top level of the object hierarchy,
  # i.e. if it's a call on the "default" method of the Clan object,
  # then we'll need to do a few extra things
  if( ref $this eq 'PfamWeb::Controller::Clan' ) {
    $c->forward( 'getSummaryData' );
    $c->forward( 'getXrefs' );        
  }

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

    $c->cache->set( $cacheKey, $alignment );    
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
  $c->forward( 'getMapping' );

  $c->stash->{template} = 'components/blocks/clan/structureTab.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _getSummaryData : Private

Populates the stash with data for the summary icons.

=cut

sub getSummaryData : Private {
  my( $this, $c ) = @_;

  my %summaryData;

  # get the auto number - should be quicker to use
  my $autoClan = $c->stash->{clan}->auto_clan;

  # number of sequences
  $summaryData{numSequences} = $c->stash->{clan}->number_sequences;

  # number of architectures
  $summaryData{numArchitectures} = $c->stash->{clan}->number_archs;

  # number of interactions
  $summaryData{numInt} = 0;

  # number of structures known for the domain
  $summaryData{numStructures} = $c->stash->{clan}->number_structures;

  my @mapping = $c->model('PfamDB::Pdb_pfamA_reg')
                  ->search( { 'clanMembers.auto_clan' =>  $c->stash->{clan}->auto_clan },
                            { join      => [ qw( pdb clanMembers ) ],
                              prefetch  => [ qw( pdb ) ] } );

  my %pdb_unique = map {$_->pdb_id => 1} @mapping;
  $c->log->debug( 'Clan::_getSummaryData: got ' . scalar(@mapping) . ' pdb mappings' );
  $c->stash->{pdbUnique} = \%pdb_unique;

  #Number of species
  $summaryData{numSpecies} = $c->stash->{clan}->number_species;

  $c->stash->{summaryData} = \%summaryData;

}

#-------------------------------------------------------------------------------

=head2 _getXrefs : Private

Retrieves database cross-references. 

=cut

sub getXrefs : Private {
  my( $this, $c ) = @_;

  my @refs = $c->model('PfamDB::Clan_database_links')
              ->search( { auto_clan => $c->stash->{clan}->auto_clan } );

  my %xRefs;
  foreach ( @refs ) {
    $xRefs{$_->db_id} = $_;
  }
  $c->stash->{xrefs} = \%xRefs;

}

#-------------------------------------------------------------------------------

=head2 _getMapping : Private

Retrieves the structure mappings for this clan. 

=cut

sub getMapping : Private {
  my( $this, $c ) = @_;

   my @mapping = $c->model('PfamDB::Clan_membership')
                   ->search( { auto_clan => $c->stash->{clan}->auto_clan,
                               pfam_region => 1 },
                             { select => [ qw( pfamseq.pfamseq_id
                                               pfamA.pfamA_id
                                               pfamA.pfamA_acc
                                               pdbmap.pfam_start_res
                                               pdbmap.pfam_end_res
                                               pdb.pdb_id
                                               pdbmap.chain
                                               pdbmap.pdb_start_res
                                               pdbmap.pdb_end_res ) ],
                               as     => [ qw( pfamseq_id
                                               pfamA_id
                                               pfamA_acc
                                               pfam_start_res
                                               pfam_end_res
                                               pdb_id
                                               chain
                                               pdb_start_res
                                               pdb_end_res ) ],
                               join   => { pdbmap => [ qw( pfamA
                                                           pfamseq
                                                           pdb ) ] }
                             }
                           );

  $c->stash->{pfamMaps} = \@mapping;
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
