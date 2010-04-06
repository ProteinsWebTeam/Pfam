
# Family.pm
# jt6 20080306 WTSI
#
# $Id: Family.pm,v 1.6 2009-01-06 11:52:06 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Family - controller to build the main Rfam family page

=cut

package RfamWeb::Controller::Family;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Rfam
families across the site. The L<begin|/"begin : Private"> method tries
to extract a Rfam ID or accession from the captured URL and tries to
load a Rfam object from the model.

Generates a B<tabbed page>.

$Id: Family.pm,v 1.6 2009-01-06 11:52:06 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;

use base 'RfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'family' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

This is the guts of this controller. It's function is to extract
the Rfam family ID or accession from the URL and get the row
in the Rfam table for that entry. Expects one of three parameters:

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
    $c->log->debug( 'Family::begin: no valid Rfam family accession or ID' )
      if $c->debug;

    $c->stash->{errorMsg} = 'Invalid Rfam family accession or ID';

    return;
  }
  
  # find out what type of alignment we need, seed, full, ncbi, etc
  $c->stash->{alnType} = 'seed';
  if ( defined $c->req->param('alnType') and
       ( $c->req->param('alnType') eq 'seed' or 
         $c->req->param('alnType') eq 'full' ) ) {
    $c->stash->{alnType} = $c->req->param( 'alnType' );
  }

  $c->log->debug( 'Family::begin: setting alnType to ' . $c->stash->{alnType} )
    if $c->debug;
  
  # for the benefit of the alignment generation controller, we also check to 
  # see if alignments should be produced with the alternate label format, or
  # in "colorstock" html format
  $c->stash->{nseLabels}  = $c->req->param('nseLabels') ? 1 : 0;
  $c->stash->{colorstock} = $c->req->param('cs') ? 1 : 0;

  $c->log->debug( 'Family::begin: use name/start-end labels ? ' . $c->stash->{nseLabels} )
    if $c->debug;
  $c->log->debug( 'Family::begin: raw or colorstock ?         ' . $c->stash->{colorstock} )
    if $c->debug;
  
  #----------------------------------------

  # retrieve data for the family
  $c->forward( 'get_data', [ $entry ] );
  
  #----------------------------------------

  # if we're outputting HTML, we're done here
  unless ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Family::begin: emitting HTML' ) if $c->debug;
    return;
  }

  # from here on we're handling XML output
  $c->log->debug( 'Family::begin: emitting XML' ) if $c->debug;

  # if there was an error...
  if ( $c->stash->{errorMsg} ) {
    $c->log->debug( 'Family::begin: there was an error: |' .
                    $c->stash->{errorMsg} . '|' ) if $c->debug;
    $c->stash->{template} = 'rest/family/error_xml.tt';
    return;
  }
}

#-------------------------------------------------------------------------------

=head2 varna : Local

This is the way into the VARNA secondary structure viewer applet.

Hands straight off to a template that generates a "tool" page containing the 
VARNA applet.

=cut

sub varna : Local {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'components/tools/varna.tt';
}

#-------------------------------------------------------------------------------

=head2 image : Local

Retrieves and returns an image from the database. Cache the image, unless
C<$ENV{NO_CACHE}> is true. 

=cut

sub image : Local {
  my ( $this, $c ) = @_;
  
  $c->cache_page( 604800 );

  my ( $image_type ) = $c->req->param('type') || '' =~ m/^(\w+)$/;

  unless ( defined $image_type ) {
    $c->log->debug( 'Family::image: no valid type specified; defaulting to normal' )
      if $c->debug;
    $image_type = 'normal';
  }

  my $cache_key = 'family_image' . $c->stash->{acc} . $image_type;
  my $image     = $c->cache->get( $cache_key );
  
  if ( defined $image ) {
    $c->log->debug( 'Family::image: retrieved image from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::image: failed to retrieve image from cache; going to DB' )
      if $c->debug;

    my $rs = $c->model('SecondaryStructureImages')
               ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                         type      => $image_type } );

    unless ( defined $rs and
             defined $rs->image ) {
      $c->stash->{errorMsg} = 'We could not find an image for ' 
                              . $c->stash->{acc};
      return;
    }

    $image = $rs->image;

    $c->cache->set( $cache_key, $image ) unless $ENV{NO_CACHE}
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $image );
    
}

#-------------------------------------------------------------------------------

=head2 cm : Local

Serves the CM file for this family.

=cut

sub cm : Local {
  my ( $this, $c ) = @_;
  
  my ( $version ) = $c->req->param('version') =~ m/^(\d+\.\d+)$/;
  
  my $rs;
  if ( defined $version ) {
    $c->log->debug( "Family::cm: looking for CM built with infernal v. |$version| ")
      if $c->debug;
    $rs = $c->stash->{rfam}->search_related( 'rfam_cms',
                                             { version => $version } );
  }
  else {
    $c->log->debug( 'Family::cm: looking for latest CM' ) if $c->debug;  
    $rs = $c->stash->{rfam}->search_related( 'rfam_cms',
                                             {},
                                             { order_by => 'version DESC' } );
  }

  my $gzipped_cm;
  unless ( defined $rs and 
           $gzipped_cm = $rs->first->cm ) {
    $c->stash->{errorMsg} = 'We could not find a covariance model that was built with that version of infernal.';
    return;
  }
  
  my $cm = Compress::Zlib::memGunzip( $gzipped_cm );
  unless ( defined $cm ) {
    $c->stash->{errorMsg} = 'We could not uncompress the covariance model file.';
    return;
  }

  my $filename = $c->stash->{acc} . '.cm';
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $cm );
}

#-------------------------------------------------------------------------------

=head2 regions : Local

Builds a tab-delimited file containing all regions for this family

=cut

sub regions : Local {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::regions: building tab-delimited list of regions' )
    if $c->debug;

  if ( $c->stash->{showText} ) {
    $c->log->debug( 'Family::regions: showText flag set earlier; retrieving regions' )
      if $c->debug;
    $c->forward( 'get_regions_data' );
  }

  unless ( defined $c->stash->{regions} ) {
    $c->log->debug( 'Family::regions: num_full > showText limit; not showing regions' )
      if $c->debug;

    $c->res->status( 403 );
    $c->res->body( 'The family has too many regions to list.' );

    return;
  }
  
  # build a sensible filename
  my $filename = $c->stash->{acc} . '_regions.txt';
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # add a meaningful header
  my $regions = '# Rfam regions for family ' . $c->stash->{rfam}->rfam_id 
                . ' (' . $c->stash->{rfam}->rfam_acc . ")\n"
                . '# file built ' . localtime(). ' using Rfam version ' 
                . $c->stash->{relData}->rfam_release
                . ' (released ' . $c->stash->{relData}->rfam_release_date . ")\n";

  # add the rows
  foreach my $region ( @{ $c->stash->{regions} } ) {
    $regions .= $region->get_column('rfamseq_acc' ) . "\t";
    $regions .= $region->bits_score . "\t";
#    $regions .= $region->type . "\t";
    $regions .= $region->seq_start . "\t";
    $regions .= $region->seq_end . "\t";
    $regions .= $region->get_column('description' ) . "\t";
    $regions .= $region->get_column('species' ) . "\n";
  }

  # stuff the content into the response and we're done. The View won't try to 
  # render a template, since the body already contains content
  $c->res->body( $regions );
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
  my $rfam = $c->model('RfamDB::Rfam')
               ->search( [ { rfam_acc => $entry },
                           { rfam_id  => $entry } ] )
               ->single;
                         
  unless ( defined $rfam ) {
    $c->log->debug( 'Family::get_data: no row for that accession/ID' )
      if $c->debug;
  
    $c->stash->{errorMsg} = 'No valid Rfam family accession or ID';

    return;
  }  

  $c->log->debug( 'Family::get_data: got a family' )
    if $c->debug;

  $c->stash->{rfam} = $rfam;
  $c->stash->{acc}  = $rfam->rfam_acc;
  $c->stash->{entryType}  = 'R';

  # get curation data
  my $rs = $c->model( 'RfamDB::AlignmentsAndTrees' )
             ->search( {
                         -and => [
                                   auto_rfam => $rfam->auto_rfam,
                                   -or => [
                                            type => 'full',
                                            type => 'seed'
                                          ]
                                 ]
                       },
                       { order_by => 'type DESC' } );

  $c->stash->{alignment_info} = $rs;

  $c->log->debug( 'Family::get_data: found ' . $rs->count 
                  . ' rows for alignment info' )
    if $c->debug;

  # if we're returning XML, we don't need the extra summary data etc.  
  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Family::get_data: returning XML; NOT adding extra info' ) 
        if $c->debug;    
    return;
  }
    
  # unless this request originates at the top level of the object hierarchy,
  # we don't need the extra summary data
  unless ( ref $this eq 'RfamWeb::Controller::Family' ) {
    $c->log->debug( 'Family::get_data: not the root Family controller; NOT adding extra family info' )
      if $c->debug;
    return;
  }

  # finally, having decided that we need it...
  $c->forward( 'get_summary_data' );

  # load the data for all regions, provided the number of regions is less than
  # the limit set in the config
  if ( $rfam->num_full <= $this->{regionsLimits}->{showAll} ) {
    $c->log->debug( 'Family::get_data: num_full <= showAll limit; retrieving regions' )
      if $c->debug;
    $c->stash->{showAll} = 1;
    $c->forward( 'get_regions_data' );
  }
  elsif ( $rfam->num_full <= $this->{regionsLimits}->{showText} ) {
    $c->log->debug( 'Family::get_data: num_full <= showText limit; retrieving regions later' )
      if $c->debug;
    $c->stash->{showText} = 1;
  }

  # add the clan details, if any
  my $clan = $c->model('RfamDB::Clans')
               ->search( { 'clan_memberships.auto_rfam' => $rfam->auto_rfam },
                         { prefetch => [ qw(clan_memberships) ] } )
               ->first;
  
  if ( $clan and defined $clan->clan_acc ) {
    $c->log->debug( 'Family::get_data: adding clan info' ) if $c->debug;
    $c->stash->{clan} = $clan;
  }
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the Rfam object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my $summaryData = {};

  # number of sequences in full alignment
  $summaryData->{numSequences} = $c->stash->{rfam}->num_full;

  # number of structures known for the domain
  my $rs = $c->model('RfamDB::PdbRfamReg')
             ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam },
                       {} );

  $summaryData->{numStructures} = $rs->count;

  # Number of species
  $summaryData->{numSpecies} = $c->stash->{rfam}->number_of_species;

  # number of interactions
  $summaryData->{numInt} = 0;

  $c->stash->{summaryData} = $summaryData;
}

#-------------------------------------------------------------------------------

=head2 get_regions_data : Private

Retrieves sequence data for the family.

=cut

sub get_regions_data : Private {
  my ( $this, $c ) = @_;
  
  # save some typing...
  my $rfam = $c->stash->{rfam};

  $c->log->debug( 'Family::get_regions_data: family has |'
                  . $rfam->num_full . '| regions' ) if $c->debug;

  my @regions = $c->model('RfamDB::RfamRegFull')
                  ->search( { auto_rfam => $rfam->auto_rfam },
                            { join      => { 'auto_rfamseq' => 'ncbi_id' },
                              '+select' => [ qw( auto_rfamseq.rfamseq_acc
                                                 auto_rfamseq.description
                                                 ncbi_id.species
                                                 auto_rfamseq.length ) ],
                              '+as'     => [ qw( rfamseq_acc
                                                 description
                                                 species
                                                 length ) ],
                              order_by  => [ 'bits_score DESC' ] } );
                                            
  $c->stash->{regions} = \@regions;
  $c->stash->{showAll} = 1;

  $c->log->debug( 'Family::get_regions_data: added |' . scalar @regions
                  . '| regions to stash' ) if $c->debug;
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
