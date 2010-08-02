
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
use MIME::Base64;

use base 'RfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'family' );

# this is really ugly, but it makes sense to have this image easily to hand.
# Ideally we'd use a __DATA__ stream, but that breaks in mod_perl and we
# probably shouldn't try it in a FastCGI environment either. 
# See: http://modperlbook.org/html/6-6-1-_-_END_-_-and-_-_DATA_-_-Tokens.html
our $no_alignment_image = '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';
    
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
  $c->forward( 'get_wikipedia' );
  
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
      $c->detach( 'no_alignment' );
      return;
      # $c->stash->{errorMsg} = 'We could not find an image for ' 
      #                         . $c->stash->{acc};
      # return;
    }

    $image = $rs->image;

    $c->cache->set( $cache_key, $image ) unless $ENV{NO_CACHE}
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $image );
}

#-------------------------------------------------------------------------------

=head2 no_alignment : Private

Drops a PNG image into the stash, with the message "we do not have this
alignment in the database".

=cut

sub no_alignment : Private {
  my ( $this, $c ) = @_;

  $c->cache_page( 604800 );

  my $cache_key = 'no_alignment_image';
  my $image     = $c->cache->get( $cache_key );
  
  if ( defined $image ) {
    $c->log->debug( 'Family::no_alignment: retrieved "no alignment" image from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::no_alignment: failed to retrieve "no alignment" image from cache; building afresh' )
      if $c->debug;

    $image = decode_base64( $no_alignment_image );

    unless ( defined $image ) {
      $c->stash->{errorMsg} = 'We could not find an image for ' 
                              . $c->stash->{acc};
      return;
    }

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
  my $full_tree = $c->model( 'RfamDB::AlignmentsAndTrees' )
                    ->find( { auto_rfam => $rfam->auto_rfam,
                              type      => 'full' } );
  $c->stash->{alignment_info}->{full} = $full_tree;

  my $seed_tree = $c->model( 'RfamDB::AlignmentsAndTrees' )
                    ->find( { auto_rfam => $rfam->auto_rfam,
                              type      => 'seed' } );
  $c->stash->{alignment_info}->{seed} = $seed_tree;

  # my $rs = $c->model( 'RfamDB::AlignmentsAndTrees' )
  #            ->search( {
  #                        -and => [
  #                                  auto_rfam => $rfam->auto_rfam,
  #                                  -or => [
  #                                           type => 'full',
  #                                           type => 'seed'
  #                                         ]
  #                                ]
  #                      },
  #                      { order_by => 'type DESC' } );

  # $c->stash->{alignment_info} = $rs;

  # $c->log->debug( 'Family::get_data: found ' . $rs->count 
  #                 . ' rows for alignment info' )
  #   if $c->debug;

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

=head2 get_wikipedia : Private

Retrieves the wikipedia content, if any, for this family.

=cut

sub get_wikipedia : Private {
  my ( $this, $c ) = @_;
  
  return unless $c->stash->{rfam}->auto_wiki;

  my $title = $c->stash->{rfam}->auto_wiki->title;

  $c->log->debug( "Family::get_wikipedia: got wiki title: |$title|" )
    if $c->debug;

  $c->stash->{article} = $c->model('WebUser::Wikitext')
                           ->find( $title );
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

__DATA__
