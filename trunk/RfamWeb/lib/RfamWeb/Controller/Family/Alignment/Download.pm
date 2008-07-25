
# DownloadAlignment.pm
# rdf 20061005 WTSI
#
# $Id: Download.pm,v 1.1 2008-07-25 13:27:26 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Family::DownloadAlignment - controller to
serve up alignments in various formats

=cut

package RfamWeb::Controller::Family::Alignment::Download;

=head1 DESCRIPTION

Generates a B<full page>.

$Id: Download.pm,v 1.1 2008-07-25 13:27:26 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;

use base 'RfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 gzipped : Local

Returns a gzip-compressed file with the full or seed alignment for the specified
family. 

=cut

sub gzipped : Local {
  my( $this, $c ) = @_;
  
  # retrieve the alignment
  $c->forward('get_gzipped_alignment');
  return unless defined $c->stash->{gzipped_alignment};

  # build a filename for it
  my $filename = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.gz';
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  
  # ... and dump it straight to the response
  $c->res->content_type( 'application/x-gzip' );
  $c->res->body( $c->stash->{gzipped_alignment} );
}

#-------------------------------------------------------------------------------

=head2 format : Local

Serves the plain text (no markup) alignment. Also applies various
changes to the alignment before hand, such as changing the gap style
or sequence order.

By default this method will generate Stockholm format, but the
following formats are also available and can be specified with the
C<format> parameter:

=over

=item * pfam

Essentially Stockholm with markup lines removed

=item * fasta

Vanilla FASTA format

=item * msf

No idea...

=item * stockholm (default)

Standard Stockholm format

=back

The exact styles (other than C<pfam>) are defined by BioPerl.

=cut

sub format : Local {
  my( $this, $c ) = @_;

  # retrieve the alignment
  $c->forward('get_alignment');

  if ( defined $c->stash->{alignment} ) {
    $c->log->debug( 'Family::Alignment::Download::format: successfully retrieved an alignment' )
      if $c->debug;
  } else {
    $c->log->debug( 'Family::Alignment::Download::format: failed to retrieve an alignment' )
      if $c->debug;
    $c->stash->{errorMsg} = 'We could not extract the alignment for '
                            . $c->stash->{acc};
    return;
  }

  # because the AlignPfam module expects an alignment to be handed in as an
  # array ref, we have to split what we get from the DB
  my @alignment = split /\n/, $c->stash->{alignment};

  # drop it into an AlignPfam object
  my $rfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $rfamaln->read_stockholm( \@alignment );
  };
  if( $@ ) {
    $c->log->debug( "Family::Alignment::Download::format: problem reading stockholm data: $@" )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem with the alignment data for '
                            . $c->stash->{acc};
    return;
  };

  # gaps param can be default, dashes, dot or none
  if( $c->req->param('gaps') ) {
    $c->log->debug( 'Family::Alignment::Download::format: handling gaps parameter' )
      if $c->debug;
      
    if( $c->req->param('gaps') eq 'none' ) {
      $rfamaln->map_chars('-', '');
      $rfamaln->map_chars('\.', '');
    } elsif( $c->req->param('gaps') eq 'dots' ) {
      $rfamaln->map_chars('-', '.');
    } elsif( $c->req->param('gaps') eq 'dashes' ) {
      $rfamaln->map_chars('\.', '-');
    }
  }

  # case param can be u or l
  if( $c->req->param('case') and $c->req->param('case') eq 'u' ) {
    $c->log->debug( 'Family::Alignment::Download::format: uppercasing alignment' )
      if $c->debug;
    $rfamaln->uppercase;
  }

  # order param can be tree or alphabetical
  if( $c->req->param('order') and $c->req->param('order') eq 'a' ) {
    $c->log->debug( 'Family::Alignment::Download::format: sorting alphabetically' )
      if $c->debug;
    $rfamaln->sort_alphabetically;
  }

  # format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if( $c->req->param( 'format' ) ) {
    if( $c->req->param( 'format' ) eq 'pfam' ) {
      $c->log->debug( 'Family::Alignment::Download::format: writing Pfam format' )
        if $c->debug;
      $output = $rfamaln->write_Pfam;
    } elsif( $c->req->param( 'format' ) eq 'fasta' ) {
      $c->log->debug( 'Family::Alignment::Download::format: writing FASTA format' )
        if $c->debug;
      $output = $rfamaln->write_fasta;
    } elsif( $c->req->param( 'format' ) eq 'msf' ) {
      $c->log->debug( 'Family::Alignment::Download::format: writing MSF format' )
        if $c->debug;
      $output = $rfamaln->write_MSF;
    }
  }

  # default to writing stockholm
  $output ||= $rfamaln->write_stockholm;
  $c->log->debug( 'Family::Alignment::Download::format: length of alignment: '
                  . scalar @$output )
    if $c->debug;

  # are we downloading this or just dumping it to the browser ?
  if( $c->req->param( 'download' ) ) {
    $c->log->debug( 'Family::Alignment::Download::format: sending alignment as download' )
      if $c->debug;

    # figure out the filename
    my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.txt';  
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  $c->res->content_type( 'text/plain' );
  $c->res->body( join '', @$output );
}

#-------------------------------------------------------------------------------

=head2 end : ActionClass

Override the default end action from the parent class and replace it with an
end that uses RenderView to take care of rendering. No need to check the
contents of the response etc., since RenderView looks after all of that.

=cut

sub end : ActionClass( 'RenderView' ) {}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub get_alignment : Private {
  my ( $this, $c ) = @_;
  
  $c->forward('get_gzipped_alignment');
  
  return unless $c->stash->{gzipped_alignment};

  $c->stash->{alignment} = Compress::Zlib::memGunzip( $c->stash->{gzipped_alignment} );
  unless ( defined $c->stash->{alignment} ) {
    $c->log->debug( 'Family::Alignment::Download::get_alignment: failed to gunzip the  alignment' )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem uncompressing the alignment data for '
                            . $c->stash->{acc};
    return;
  }
  
}

#-------------------------------------------------------------------------------

=head2 get_gzipped_alignment : Private

=cut

sub get_gzipped_alignment : Private {
  my ( $this, $c ) = @_;
  
  # first try the cache...
  my $cacheKey = 'gzipped_alignment'
                 . $c->stash->{acc}
                 . $c->stash->{alnType};
  my $gzipped_alignment = $c->cache->get( $cacheKey );

  if ( defined $gzipped_alignment ) {
    $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: extracted gzipped alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: failed to extract gzipped alignment from cache; going to DB' )
      if $c->debug;

    # failed to get a cached version; retrieve the alignment from the DB
    my $rs = $c->stash->{rfam}->search_related( 'alignments_and_trees',
                                                { type => $c->stash->{alnType} } );

    # make sure the query returned something
    unless ( defined $rs ) {
      $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: failed to retrieve a row' )
        if $c->debug;
      $c->stash->{errorMsg} = 'There was a problem retrieving the alignment data for '
                              . $c->stash->{acc};
      return;
    }

    # make sure we can get the alignment out of the returned row
    unless ( $gzipped_alignment = $rs->first->alignment ) {
      $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: failed to retrieve an alignment' )
        if $c->debug;
      $c->stash->{errorMsg} = 'There was a problem retrieving the alignment data for '
                              . $c->stash->{acc};
      return;
    }

    # cache the gzipped alignment
    $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: retrieved gzipped alignment from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $gzipped_alignment ) unless $ENV{NO_CACHE};
  }

  # stash the gzipped alignment and let the actions themselves gunzip it if
  # they need to
  $c->stash->{gzipped_alignment} = $gzipped_alignment;
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
