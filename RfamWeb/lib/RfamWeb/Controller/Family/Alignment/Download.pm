
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
use File::Temp qw( tempfile );

use base 'RfamWeb::Controller::Family::Alignment';

my %supported_formats = (
  stockholm => 'stockholm',
  pfam      => 'pfam',
  fasta     => 'afa',   # regular, gapped fasta
  fastau    => 'fasta', # ungapped fasta
);                        

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 gzipped : Local

Returns a gzip-compressed file with the full or seed alignment for the specified
family. 

=cut

sub gzipped : Local {
  my( $this, $c ) = @_;
  
  # retrieve the alignment
  my $filename;
  if ( $c->stash->{colorstock} ) {
    # we need the "colorstock" alignment

    # retrieve the alignment, either from cache or the DB
    $c->forward('get_colorstock_alignment');

    # build a filename for it
    $filename = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.colorstock.html.gz';
    
  } else {
    # we need the "regular" alignment
    $c->forward('get_gzipped_alignment');
    $filename = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.gz';
  }

  return unless defined $c->stash->{gzipped_alignment};

  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  
  # ... and dump it straight to the response
  $c->res->content_type( 'application/x-gzip' );
  $c->res->body( $c->stash->{gzipped_alignment} );
}

#-------------------------------------------------------------------------------

=head2 format : Local

Serves the plain text (no markup) alignment. By default this method will 
serve the Stockholm format file straight from the database, but will also 
re-format it (using esl-reformat) into various other output formats:

=over

=item * pfam

Stockholm with sequences on a single line

=item * fasta

Vanilla FASTA format; ungapped

=item * fastau

Gapped FASTA format

=item * stockholm (default)

Standard Stockholm format

=back

The exact styles (other than C<pfam>) are defined by C<esl-reformat>.

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

  # format param can be one of pfam, stockholm, fasta or MSF. Get the default
  # from the configuration
  my $output_format = $this->{default_output_format};
  if ( exists $supported_formats{ $c->req->param('format') } ) {    
    $output_format = $supported_formats{ $c->req->param('format') };
    $c->log->debug( "Family::Alignment::Download::format: requested output format: |$output_format|" )
      if $c->debug;
  }
    
  my $output;
  if ( $c->req->param('format') eq 'stockholm' ) {
    $c->log->debug( 'Family::Alignment::Download::format: stockholm format requested; returning alignment unchanged' )
      if $c->debug;
    $output = $c->stash->{alignment}
  }
  else {

    # get a temporary file and filehandle
    my ( $fh, $fn ) = tempfile();
    unless ( $fn and $fh ) {
      $c->log->error( "Family::Alignment::Download::format: couldn't open temp file for alignment: $!" );
      return;
    }

    # dump the alignment to that file
    print $fh $c->stash->{alignment};
    close $fh;

    # build the command for running esl-reformat    
    my $cmd = $this->{eslreformat_binary} . " -u -r --mingap --informat stockholm $output_format $fn";
    $c->log->debug( "Family::Alignment::Download::format: running system command: |$cmd|" )
      if $c->debug;

    unless ( open OUTPUT, "$cmd|" ) {    
      $c->log->error( "Family::Alignment::Download::format: couldn't run esl-reformat: $!" );
      return;
    }

    # stick the output of esl-reformat back together and remove the temp file
    $output = join '', <OUTPUT>;
    close OUTPUT;

    unlink $fn;

    # as far as I can see, this SHOULD work too, and, if it did, would have the 
    # advantage of not requiring any temp files at all...
    #    my $cmd = $this->{eslreformat_binary} . " --informat stockholm $output_format -";
    #    $c->log->debug( "Family::Alignment::Download::format: running esl-reformat command: |$cmd|" )
    #      if $c->debug;
    #     
    #    my ( $out, $in );
    #    my $pid = open2( $out, $in, $cmd );
    #    unless ( $pid ) {
    #      $c->log->error( "Family::Alignment::Download::format: failed to run esl-reformat command ($cmd): $!" );
    #      return undef;
    #    }
    #    $c->log->debug( "Family::Alignment::Download::format: running esl-reformat with PID $pid" )
    #      if $c->debug;
    #
    #    $c->log->debug( 'Family::Alignment::Download::format: raw alignment: '
    #                    . $c->stash->{alignment} )
    #      if $c->debug;
    #
    #    print $in $c->stash->{alignment};
    #    close $in;
    #    $c->log->debug( 'Family::Alignment::Download::format: printed input...' )
    #      if $c->debug;
    #
    #    $output = join '', <$out>;
    #
    #    $c->log->debug( "Family::Alignment::Download::format: esl-reformat output: |$output|" )
    #      if $c->debug;
  }

  # are we downloading this or just dumping it to the browser ?
  if ( $c->req->param( 'download' ) ) {
    $c->log->debug( 'Family::Alignment::Download::format: sending alignment as download' )
      if $c->debug;

    # figure out the filename
    my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.txt';  
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  $c->res->content_type( 'text/plain' );
  $c->res->body( join '', $output );
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

=head2 get_alignment : Private

Retrieves the alignment from the database and drops it into the stash. Note that
the database stores the alignments in gzip format. This action uncompresses the
alignment before stashing it.

=cut

sub get_alignment : Private {
  my ( $this, $c ) = @_;
  
  $c->forward('get_gzipped_alignment');
  
  return unless $c->stash->{gzipped_alignment};

  $c->stash->{alignment} = Compress::Zlib::memGunzip( $c->stash->{gzipped_alignment} );
  
  unless ( defined $c->stash->{alignment} ) {
    $c->log->debug( 'Family::Alignment::Download::get_alignment: failed to gunzip the alignment' )
      if $c->debug;      
    $c->stash->{errorMsg} = 'There was a problem uncompressing the alignment data for '
                            . $c->stash->{acc};
    return;
  }
}

#-------------------------------------------------------------------------------

=head2 get_gzipped_alignment : Private

Retrieves the gzipped alignment from the database and stashes it, still 
compressed. Caches the gzipped alignment too, if caching is enabled. 

=cut

sub get_gzipped_alignment : Private {
  my ( $this, $c ) = @_;

  my $alnType = $c->stash->{alnType} . ( $c->stash->{nseLabels} ? '' : 'Tax' );
  $c->log->debug( "Family::Alignment::Download::get_gzipped_alignment: setting alignment type to |$alnType|" )
    if $c->debug;
  
  # first try the cache...
  my $cacheKey = 'gzipped_alignment'
                 . $c->stash->{acc}
                 . $alnType;
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
                                                { type => $alnType } );

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
      $c->stash->{errorMsg} = 'There was a problem uncompressing the alignment data for '
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

=head2 get_colorstock_alignment : Private

Retrieves the gzipped "colorstock" HTML alignment from the database and stashes 
it, still compressed. Caches the gzipped alignment too, if caching is enabled.

=cut

sub get_colorstock_alignment : Private {
  my ( $this, $c ) = @_;

  my $alnType = $c->stash->{alnType} . 'Colorstock';
  $c->log->debug( "Family::Alignment::Download::get_colorstock_alignment: setting alignment type to |$alnType|" )
    if $c->debug;
  
  # first try the cache...
  my $cache_key = 'gzipped_alignment'
                  . $c->stash->{acc}
                  . $alnType;
  my $gzipped_alignment = $c->cache->get( $cache_key );
  
  if ( defined $gzipped_alignment ) {
    $c->log->debug( 'Family::Alignment::Download::get_colorstock_alignment: extracted gzipped alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::Download::get_colorstock_alignment: failed to extract gzipped alignment from cache; going to DB' )
      if $c->debug;

    # failed to get a cached version; retrieve the alignment from the DB
    my $rs = $c->stash->{rfam}->search_related( 'html_alignments',
                                                { type => $alnType } );

    # make sure the query returned something
    unless ( defined $rs ) {
      $c->log->debug( 'Family::Alignment::Download::get_colorstock_alignment: failed to retrieve a row' )
        if $c->debug;
      $c->stash->{errorMsg} = 'There was a problem retrieving the colorstock alignment for '
                              . $c->stash->{acc};
      return;
    }

    # make sure we can retrieve the gzipped HTML alignment from the row object
    $gzipped_alignment = $rs->first->html;

    unless ( defined $gzipped_alignment ) {
      $c->log->debug( 'Family::Alignment::Download::get_colorstock_alignment: failed to retrieve HTML' )
        if $c->debug;
      $c->stash->{errorMsg} = 'There was a problem retrieving the HTML colorstock alignment for '
                              . $c->stash->{acc};
      return;
    }

    # cache the gzipped alignment
    $c->log->debug( 'Family::Alignment::Download::get_colorstock_alignment: retrieved gzipped alignment from DB' )
      if $c->debug;
    $c->cache->set( $cache_key, $gzipped_alignment ) unless $ENV{NO_CACHE};
  }

  # stash the gzipped alignment and let the actions themselves gunzip it if
  # they need to
  $c->stash->{gzipped_alignment} = $gzipped_alignment;

  $c->log->debug( 'Family::Alignment::Download::get_colorstock_alignment: gzipped alignment length: '
                  . length $c->stash->{gzipped_alignment} )
    if $c->debug;
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
