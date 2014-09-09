# AlignmentMethods.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Family::AlignmentMethods - role to add alignment-related
methods to the motif page controller

=cut

package RfamWeb::Roles::Motif::AlignmentMethods;

=head1 DESCRIPTION

This is a role to add alignment-related methods to the Motif controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use File::Temp qw( tempfile );
 
# these are the supported output file formats. This is a map between a format
# specified by the user and the format name used by esl-reformat
my %supported_formats = (
  stockholm  => 'stockholm',
  pfam       => 'pfam',
  fasta      => 'afa',   # regular, gapped fasta
  fastau     => 'fasta', # ungapped fasta
);                        

#--------------------------------------------------------------------------------

=head2 alignment : Chained('motif') PathPart('alignment') CaptureArgs(1)

Mid-point in a chain for handling alignments. Captures various parameters:

=over

=item download

flag the alignment for download (add Content-Disposition header) ?

=back

=cut

sub alignment : Chained( 'motif' )
                PathPart( 'alignment' ) {
  my ( $this, $c) = @_;

  # Which alignment, what type of labels, view or download, and should we gzip
  # the output, if that makes sense for the specified format ?
  $c->stash->{download}   = ( $c->req->param('download')  || 0 ) ? 1 : 0;
  my $format = $c->req->param('format');

  if ( $c->debug ) {
    $c->log->debug( 'Motif::alignment: download rather than view ? ' . $c->stash->{download} . ". Format: $format" );
  }
  
  # the output...
  $c->stash->{output_alignment} = ''; # a slot to hold the output alignment
  $c->stash->{filename} = '';         # the output filename, if needed

  # what format should we write ?
    $c->log->debug( 'Motif:::alignment_format: raw alignment format' )
      if $c->debug;
    $c->forward('get_gzipped_alignment');

    unless ( $c->stash->{gzipped_alignment} ) {
      $c->log->debug( 'Family::alignment_format: failed to retrieve raw alignment' )
        if $c->debug;

      $c->stash->{rest}->{error} ||= 'There was a problem retrieving the raw alignment for '
                                     . $c->stash->{acc};
      $c->res->status( 500 ); # Internal server error

      return;
    }

    $c->forward( 'format', [ $format ] );

  # finally, having retrieved an alignment, decide if it needs to be gzipped
  # before being dumped into the response
  my $output;

  if ( $c->stash->{gzip} ) {
    $c->stash->{filename} .= '.gz';
    $c->res->content_type( 'application/x-gzip' );
    $output = $c->stash->{is_gzipped}
    ? $c->stash->{output_alignment}
    : Compress::Zlib::memGzip( $c->stash->{output_alignment} );
  }

  else {
    $c->res->content_type( 'text/plain' );
    $output = $c->stash->{is_gzipped}
    ? Compress::Zlib::memGunzip( $c->stash->{output_alignment} )
    : $c->stash->{output_alignment};
  }


  # should we mark the output for download ? (default is to set download header)
  if ( $c->stash->{download} ) {
    $c->res->header( 'Content-disposition' => 'attachment; filename=' . $c->stash->{filename} );
  }

  # and dump the alignment to the response
  $c->res->body( $output );
}

#-------------------------------------------------------------------------------
#- private ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 Alignment download actions

=head2 format : Private

Builds a plain text (no markup) alignment. Re-formats alignments (using
esl-reformat) into various output formats.

=cut

sub format : Private {
  my ( $this, $c, $format ) = @_;

  # translate the requested format into a real "output format". That is,
  # something that esl-reformat will be happy with
  $format ||= '';
  $format   = lc $format;

  my $output_format;
  if ( exists $supported_formats{ $format } ) {
    $output_format = $supported_formats{ $format };
    $c->log->debug( "Motif::format: generating '$output_format' alignment" )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Motif::format: no supported format specified' )
      if $c->debug;

    $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' )
                          ? 'rest/motif/error_xml.tt'
                          : 'components/blocks/motif/error.tt';

    $this->status_bad_request( $c, message => 'Not a valid alignment format');

    return;
  }


  if ( $output_format eq 'stockholm' ) {

    # special case: we can dump out the raw alignment directly if the user
    # wants stockholm format
    $c->log->debug( 'Motif::format: stockholm format requested' )
      if $c->debug;

    $c->stash->{output_alignment} = $c->stash->{gzipped_alignment};
    $c->stash->{is_gzipped}       = 1;
    $c->stash->{filename}         = $c->stash->{acc} . '.stockholm.txt';
  }
  else {

    # use esl-reformat to output the requested format

    # get a temporary file and filehandle
    my ( $fh, $fn ) = tempfile();
    unless ( $fn and $fh ) {
      $c->log->debug( "Family::format: couldn't open temp file for alignment: $!" )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem while reformatting the alignment for '
                              . $c->stash->{acc};

      return;
    }

    # dump the alignment to that file
    print $fh Compress::Zlib::memGunzip( $c->stash->{gzipped_alignment} );
    close $fh;

    # build the command for running esl-reformat 
    my $eslreformatLocation = $this->{eslreformat_binary};   
    my $cmd = $this->{eslreformat_binary} . " -u -r --mingap --informat stockholm $output_format $fn";
    $c->log->debug( "Motif::format: running system command: |$cmd|" )
      if $c->debug;
    $c->log->debug( "Motif::format: running esl-reformat at |$eslreformatLocation|" )
      if $c->debug;

    unless ( open OUTPUT, "$cmd|" ) {    
      $c->log->debug( "Motif::format: couldn't run esl-reformat: $!" )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem reformatting the alignment for '
                              . $c->stash->{acc};

      return;
    }

    # stick the output of esl-reformat back together and we're done
    $c->stash->{output_alignment} = join '', <OUTPUT>;
    $c->stash->{is_gzipped}       = 0;
    $c->stash->{filename}         = $c->stash->{acc} . ".$output_format.txt";

    # tidy up
    close OUTPUT;
    unlink $fn;
  }
}

#-------------------------------------------------------------------------------

=head2 get_gzipped_alignment : Private

Retrieves the gzipped alignment from the database and stashes it, still 
compressed. Caches the gzipped alignment too, if caching is enabled. 

=cut

sub get_gzipped_alignment : Private {
  my ( $this, $c ) = @_;
  
  # first try the cache...
  my $cacheKey = 'gzipped_alignment' . $c->stash->{acc};

  my $gzipped_alignment = $c->cache->get( $cacheKey );

  if ( defined $gzipped_alignment ) {
    $c->log->debug( 'Motif::get_gzipped_alignment: extracted gzipped alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( "Motif::get_gzipped_alignment: failed to extract gzipped alignment from cache; going to DB" )
      if $c->debug;

    # failed to get a cached version; retrieve the alignment from the DB
    my $rs = $c->stash->{db}->resultset('MotifFile')->search( motif_acc => $c->stash->{acc} );


    # make sure the query returned something
    my $alignment_row;
    unless ( defined $rs and $alignment_row = $rs->first ) {
      $c->log->debug( 'Motif::get_gzipped_alignment: failed to retrieve a row' ) if $c->debug;

      $c->stash->{rest}->{error} = 'There was a problem retrieving the alignment data for ' . $c->stash->{acc};

      return;
    }

    # make sure we can get the alignment out of the returned row
    unless ( $gzipped_alignment = $alignment_row->seed ) {
      $c->log->debug( 'Motif::get_gzipped_alignment: failed to retrieve an alignment' ) if $c->debug;

      $c->stash->{rest}->{error} = 'There was a problem uncompressing the alignment data for '
                                   . $c->stash->{acc};

      return;
    }

    # cache the gzipped alignment
    $c->log->debug( 'Motif::get_gzipped_alignment: retrieved gzipped alignment from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $gzipped_alignment ) unless $ENV{NO_CACHE};
  }

  $c->stash->{gzipped_alignment} = $gzipped_alignment;
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



