
# AlignmentMethods.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Family::AlignmentMethods - role to add alignment-related
methods to the family page controller

=cut

package RfamWeb::Roles::Family::AlignmentMethods;

=head1 DESCRIPTION

This is a role to add alignment-related methods to the Family controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use MIME::Base64;
use Compress::Zlib;
use File::Temp qw( tempfile );

# this is really ugly, but it makes sense to have this image easily to hand.
# Ideally we'd use a __DATA__ stream, but that breaks in mod_perl and we
# probably shouldn't try it in a FastCGI environment either. 
# See: http://modperlbook.org/html/6-6-1-_-_END_-_-and-_-_DATA_-_-Tokens.html
our $no_alignment_image = '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';
 

#-------------------------------------------------------------------------------

=head2 alignment_default : Chained('family') PathPart('alignment') Args(0)

Catches alignment requests with no alignment type (seed or full) and no format
specified. Defaults to Stockholm-format seed alignment.

=cut 

sub alignment_default: Chained( 'family' ) 
                       PathPart( 'alignment' )
                       Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->forward( 'alignment',        [ 'seed' ] );
  $c->forward( 'alignment_format', [ 'stockholm' ] );
}

#---------------------------------------

=head2 alignment : Chained('family') PathPart('alignment') CaptureArgs(1)

Mid-point in a chain for handling alignments. Captures various parameters:

=over

=item alnType

alignment type

=item nseLabels

use "name/start-end" to label sequences ? default is to use species names

=item view

flag the alignment for download (add Content-Disposition header) ?

=item gzip

should the output alignment be gzip-compressed ?

=back

=cut

sub alignment : Chained( 'family' )
                PathPart( 'alignment' )
                CaptureArgs( 1 ) {
  my ( $this, $c, $aln_type ) = @_;

  # which alignment, what type of labels, view or download, and should we gzip
  # the output, if that makes sense for the specified format ?
  $c->stash->{alnType}    = ( $aln_type || '' ) eq 'seed' ? 'seed' : 'full'; 
  $c->stash->{nseLabels}  = ( $c->req->param('nseLabels') || 0 ) ? 1 : 0;
  $c->stash->{view}       = ( $c->req->param('view')      || 0 ) ? 1 : 0;
  $c->stash->{gzip}       = ( $c->req->param('gzip')      || 0 ) ? 1 : 0;

  if ( $c->debug ) {
    $c->log->debug( 'Family::alignment: which alignment type ?      ' . $c->stash->{alnType} );
    $c->log->debug( 'Family::alignment: use name/start-end labels ? ' . $c->stash->{nseLabels} );
    $c->log->debug( 'Family::alignment: view rather than download ? ' . $c->stash->{view} );
    $c->log->debug( 'Family::alignment: gzipped output ?            ' . $c->stash->{gzip} );
  }
}

#---------------------------------------

=head2 alignment_format_default : Chained('alignment') PathPart('') Args(0)

Catches requests with an alignment type (seed or full) but no format specified.
Defaults to Stockholm-format alignment.

=cut 

sub alignment_format_default : Chained( 'alignment' ) 
                               PathPart( '' )
                               Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->forward( 'alignment_format', [ 'stockholm' ] );
}

#---------------------------------------

=head2 alignment_format : Chained('alignment') PathPart('') Args(1)

Outputs the specified seed or full alignment in the specified format.
Supported formats, given as the first argument, are:

=over

=item * pfam

Stockholm with sequences on a single line

=item * fasta

Gapped FASTA format

=item * fastau

Vanilla FASTA format; ungapped

=item * stockholm

Standard Stockholm format

=back

The exact styles (other than C<pfam>) are defined by C<esl-reformat>. There is
no default. If the specified format is not in the supported list, we throw an
error.

=cut

# these are the supported output file formats. This is a map between a format
# specified by the user and the format name used by esl-reformat
my %supported_formats = (
  stockholm  => 'stockholm',
  pfam       => 'pfam',
  fasta      => 'afa',   # regular, gapped fasta
  fastau     => 'fasta', # ungapped fasta
);                        

sub alignment_format : Chained( 'alignment' ) 
                       PathPart( '' )
                       Args( 1 ) 
                       ActionClass( 'REST' ) { }

sub alignment_format_GET {
  my ( $this, $c, $format ) = @_;

  # the output...
  $c->stash->{output_alignment} = ''; # a slot to hold the output alignment
  $c->stash->{is_gzipped} = 0;        # a flag: is it gzipped ?
  $c->stash->{filename} = '';         # the output filename, if needed

  # what format should we write ?
  if ( $format eq 'jalview' ) {
    # hand off to a jalview tool window
    $c->log->debug( 'Family::alignment_format_GET: format: "jalview"' )
      if $c->debug;

    $c->stash->{template} = 'components/tools/jalview.tt';

    return;
  }

  elsif ( $format eq 'html' ) {
    # builds an HTML page that displays the alignment as paged blocks
    $c->log->debug( 'Family::alignment_format_GET: format: "html"' )
      if $c->debug;

    $c->stash->{template} = 'components/tools/html_alignment.tt';
    $c->forward( 'get_html_alignment' );

    unless ( $c->stash->{alignment_block} ) {
      $c->log->debug( 'Family::alignment_format: failed to retrieve HTML alignment block' )
        if $c->debug;

      $c->stash->{rest}->{error} ||= 'There was a problem retrieving the HTML alignment for '
                                     . $c->stash->{acc};
    }

    return;
  }

  elsif ( $format eq 'colorstock' ) {
    # colorstock: HTML marked-up stockholm
    $c->log->debug( 'Family::alignment_format_GET: format: "colorstock"' )
      if $c->debug;

    $c->forward('get_colorstock_alignment' );

    unless ( $c->stash->{gzipped_alignment} ) {
      $c->log->debug( 'Family::alignment_format: failed to retrieve colorstock alignment' )
        if $c->debug;

      $c->stash->{rest}->{error} ||= 'There was a problem retrieving the colorstock alignment for '
                                     . $c->stash->{acc};
      return;
    }

    $c->stash->{output_alignment} = $c->stash->{gzipped_alignment};
    $c->stash->{is_gzipped}       = 1;
    $c->stash->{filename}         = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.colorstock.html.gz';
    # $c->stash->{gzip}             = 1; # force colorstock to be gzipped
  }

  else {
    # everything else. This bit handles stockholm, pfam, fasta and ungapped fasta
    $c->log->debug( 'Family::alignment_format_GET: raw alignment format' )
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
  }

  # finally, having retrieved an alignment, decide if it needs to be gzipped
  # before being dumped into the response
  my $output;

  # should the output by gzipped ?
  if ( $c->stash->{gzip} ) {
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

  unless ( defined $output ) {
    $c->log->debug( 'Family::alignment_format: failed to gzip/gunzip the alignment' )
      if $c->debug;      

    $c->stash->{rest}->{error} ||= 'There was a problem building the requested alignment for '
                                   . $c->stash->{acc};
    $c->res->status(500); # Internal server error

    return;
  }

  # should we mark the output for download ? (default is to set download header)
  if ( not $c->stash->{view} ) {
    $c->res->header( 'Content-disposition' => 'attachment; filename=' . $c->stash->{filename} );
  }

  # and dump the alignment to the response
  $c->res->body( $output );
}

#---------------------------------------

=head2 old_jalview : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_jalview : Path( '/family/alignment/jalview' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_jalview: redirecting to "jalview"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{viewer};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                     $aln_type, 'jalview', $c->req->params ) );
}

#---------------------------------------

=head2 old_html : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_html : Path( '/family/alignment/html' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_html: redirecting to "html"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{viewer};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                     $aln_type, 'html', $c->req->params ) );
}

#---------------------------------------

=head2 old_gzipped : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_gzipped : Path( '/family/alignment/download/gzipped' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_gzipped: redirecting to "gzipped"' )
    if $c->debug;

  my $aln_type   = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';
  my $colorstock = ( $c->req->param('cs') || 0 ) ? 1 : 0;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{cs};

  if ( $colorstock ) {
    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                       $aln_type, 'colorstock', $c->req->params ) );
  }
  else {
    # firkle with the params. Need to add "gzip=1" to make sure the output 
    # really is gzipped
    my $params = { %{ $c->req->params }, 'gzip' => 1 };

    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                       $aln_type, 'stockholm', $params ) );
  }
}

#---------------------------------------

=head2 old_format : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_format : Path( '/family/alignment/download/format' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_format: redirecting to "format"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';

  my $output_format = $this->{default_output_format};
  if ( defined $c->req->param('format') and
       exists $supported_formats{ $c->req->param('format') } ) {
    $output_format = $c->req->param('format');
  }

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{format};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                     $aln_type, $output_format, $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- private ---------------------------------------------------------------------
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
      $c->res->status( 500 ); # Internal server error

      $c->stash->{rest}->{error} = 'We could not find an image for ' . $c->stash->{acc};
      $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/html' )
                            ? 'components/blocks/family/error.tt'
                            : 'rest/family/error_xml.tt';

      return;
    }

    $c->cache->set( $cache_key, $image ) unless $ENV{NO_CACHE}
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $image );
}

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
    $c->log->debug( "Family::format: generating '$output_format' alignment" )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::format: no supported format specified' )
      if $c->debug;

    $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' )
                          ? 'rest/family/error_xml.tt'
                          : 'components/blocks/family/error.tt';

    $this->status_bad_request( $c, message => 'Not a valid alignment format');

    return;
  }

  #---------------------------------------

  if ( $output_format eq 'stockholm' ) {

    # special case: we can dump out the raw alignment directly if the user
    # wants stockholm format
    $c->log->debug( 'Family::format: stockholm format requested' )
      if $c->debug;

    $c->stash->{output_alignment} = $c->stash->{gzipped_alignment};
    $c->stash->{is_gzipped}       = 1;
    $c->stash->{filename}         = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.stockholm.txt';
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
    my $cmd = $this->{eslreformat_binary} . " -u -r --mingap --informat stockholm $output_format $fn";
    $c->log->debug( "Family::format: running system command: |$cmd|" )
      if $c->debug;

    unless ( open OUTPUT, "$cmd|" ) {    
      $c->log->debug( "Family::format: couldn't run esl-reformat: $!" )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem reformatting the alignment for '
                              . $c->stash->{acc};

      return;
    }

    # stick the output of esl-reformat back together and we're done
    $c->stash->{output_alignment} = join '', <OUTPUT>;
    $c->stash->{is_gzipped}       = 0;
    $c->stash->{filename}         = $c->stash->{acc} . '.' . $c->stash->{alnType} . ".$output_format.txt";

    # tidy up
    close OUTPUT;
    unlink $fn;

    # as far as I can see, this SHOULD work too, and, if it did, would have the 
    # advantage of not requiring any temp files at all...
    #    my $cmd = $this->{eslreformat_binary} . " --informat stockholm $output_format -";
    #    $c->log->debug( "Family::format: running esl-reformat command: |$cmd|" )
    #      if $c->debug;
    #     
    #    my ( $out, $in );
    #    my $pid = open2( $out, $in, $cmd );
    #    unless ( $pid ) {
    #      $c->log->error( "Family::format: failed to run esl-reformat command ($cmd): $!" );
    #      return undef;
    #    }
    #    $c->log->debug( "Family::format: running esl-reformat with PID $pid" )
    #      if $c->debug;
    #
    #    $c->log->debug( 'Family::format: raw alignment: '
    #                    . $c->stash->{alignment} )
    #      if $c->debug;
    #
    #    print $in $c->stash->{alignment};
    #    close $in;
    #    $c->log->debug( 'Family::format: printed input...' )
    #      if $c->debug;
    #
    #    $output = join '', <$out>;
    #
    #    $c->log->debug( "Family::format: esl-reformat output: |$output|" )
    #      if $c->debug;
  }
}

#-------------------------------------------------------------------------------

=head2 get_html_alignment : Private

Retrieves the HTML alignment. We first try to extract the HTML from the cache
or, if that fails, we retrieve it from the DB.

=cut

sub get_html_alignment : Private {
  my ( $this, $c ) = @_;

  # get all of the blocks  
  my @rs = $c->stash->{rfam}->search_related( 'html_alignments',
                                              { type => $c->stash->{alnType} } );

  unless ( scalar @rs ) {
    $c->log->debug( 'Family::get_html_alignment: failed to retrieve an alignment block' )
      if $c->debug;

    $c->stash->{rest}->{error} = 'There was a problem extracting the requested alignment block for '
                                 . $c->stash->{acc};
    return;
  }

  $c->log->debug( 'Family::get_html_alignment: found ' . scalar @rs . ' blocks' )
    if $c->debug;

  # decide which block to show
  my ( $block_num ) = ( $c->req->param('block') || 0 ) =~ m/^(\d+)$/;
  $block_num ||= 0;

  $c->log->debug( "Family::get_html_alignment: showing block $block_num" )
    if $c->debug;

  # gunzip the html
  my $gzipped_html = $rs[$block_num]->html;
  my $block = Compress::Zlib::memGunzip( $gzipped_html );
  unless ( defined $block ) {
    $c->log->debug( 'Family::get_html_alignment: failed to gunzip the alignment block' )
      if $c->debug;

    $c->stash->{rest}->{error} = 'There was a problem uncompressing an alignment block for '
                                 . $c->stash->{acc};
    return;
  }
 
  # stash the stuff that's used for paging in the template
  $c->stash->{alignment_block}   = $block;
  $c->stash->{current_block_num} = $block_num;
  $c->stash->{last_block_num}    = scalar @rs - 1;
}

#-------------------------------------------------------------------------------

=head2 get_gzipped_alignment : Private

Retrieves the gzipped alignment from the database and stashes it, still 
compressed. Caches the gzipped alignment too, if caching is enabled. 

=cut

sub get_gzipped_alignment : Private {
  my ( $this, $c ) = @_;

  my $alnType = $c->stash->{alnType} . ( $c->stash->{nseLabels} ? '' : 'Tax' );
  $c->log->debug( "Family::get_gzipped_alignment: setting alignment type to |$alnType|" )
    if $c->debug;
  
  # first try the cache...
  my $cacheKey = 'gzipped_alignment'
                 . $c->stash->{acc}
                 . $alnType;
  my $gzipped_alignment = $c->cache->get( $cacheKey );

  if ( defined $gzipped_alignment ) {
    $c->log->debug( 'Family::get_gzipped_alignment: extracted gzipped alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::get_gzipped_alignment: failed to extract gzipped alignment from cache; going to DB' )
      if $c->debug;

    # failed to get a cached version; retrieve the alignment from the DB
    my $rs = $c->stash->{rfam}->search_related( 'alignments_and_trees',
                                                { type => $alnType },
                                                { columns => [ 'alignment' ] } );

    # make sure the query returned something
    unless ( defined $rs and
             defined $rs->first ) {
      $c->log->debug( 'Family::get_gzipped_alignment: failed to retrieve a row' )
        if $c->debug;

      $c->stash->{rest}->{error} = 'There was a problem retrieving the alignment data for '
                                   . $c->stash->{acc};

      return;
    }

    # make sure we can get the alignment out of the returned row
    unless ( $gzipped_alignment = $rs->first->alignment ) {
      $c->log->debug( 'Family::get_gzipped_alignment: failed to retrieve an alignment' )
        if $c->debug;

      $c->stash->{rest}->{error} = 'There was a problem uncompressing the alignment data for '
                                   . $c->stash->{acc};

      return;
    }

    # cache the gzipped alignment
    $c->log->debug( 'Family::get_gzipped_alignment: retrieved gzipped alignment from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $gzipped_alignment ) unless $ENV{NO_CACHE};
  }

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
  $c->log->debug( "Family::get_colorstock_alignment: setting alignment type to |$alnType|" )
    if $c->debug;
  
  # first try the cache...
  my $cache_key = 'gzipped_alignment'
                  . $c->stash->{acc}
                  . $alnType;
  my $gzipped_alignment = $c->cache->get( $cache_key );
  
  if ( defined $gzipped_alignment ) {
    $c->log->debug( 'Family::get_colorstock_alignment: extracted gzipped alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::get_colorstock_alignment: failed to extract gzipped alignment from cache; going to DB' )
      if $c->debug;

    # failed to get a cached version; retrieve the alignment from the DB
    my $rs = $c->stash->{rfam}->search_related( 'html_alignments',
                                                { type => $alnType },
                                                { columns => [ 'html' ] } );

    # make sure the query returned something
    unless ( defined $rs ) {
      $c->log->debug( 'Family::get_colorstock_alignment: failed to retrieve a row' )
        if $c->debug;

      $c->stash->{rest}->{error} = 'There was a problem retrieving the colorstock alignment for '
                                   . $c->stash->{acc};

      return;
    }

    # make sure we can retrieve the gzipped HTML alignment from the row object
    $gzipped_alignment = $rs->first->html;

    unless ( defined $gzipped_alignment ) {
      $c->log->debug( 'Family::get_colorstock_alignment: failed to retrieve HTML' )
        if $c->debug;

      $c->stash->{rest}->{error} = 'There was a problem retrieving the HTML colorstock alignment for '
                                   . $c->stash->{acc};

      return;
    }

    # cache the gzipped alignment
    $c->log->debug( 'Family::get_colorstock_alignment: retrieved gzipped alignment from DB' )
      if $c->debug;
    $c->cache->set( $cache_key, $gzipped_alignment ) unless $ENV{NO_CACHE};
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



