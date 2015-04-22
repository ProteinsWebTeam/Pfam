# Alignment.pm
# jt6 20120920 WTSI
#
# $Id$

=head1 NAME

PfamWeb::Roles::Family::Alignment - role to add alignment-specific methods to
the family controller

=cut

package PfamWeb::Roles::Family::Alignment;

=head1 DESCRIPTION

This role adds alignment-specific methods to the L<Family> controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use Text::Wrap;
use Bio::Pfam::AlignPfam;
use JSON;
use Data::UUID;
use Bio::Pfam::ColourAlign;
use Data::Dump qw( dump );

#-------------------------------------------------------------------------------

=head1 ALIGNMENT ACTIONS

=head2 alignment : Chained

Start of a chain for the other methods in this controller. Sets the alignment
type in the stash, based on the "alnType" parameter.

=cut

sub alignment : Chained( 'family' )
                PathPart( 'alignment' )
                CaptureArgs( 1 ) {
  my ( $this, $c, $aln_type ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  $c->stash->{alnType} = 'seed';

  if ( defined $aln_type and
       exists $this->allowed_alignment_types->{ $aln_type } ) {
    $c->stash->{alnType} = $aln_type;
  }

  $c->log->debug( 'Family::alignment: aln_type: |' . $c->stash->{alnType} .'|' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 alignment_link : Chained

An endpoint for the /family/ACC/alignment chain that doesn't need the alignment
type specified. 

=cut

sub alignment_link : Chained( 'family' )
                     PathPart( 'alignment' )
                     CaptureArgs( 0 ) { }

#-------------------------------------------------------------------------------

=head2 raw_alignment : Chained

Dumps the raw alignment to the response as plain text.

=cut

sub raw_alignment : Chained( 'alignment' )
                    PathPart( '' )
                    Args( 0 ) {
	my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  # retrieve the alignment
  $c->forward( 'get_alignment_from_db' );

  $c->res->content_type( 'text/plain' );
  $c->res->body( $c->stash->{alignment} );
}

#-------------------------------------------------------------------------------

=head2 gzipped : Local

Returns a gzip-compressed file with the full or seed alignment for the specified
family. 

=cut

sub gzipped : Chained( 'alignment' )
              PathPart( 'gzipped' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  my ( $alignment, $filename );

  if ( $c->stash->{alnType} eq 'long' ) {
    $c->log->debug( 'Family::gzipped: building full length sequence FASTA' )
      if $c->debug;

    # build the alignment file
    my $rs = $c->model('PfamDB::PfamaRegFullSignificant')
               ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                           in_full    => 1 },
                         { join     => [ qw( pfamseq_acc ) ],
                           prefetch => [ qw( pfamseq_acc ) ],
                           columns  => [ qw( pfamseq_acc.pfamseq_id 
                                             pfamseq_acc.pfamseq_acc
                                             pfmaseq_acc.sequence ) ] } );
    my $sequences = '';
    while ( my $seq_row = $rs->next ) {
      $Text::Wrap::columns = 60;
      $sequences .= '>' . $seq_row->pfamseq_acc->pfamseq_id . ' (' . $seq_row->pfamseq_acc->pfamseq_acc . ")\n";
      $sequences .= wrap( '', '', $seq_row->pfamseq_acc->sequence ) . "\n";
    }

    # compress it
    $alignment = Compress::Zlib::memGzip( $sequences );

    # build a filename for it
    $filename = $c->stash->{acc} . '_full_length_sequences.fasta.gz';
  }
  else {
    # retrieve the alignment
     my $rs = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                            type       => $c->stash->{alnType} },
                          { columns    => [ qw( alignment ) ] } )
                ->single();

    if ( defined $rs ) {
      $alignment = $rs->alignment;
      $filename  = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.gz';
    }
  }

  unless ( defined $alignment ) {
    $c->log->warn( 'Family::gzipped: failed to retrieve alignment for '
                    . $c->stash->{acc} ) if $c->debug;
      
    $c->res->status( 204 ); # "no content"
    
    return;
  } 

  # set the filename on the HTTP headers, so that the browser will offer to 
  # download and save it
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  
  # ... and dump it straight to the response
  $c->res->content_type( 'application/x-gzip' );
  $c->res->body( $alignment );
}

#---------------------------------------

=head2 old_gzipped : Path

This is used by the form in the Pfam family page. The form is currently 
submitted by the browser directly, so there's no javascript to intervene
and convert the parameters into URL arguments. This action will accept 
the parameters and redirect to the Chained action above.

=cut

sub old_gzipped : Path( '/family/alignment/download/gzipped' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $this->allowed_alignment_types->{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_gzipped: redirecting to "gzipped"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/gzipped", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 format : Chained

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

sub format : Chained( 'alignment' )
             PathPart( 'format' )
             Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  # retrieve the alignment
  $c->forward( 'get_alignment_from_db' );

  # we need the alignment as an array ref, so...
  my @alignment = split /\n/, $c->stash->{alignment};
  $c->stash->{alignment_rows} = \@alignment;
  
  $c->log->debug( 'Family::format: got '
                  . scalar @alignment . ' rows in alignment' ) if $c->debug;

  if ( defined $c->stash->{alignment_rows} ) {
    $c->log->debug( 'Family::format: successfully retrieved an alignment' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::format: failed to retrieve an alignment' )
      if $c->debug;
  }

  # drop it into an AlignPfam object
  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $c->stash->{alignment_rows} );
  };
  if ( $@ ) {
    $c->log->debug( "Family::format: problem reading stockholm data: $@" )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem with the alignment data for '
                            . $c->stash->{acc};
    return;
  };

  # gaps param can be default, dashes, dot or none
  if ( $c->req->param('gaps') ) {
    $c->log->debug( 'Family::format: handling gaps parameter' )
      if $c->debug;
      
    if ( $c->req->param('gaps') =~ m/^n\w*/ ) {
      $pfamaln->map_chars('-', '');
      $pfamaln->map_chars('\.', '');
    }
    elsif ( $c->req->param('gaps') =~ m/^do\w*$/ ) {
      $pfamaln->map_chars('-', '.');
    }
    elsif ( $c->req->param('gaps') =~ m/^da\w*$/ ) {
      $pfamaln->map_chars('\.', '-');
    }
  }

  # case param can be u or l
  if ( $c->req->param('case') and $c->req->param('case') =~ m/^u\w*$/ ) {
    $c->log->debug( 'Family::format: uppercasing alignment' )
      if $c->debug;
    $pfamaln->uppercase;
  }

  # order param can be tree or alphabetical
  if ( $c->req->param('order') and $c->req->param('order') =~ m/^a\w*$/ ) {
    $c->log->debug( 'Family::format: sorting alphabetically' )
      if $c->debug;
    $pfamaln->sort_alphabetically;
  }

  # format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if ( $c->req->param( 'format' ) ) {
    if ( $c->req->param( 'format' ) =~ m/^p\w*$/ ) {
      $c->log->debug( 'Family::format: writing Pfam format' )
        if $c->debug;
      $output = $pfamaln->write_Pfam;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^f\w*$/ ) {
      $c->log->debug( 'Family::format: writing FASTA format' )
        if $c->debug;
      $output = $pfamaln->write_fasta;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^m\w*$/ ) {
      $c->log->debug( 'Family::format: writing MSF format' )
        if $c->debug;
      $output = $pfamaln->write_MSF;
    }
  }

  # default to writing stockholm
  $output ||= $pfamaln->write_stockholm;

  # are we downloading this or just dumping it to the browser ?
  if ( $c->req->param( 'download' ) ) {
    $c->log->debug( 'Family::format: sending alignment as download' )
      if $c->debug;

    my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.txt';
  
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  $c->res->content_type( 'text/plain' );
  $c->stash->{serialize_to_content_type} = 'text/plain';
  $c->res->body( join '', @$output );
}

#---------------------------------------

=head2 old_format : Path

This is used by the form in the Pfam family page. The form is currently 
submitted by the browser directly, so there's no javascript to intervene
and convert the parameters into URL arguments. This action will accept 
the parameters and redirect to the Chained action above.

=cut

sub old_format : Path( '/family/alignment/download/format' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $this->allowed_alignment_types->{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_format: redirecting to "format"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/format", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 html : Chained

Retrieves the HTML alignment and dumps it to the response. We first try to 
extract the HTML from the cache or, if that fails, we retrieve it from the DB.

=cut

sub html : Chained( 'alignment' )
           PathPart( 'html' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';
  
  my $cacheKey = 'jtml' . $c->stash->{acc} . $c->stash->{alnType};
  
  my $jtml = $c->cache->get( $cacheKey );
  if ( defined $jtml ) {
    $c->log->debug( 'Family::html: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::html: failed to extract HTML from cache; going to DB' )
      if $c->debug;  

    # retrieve the HTML from the DB
    my $row = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                            type       => $c->stash->{alnType} },
                           { columns    => [ qw( jtml ) ] } )
                ->single;
  
    # final check...
    unless ( $row and
             defined $row->jtml ) {
      $c->log->debug( 'Family::html: failed to retrieve JTML' )
        if $c->debug;  

      $c->stash->{errorMsg} = 'We do not have the HTML alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress the row to get the raw HTML
    $jtml = Compress::Zlib::memGunzip( $row->jtml );
    unless ( defined $jtml ) {
      $c->stash->{errorMsg} = 'We could not extract the HTML alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Family::html: retrieved HTML from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $jtml ) unless $ENV{NO_CACHE};
  }

  # stash the HTML
  $c->stash->{html_alignment} = $jtml;
  
}

#---------------------------------------

=head2 old_html : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_html : Path( '/family/alignment/download/html' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';

  if ( defined $c->req->param('alnType') and
       exists $this->allowed_alignment_types->{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_html: redirecting to "html"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/html", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 heatmap : Chained

Retrieves the HTML "heatmap" coloured alignment and dumps it to the response. 
We first try to extract the HTML from the cache or, if that fails, we retrieve 
it from the DB.

=cut

sub heatmap : Chained( 'alignment' )
              PathPart( 'heatmap' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';
  $c->stash->{heatmap}  = 1; # flag that this is a heatmap rather than a regular alignment
  
  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  my $cacheKey = 'heatmap' . $c->stash->{acc};
  
  my $hm = $c->cache->get( $cacheKey );
  if ( defined $hm ) {
    $c->log->debug( 'Family::heatmap: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::heatmap: failed to extract HTML from cache; going to DB' )
      if $c->debug;  

    # retrieve the HTML from the DB
    my $row = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                            type       => $c->stash->{alnType} }, 
                          { columns    => [ qw( post ) ] } )
                ->single;
  
    unless ( $row and
             defined $row->post ) {
      $c->log->debug( 'Family::heatmap: failed to retrieve heatmap for ' . $c->stash->{alnType} )
        if $c->debug;  
      $c->stash->{errorMsg} = 'We do not have the ' . $c->stash->{alnType} 
                              . ' heatmap alignment for ' . $c->stash->{acc};
      return;
    }

    # uncompress the row to get the raw HTML
    $hm = Compress::Zlib::memGunzip( $row->post );
    unless ( defined $hm ) {
      $c->stash->{errorMsg} = 'We could not extract the heatmap alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Family::heatmap: retrieved HTML from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $hm ) unless $ENV{NO_CACHE};
  }

  # stash the HTML
  $c->stash->{html_alignment} = $hm;  
}

#---------------------------------------

=head2 old_heatmap : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_heatmap : Path( '/family/alignment/download/heatmap' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_heatmap: redirecting to "heatmap"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment/full/heatmap', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 jalview : Chained

This is the way into the JalView alignment viewer applet.

Hands straight off to a template that generates a "tool" page containing the 
JalView applet.

=cut

sub jalview : Chained( 'alignment' )
              PathPart( 'jalview' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  $c->stash->{template} = 'components/tools/jalview.tt';
}

#---------------------------------------

=head2 old_jalview : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_jalview : Path( '/family/alignment/jalview' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $this->allowed_alignment_types->{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_jalview: redirecting to "jalview"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/jalview", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 das_viewer : Chained

=cut

sub dasviewer : Chained( 'alignment' )
                PathPart( 'dasviewer' )
                Args( 0 ) {
  my ( $self, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  # build a "title" string, which will be used as the heading for the
  # alignment tool window
  my $title = 'Pfam ' . $c->stash->{alnType} . ' alignment for '
              . $c->stash->{acc};

  # find out how many rows are in the alignment
  my $num_rows = ( $c->stash->{alnType} eq 'seed' )
                 ? $c->stash->{pfam}->num_seed
                 : $c->stash->{pfam}->num_full;

  my $das_source = $c->stash->{ alnType } eq 'seed' 
                 ? 'Pfam_Seed_Alignments' 
                 : 'Pfam_Full_Alignments' ;

  $c->log->debug( 'Family::dasviewer: setting up for get_das_alignment' )
    if $c->debug;

  $c->stash->{params} = { source             => 'family',
                          dasSource          => $das_source,
                          title              => $title,
                          acc                => $c->stash->{acc},
                          alnType            => $c->stash->{alnType},
                          numRowsInAlignment => $num_rows };

  # now stash the template which will have the necessary params to
  $c->stash->{ template } = 'components/tools/pfamviewer/dasviewer.tt';
}

#---------------------------------------

=head2 old_dasviewer : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

# http://pfam.sanger.ac.uk/family/alignment/dasviewer?acc=PF02171&alnType=full&viewer=viewer

sub old_dasviewer : Path( '/family/alignment/dasviewer' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $this->allowed_alignment_types->{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_dasviewer: redirecting to "dasviewer"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/dasViewer", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 build : Path

Builds a sequence alignment from the specified sequences. The "jobId" parameter
in this action refers to the list of accessions that were previously stored in
the species_collection table, rather than the job that we run to align them.

=cut

sub build : Chained( 'alignment_link' )
            PathPart( 'build' )
            CaptureArgs( 0 ) { }
  
sub build_alignment_from_selection : Chained( 'build' )
                                     PathPart( '' )
                                     Args( 0 ) {
  my ( $this, $c ) = @_;
  
  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  $c->log->debug( 'Family::build: checking for sequences' )
    if $c->debug;

  unless ( $c->req->param('jobId') ) {
    $c->log->debug( 'Family::build: no job ID supplied' )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was no job ID for this alignment..';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # validate the UUID
  my $collection_id = $c->req->param('jobId');
  unless ( $collection_id =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( 'Family::build: bad job id' )
      if $c->debug;
    $c->stash->{errorMsg} = 'Invalid job ID';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # retrieve the sequences
  $c->stash->{fasta} = $c->forward( '/utils/get_sequences', 
                                    [ $collection_id, $c->stash->{pfam} ] );
  
  # make sure we got something...
  unless ( length $c->stash->{fasta} ) {
    $c->log->debug( 'Family::build: failed to get a FASTA sequence' )
      if $c->debug;
    $c->stash->{errorMsg} = 'We failed to get a FASTA format sequence file for your selected sequences.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # submit the job to actually build the alignment
  my $submissionStatus = $c->forward( 'queueAlignment' );

  # and see if we managed it...
  if ( $submissionStatus < 0 ) {
    $c->log->debug( 'Family::build: problem with submission; returning error page' )
      if $c->debug; 
    $c->stash->{errorMsg} = 'There was an error when submitting your sequences to be aligned.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
  }
  else {
    $c->log->debug( 'Family::build: alignment job submitted; polling' )
      if $c->debug; 
    $c->stash->{template} = 'components/tools/seqViewAlignmentPolling.tt';
  }
}

#---------------------------------------

=head2 old_build : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_build : Path( '/family/alignment/builder' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_build: redirecting to "build"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment/build', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 view : Local

Retrieves the sequence alignment that we generated.

=cut

sub view : Chained( 'alignment_link' )
           PathPart( 'view' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  # retrieve the job results
  my ( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/i;
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );
  
  unless ( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::view: no results found' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }   

  # count the number of rows in the alignment. The raw alignment includes 
  # the consensus string as the last line
  my @rows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};
  my $numRowsInAlignment = scalar @rows - 1;
  $c->log->debug( "Family::view: alignment has |$numRowsInAlignment| rows" )
    if $c->debug;

  # configure the viewer...
  $c->stash->{params} = { source             => 'species',
                          title              => 'Alignment for selected sequences',
                          jobId              => $jobId,
                          acc                => $c->stash->{acc},
                          numRowsInAlignment => $numRowsInAlignment };

  # and hand off to it
  $c->forward( 'PfamViewer', 'showPfamViewer' );
}

#---------------------------------------

=head2 old_view : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_view : Path( '/family/alignment/builder/view' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_view: redirecting to "view"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment/view', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 save_built_alignment : Chained('build') PathPart('download') Args(0)

Retrieves the sequence alignment that we generated and serves it, raw, in the 
response body.

=cut

sub save_built_alignment : Chained( 'build' )
                           PathPart( 'download' )
                           Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  # retrieve the job results
  my ( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/i;
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );
  
  unless ( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::save_built_alignment: no results found' )
      if $c->debug;
    $c->res->status( 404 ); # Not found
    $c->res->body( 'No sequence alignment found.' );
    return;
  }   

  $c->log->debug( 'Family::save_built_alignment: returning raw alignment' )
    if $c->debug;

  my $filename = 'alignment_of_selected_' . $c->stash->{acc} . '_sequences.txt';

  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $c->stash->{results}->{$jobId}->{rawData} );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 PRIVATE ACTIONS

=head2 get_das_alignment : Private

Retrieves a family alignment, seed or full, from the DAS sources.

=cut

sub get_das_alignment : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Family::get_das_alignment: retrieving alignment' )
    if $c->debug;

  # set the DAS dsn based on the alignment type parameter
  my $dsn = ( $c->stash->{alnType} eq 'seed' )
            ? $this->{urls}->{seed}
            : $this->{urls}->{full};

  if ( $c->debug ) {
    $c->log->debug( 'Family::get_das_alignment: dsn:  |' . $dsn . '|' ); 
    $c->log->debug( 'Family::get_das_alignment: acc:  |' . $c->stash->{acc} . '|' ); 
    $c->log->debug( 'Family::get_das_alignment: rows: |' . $c->stash->{rows} . '|' ); 
  }

  # retrieve the DasLite client from the base model class and hand it the DSN
  my $dl = $c->model('PfamDB')->getDasLite;
  $dl->dsn( $dsn );

  # put the rows specification into the right format for DAS
  my $rows = $c->stash->{rows}->[0] . '-' . $c->stash->{rows}->[1];

  # retrieve the raw alignment fragment and associated features via DAS and 
  # generate the consensus sequence 

  # retrieve the raw alignment from the DAS source
  my $raw_alignment  = $dl->alignment( { query => $c->stash->{acc},
                                         rows  => $rows } );

  # build the marked-up alignment
  my ( $alignment, $alignment_lengths ) = reconstruct_alignment( $raw_alignment );

  # retrieve the features  
  my $features_hash = $dl->features( $c->stash->{acc} );

  my ( $source, $features ) = each %$features_hash;
  my $label = $features->[0]->{feature_label};  
 
  # build the consensus string
  my $consensus = [ Bio::Pfam::ColourAlign::parseConsensus( $label ) ];
  
  # stash the arrays of alignments, alignment lengths and consensus strings  
  $c->stash->{alignments}->{rawAlignments} = $alignment;
  $c->stash->{alignments}->{lengths}       = $alignment_lengths;
  $c->stash->{alignments}->{consensus}     = $consensus;
}

#-------------------------------------------------------------------------------

=head2 get_alignment_from_db : Private

Retrieves the complete alignment ("seed", "full", etc.) from the database.

=cut

sub get_alignment_from_db : Private {
  my ( $this, $c ) = @_;

  # see if we can extract the raw alignment from the cache first
  my $cacheKey  = 'alignment' . $c->stash->{acc} . $c->stash->{alnType};
  my $alignment = $c->cache->get( $cacheKey );

  if ( defined $alignment ) {
    $c->log->debug( 'Family::get_alignment_from_db: extracted alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::get_alignment_from_db: failed to extract alignment from cache; going to DB' )
      if $c->debug;

    # retrieve the alignment from the DB
    my $row = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                            type       => $c->stash->{alnType} },
                          { columns    => [ qw( alignment ) ] } )
                ->single;

    unless ( defined $row and defined $row->alignment ) {

      $c->log->warn( 'Family::get_alignment_from_db: failed to retrieve '
        . $c->stash->{alnType} . ' alignment for ' . $c->stash->{acc} )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress it
    $alignment = Compress::Zlib::memGunzip( $row->alignment );
    unless ( defined $alignment ) {

      $c->log->warn( 'Family::get_alignment_from_db: failed to uncompress '
        . $c->stash->{alnType} . ' alignment for ' . $c->stash->{acc} )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }
  
    # cache the raw alignment
    $c->cache->set( $cacheKey, $alignment ) unless $ENV{NO_CACHE};
  }

	# store the raw text alignment
	$c->stash->{alignment} = $alignment;

  $c->log->debug( 'Family::get_alignment_from_db: stashed raw alignment' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 getAlignment : Private

Builds an alignment of selected sequences. The set of sequences is identified
by a job ID that's used by the L<JobManager> to retrieve them from the DB. 

=cut

sub getAlignment : Private {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::getAlignment: retrieving alignment...' )
    if $c->debug;

  # first get a job ID. The call to retrieve results will get the job ID for
  # itself, but we'll need it here anyway
  my ( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/i;

  unless ( defined $jobId ) {
    $c->log->debug( 'Family::getAlignment: no job ID found' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No job ID found for the sequence alignment job.';
    return;
  }   

  # retrieve the job results
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );
  unless ( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::getAlignment: no results found' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    return;
  }   

  # $c->log->debug( 'Family::getAlignment: job results: |'
  #                 . $c->stash->{results}->{$jobId}->{rawData} . '|' );

  # the rawData is just a string containing the alignment lines
  my @alignmentRows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};
  
  # the consensus string is the last row of the alignment
  my $consensusString = pop @alignmentRows;
  $consensusString =~ s/^ConSeq\s+(\S+)$/$1/;
  
  # take a slice of that array, based on the "rows" setting from PfamViewer.
  # Rows are numbered from 1, not zero, so we need to offset the row values
  my $from = $c->stash->{rows}->[0] - 1;
  my $to   = $c->stash->{rows}->[1] - 1;
  # $c->log->debug( 'Family::getAlignment: showing rows |'
  #                 . "$from| to |$to|" );
  
  my %alignment;
  my $length;
  foreach ( @alignmentRows[ $from .. $to ] ) {
    next unless m|(\S+/\d+\-\d+)\s+(\S+)|;
    $alignment{$1} = $2;
    $length++;
  }
  
  # parse the consensus string
  my $consensus = Bio::Pfam::ColourAlign::parseConsensus( $consensusString );
 
  # stash everything
  $c->stash->{alignments}->{rawAlignments} = [ \%alignment ];
  $c->stash->{alignments}->{lengths}       = [ $length ];
  $c->stash->{alignments}->{consensus}     = [ $consensus ];
}

#-------------------------------------------------------------------------------

=head2 queueAlignment : Private

Queues the job that will actually generate the sequence alignment.

=cut

sub queueAlignment : Private {
  my($this, $c) = @_; 

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # set the options
  my $opts = '-acc ' . $c->stash->{acc} . '.' . $c->stash->{pfam}->version; 

  # guesstimate the time it will take to build the alignment
  my $estimatedTime = int( 1 + ( $c->stash->{numRows} / 100 ) ); 

  # add this job to the tracking tables
  my $jobHistory = $c->model('WebUser::JobHistory')
                     ->create( { options        => $opts,
                                 job_type       => 'align',
                                 estimated_time => $estimatedTime,
                                 job_id         => $jobId,
                                 opened         => \'NOW()',
                                 status         => 'PEND' } );

  my $jobStream = $c->model('WebUser::JobStream')
                    ->create( { id    => $jobHistory->id,
                                stdin => $c->stash->{fasta} || q() } );

  # check the submission time with a separate query
  my $historyRow = $c->model( 'WebUser::JobHistory' )
                     ->find( { id => $jobHistory->id } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side. Because the queuing system allows
  # multiple Jobs in one page, the jobStatus JSON string needs to be an array
  # of hashes, each of which gives details of a separate job
  my $jobStatus = [
                    {
                      checkURI      => $c->uri_for( '/jobmanager/checkStatus' )
                                         ->as_string,
                      doneURI       => $c->uri_for( '/family/'.$c->stash->{acc}.'/alignment/view' )->as_string,
                      estimatedTime => $estimatedTime,
                      interval      => $this->{pollingInterval},
                      jobId         => $jobId,
                      name          => 'Sequence alignment',
                      jobClass      => 'alignment',
                      opened        => $historyRow->opened,
                    }
                  ];
  # $c->stash->{jobStatusJSON} = to_json( $jobStatus );
  my $json = JSON::XS->new->utf8->convert_blessed;
  $c->stash->{jobStatusJSON} = $json->encode( $jobStatus );

  $c->log->debug( 'Family::queueAlignment: job status: ',
                  dump( $jobStatus ) ) if $c->debug;
  $c->log->debug( 'Family::queueAlignment: submitted job '
                  . "|$jobId| at |" . $historyRow->opened . '|' ) if $c->debug;
                  
  return 0;
} 

#-------------------------------------------------------------------------------
#- regular perl methods (not actions) ------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 reconstruct_alignment

Reconstructs a blocked alignment from a raw alignment.

=cut

sub reconstruct_alignment {
  my $ali = shift;
  my ( $source, $aliData) = each %$ali;

  my ( @alignments, @alignmentLengths );

  for ( my $i = 0; $i < scalar( @$aliData ); $i++ ) {
    my %aliObjects = 
      map{ $_->{alignobject_intObjectId} => $_ } @{ $aliData->[$i]->{alignobject} };
  
    push @alignmentLengths, $aliData->[$i]->{alignment_max};
  
    foreach my $block ( sort { $a->{block_blockOrder} <=> $b->{block_blockOrder} }
                             @{$aliData->[$i]->{block} } ) {
      my %ali;
      foreach my $bseqRef (@{ $block->{segment} } ) {
  
        my $key = $bseqRef->{segment_intObjectId} . '/' . 
                  $bseqRef->{segment_start}       . '-' . 
                  $bseqRef->{segment_end};
    
        $ali{$key} = get_alignment_string($bseqRef, \%aliObjects);
      }
      push @alignments, \%ali;
    }
  }
  return \@alignments, \@alignmentLengths;
}

#-------------------------------------------------------------------------------

=head2 get_alignment_string

Gets the alignment string from the alignment.

=cut

sub get_alignment_string {
  my ( $bseqRef, $aliObjectsRef ) = @_;

  my $seqStr = $aliObjectsRef->{ $bseqRef->{segment_intObjectId} }->{sequence};

  my $seq = substr( $seqStr,
                    $bseqRef->{segment_start} - 1,
                    $bseqRef->{segment_end} - $bseqRef->{segment_start} + 1 );

  return cigar_to_alignment( $bseqRef->{cigar}, $seq );
}

#-------------------------------------------------------------------------------

=head2 cigar_to_alignment

Converts a cigar string into an alignment row.

=cut

sub cigar_to_alignment {
  my $cigar = shift;

  $cigar =~ s/\"//g;

  my $seq = shift;
  my $tmp = $cigar;
  my $start = 0;
  my $len = length($seq);

  $tmp =~ s/(\d+)D/'-'x$1/eg;
  $tmp =~ s/D/\-/g;
  $tmp =~ s/(\d+)I/'.'x$1/eg;
  $tmp =~ s/I/\./g;

  $tmp =~ s/(\d{0,5})M/if($1){$start+=$1,($start<=$len)?substr($seq,$start-$1,$1):'~'x$1}else{$start+=1,($start<=$len)?substr($seq,$start-1,1):'~'}/eg;

  return $tmp;
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

