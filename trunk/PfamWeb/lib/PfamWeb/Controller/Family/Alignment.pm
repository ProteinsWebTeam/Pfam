
# Alignment.pm
# jt6 20070725 WTSI
#
# $Id: Alignment.pm,v 1.6 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment - a base class for alignment handling

=cut

package PfamWeb::Controller::Family;

=head1 DESCRIPTION

This is intended as the basis for alignment-related code.

$Id: Alignment.pm,v 1.6 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;
use Text::Wrap;
use Bio::Pfam::AlignPfam;

use Data::Dump qw( dump );

# use base 'PfamWeb::Controller::Family';
use base 'Catalyst::Controller';

my %allowed_alignment_types = ( full => 1,
                                seed => 1,
                                ncbi => 1,
                                meta => 1,
                                long => 1 );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 alignment : Chained

Start of a chain for the other methods in this controller. Sets the alignment
type in the stash, based on the "alnType" parameter.

=cut

sub alignment : Chained( 'family' )
                PathPart( 'alignment' )
								CaptureArgs( 1 ) {
  my ( $this, $c, $aln_type ) = @_;

  $c->stash->{alnType} = 'seed';

  if ( defined $aln_type and
       exists $allowed_alignment_types{ $aln_type } ) {
    $c->stash->{alnType} = $aln_type;
  }

  $c->log->debug( 'Family::Alignment::alignment: aln_type: |' . $c->stash->{alnType} .'|' )
    if $c->debug;
}

sub raw_alignment : Chained( 'alignment' )
                    PathPart( '' )
                    Args( 0 ) {
	my ( $this, $c ) = @_;

  # retrieve the alignment
  $c->forward( 'get_alignment_from_db' );

  $c->res->content_type( 'text/plain' );
  $c->res->body( $c->stash->{alignment} );
}

#-------------------------------------------------------------------------------
#- raw alignment methods -------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 gzipped : Local

Returns a gzip-compressed file with the full or seed alignment for the specified
family. 

=cut

sub gzipped : Chained( 'alignment' )
              PathPart( 'gzipped' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  my ( $alignment, $filename );

  if ( $c->stash->{alnType} eq 'long' ) {
    $c->log->debug( 'Family::Alignment::Download::gzipped: building full length sequence FASTA' )
      if $c->debug;

    # build the alignment file
    my $rs = $c->model('PfamDB::PfamaRegFullSignificant')
               ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama },
                         { prefetch => [ qw( auto_pfamseq ) ],
                           columns  => [ qw( auto_pfamseq.pfamseq_id 
                                             auto_pfamseq.pfamseq_acc
                                             auto_pfamseq.sequence ) ] } );
    my $sequences = '';
    while ( my $seq_row = $rs->next ) {
      $Text::Wrap::columns = 60;
      $sequences .= '>' . $seq_row->pfamseq_id . ' (' . $seq_row->pfamseq_acc . ")\n";
      $sequences .= wrap( '', '', $seq_row->sequence ) . "\n";
    }

    # compress it
    $alignment = Compress::Zlib::memGzip( $sequences );

    # build a filename for it
    $filename = $c->stash->{acc} . '_full_length_sequences.fasta.gz';
  }
  else {
    # retrieve the alignment
     my $rs = $c->model('PfamDB::AlignmentsAndTrees')
                ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                            type       => $c->stash->{alnType} },
                          { columns    => [ qw( alignment ) ] } )
                ->single();

    $alignment = $rs->alignment;
    $filename  = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.gz';
  }

  unless ( defined $alignment ) {
    $c->log->warn( 'Family::Alignment::Download::gzipped: failed to retrieve alignment for '
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
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::Alignment::old_gzipped: redirecting to "gzipped"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/'.$c->stash->{param_entry}."/alignment/$aln_type/gzipped" ) );
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

  # retrieve the alignment
  $c->forward( 'get_alignment_from_db' );

  if ( defined $c->stash->{alignment_rows} ) {
    $c->log->debug( 'Family::Alignment::format: successfully retrieved an alignment' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::format: failed to retrieve an alignment' )
      if $c->debug;
  }

  # drop it into an AlignPfam object
  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $c->stash->{alignment_rows} );
  };
  if ( $@ ) {
    $c->log->debug( "Family::Alignment::format: problem reading stockholm data: $@" )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem with the alignment data for '
                            . $c->stash->{acc};
    return;
  };

  # gaps param can be default, dashes, dot or none
  if ( $c->req->param('gaps') ) {
    $c->log->debug( 'Family::Alignment::format: handling gaps parameter' )
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
    $c->log->debug( 'Family::Alignment::format: uppercasing alignment' )
      if $c->debug;
    $pfamaln->uppercase;
  }

  # order param can be tree or alphabetical
  if ( $c->req->param('order') and $c->req->param('order') =~ m/^a\w*$/ ) {
    $c->log->debug( 'Family::Alignment::format: sorting alphabetically' )
      if $c->debug;
    $pfamaln->sort_alphabetically;
  }

  # format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if ( $c->req->param( 'format' ) ) {
    if ( $c->req->param( 'format' ) =~ m/^p\w*$/ ) {
      $c->log->debug( 'Family::Alignment::format: writing Pfam format' )
        if $c->debug;
      $output = $pfamaln->write_Pfam;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^f\w*$/ ) {
      $c->log->debug( 'Family::Alignment::format: writing FASTA format' )
        if $c->debug;
      $output = $pfamaln->write_fasta;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^m\w*$/ ) {
      $c->log->debug( 'Family::Alignment::format: writing MSF format' )
        if $c->debug;
      $output = $pfamaln->write_MSF;
    }
  }

  # default to writing stockholm
  $output ||= $pfamaln->write_stockholm;

  # are we downloading this or just dumping it to the browser ?
  if ( $c->req->param( 'download' ) ) {
    $c->log->debug( 'Family::Alignment::format: sending alignment as download' )
      if $c->debug;

    my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.txt';
  
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  $c->res->content_type( 'text/plain' );
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
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::Alignment::old_format: redirecting to "format"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} .
                                  "/alignment/$aln_type/format",
                                  $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- alignment presentation methods ----------------------------------------------
#-------------------------------------------------------------------------------

=head2 html : Chained

Retrieves the HTML alignment and dumps it to the response. We first try to 
extract the HTML from the cache or, if that fails, we retrieve it from the DB.

=cut

sub html : Chained( 'alignment' )
           PathPart( 'html' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';
  
  my $cacheKey = 'jtml' . $c->stash->{acc} . $c->stash->{alnType};
  
  my $jtml = $c->cache->get( $cacheKey );
  if ( defined $jtml ) {
    $c->log->debug( 'Family::Alignment::Download::html: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::Download::html: failed to extract HTML from cache; going to DB' )
      if $c->debug;  

    # retrieve the HTML from the DB
    my $row = $c->model('PfamDB::AlignmentsAndTrees')
                ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                            type       => $c->stash->{alnType} },
                           { columns    => [ qw( jtml ) ] } )
                ->single;
  
    # final check...
    unless ( defined $row->jtml ) {
      $c->log->debug( 'Family::Alignment::Download::html: failed to retrieve JTML' )
        if $c->debug;  

      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress the row to get the raw HTML
    $jtml = Compress::Zlib::memGunzip( $row->jtml );
    unless ( defined $jtml ) {
      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Family::Alignment::Download::html: retrieved HTML from DB' )
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
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::Alignment::old_html: redirecting to "html"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/'.$c->stash->{param_entry}."/alignment/$aln_type/html" ) );
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

  # heatmaps are only available for full alignments
  if ( $c->stash->{alnType} ne 'full' ) {
    $c->stash->{errorMsg} = 'Heatmaps are only available for full alignments';
    return;
  }

  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';
  $c->stash->{alnType}  = 'heatmap';
  
  my $cacheKey = 'heatmap' . $c->stash->{acc};
  
  my $hm = $c->cache->get( $cacheKey );
  if ( defined $hm ) {
    $c->log->debug( 'Family::Alignment::Download::heatmap: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::Download::heatmap: failed to extract HTML from cache; going to DB' )
      if $c->debug;  

    # retrieve the HTML from the DB
    my $row = $c->model('PfamDB::AlignmentsAndTrees')
                ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                            type       => 'full' }, 
                          { columns    => [ qw( post ) ] } )
                ->single;
  
    # final check...
    unless ( defined $row->post ) {
      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress the row to get the raw HTML
    $hm = Compress::Zlib::memGunzip( $row->post );
    unless ( defined $hm ) {
      $c->stash->{errorMsg} = 'We could not extract the heatmap alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Family::Alignment::Download::heatmap: retrieved HTML from DB' )
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

  $c->log->debug( 'Family::Alignment::old_heatmap: redirecting to "heatmap"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/'.$c->stash->{param_entry}."/alignment/full/heatmap" ) );
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
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::Alignment::old_jalview: redirecting to "jalview"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry}
                     . "/alignment/$aln_type/jalview" ) );
}

#-------------------------------------------------------------------------------

=head2 das_viewer : Chained

=cut

sub dasviewer : Chained( 'alignment' )
                PathPart( 'dasviewer' )
                Args( 0 ) {
  my ( $self, $c ) = @_;

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

  $c->log->debug( 'Family::Alignment::dasviewer: setting up for get_das_alignment' )
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

# http://web-vm-pfam.internal.sanger.ac.uk:3000/family/alignment/dasviewer?acc=PF02171&alnType=full&viewer=viewer

sub old_dasviewer : Path( '/family/alignment/dasviewer' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::Alignment::old_dasviewer: redirecting to "dasviewer"' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family/' . $c->stash->{param_entry} .
                                  "/alignment/$aln_type/dasviewer",
                                  $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_das_alignment : Private

Retrieves a family alignment, seed or full, from the DAS sources.

=cut

sub get_das_alignment : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Family::Alignment::get_das_alignment: retrieving alignment' )
    if $c->debug;

  # set the DAS dsn based on the alignment type parameter
  my $dsn = ( $c->stash->{alnType} eq 'seed' )
            ? $this->{urls}->{seed}
            : $this->{urls}->{full};

  if ( $c->debug ) {
    $c->log->debug( 'Family::Alignment::get_das_alignment: dsn:  |' . $dsn . '|' ); 
    $c->log->debug( 'Family::Alignment::get_das_alignment: acc:  |' . $c->stash->{acc} . '|' ); 
    $c->log->debug( 'Family::Alignment::get_das_alignment: rows: |' . $c->stash->{rows} . '|' ); 
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
    $c->log->debug( 'Family::Alignment::get_alignment_from_db: extracted alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::get_alignment_from_db: failed to extract alignment from cache; going to DB' )
      if $c->debug;

    # retrieve the alignment from the DB
    my $row = $c->model('PfamDB::AlignmentsAndTrees')
                ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                            type       => $c->stash->{alnType} },
                          { columns    => [ qw( alignment ) ] } )
                ->single;

    unless ( defined $row and defined $row->alignment ) {

      $c->log->warn( 'Family::Alignment::get_alignment_from_db: failed to retrieve '
        . $c->stash->{alnType} . ' alignment for ' . $c->stash->{acc} )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress it
    $alignment = Compress::Zlib::memGunzip( $row->alignment );
    unless ( defined $alignment ) {

      $c->log->warn( 'Family::Alignment::get_alignment_from_db: failed to uncompress '
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

  # we need the alignment as an array ref, so...
  my @alignment = split /\n/, $alignment;
  $c->stash->{alignment_rows} = \@alignment;
  
  $c->log->debug( 'Family::Alignment::get_alignment_from_db: got '
                  . scalar @alignment . ' rows in alignment' ) if $c->debug;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

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
