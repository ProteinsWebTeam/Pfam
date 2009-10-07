
# DownloadAlignment.pm
# rdf 20061005 WTSI
#
# $Id: Download.pm,v 1.10 2009-10-07 12:10:45 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::DownloadAlignment - controller to
serve up alignments in various formats

=cut

package PfamWeb::Controller::Family::Alignment::Download;

=head1 DESCRIPTION

Generates a B<full page>.

$Id: Download.pm,v 1.10 2009-10-07 12:10:45 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;
use Text::Wrap;

use base 'PfamWeb::Controller::Family::Alignment';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 html : Path

Retrieves the HTML alignment and dumps it to the response. We first try to 
extract the HTML from the cache or, if that fails, we retrieve it from the DB.

=cut

sub html : Local {
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

    # see what type of family we have, A or B
    my $row;
    if ( $c->stash->{entryType} eq 'A' ) {
  
      # retrieve the HTML from the DB
      $row = $c->model('PfamDB::AlignmentsAndTrees')
               ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                           type       => $c->stash->{alnType} } )
               ->single;
    
      # final check...
      unless ( defined $row->jtml ) {
        $c->log->debug( 'Family::Alignment::Download::html: failed to retrieve JTML' )
          if $c->debug;  

        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }
  
    }
    elsif ( $c->stash->{entryType} eq 'B' ) {

      # make sure the Pfam-B HTML is already available
      unless ( defined $c->stash->{pfam}->pfamb_stockholms->single ) {
        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }

      $row = $c->stash->{pfam}->pfamb_stockholms->single;
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

#-------------------------------------------------------------------------------

=head2 heatmap : Local

Retrieves the HTML "heatmap" coloured alignment and dumps it to the response. 
We first try to extract the HTML from the cache or, if that fails, we retrieve 
it from the DB.

=cut

sub heatmap : Local {
  my ( $this, $c ) = @_;

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

    # see what type of family we have, A or B
    my $row;
    if ( $c->stash->{entryType} eq 'A' ) {
  
      # retrieve the HTML from the DB
      $row = $c->model('PfamDB::AlignmentsAndTrees')
               ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                           type       => 'full' } )
               ->single;
    
      # final check...
      unless ( defined $row->post ) {
        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }
  
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

#-------------------------------------------------------------------------------

=head2 gzipped : Local

Returns a gzip-compressed file with the full or seed alignment for the specified
family. 

=cut

sub gzipped : Local {
  my( $this, $c ) = @_;
  
  my ( $alignment, $filename );

  if ( $c->stash->{alnType} eq 'long' ) {
    $c->log->debug( 'Family::Alignment::Download::gzipped: building full length sequence FASTA' )
      if $c->debug;

    # build the alignment file
    my @rs = $c->model('PfamDB::PfamaRegFullSignificant')
               ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama },
                         { prefetch => [ 'auto_pfamseq' ] } );
    my $sequences = '';
    foreach my $seq_row ( @rs ) {
      $Text::Wrap::columns = 60;
      $sequences .= '> ' . $seq_row->pfamseq_id . ' (' . $seq_row->pfamseq_acc . ")\n";
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
                            type       => $c->stash->{alnType} } )
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
  my ( $this, $c ) = @_;

  # retrieve the alignment
  $c->forward( 'getAlignment' );

  if ( defined $c->stash->{alignment} ) {
    $c->log->debug( 'Family::Alignment::Download::format: successfully retrieved an alignment' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::Download::format: failed to retrieve an alignment' )
      if $c->debug;
  }

  # drop it into an AlignPfam object
  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $c->stash->{alignment} );
  };
  if ( $@ ) {
    $c->log->debug( "Family::Alignment::Download::format: problem reading stockholm data: $@" )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem with the alignment data for '
                            . $c->stash->{acc};
    return;
  };

  # gaps param can be default, dashes, dot or none
  if ( $c->req->param('gaps') ) {
    $c->log->debug( 'Family::Alignment::Download::format: handling gaps parameter' )
      if $c->debug;
      
    if ( $c->req->param('gaps') eq 'none' ) {
      $pfamaln->map_chars('-', '');
      $pfamaln->map_chars('\.', '');
    }
    elsif ( $c->req->param('gaps') eq 'dots' ) {
      $pfamaln->map_chars('-', '.');
    }
    elsif ( $c->req->param('gaps') eq 'dashes' ) {
      $pfamaln->map_chars('\.', '-');
    }
  }

  # case param can be u or l
  if ( $c->req->param('case') and $c->req->param('case') eq 'u' ) {
    $c->log->debug( 'Family::Alignment::Download::format: uppercasing alignment' )
      if $c->debug;
    $pfamaln->uppercase;
  }

  # order param can be tree or alphabetical
  if ( $c->req->param('order') and $c->req->param('order') eq 'a' ) {
    $c->log->debug( 'Family::Alignment::Download::format: sorting alphabetically' )
      if $c->debug;
    $pfamaln->sort_alphabetically;
  }

  # format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if ( $c->req->param( 'format' ) ) {
    if ( $c->req->param( 'format' ) eq 'pfam' ) {
      $c->log->debug( 'Family::Alignment::Download::format: writing Pfam format' )
        if $c->debug;
      $output = $pfamaln->write_Pfam;
    }
    elsif ( $c->req->param( 'format' ) eq 'fasta' ) {
      $c->log->debug( 'Family::Alignment::Download::format: writing FASTA format' )
        if $c->debug;
      $output = $pfamaln->write_fasta;
    }
    elsif ( $c->req->param( 'format' ) eq 'msf' ) {
      $c->log->debug( 'Family::Alignment::Download::format: writing MSF format' )
        if $c->debug;
      $output = $pfamaln->write_MSF;
    }
  }

  # default to writing stockholm
  $output ||= $pfamaln->write_stockholm;

  # are we downloading this or just dumping it to the browser ?
  if ( $c->req->param( 'download' ) ) {
    $c->log->debug( 'Family::Alignment::Download::format: sending alignment as download' )
      if $c->debug;

    # figure out the filename
    my $filename;
    if ( $c->stash->{entryType} eq 'A' ) {
      $filename = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.txt';
    }
    else {
      # don't bother sticking 'seed' or 'full' in there if it's a PfamB
      $filename = $c->stash->{acc} . '.txt';
    }  
  
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

=head2 getAlignment : Private

Retrieves the alignment.

=cut

sub getAlignment : Private {
  my( $this, $c ) = @_;

  # see if we can extract the raw alignment from the cache first
  my $cacheKey  = 'alignment' . $c->stash->{acc} . $c->stash->{alnType};
  my $alignment = $c->cache->get( $cacheKey );

  if( defined $alignment ) {
    $c->log->debug( 'Family::Alignment::Download::getAlignment: extracted alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::Download::getAlignment: failed to extract alignment from cache; going to DB' )
      if $c->debug;

    # see what type of family we have, A or B
    if ( $c->stash->{entryType} eq 'A' ) {
  
      # retrieve the alignment from the DB
      my $row = $c->model('PfamDB::AlignmentsAndTrees')
                  ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                              type       => $c->stash->{alnType} } )
                  ->single;
  
      unless ( defined $row and defined $row->alignment ) {

        $c->log->warn( 'Family::Alignment::Download::getAlignment: failed to retrieve '
          . $c->stash->{alnType} . ' alignment for ' . $c->stash->{acc} )
          if $c->debug;

        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }
  
      # uncompress it
      $alignment = Compress::Zlib::memGunzip( $row->alignment );
      unless ( defined $alignment ) {

        $c->log->warn( 'Family::Alignment::Download::getAlignment: failed to uncompress '
          . $c->stash->{alnType} . ' alignment for ' . $c->stash->{acc} )
          if $c->debug;

        $c->stash->{errorMsg} = 'We could not extract the alignment for '
                                . $c->stash->{acc};
        return;
      }
  
    }
    elsif ( $c->stash->{entryType} eq 'B' ) {

      # make sure the relationship to the pfamB_stockholm table works
      unless ( $c->stash->{pfam}->pfamb_stockholms->single->stockholm_data ) {

        $c->log->warn( 'Family::Alignment::Download::getAlignment: failed to retrieve '
          . ' alignment for Pfam-B ' . $c->stash->{acc} )
          if $c->debug;

        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }

      # uncompress it
      $alignment = Compress::Zlib::memGunzip( $c->stash->{pfam}->pfamb_stockholms->single->stockholm_data );

      unless ( defined $alignment ) {

        $c->log->warn( 'Family::Alignment::Download::getAlignment: failed to uncompress '
          . ' alignment for Pfam-B ' . $c->stash->{acc} )
          if $c->debug;

        $c->stash->{errorMsg} = 'We could not extract the alignment for '
                                . $c->stash->{acc};
        return;
      }

    }

    # cache the raw alignment
    $c->cache->set( $cacheKey, $alignment ) unless $ENV{NO_CACHE};
  }

  # we need the alignment as an array ref, so...
  my @alignment = split /\n/, $alignment;
  $c->stash->{alignment} = \@alignment;
  
  $c->log->debug( 'Family::Alignment::Download::getAlignment: got '
                  . scalar @alignment . ' rows in alignment' ) if $c->debug;
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
