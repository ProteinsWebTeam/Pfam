
# DownloadAlignment.pm
# rdf 20061005 WTSI
#
# $Id: Download.pm,v 1.2 2007-08-02 15:24:13 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::DownloadAlignment - controller to
serve up alignments in various formats

=cut

package PfamWeb::Controller::Family::Alignment::Download;

=head1 DESCRIPTION

Generates a B<full page>.

$Id: Download.pm,v 1.2 2007-08-02 15:24:13 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;

use base 'PfamWeb::Controller::Family::Alignment';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 html : Path

Retrieves the JTML alignment and dumps it to the response. We first try to 
extract the JTML from the cache or, if that fails, we retrieve it from the DB.

Note that we can't use C<$c->cache_page> here because we're printing directly
to the response, which causes problems for the cache plugin. 

=cut

sub html : Local {
  my( $this, $c ) = @_;

  my $cacheKey = 'jtml' . $c->stash->{acc} . $c->stash->{alnType};
  
  my $jtml = $c->cache->get( $cacheKey );
  if( defined $jtml ) {
    $c->log->debug( 'Family::Alignment::Download::html: extracted JTML from cache' );
  } else {
    $c->log->debug( 'Family::Alignment::Download::html: failed to extract JTML from cache; going to DB' );  

    # see what type of family we have, A or B
    my $row;
    if ( $c->stash->{entryType} eq 'A' ) {
  
      # retrieve the JTML from the DB
      my $rs = $c->model('PfamDB::AlignmentsAndTrees')
                 ->search( { auto_pfamA => $c->stash->{pfam}->auto_pfamA,
                             type       => $c->stash->{alnType} } );
      $row = $rs->first;
    
      # final check...
      unless( defined $row->jtml ) {
        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }
  
    } elsif ( $c->stash->{entryType} eq 'B' ) {

      # make sure the Pfam-B JTML is already available
      unless( defined $c->stash->{pfam}->pfamB_stockholm->jtml ) {
        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }

      $row = $c->stash->{pfam}->pfamB_stockholm;
    }

    # uncompress the row to get the raw JTML
    $jtml = Compress::Zlib::memGunzip( $row->jtml );
    unless( defined $jtml ) {
      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Family::Alignment::Download::html: retrieved JTML from DB' );
    $c->cache->set( $cacheKey, $jtml );
  }

  # write the JTML to the output stream
  $c->res->content_type( 'text/html' );
  $c->res->write( $jtml );
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
  $c->forward( 'getAlignment' );

  # drop it into an AlignPfam object
  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $c->stash->{alignment} );
  };
  if( $@ ) {
    $c->stash->{errorMsg} = 'There was a problem with the alignment data for '
                            . $c->stash->{acc};
    return;
  };

  # gaps param can be default, dashes, dot or none
  if( $c->req->param('gaps') ) {
    if( $c->req->param('gaps') eq 'none' ) {
      $pfamaln->map_chars('-', '');
      $pfamaln->map_chars('\.', '');
    } elsif( $c->req->param('gaps') eq 'dots' ) {
      $pfamaln->map_chars('-', '.');
    } elsif( $c->req->param('gaps') eq 'dashes' ) {
      $pfamaln->map_chars('\.', '-');
    }
  }

  # case param can be u or l
  if( $c->req->param('case') and $c->req->param('case') eq 'u' ) {
    $pfamaln->uppercase;
  }

  # order param can be tree or alphabetical
  if( $c->req->param('order') and $c->req->param('order') eq 'alpha' ) {
    $pfamaln->sort_alphabetically;
  }

  # format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if( $c->req->param( 'format' ) ) {
    if( $c->req->param( 'format' ) eq 'pfam' ) {
      $output = $pfamaln->write_Pfam;
    } elsif( $c->req->param( 'format' ) eq 'fasta' ) {
      $output = $pfamaln->write_fasta;
    } elsif( $c->req->param( 'format' ) eq 'msf' ) {
      $output = $pfamaln->write_MSF;
    } else {
      # stockholm and "anything else"
      $output = $pfamaln->write_stockholm;
    }
  }

  $c->stash->{output} = $output;
}

#-------------------------------------------------------------------------------

=head2 end : Private

Writes the sequence alignment directly to the response object.

=cut

sub end : Private {
  my( $this, $c ) = @_;
  
  # we only return plain text from this controller
  $c->res->content_type( 'text/plain' );

  # are we downloading this or just dumping it to the browser ?
  if( $c->req->param( 'download' ) ) {
  
    # figure out the filename
    my $filename;
    if( $c->stash->{entryType} eq 'A' ) {
      $filename = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.txt';
    } else {
      # don't bother sticking 'seed' or 'full' in there if it's a PfamB
      $filename = $c->stash->{acc} . '.txt';
    }  
  
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  foreach ( @{$c->stash->{output}} ) {
    $c->res->write( $_ );
  }

}

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
    $c->log->debug( 'Family::Alignment::Download::getAlignment: extracted alignment from cache' );
  } else {
    $c->log->debug( 'Family::Alignment::Download::getAlignment: failed to extract alignment from cache; going to DB' );

    # see what type of family we have, A or B
    if ( $c->stash->{entryType} eq 'A' ) {
  
      # retrieve the alignment from the DB
      my $rs = $c->model('PfamDB::AlignmentsAndTrees')
                 ->search( { auto_pfamA => $c->stash->{pfam}->auto_pfamA,
                             type       => $c->stash->{alnType} } );
      my $row = $rs->first;
  
      unless( defined $row->alignment ) {
        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }
  
      # uncompress it
      $alignment = Compress::Zlib::memGunzip( $row->alignment );
      unless( defined $alignment ) {
        $c->stash->{errorMsg} = 'We could not extract the alignment for '
                                . $c->stash->{acc};
        return;
      }
  
    } elsif ( $c->stash->{entryType} eq 'B' ) {

      # make sure the Pfam-B alignment is already available
      unless( defined $c->stash->{pfam}->pfamB_stockholm->stockholm_data ) {
        $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                                . $c->stash->{acc};
        return;
      }

      # need to tack on a header and a footer line before it's really
      # Stockholm-format... go figure
      $alignment = "# STOCKHOLM 1.0\n" .
                   $c->stash->{pfam}->pfamB_stockholm->stockholm_data .
                   "//\n";
    }

    # cache the raw alignment
    $c->cache->set( $cacheKey, $alignment );
  }

  # we need the alignment as an array ref, so...
  my @alignment = split /\n/, $alignment;
  $c->stash->{alignment} = \@alignment;
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
