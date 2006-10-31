
# DownloadAlignment.pm
# rdf 20061005 WTSI
#
# $Id: DownloadAlignment.pm,v 1.4 2006-10-31 15:16:45 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::DownloadAlignment - controller to
serve up alignments in various formats

=cut

package PfamWeb::Controller::Family::DownloadAlignment;

=head1 DESCRIPTION

Generates a B<full page>.

$Id: DownloadAlignment.pm,v 1.4 2006-10-31 15:16:45 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );
use Compress::Zlib;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 downloadAlignment : Private

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

sub downloadAlignment : Path( "/family/downloadalignment" ) {
  my( $this, $c ) = @_;

  my $alignment = $c->forward( "getAlignment" );

  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $alignment );
  };
  if($@){
    $pfamaln->throw("Error reading stockholm file:[$@]");
  };

  # gaps param can be default, dashes, dot or none
  if( $c->req->param("gaps") ) {
	if( $c->req->param("gaps") eq "none" ) {
	  $pfamaln->map_chars('-', '');
	  $pfamaln->map_chars('\.', '');
	}elsif( $c->req->param("gaps") eq "dots" ) {
	  $pfamaln->map_chars('-', '.');
	}elsif( $c->req->param("gaps") eq "dashes" ) {
	  $pfamaln->map_chars('\.', '-');
	}
  }

  # case param can be u or l
  if( $c->req->param("case") eq "u" ) {
    $pfamaln->uppercase;
  }

  #order param can be tree or alphabetical
  if( $c->req->param("order") eq "alpha" ) {
    $pfamaln->sort_alphabetically;
  }

  #Format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if( $c->req->param( "format" ) ) {
	if( $c->req->param( "format" ) eq "pfam" ) {
	  $output = $pfamaln->write_Pfam;
	}elsif( $c->req->param( "format" ) eq "fasta" ) {
	  $output = $pfamaln->write_fasta;
	}elsif( $c->req->param( "format" ) eq "msf" ) {
	  $output = $pfamaln->write_MSF;
	} else {
	  # stockholm and "anything else"
	  $output = $pfamaln->write_stockholm;
	}
  }

  $c->stash->{output} = $output;
}

#-------------------------------------------------------------------------------

=head2 getAlignment : Private

Retrieves an alignment file, either from disk for a PfamA family or
from the DB for a PfamB family.

=cut

sub getAlignment : Private {
  my( $this, $c ) = @_;

  # see what type of family we have, A or B
  if ( $c->stash->{acc} =~ /^PF\d{5}$/ ) {
	return $c->forward( "getAlignmentFile" );
  } else {
	return $c->forward( "getAlignmentDB" );
  }
}

#-------------------------------------------------------------------------------

=head2 getAlignmentFile : Private

Reads a gzipped alignment file from disk and returns it as an array reference.

=cut

sub getAlignmentFile : Private {
  my( $this, $c ) = @_;

  my $file =
	$this->{alnFileDir} . "/"
	  . $c->stash->{alnType}
		. "/" . $c->stash->{pfam}->pfamA_acc . ".full.gz";

  my $gz = gzopen( $file, "rb" )
	or die "Couldn't open alignment file \"$file\": $gzerrno";

  my( $input, $buffer );
  $input .= $buffer while $gz->gzread( $buffer ) > 0;
  my @input = split /\n/, $input;

  die "Error reading from alignment file \"$file\": $gzerrno" . ( $gzerrno+0)
	unless $gzerrno == Z_STREAM_END;

  $gz->gzclose;

  return \@input;
}

#-------------------------------------------------------------------------------

=head2 getAlignmentDB : Private

Reads a (butchered) Stockholm-format alignment file from the database
and returns it as an array reference. The only alignments that are
stored in the DB are for B<PfamB> families.

=cut

sub getAlignmentDB : Private {
  my( $this, $c ) = @_;

  # need to tack on a header and a footer line before it's really
  # Stockholm-format... go figure
  my @aln = split/\n/, "# STOCKHOLM 1.0\n"
	. $c->stash->{pfam}->pfamB_stockholm->stockholm_data . "//\n";

  return \@aln;
}

#-------------------------------------------------------------------------------

=head2 end : Private

Writes the sequence alignment directly to the response object.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  $c->res->content_type( "text/plain" );

  # are we downloading this or just dumping it to the browser ?
  if( $c->req->param( "download" ) ) {
	
	# figure out the filename
	my $filename;
	if( $c->stash->{entryType} eq "A" ) {
	  $filename = $c->stash->{acc} . "_" . $c->stash->{alnType}. ".txt";
	} else {
	  # don't bother sticking "seed" or "full" in there if it's a PfamB
	  $filename = $c->stash->{acc} . ".txt";
	}	

	$c->res->headers->header( "Content-disposition" => "attachment; filename=$filename" );
  }

  foreach ( @{$c->stash->{output}} ) {
	$c->res->write( $_ );
  }

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
