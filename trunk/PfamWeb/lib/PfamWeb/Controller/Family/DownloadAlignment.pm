
# DownloadAlignment.pm
# rdf 20061005 WTSI
#
# $Id: DownloadAlignment.pm,v 1.2 2006-10-06 15:15:09 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::DownloadAlignment - controller to
serve up alignments in various formats

=cut

package PfamWeb::Controller::Family::DownloadAlignment;

=head1 DESCRIPTION

Generates a B<full page>.

$Id: DownloadAlignment.pm,v 1.2 2006-10-06 15:15:09 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;
use Bio::Pfam::ColourAlign;
use Compress::Zlib;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Just adds the alignment type (seed or full) to the stash.

=cut

sub auto : Private {
  my( $this, $c ) = @_;

  $c->stash->{type} = ( $c->req->param( "type" ) eq "seed" ) ? "seed" : "full";
}

#-------------------------------------------------------------------------------

=head2 downloadAlignment : Path

Serves up the plain (no markup) alignment. Also applies various
changes to the alignment before hand, such as changing the gap style
or sequence order.

By default this method will write out in Stockholm format, but the
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

  my $alignment = $c->forward( "getAlignFile" );

  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $alignment );
  };
  if($@){
    $pfamaln->throw("Error reading stockholm file:[$@]");
  };

  # gaps param can be default, dashes, dot or none
  if( $c->req->param("gaps") eq "none" ) {
    $pfamaln->map_chars('-', '');
    $pfamaln->map_chars('\.', '');
  }elsif( $c->req->param("gaps") eq "dots" ) {
    $pfamaln->map_chars('-', '.');
  }elsif( $c->req->param("gaps") eq "dashes" ) {
    $pfamaln->map_chars('\.', '-');
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

  $c->stash->{output} = $output;
}

#-------------------------------------------------------------------------------

=head2 getAlignFile : Private

Reads a gzipped alignment file from disk and return it as an array reference.

=cut

sub getAlignFile : Private {
  my( $this, $c ) = @_;

  my $file =
	$this->{alnFileDir} . "/"
	  . $c->stash->{type}
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

=head2 end : Private

Writes the sequence alignment directly to the response object.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  $c->res->content_type( "text/plain" );

  if( $c->req->param( "download" ) ) {
	$c->res->headers->header( "Content-disposition" => "attachment; filename="
							  . $c->stash->{pfam}->pfamA_id . "." . $c->stash->{type} );
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
