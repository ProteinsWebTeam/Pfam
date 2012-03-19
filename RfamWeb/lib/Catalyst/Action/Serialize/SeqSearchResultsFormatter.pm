
# Catalyst::Action::Serialize::SeqSearchResultsFormatter.pm
# jt6 20120709 WTSI
#
# $Id$

=head1 NAME

Catalyst::Action::Serialize::SeqSearchResultsFormatter - format search results as tab-separated values or GFF3

=cut

package Catalyst::Action::Serialize::SeqSearchResultsFormatter;

=head1 DESCRIPTION

This is a plugin for the L<Catalyst::Action::REST> serialisation classes. It
can serialise the results of an Rfam sequence search into a plain text file
with tab-separated values, or a GFF-style file.

$Id$

=cut

use Moose;
use namespace::clean;

use Catalyst::Exception;
use Text::Wrap;

extends 'Catalyst::Action';

#-------------------------------------------------------------------------------

=head2 execute

Serialises the contents of the stash in the requested format. Accepts either 
"tsv" or "gff" as the first argument.

=cut

sub execute {
  my $this = shift;
  my ( $controller, $c, $format ) = @_;

  $this->{stash_key} = (
      $controller->config->{'deserialize'} 
    ? $controller->config->{'deserialize'}->{'stash_key'} 
    : $controller->config->{'stash_key'}
  ) || 'rest';

  my ( $data, $filename );
  if ( $format eq 'tsv' ) {
    ( $data, $filename ) = $this->_build_tsv( $c );
  }
  elsif ( $format eq 'gff' ) {
    ( $data, $filename ) = $this->_build_gff( $c );
  }
  else {
    Catalyst::Exception->throw( 'No valid file format specified.' );
  }

  $c->res->content_type( 'text/plain ');
  $c->res->header( 'Content-Disposition' => "attachment; filename=$filename" );
  $c->res->output( $data );

  return 1;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# builds a tab-separated text file with the search results

sub _build_tsv {
  my ( $this, $c ) = @_;

  my $job_id   = $c->stash->{jobId};
  my $release  = $c->stash->{relData}->rfam_release;
  my $rel_date = $c->stash->{relData}->rfam_release_date;
  my $opened   = $c->stash->{$this->{stash_key}}->{opened};
  my $seq_len  = length $c->stash->{$this->{stash_key}}->{searchSequence};

  $Text::Wrap::columns = 60;
  my $sequence = wrap( '# ', '# ', $c->stash->{$this->{stash_key}}->{searchSequence} );

  my $data = <<EOF_data;
# Rfam sequence search results for sequence:
$sequence
# job ID: $job_id, executed: $opened
# Rfam $release $rel_date
# columns are:
#   ID, accession, start, end, bits score, E-value, strand
EOF_data

  foreach my $id ( %{ $c->stash->{$this->{stash_key}}->{hits} } ) {
    my $hits = $c->stash->{$this->{stash_key}}->{hits}->{$id};
    foreach my $hit ( @$hits ) {
      $data .= $hit->{id} . "\t" .
               $hit->{acc} . "\t" .
               $hit->{start} . "\t" .
               $hit->{end} . "\t" .
               $hit->{score} . "\t" .
               $hit->{E} . "\t" .
               $hit->{strand} . "\n";
    }
  }

  my $filename = 'rfam_search_' . $c->stash->{jobId} . '.text';

  return ( $data, $filename );
}

#-------------------------------------------------------------------------------

# builds a vaguely GFF-format file with the results

sub _build_gff {
  my ( $this, $c ) = @_;

  my $release  = $c->stash->{relData}->rfam_release;
  my $rel_date = $c->stash->{relData}->rfam_release_date;
  my $seq_len  = length $c->stash->{$this->{stash_key}}->{searchSequence};

  my $data = <<EOF_data;
##gff-version 3
##Rfam $release $rel_date
##sequence-region UserSeq 1 $seq_len
EOF_data

  foreach my $id ( keys %{ $c->stash->{$this->{stash_key}}->{hits} } ) {
    my $hits = $c->stash->{$this->{stash_key}}->{hits}->{$id};
    foreach my $hit ( @{ $hits } ) {
      $data .= $hit->{id} . "\t" .
               "rfam_scan.pl\t" .
               ".\t" .
               $hit->{start} . "\t" .
               $hit->{end} . "\t" .
               $hit->{score} . "\t" .
               $hit->{strand} . "\t" .
               ".\t" . 
               ".\n";
    }
  }

  my $filename = 'rfam_search_' . $c->stash->{jobId} . '.gff';

  return ( $data, $filename );
}

__PACKAGE__->meta->make_immutable;

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk)

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

