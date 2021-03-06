
# Utils.pm
# jt6 20080104 WTSI
#
# $Id: Utils.pm,v 1.5 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Utils - a set of utility actions

=cut

package PfamWeb::Controller::Utils;

=head1 DESCRIPTION

These are a few actions that can be used in various places around the code.

$Id: Utils.pm,v 1.5 2008-05-16 15:29:28 jt6 Exp $

=cut

use utf8;
use strict;
use warnings;

use Text::Wrap;

# use base 'PfamBase::Controller::Utils';
use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 retrieve_ids : Private

Retrieves a list of IDs/accessions from the DB, given a "job ID" for the list.
This action doesn't attempt to validate the job ID, it just assumes it will
work. The ID should be checked before calling this action.

=cut

sub retrieve_ids : Private {
  my( $this, $c, $job_id ) = @_;

  # retrieve the IDs/accessions for that job ID
  my $rs = $c->model('WebUser::Species_collection')
             ->find( $job_id );
  unless( defined $rs ) {
    $c->log->debug( 'Utils::retrieve_ids: no accessions for that job ID' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No sequence accessions found for that job ID';
    return;
  }

  $c->log->debug( 'Utils::retrieve_ids: accessions_list: |'
                   . $rs->id_list . '|' ) if $c->debug;

  # get the individual accessions
  my @seqAccs;
  foreach ( split /\s+|,/, $rs->id_list ) {
    next unless  m/^(.*?)$/i;
    push @seqAccs, $1;
  }
  $c->log->debug( 'Utils::retrieve_ids: found |' . scalar @seqAccs
                  . '| valid sequence accessions' ) if $c->debug;

  return \@seqAccs;
}

#-------------------------------------------------------------------------------

=head2 get_sequences : Private

This action accepts a "job ID" that identifies a list of sequence accessions
and returns those sequences as a fasta-format file.

=cut

sub get_sequences : Private {
  my( $this, $c, $job_id, $pfam ) = @_;

  # retrieve the accessions for that job ID
  my $accession_list = $c->forward( '/utils/retrieve_ids', [ $job_id ] );
  unless( $accession_list ) {
    $c->stash->{errorMsg} ||= 'Could not retrieve sequences for that job ID';
    return;
  }

  $c->log->debug( 'Utils::get_sequences: found |'
                  . scalar @$accession_list . '| valid sequence accessions' )
    if $c->debug;
  $c->stash->{selectedSeqAccs} = $accession_list;

  # get each of the sequences in turn and turn them into FASTA format
  # (note that we take the auto-number for the family from "$c->stash->{entry}"
  # because the begin method on SpeciesTree can handle Pfam-As, Pfam-Bs and
  # clans, so "entry" is more appropriate than "pfam", which we usually use.)
  my $fasta = '';
  foreach my $seqAcc ( @$accession_list ) {
    my @rows = $c->model('PfamDB::PfamaRegFullSignificant')
                 ->search( { 'pfamseq.pfamseq_acc' => $seqAcc,
                             'me.pfama_acc'        => $pfam->pfama_acc,
                             in_full               => 1 },
                           { prefetch              => [ qw( pfamseq ) ] } );
    #MAQ - join to taxonony table
    #my @trows = $c->model('PfamDB::Pfamseq')
    #             ->search( { 'me.pfamseq_acc' => $seqAcc,
    #                         in_full               => 1 },
    #                       { prefetch              => [ qw( taxonomy ) ] } );
    $c->stash->{numRows} = scalar @rows;

    # need to remember the final character in each row, the "\n"
    $Text::Wrap::columns = 61;

    foreach my $r (@rows){
      $fasta .= '>'.$r->pfamseq_acc.'/'.$r->seq_start.'-'.$r->seq_end."\n";
      $fasta .= wrap( '', '',
                      substr( $r->sequence,
                              $r->seq_start - 1,
                              $r->seq_end - $r->seq_start + 1 )."\n" );
    }
  }

  return $fasta;
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
