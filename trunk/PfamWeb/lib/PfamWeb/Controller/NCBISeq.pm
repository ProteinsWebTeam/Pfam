
# NCBISeq.pm
# jt6 20071010 WTSI
#
# $Id: NCBISeq.pm,v 1.5 2009-10-28 11:55:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::NCBISeq - controller to build pages for sequences with
NCBI identifiers

=cut

package PfamWeb::Controller::NCBISeq;

=head1 DESCRIPTION

Generates a B<tabbed page>.

$Id: NCBISeq.pm,v 1.5 2009-10-28 11:55:58 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Drawing::Layout::LayoutManager;

use JSON qw( -convert_blessed_universally );

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'ncbiseq' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the entry.

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') and
       $c->req->param('output') eq 'xml' ) {
    $c->stash->{output_xml} = 1;
    $c->res->content_type('text/xml');    
  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('gi')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';

  # although these next checks might fail and end up putting an error message
  # into the stash, we don't "return", because we might want to process the 
  # error message using a template that returns XML rather than simply HTML
  # (XML output not yet implemented for metaseq data
  # jt6 20080603 WTSI.)
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\.-]+)$/;
    $c->stash->{errorMsg} = 'Invalid ncbiseq accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No ncbiseq accession or ID specified';
  }

  # retrieve data for this entry
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # TODO add an index for the secondary_acc column 
#  my $rs = $c->model('PfamDB::NcbiSeq')
#             ->search( [ { gi            => $entry }, 
#                         { secondary_acc => $entry } ] );
#
#  my $ncbiseq = $rs->first if defined $rs;

  my $ncbiseq = $c->model('PfamDB::NcbiSeq')
                  ->find ( $entry );
 
  unless ( defined $ncbiseq ) {
    $c->stash->{errorMsg} = 'No valid ncbiseq accession or ID';
    return;
  }
  
  $c->log->debug( 'Ncbiseq::get_data: got a ncbiseq entry' ) if $c->debug;
  $c->stash->{ncbiseq} = $ncbiseq;
  $c->stash->{acc}     = $ncbiseq->gi;
  
  # only add extra data to the stash if we're actually going to use it later
  if ( not $c->stash->{output_xml} and 
       ref $this eq 'PfamWeb::Controller::NCBISeq' ) {
    
    $c->log->debug( 'Ncbiseq::get_data: adding extra ncbiseq info' )
      if $c->debug;
    
    $c->forward('generate_pfam_graphic');
    $c->forward('get_summary_data');
  }
  
}

#-------------------------------------------------------------------------------

=head2 generate_pfam_graphic : Private

Generates the Pfam graphic.

=cut

sub generate_pfam_graphic : Private {
  my( $this, $c ) = @_;

  my @rs = $c->model('PfamDB::NcbiPfamaReg')
             ->search( { 'me.gi' => $c->stash->{ncbiseq}->gi },
                       { prefetch => [ qw( auto_pfama gi ) ] } );

  $c->log->debug( 'NCBISeq::generate_pfam_graphic: got ' . scalar @rs . ' rows' )
    if $c->debug;

  # build a Sequence object to describe the graphic
  my @regions = ();
  foreach my $row ( @rs ) {

    my $region = Bio::Pfam::Sequence::Region->new( {
      start      => $row->seq_start,
      end        => $row->seq_end,
      aliStart   => $row->ali_start,
      aliEnd     => $row->ali_end,      
      modelStart => $row->model_start,
      modelEnd   => $row->model_end,
      type       => 'pfama',
      # label      => '',
      # href       => '',
      metadata   => Bio::Pfam::Sequence::MetaData->new( {
        accession   => $row->auto_pfama->pfama_acc,
        identifier  => $row->auto_pfama->pfama_id,
        description => $row->auto_pfama->description,
        score       => $row->domain_evalue_score,
        scoreName   => 'e-value',
        start       => $row->seq_start,
        end         => $row->seq_end,
        aliStart    => $row->ali_start,
        aliEnd      => $row->ali_end,
        type        => $row->auto_pfama->type,
      } )
    } );

    push @regions, $region;
  }
  
  my $sequence = Bio::Pfam::Sequence->new( {
    length   => $c->stash->{ncbiseq}->length,
    regions  => \@regions,
    motifs   => [],
    markups  => [],
    metadata => Bio::Pfam::Sequence::MetaData->new( {
      accession   => $c->stash->{ncbiseq}->gi,
      identifier  => $c->stash->{ncbiseq}->secondary_acc,
      description => $c->stash->{ncbiseq}->description,
      # database    => 'ncbi', # "ncbi" isn't an allowed value, for some reason
    } )
  } );
  
  $c->log->debug( 'NCBISeq::generate_pfam_graphic: sequence object: '
                  . dump( $sequence ) ) if $c->debug;
  
  my $seqs = [ $sequence ];
  
  my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
  $lm->layoutSequences( $seqs );

  $c->log->debug( 'NCBISeq::generate_pfam_graphic: laid out sequences: '
                  . dump( $seqs ) ) if $c->debug;

  # configure the JSON object to correctly stringify the layout manager output
  my $json = new JSON;
  $json->pretty(1);
  $json->allow_blessed;
  $json->convert_blessed;

  # encode and stash the sequences as a JSON string
  $c->stash->{layout} = $json->encode( $seqs );
}

#-------------------------------------------------------------------------------

=head2 generateKey : Private

Generates a data structure representing the key to the domain image, which 
can be formatted sensibly by the view.

=cut

sub generateKey : Private {
  my( $this, $c, $lm ) = @_;
  
  # retrieve a hash of BioPerl objects indexed on sequence ID
  my %hash = $lm->seqHash;

  # pull out the sequence object for just the sequence that we're dealing with
  # (there should be only that one anyway) 
  my $seq = $hash{ $c->stash->{ncbiseq}->gi };

  # and get the raw key data from that
  my %key = $seq->getKey;

  # from this point on we're mimicking old, crufty code...
  
  # sort the rows according to the start position of the domain
  my @rows;
  foreach my $row ( sort{$key{$a}{start} <=> $key{$b}{start} } keys %key ) {

    # shouldn't they always be a number ?
    next unless $key{$row}{start} =~ /^\d+$/;
   
    # just store the hash for this row and we're done here; let the view
    # figure out what to render 
    push @rows, $key{$row};
  }
  
  $c->stash->{imageKey} = \@rows;
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Gets the data items for the overview bar

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my %summaryData;

  # first, the number of sequences... pretty easy...
  $summaryData{numSequences} = 1;

  # also, the number of architectures
  $summaryData{numArchitectures} = 1;

  # number of species
  $summaryData{numSpecies} = 0;

  # number of structures
  $summaryData{numStructures} = 0;

  # number of interactions
  $summaryData{numInt} = 0;

  $c->stash->{summaryData} = \%summaryData;

  $c->log->debug('NCBISeq::get_summary_data: added the summary data to the stash')
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

Andy Jenkinson, C<aj5@sanger.ac.uk>

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
