
# Metaseq.pm
# jt6 20071008 WTSI
#
# $Id: Metaseq.pm,v 1.5 2009-12-07 22:23:00 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Metaseq - controller to build pages for meta-genomics
sequences

=cut

package PfamWeb::Controller::Metaseq;

=head1 DESCRIPTION

Generates a page set for a metagenomics sequence.

$Id: Metaseq.pm,v 1.5 2009-12-07 22:23:00 jt6 Exp $

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

__PACKAGE__->config( SECTION => 'metaseq' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the metaseq entry.

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
                      $c->req->param('id')    ||
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
    $c->stash->{errorMsg} = 'Invalid metaseq accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No metaseq accession or ID specified';
  }

  # retrieve data for this entry
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
  
}

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub stats : Local{
  my ( $this, $c ) = @_;

  # detaint the source name
  if ( defined $c->req->param('source') and 
       $c->req->param('source') =~ m/^([A-Za-z\d_])+$/ ) {
    $c->log->debug( "Metaseq::stats: got a valid source name: |$1|" )
      if $c->debug;
  }  
  else {
    $c->log->debug( 'Metaseq::stats: no valid source name' ) if $c->debug;
  }

  my $source = $1;

  # a long, long query...
  my $rs = $c->model( 'PfamDB::Meta_pfama_reg' )
             ->search( { 'metaseq.source' => $source },
                       { join     => [ 'pfamA', 'metaseq' ],
                         select   => [ qw( pfamA_id pfamA_acc ), { count => 'auto_pfamA' } ],
                         as       => [ qw( pfamA_id pfamA_id num_domains ) ],
                         group_by => 'auto_pfamA' } );

  # SELECT   count(*), 
  #          auto_pfamA 
  # FROM     meta_pfamA_reg r,
  #          metaseq m
  # WHERE    source = 'acid_mine'
  # AND      m.auto_metaseq = r.auto_metaseq
  # GROUP BY auto_pfamA;
  
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves the data for the given metaseq accession or ID.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  my $rs = $c->model('PfamDB::Metaseq')
             ->search( [ { metaseq_acc => $entry }, 
                         { metaseq_id  => $entry } ] );

  my $metaseq;
  $metaseq = $rs->first if defined $rs;
  
  unless ( defined $metaseq ) {
    $c->stash->{errorMsg} = 'No valid metaseq accession or ID';
    return;
  }
  
  $c->log->debug( 'Metaseq::get_data: got a metaseq entry' ) if $c->debug;
  $c->stash->{metaseq}   = $metaseq;
  $c->stash->{acc}       = $metaseq->metaseq_acc;
  
  # only add extra data to the stash if we're actually going to use it later
  if ( not $c->stash->{output_xml} and 
       ref $this eq 'PfamWeb::Controller::Metaseq' ) {
    
    $c->log->debug( 'Metaseq::get_data: adding extra metaseq info' )
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
  
  my @rs = $c->model('PfamDB::MetaPfamaReg')
             ->search( { 'me.auto_metaseq' => $c->stash->{metaseq}->auto_metaseq },
                       { prefetch => [ qw( auto_pfama auto_metaseq ) ] } );

  $c->log->debug( 'Metaseq::generate_pfam_graphic: got ' . scalar @rs . ' rows' )
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
    length   => $c->stash->{metaseq}->length,
    regions  => \@regions,
    motifs   => [],
    markups  => [],
    metadata => Bio::Pfam::Sequence::MetaData->new( {
      accession   => $c->stash->{metaseq}->gi,
      identifier  => $c->stash->{metaseq}->secondary_acc,
      description => $c->stash->{metaseq}->description,
    } )
  } );
  
  $c->log->debug( 'Metaseq::generate_pfam_graphic: sequence object: '
                  . dump( $sequence ) ) if $c->debug;
  
  my $seqs = [ $sequence ];
  
  my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
  $lm->layoutSequences( $seqs );

  $c->log->debug( 'Metaseq::generate_pfam_graphic: laid out sequences: '
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
  my $seq = $hash{ $c->stash->{metaseq}->metaseq_id };

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

  $c->log->debug( 'Metaseq::get_summary_data: added the summary data to the stash' )
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
