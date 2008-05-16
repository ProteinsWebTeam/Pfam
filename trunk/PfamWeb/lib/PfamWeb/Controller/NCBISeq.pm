
# NCBISeq.pm
# jt6 20071010 WTSI
#
# $Id: NCBISeq.pm,v 1.3 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::NCBISeq - controller to build pages for sequences with
NCBI identifiers

=cut

package PfamWeb::Controller::NCBISeq;

=head1 DESCRIPTION

Generates a B<tabbed page>.

$Id: NCBISeq.pm,v 1.3 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::Pfam::PfamRegion;
use Bio::Pfam::AnnSeqFactory;
use Bio::SeqFeature::Generic;
use Bio::Pfam::OtherRegion;
use Bio::Pfam::SeqPfam;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::Pfam::Drawing::Image::Image;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'ncbiseq' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the entry.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  my $n;
  if ( defined $c->req->param('gi') ) {

    $c->req->param('gi') =~ m/^(GI:?)?(\d+)$/i;
    $c->log->debug( "NCBISeq::begin: found a GI number: |$2|" );
  
    $n = $c->model('PfamDB::Ncbi_seq')
           ->find( { gi => $2 } );

  } elsif ( defined $c->req->param('acc') ) {

    # TODO this is a bit dangerous; try to find a tighter pattern for these
    $c->req->param('acc') =~ m/^(\w+\d+)(\.\d+)?$/i;
    $c->log->debug( "NCBISeq::begin: found an NCBI secondary accession: |$1|" );
  
    my @rs = $c->model('PfamDB::Ncbi_seq')
               ->search( { secondary_acc => { 'like', "$1%" } } );
    $n = shift @rs;

  } elsif ( defined $c->req->param('entry') ) {

    # we don't know if this is a GI or an accession; try both

    if( $c->req->param('entry') =~ m/^(GI:?)?(\d+)$/i ) {
  
      # looks like a GI; redirect to this action, appending the GI number
      $c->log->debug( "NCBISeq::begin: looks like a GI ($2); detaching");
      $c->req->param( gi => $2 );
      $c->detach( 'begin' );
      return 1;
  
    } elsif( $c->req->param('entry') =~ m/^(\w+\d+)(\.\d+)?$/i ) {
  
      # looks like an accession; redirect to this action, appending the accession
      $c->log->debug( "NCBISeq::begin: looks like a secondary accession ($1); detaching");
      $c->req->param( acc => $1 );
      $c->detach( 'begin' );
      return 1;
    }
  }

  # we're done here unless there's an entry specified
  unless ( defined $n ) {

    # de-taint the accession or ID
    my $input = $c->req->param('acc')
      || $c->req->param('gi')
      || $c->req->param('entry')
      || '';
    $input =~ s/^(\w+)/$1/;
  
    # see if this was an internal link and, if so, report it
    my $b = $c->req->base;
    if ( $c->req->referer =~ /^$b/ ) {
  
      # this means that the link that got us here was somewhere within
      # the Pfam site and that the accession or ID which it specified
      # doesn't actually exist in the DB
  
      # report the error as a broken internal link
      $c->error(
          'Found a broken internal link; no valid GI or accession ("'
          . $input . '") in "' . $c->req->referer . '"' );
      $c->forward('/reportError');
  
      # now reset the errors array so that we can add the message for
      # public consumption
      $c->clear_errors;
  
    }
  
    # the message that we'll show to the user
    $c->stash->{errorMsg} = 'No valid GI or secondary accession';
  
    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn('NCBISeq::begin: no valid GI or secondary accession');
  
    return;
  }

  $c->log->debug('NCBISeq::begin: successfully retrieved an NCBI_seq object');
  $c->stash->{ncbiseq} = $n;
    
  #----------------------------------------
  # add extra data to the stash

  $c->forward('generatePfamGraphic');
  $c->forward('getSummaryData');
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 generatePfamGraphic : Private

Generates the Pfam graphic.

=cut

sub generatePfamGraphic : Private {
  my( $this, $c ) = @_;
  
  my $factory = new Bio::Pfam::AnnSeqFactory;
  my $annseq  = $factory->createAnnotatedSequence();
  
  my %args = (
    '-seq'   => $c->stash->{ncbiseq}->sequence,
    '-start' => 1,
    '-end'   => $c->stash->{ncbiseq}->length,
    '-id'    => $c->stash->{ncbiseq}->gi,
    '-acc'   => $c->stash->{ncbiseq}->secondary_acc,
    '-desc'  => $c->stash->{ncbiseq}->description
  );
  $c->log->debug( 'NCBISeq::generatePfamGraphic: annseq args: ', dump \%args )
    if $c->debug;

  $annseq->sequence( Bio::Pfam::SeqPfam->new( %args ) );
  
  my @rs = $c->model('PfamDB::Ncbi_pfama_reg')
             ->search( { gi      => $c->stash->{ncbiseq}->gi,
                         in_full => 1 },
                       { join     => [ 'pfamA' ],
                         prefetch => [ 'pfamA' ] } );

  foreach my $row ( @rs ) {
    %args = (
      '-PFAM_ACCESSION' => $row->pfamA_acc,
      '-PFAM_ID'        => $row->pfamA_id,
      '-SEQ_ID'         => $annseq->id,
      '-FROM'           => $row->seq_start,
      '-TO'             => $row->seq_end,
      '-MODEL_FROM'     => $row->model_start,
      '-MODEL_TO'       => $row->model_end,
      '-MODEL_LENGTH'   => $row->model_length,
      '-BITS'           => $row->domain_bits_score,
      '-EVALUE'         => $row->domain_evalue_score,
      '-TYPE'           => 'PfamA',
      '-REGION'         => $row->type
    );
    $annseq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new( %args ) );
  }
  
  # build a layout manager
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->layout_sequences( $annseq );
  
   # if we've arrived here from the top-level controller, rather than from one 
  # of the sub-classes, we will be displaying a key for the domain graphic.
  # For now at least, the sub-classes, such as the interactive feature viewer,
  # don't bother with the key, so they don't need this extra blob of data in 
  # the stash. 
  $c->forward( 'generateKey', [ $layout ] )
    if ref $this eq 'PfamWeb::Controller::NCBISeq';
  
  # and use it to create an ImageSet
  my $imageSet = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageSet->create_images( $layout->layout_to_XMLDOM );
 
  $c->stash->{imageSet} = $imageSet;
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

=head2 getSummaryData : Private

Gets the data items for the overview bar

=cut

sub getSummaryData : Private {
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

  $c->log->debug('NCBISeq::getSummaryData: added the summary data to the stash');
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
