
# MetaSeq.pm
# jt6 20071008 WTSI
#
# $Id: MetaSeq.pm,v 1.1 2007-10-08 16:12:10 jt6 Exp $

=head1 NAME

PfamWeb::Controller::MetaSeq - controller to build pages for meta-genomics
sequences

=cut

package PfamWeb::Controller::MetaSeq;

=head1 DESCRIPTION

Generates a B<tabbed page>.

$Id: MetaSeq.pm,v 1.1 2007-10-08 16:12:10 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'metaseq' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the metaseq entry.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  my $m;
  if ( defined $c->req->param('acc') ) {

    $c->req->param('acc') =~ m/^((\w+_?)+)$/i;
    $c->log->debug( "MetaSeq::begin: found a metaseq accession |$1|" );
  
    $m = $c->model('PfamDB::Metaseq')
           ->find( { metaseq_acc => $1 } );

  } elsif ( defined $c->req->param('id') ) {

    $c->req->param('id') =~ m/^((\w+_?)+)$/i;
    $c->log->debug( "MetaSeq::begin: found a metaseq ID |$1|" );
  
    $m = $c->model('PfamDB::Metaseq')
           ->find( { metaseq_id => $1 } );

  } elsif ( defined $c->req->param('entry') ) {

    $c->req->param('id') =~ m/^((\w+_?)+)$/i;
    $c->log->debug( "MetaSeq::begin: found a metaseq entry |$1|" );
    
    my $rs = $c->model('PfamDB::MetaSeq')
               ->search( [ { metaseq_acc => $1 }, 
                           { metaseq_id  => $1 } ] );

    $m = $rs->first if $rs->count == 1;
  }

  # we're done here unless there's an entry specified
  unless ( defined $m ) {

    # de-taint the accession or ID
    my $input = $c->req->param('acc')
      || $c->req->param('id')
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
          'Found a broken internal link; no valid metaseq accession or ID ("'
          . $input . '") in "' . $c->req->referer . '"' );
      $c->forward('/reportError');
  
      # now reset the errors array so that we can add the message for
      # public consumption
      $c->clear_errors;
  
    }
  
    # the message that we'll show to the user
    $c->stash->{errorMsg} = 'No valid MetaSeq accession or ID';
  
    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn('MetaSeq::begin: no valid metaseq ID or accession');
  
    return;
  }

  $c->log->debug('MetaSeq::begin: successfully retrieved a metaseq object');
  $c->stash->{metaseq} = $m;
    
  #----------------------------------------
  # add extra data to the stash

#  $c->forward('generatePfamGraphic');
#  $c->forward('getMapping');
#  $c->forward('getSummaryData');
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getMapping : Private

Gets the structure-to-sequence-to-family mapping.

=cut

sub getMapping : Private {
  my ( $this, $c ) = @_;

  my @mapping = $c->model('PfamDB::Pdb_pfamA_reg')
                  ->search( { 'pfamseq.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq },
                            {  join                  => [ qw( pfamA pfamseq pdb ) ],
                               prefetch              => [ qw( pfamA pfamseq pdb ) ] } );

  $c->stash->{pfamMaps} = \@mapping;

  $c->log->debug('Protein::begin: added the structure mapping to the stash');
}

#-------------------------------------------------------------------------------

=head2 generatePfamGraphic : Private

Generates the Pfam graphic.

=cut

sub generatePfamGraphic : Private {
  my ( $this, $c ) = @_;

  # get a layout manager and set the X scale
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layoutPfam->scale_x(1);
  $c->log->debug('Protein::generatePfamGraphic: instantiated a layout manager');

  # retrieve the Storable containing the annotated sequence, thaw it
  # and hand it off to the layout manager
  my $annseq;
  eval {
    $annseq = thaw( $c->stash->{pfamseq}->annseq->annseq_storable );
  };
  if ($@) {
    $c->log->error("Protein::begin: ERROR: failed to thaw annseq: $@");
  }

  $layoutPfam->layout_sequences_with_regions_and_features( [$annseq],
                                                           { PfamA      => 1,
                                                             PfamB      => 1,
                                                             noFeatures => 0 } );

  # if we've arrived here from the top-level controller, rather than from one 
  # of the sub-classes, we will be displaying a key for the domain graphic.
  # For now at least, the sub-classes, such as the interactive feature viewer,
  # don't bother with the key, so they don't need this extra blob of data in 
  # the stash. 
  $c->forward( 'generateKey', [ $layoutPfam ] )
    if ref $this eq 'PfamWeb::Controller::Protein';

  # and build an imageset
  my $pfamImageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $pfamImageset->create_images( $layoutPfam->layout_to_XMLDOM );
  $c->log->debug('Protein::generatePfamGraphic: created images');

  $c->stash->{pfamImageset} = $pfamImageset;

  $c->log->debug('Protein::generatePfamGraphic: successfully generated an imageset object');
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
  my $seq = $hash{ $c->stash->{pfamseq}->pfamseq_id };

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
  $summaryData{numSpecies} = 1;

  # number of structures
  # my $rs = $c->model('PfamDB::Pdb_residue')
  #            ->find( { auto_pfamseq => $c->stash->{pfamseq}->auto_pfamseq },
  #                    { select       => [
  #                                        {
  #                                          count => [ { distinct => [ qw( auto_pdb )] } ]
  #                                        }
  #                                      ],
  #                      as           => [ qw( numberPdbs ) ] } );
  # $summaryData{numStructures} = $rs->get_column( 'numberPdbs' );
  $summaryData{numStructures} = 0;

  # number of interactions
  # TODO Has interactions
  #$rs = $c->model('PfamDB::Interactions')
  #        ->find( { auto_pfamseq_A => $c->stash->{pfamseq}->auto_pfamseq },
  #               { select         => [
  #                                     {
  #                                       count => [ { distinct => [ 'auto_int_pfamAs' ] } ]
  #                                    }
  #                                   ],
  #                 as             => [ qw( numInts ) ] } );

  #$summaryData{numInt} = $rs->get_column( 'numInts' );
  $summaryData{numInt} = 0;

  $c->stash->{summaryData} = \%summaryData;

  $c->log->debug('Protein::getSummaryData: added the summary data to the stash');
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _sortWithPref

A regular Perl method implementing a sort.

=cut

sub _sortWithPref {
  my $pref = shift;
  return sort {
    my $i = (lc $a) cmp (lc $b);
    return $i if $i == 0;
    if( $a eq $pref ) {
      $i = -1;
    } elsif( $b eq $pref ) {
      $i = 1;
    }
    return $i;
  } @_;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

Andy Jenkinson, C<aj5@sanger.ac.uk>

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
