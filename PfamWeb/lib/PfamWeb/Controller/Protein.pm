
# Protein.pm
# jt6 20060427 WTSI
#
# $Id: Protein.pm,v 1.29 2007-08-20 09:00:44 rdf Exp $

=head1 NAME

PfamWeb::Controller::Family - controller to build the main protein
page

=cut

package PfamWeb::Controller::Protein;

=head1 DESCRIPTION

This is intended to be the base class for everything related to
UniProt entries across the site. 
Generates a B<tabbed page>.

$Id: Protein.pm,v 1.29 2007-08-20 09:00:44 rdf Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use Storable qw( thaw );
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use Bio::Pfam::AnnotatedSequence;
use Bio::Pfam::AnnotatedRegion;
use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::Pfam::OtherRegion;
use Bio::Pfam::SmartRegion;
use Bio::Pfam::ContextPfamRegion;
use Bio::Pfam::CATHRegion;
use Bio::Pfam::SCOPRegion;
use Bio::Pfam::SeqPfam;
use Bio::Pfam::HMMOtherRegion;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::SeqFeature::Generic;

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'protein' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the UniProt entry.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  my $p;
  if ( defined $c->req->param('acc') ) {

    $c->req->param('acc') =~ m/^([AOPQ]\d[A-Z0-9]{3}\d)$/i;
    $c->log->debug( "Protein::begin: found a uniprot accession |$1|" );
  
    # try a lookup in the main pfamseq table first
    $p = $c->model('PfamDB::Pfamseq')
           ->find( { pfamseq_acc => $1 } );
  
    # if we got a result there, so much the better...
    unless( defined $p ) {
  
      # ... otherwise, see if this is really a secondary accession
      $p = $c->model('PfamDB::Secondary_pfamseq_acc')
            ->find( { secondary_acc => $1 },
                    { join          => [ qw( pfamseq ) ],
                      prefetch      => [ qw( pfamseq ) ] } );
    }
  
  } elsif ( defined $c->req->param('id') ) {

    $c->req->param('id') =~ m/^(\w+)$/;
    $c->log->debug("Protein::begin: found a uniprot ID |$1|");
  
    # try a lookup in the main pfamseq table first
    $p = $c->model('PfamDB::Pfamseq')
           ->find( { pfamseq_id => $1 } );
  
  } elsif ( defined $c->req->param('entry') ) {

    # we don't know if this is an accession or an ID; try both
  
    if ( $c->req->param('entry') =~ m/^([AOPQ]\d[A-Z0-9]{3}\d)$/i ) {
  
      # looks like an accession; redirect to this action, appending the accession
      $c->log->debug(
             "Protein::begin: looks like a uniprot accession ($1); redirecting");
      $c->res->redirect( $c->uri_for( '/protein', { acc => $1 } ) );
      return 1;
  
    } elsif ( $c->req->param('entry') =~ m/^(\w+_\w+)$/ ) {
  
      # looks like an ID; redirect to this action, appending the ID
      $c->log->debug('Protein::begin: looks like a uniprot ID; redirecting');
      $c->res->redirect( $c->uri_for( '/protein', { id => $1 } ) );
      return 1;
    }
  }

  # we're done here unless there's an entry specified
  unless ( defined $p ) {

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
          'Found a broken internal link; no valid UniProt accession or ID ("'
          . $input . '") in "' . $c->req->referer . '"' );
      $c->forward('/reportError');
  
      # now reset the errors array so that we can add the message for
      # public consumption
      $c->clear_errors;
  
    }
  
    # the message that we'll show to the user
    $c->stash->{errorMsg} = 'No valid UniProt accession or ID';
  
    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn('Family::begin: no valid UniProt ID or accession');
  
    return;
  }

  $c->log->debug('Protein::begin: successfully retrieved a pfamseq object');
  $c->stash->{pfamseq} = $p;
  $c->stash->{genomeCode} = $p->ncbi_code if ( $p->genome_seq );

    
  #----------------------------------------
  # add extra data to the stash

  $c->forward('generatePfamGraphic');
  $c->forward('getDasSources');
  $c->forward('getMapping');
  $c->forward('getSummaryData');
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getDasSources : Private

Retrieves DAS sources with the appropriate object type and coordinate system.

=cut

sub getDasSources : Private {
  my( $this, $c) = @_;
  
  my @dasSources = $c->model('WebUser::Feature_das_sources')
                     ->search();
  my $keptSources = {};
  my( $baseCoord, $baseType ) = ('UniProt', 'Protein Sequence');
  my $seqAcc = $c->stash->{pfamseq}->pfamseq_acc;
  my $dl = $c->model('PfamDB')
             ->getDasLite;
  my %alignMatches;
  
  FEATURE: foreach my $f (@dasSources) {
    if ($f->sequence_type eq $baseType and $f->system eq $baseCoord) {
      push (@{ $keptSources->{$f->sequence_type}{$f->system} }, $f);
      next;
    }
    ALN: foreach my $a ($f->alignment_sources_to) {
      next ALN unless (defined $a);
      next ALN unless ($a->from_type eq $baseType and $a->from_system eq $baseCoord);
    
      # Find out if we have any alignments for this object type and co-ord system.
      unless (defined $alignMatches{$f->sequence_type}{$f->system}) {
        $dl->dsn( [$a->url] );
        my (undef, $alignments) = each %{ $dl->alignment( { 'query' => $seqAcc } ) };
        if (ref $alignments eq 'ARRAY' and scalar @{$alignments}) {
          $alignMatches{$f->sequence_type}{$f->system} = 1;
        } else {
          $alignMatches{$f->sequence_type}{$f->system} = 0;
        }
      }
      
      push @{ $keptSources->{$f->sequence_type}{$f->system} }, $f 
        if $alignMatches{$f->sequence_type}{$f->system};
      next FEATURE;
    }
  }
  
  my @keptSourcesArr = ();
  my @types = _sortWithPref($baseType, keys %{ $keptSources } );
  foreach my $type (@types) {
    my @systems = _sortWithPref($baseCoord, keys %{ $keptSources->{$type} } );
    foreach my $system (@systems) {
      my $id = $type.'_'.$system;
      $id =~ s/\s+/_/g;
        push (@keptSourcesArr, { type=>$type, system=>$system, servers=>$keptSources->{$type}{$system}, id=>$id } );
    }
  }
  $c->stash->{dasSourcesRs} = \@keptSourcesArr;

  $c->log->debug('Protein::getDasSources: added DAS sources to the stash');
}

#-------------------------------------------------------------------------------


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
  my $rs = $c->model('PfamDB::Pdb_residue')
             ->find( { auto_pfamseq => $c->stash->{pfamseq}->auto_pfamseq },
                     { select       => [
                                         {
                                           count => [ { distinct => [ qw( auto_pdb )] } ]
                                         }
                                       ],
                       as           => [ qw( numberPdbs ) ] } );

  $summaryData{numStructures} = $rs->get_column( 'numberPdbs' );

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
