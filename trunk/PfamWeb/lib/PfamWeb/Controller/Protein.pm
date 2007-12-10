
# Protein.pm
# jt6 20060427 WTSI
#
# $Id: Protein.pm,v 1.34 2007-12-10 14:40:29 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Protein - controller to build the main protein
page

=cut

package PfamWeb::Controller::Protein;

=head1 DESCRIPTION

This is intended to be the base class for everything related to
UniProt entries across the site. 
Generates a B<tabbed page>.

$Id: Protein.pm,v 1.34 2007-12-10 14:40:29 jt6 Exp $

=cut

use strict;
use warnings;

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
  my( $this, $c, $entry_arg ) = @_;

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if( defined $c->req->param('output') and
      $c->req->param('output') eq 'xml' ) {
    $c->stash->{output_xml} = 1;
    $c->res->content_type('text/xml');
  }

  #----------------------------------------

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';
  
  my $entry;
  if( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;
    $c->stash->{errorMsg} = 'Invalid UniProt accession or ID' 
      unless defined $entry;
  } else {
    $c->stash->{errorMsg} = 'No UniProt accession or ID specified';
  }

  # retrieve the data for this sequence entry  
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
  
  #----------------------------------------

  # if we're outputting HTML, we're done here
  unless( $c->stash->{output_xml} ) {
    $c->log->debug( 'Protein::begin: emitting HTML' ) if $c->debug;
    return;
  }

  #----------------------------------------
  # from here on we're handling XML output

  $c->log->debug( 'Protein::begin: emitting XML' ) if $c->debug;

  # if there was an error...
  if( $c->stash->{errorMsg} ) {
    $c->log->debug( 'Protein::begin: there was an error: |' .
                    $c->stash->{errorMsg} . '|' ) if $c->debug;
    $c->stash->{template} = 'rest/protein/error_xml.tt';
    return;
  } else {    
    $c->stash->{template} = 'rest/protein/entry_xml.tt';
  }
  
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves the data for the specified entry from the pfamseq table. Requires
a single argument, the ID or accession of the entry. Drops the resulting row,
if any, into the stash. The entry identifier should be detainted before calling
this method.

=cut

sub get_data : Private {
  my( $this, $c, $entry ) = @_;
  
  # look for the entry itself
  my $rs = $c->model('PfamDB::Pfamseq')
             ->search( [ { pfamseq_acc => $entry },
                         { pfamseq_id  => $entry } ] );
                           
  my $p;
  if( defined $rs ) {
    # we got a resultset using that accession/id
    $p = $rs->first;
  
  } else {
    # ... otherwise, see if this is really a secondary accession
    $p = $c->model('PfamDB::Secondary_pfamseq_acc')
          ->find( { secondary_acc => $1 },
                  { join          => [ qw( pfamseq ) ],
                    prefetch      => [ qw( pfamseq ) ] } );
  }
  
  unless( $p ) {
    $c->log->debug('Protein::get_data: failed to retrieve a pfamseq object')
      if $c->debug;
    $c->stash->{errorMsg} = 'No valid UniProt accession or ID';
    return;
  }
    
  $c->stash->{pfamseq} = $p;

  # add Pfam-A regions  
  $c->log->debug( 'Protein::get_data: adding region info' ) if $c->debug;
  my @a_regions = $c->model('PfamDB::PfamA_reg_full_significant')
                    ->search( { 'me.auto_pfamseq' => $p->auto_pfamseq,
                                in_full           => 1 },
                              { join     => [ qw( pfamA ) ],
                                prefetch => [ qw( pfamA ) ] } );
  $c->stash->{pfama_regions} = \@a_regions;
  
  # add Pfam-B regions
  my @b_regions = $c->model('PfamDB::PfamB_reg')
                    ->search( { 'me.auto_pfamseq' => $p->auto_pfamseq },
                              { join     => [ qw( pfamB ) ],
                                prefetch => [ qw( pfamB ) ] } );
  $c->stash->{pfamb_regions} = \@b_regions;
  
  # add extra data to the stash  
  $c->forward('generate_pfam_graphic');
  $c->forward('get_das_sources');
  $c->forward('get_mapping');
  $c->forward('get_summary_data');
  
}

#-------------------------------------------------------------------------------

=head2 get_das_sources : Private

Retrieves DAS sources with the appropriate object type and coordinate system.

=cut

sub get_das_sources : Private {
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

  $c->log->debug('Protein::get_das_sources: added DAS sources to the stash');
}

#-------------------------------------------------------------------------------

=head2 get_mapping : Private

Gets the structure-to-sequence-to-family mapping.

=cut

sub get_mapping : Private {
  my ( $this, $c ) = @_;

  my @mapping = $c->model('PfamDB::Pdb_pfamA_reg')
                  ->search( { 'pfamseq.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq },
                            {  join                  => [ qw( pfamA pfamseq pdb ) ],
                               prefetch              => [ qw( pfamA pfamseq pdb ) ] } );

  $c->stash->{pfamMaps} = \@mapping;

  $c->log->debug('Protein::begin: added the structure mapping to the stash');
}

#-------------------------------------------------------------------------------

=head2 generate_pfam_graphic : Private

Generates the Pfam graphic.

=cut

sub generate_pfam_graphic : Private {
  my ( $this, $c ) = @_;

  # get a layout manager and set the X scale
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layoutPfam->scale_x(1);
  $c->log->debug('Protein::generate_pfam_graphic: instantiated a layout manager');

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
  $c->forward( 'generate_key', [ $layoutPfam ] )
    if ref $this eq 'PfamWeb::Controller::Protein';

  # and build an imageset
  my $pfamImageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $pfamImageset->create_images( $layoutPfam->layout_to_XMLDOM );
  $c->log->debug('Protein::generate_pfam_graphic: created images');

  $c->stash->{pfamImageset} = $pfamImageset;

  $c->log->debug('Protein::generate_pfam_graphic: successfully generated an imageset object');
}

#-------------------------------------------------------------------------------

=head2 generate_key : Private

Generates a data structure representing the key to the domain image, which 
can be formatted sensibly by the view. Needs a reference to the 
L<LayoutManager> that will build the Pfam graphic.

=cut

sub generate_key : Private {
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
  #                { select         => [
  #                                      {
  #                                        count => [ { distinct => [ 'auto_int_pfamAs' ] } ]
  #                                     }
  #                                    ],
  #                  as             => [ qw( numInts ) ] } );

  #$summaryData{numInt} = $rs->get_column( 'numInts' );
  $summaryData{numInt} = 0;
  $c->stash->{summaryData} = \%summaryData;

  $c->log->debug('Protein::get_summary_data: added the summary data to the stash');
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
