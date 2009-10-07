
# Protein.pm
# jt6 20060427 WTSI
#
# $Id: Protein.pm,v 1.41 2009-10-07 10:29:18 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Protein - controller to build the main protein
page

=cut

package PfamWeb::Controller::Protein;

=head1 DESCRIPTION

This is intended to be the base class for everything related to
UniProt entries across the site. 
Generates a B<tabbed page>.

$Id: Protein.pm,v 1.41 2009-10-07 10:29:18 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw( thaw );
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'protein' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the UniProt entry.

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

  #----------------------------------------

  # see if we should highlight a particular DAS track (or tracks) in the 
  # features tab
  if ( defined $c->req->param('highlight') ) {
    my ( $highlight ) = $c->req->param('highlight') =~ m/^([\w\s]+)$/;
    if ( defined $highlight ) {
      $c->log->debug( "Protein::begin: highlighting tracks for '$highlight'" )
        if $c->debug;
      $c->stash->{highlightSource} = $highlight;
    }
  }

  #----------------------------------------

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $c->req->param('name')  || # cope with redirects "swisspfamget.pl"
                      $entry_arg              ||
                      '';
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;
    $c->stash->{errorMsg} = 'Invalid UniProt accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No UniProt accession or ID specified';
  }
  
  # strip off sequence versions, if present
  $entry =~ s/^(.{6})\.\d+$/$1/;

  $c->log->debug( "Protein::begin: looking up sequence '$entry'" )
    if $c->debug;

  # retrieve the data for this sequence entry  
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
  
  #----------------------------------------

  # if we're outputting HTML, we're done here
  unless ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Protein::begin: emitting HTML' ) if $c->debug;
    return;
  }

  #----------------------------------------
  # from here on we're handling XML output

  $c->log->debug( 'Protein::begin: emitting XML' ) if $c->debug;

  # if there was an error...
  if ( $c->stash->{errorMsg} ) {
    $c->log->debug( 'Protein::begin: there was an error: |' .
                    $c->stash->{errorMsg} . '|' ) if $c->debug;
    $c->stash->{template} = 'rest/protein/error_xml.tt';
    return;
  }
  else {    
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
  my ( $this, $c, $entry ) = @_;
  
  # look for the entry itself
  $c->stash->{pfamseq} = $c->model('PfamDB::Pfamseq')
                           ->search( [ { pfamseq_acc => $entry },
                                       { pfamseq_id  => $entry } ], 
                                     { prefetch => [ qw( annseqs ) ] } )
                           ->single;
                           
  unless ( defined $c->stash->{pfamseq} ) {
    $c->log->debug( "Protein::get_data: no such entry |$entry|; checking as a secondary accession" )
      if $c->debug;

    # see if this is really a secondary accession
    $c->stash->{pfamseq} = $c->model('PfamDB::Secondary_pfamseq_acc')
                             ->search( { secondary_acc => $entry },
                                       { prefetch      => [ qw( pfamseq ) ] } )
                             ->single;
  }
  
  unless ( $c->stash->{pfamseq} ) {
    $c->log->debug('Protein::get_data: failed to retrieve a pfamseq object')
      if $c->debug;

    $c->stash->{errorMsg} = 'No valid UniProt accession or ID';

    return;
  }
    
  #----------------------------------------

  # add Pfam-A regions  
  $c->log->debug( 'Protein::get_data: adding region info' ) if $c->debug;
  
  $c->stash->{pfama_regions} = $c->model('PfamDB::PfamaRegFullSignificant')
                    ->search( { 'me.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq,
                                in_full           => 1 },
                              { prefetch => [ qw( auto_pfama ) ] } );
  
  # add Pfam-B regions
  $c->stash->{pfamb_regions} = $c->model('PfamDB::PfambReg')
                    ->search( { 'me.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq },
                              { prefetch => [ qw( auto_pfamb ) ] } );
  
  #----------------------------------------

  my $storable = thaw $c->stash->{pfamseq}->annseqs->annseq_storable;
  if ( defined $storable ) {
    $c->stash->{seqs} = [ $storable ];

    my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
    $lm->layoutSequences( $c->stash->{seqs} );

    # configure the JSON object to correctly stringify the layout manager output
    my $json = new JSON;
    # $json->pretty(1);
    $json->allow_blessed;
    $json->convert_blessed;

    # encode and stash the sequences as a JSON string
    $c->stash->{layout} = $json->encode( $c->stash->{seqs} );
  }

  # if we've arrived here from the top-level controller, rather than from one 
  # of the sub-classes, we will be displaying a key for the domain graphic.
  # For now at least, the sub-classes, such as the interactive feature viewer,
  # don't bother with the key, so they don't need this extra blob of data in 
  # the stash. 
  # $c->forward( 'generate_key', [ $layoutPfam ] )
  #   if ref $this eq 'PfamWeb::Controller::Protein';

  #----------------------------------------

  # add extra data to the stash  
  # $c->forward('generate_pfam_graphic');
  # $c->forward('get_das_sources');
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

  $c->log->debug('Protein::get_das_sources: added DAS sources to the stash')
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_mapping : Private

Gets the structure-to-sequence-to-family mapping.

=cut

sub get_mapping : Private {
  my ( $this, $c ) = @_;

  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { 'auto_pfamseq.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq },
                            { prefetch              => [ qw( auto_pfama
                                                             auto_pfamseq
                                                             pdb_id ) ] } );

  $c->stash->{pfamMaps} = \@mapping;

  $c->log->debug('Protein::begin: added the structure mapping to the stash')
    if $c->debug;
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
  $c->log->debug('Protein::generate_pfam_graphic: instantiated a layout manager')
    if $c->debug;

  # retrieve the Storable containing the annotated sequence, thaw it
  # and hand it off to the layout manager
  my $annseq;
  eval {
    $c->log->debug( 'Protein::generate_pfam_graphic: pfamseq:  ' . ref $c->stash->{pfamseq} )
      if $c->debug;

    my $annseq_row = $c->stash->{pfamseq}->annseqs;
    $c->log->debug( 'Protein::generate_pfam_graphic: annseq:   ' . ref $annseq_row )
      if $c->debug;

    my $storable   = $annseq_row->annseq_storable;
    $c->log->debug( 'Protein::generate_pfam_graphic: storable: ' . ref $storable )
      if $c->debug;

    $annseq = thaw( $storable );
  };
  if ($@) {
    $c->log->error("Protein::generate_pfam_graphic: ERROR: failed to thaw annseq: $@");
    return;
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

  # should we use a document store rather than temp space for the images ?
  my $imageset;  
  if ( $c->config->{use_image_store} ) {
    $c->log->debug( 'Protein::generate_pfam_graphic: using document store for image' )
      if $c->debug;
    require PfamWeb::ImageSet;
    $imageset = PfamWeb::ImageSet->new;
  }
  else {
    $c->log->debug( 'Protein::generate_pfam_graphic: using temporary directory for image' )
      if $c->debug;
    require Bio::Pfam::Drawing::Image::ImageSet;
    $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  }

  $imageset->create_images( $layoutPfam->layout_to_XMLDOM );
  $c->log->debug('Protein::generate_pfam_graphic: created images')
    if $c->debug;

  $c->stash->{pfamImageset} = $imageset;

  $c->log->debug('Protein::generate_pfam_graphic: successfully generated an imageset object')
    if $c->debug;
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
  my $rs = $c->model('PfamDB::PdbResidueData')
             ->search( { auto_pfamseq => $c->stash->{pfamseq}->auto_pfamseq },
                     { select       => [
                                         {
                                           count => [ { distinct => [ qw( pdb_id )] } ]
                                         }
                                       ],
                       as           => [ qw( numberPdbs ) ] } )
             ->single;

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

  $c->log->debug('Protein::get_summary_data: added the summary data to the stash')
    if $c->debug;
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
