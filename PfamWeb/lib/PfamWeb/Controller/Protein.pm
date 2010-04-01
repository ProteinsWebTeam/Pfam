
# Protein.pm
# jt6 20060427 WTSI
#
# $Id: Protein.pm,v 1.43 2009-11-23 13:05:33 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Protein - controller to build the main protein
page

=cut

package PfamWeb::Controller::Protein;

=head1 DESCRIPTION

This is intended to be the base class for everything related to
UniProt entries across the site. 
Generates a B<tabbed page>.

$Id: Protein.pm,v 1.43 2009-11-23 13:05:33 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw( thaw );
use JSON qw( -convert_blessed_universally );

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
  if ( not $c->stash->{output_xml} ) {
    $c->log->debug( 'Protein::begin: emitting HTML' ) if $c->debug;

    # we're going to need to add extra data to the stash, data that is
    # only used in the HTML templates, not the XML templates. Only 
    # attempt this if we actually have a sequence to work with
    if ( $c->stash->{pfamseq} ) {
      $c->forward('get_annseq');
      $c->forward('get_mapping');
      $c->forward('get_summary_data');

      # not currently needed
      # $c->forward('get_das_sources');
    }
    
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
    # there were no errors retrieving data and we now know that we're going 
    # to need the regions
    $c->forward('get_regions');

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
  
  $c->log->debug( 'Protein::get_data: adding protein data' ) if $c->debug;

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
    $c->stash->{pfamseq} = $c->model('PfamDB::SecondaryPfamseqAcc')
                             ->search( { secondary_acc => $entry },
                                       { prefetch      => [ qw( auto_pfamseq ) ] } )
                             ->single;
  }
  
  unless ( $c->stash->{pfamseq} ) {
    $c->log->debug('Protein::get_data: failed to retrieve a pfamseq object')
      if $c->debug;

    $c->stash->{errorMsg} = 'No valid UniProt accession or ID';

    return;
  }
}

#-------------------------------------------------------------------------------

=head2 get_regions : Private

Retrieves and stashes the regions for this protein.

=cut

sub get_regions : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Protein::get_regions: adding region info' ) if $c->debug;
  
  my @pfama_regions = $c->model('PfamDB::PfamaRegFullSignificant')
             ->search( { 'me.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq,
                         in_full           => 1 },
                       { prefetch => [ qw( auto_pfama ) ] } );
  $c->stash->{pfama_regions} = \@pfama_regions;

  $c->log->debug( 'Protein::get_regions: found ' 
                  . scalar( @{ $c->stash->{pfama_regions} } ) . ' Pfam-A hits' )
      if $c->debug;
  
  # add Pfam-B regions
  my @pfamb_regions = $c->model('PfamDB::PfambReg')
          ->search( { 'me.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq },
                    { prefetch => [ qw( auto_pfamb ) ] } );
  $c->stash->{pfamb_regions} = \@pfamb_regions; 

  $c->log->debug( 'Protein::get_regions: found ' 
                  . scalar( @{ $c->stash->{pfamb_regions} } ) . ' Pfam-B hits' )
      if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_annseq : Private

Retrieves and stashes the Storable for the sequence annotation data structure.

=cut

sub get_annseq : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Protein::get_annseq: adding annseq storable' ) if $c->debug;

  my $storable = thaw $c->stash->{pfamseq}->annseqs->annseq_storable;
  return unless defined $storable;

  $c->log->debug( 'Protein::get_annseq: got a storable; encoding as JSON' ) if $c->debug;

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

  # note the use of the ref-to-scalar for the second part of the where clause.
  # We need to make sure that that constraint gets interpreted as 
  #
  #     ... where pdb_res_start != pdb_res_end and ...
  
  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { 'auto_pfamseq.auto_pfamseq' => $c->stash->{pfamseq}->auto_pfamseq,
                              'pdb_res_start'             => \'!= pdb_res_end' },
                            { prefetch => [ qw( auto_pfama
                                                auto_pfamseq
                                                pdb_id ) ] } );

  $c->stash->{pfamMaps} = \@mapping;

  $c->log->debug('Protein::get_mapping: added the structure mapping to the stash')
    if $c->debug;
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
