
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

use utf8;
use strict;
use warnings;

use Storable qw( thaw );
use JSON qw( -convert_blessed_universally );
use Data::Dump qw( dump );
use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'protein' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Handles figuring out the output format and takes care of setting the
appropriate stash key for highlighting tracks.

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') ) {
    if ( $c->req->param('output') eq 'xml' ) {
      $c->stash->{output_xml} = 1;
      $c->res->content_type('text/xml');

      # enable CORS (see http://www.w3.org/wiki/CORS_Enabled)
      $c->res->header( 'Access-Control-Allow-Origin' => '*' );
    }
    elsif ( $c->req->param('output') eq 'pfamalyzer' ) {
      $c->stash->{output_pfamalyzer} = 1;
      $c->res->content_type('text/plain');
    }
  }

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

  # see if the entry is specified as a parameter
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $c->req->param('name')  || # cope with redirects from "swisspfamget.pl"
                      $entry_arg              ||
                      '';

  if ( $tainted_entry ) {
    $c->log->debug( 'Protein::begin: got a tainted entry ' )
      if $c->debug;
    $c->stash->{param_entry} = $tainted_entry;
  }

}

#-------------------------------------------------------------------------------

=head2 protein : Chained

This is the method that takes care of retrieving protein data from the database.
There are two possible entry points, depending whether the accession/ID arrived
as an argument on the URL, in which case we arrive here via a chain, or as a
parameter, in which case the "default" method detaches here. Hopefully the end
result is the same in either case.

=cut

sub protein : Chained( '/' )
              PathPart( 'protein' )
              CaptureArgs( 1 ) {
  my ( $this, $c, $entry_arg ) = @_;

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->stash->{param_entry} ||
                      $entry_arg               ||
                      '';

  # check for multiple protein accessions; if found redirect to the method
  # that handles those specifically for PfamAlyzer
  if ( $tainted_entry =~ m/\,/ ) {
    $c->log->debug( 'Protein::default: got multiple accessions' )
      if $c->debug;
    $c->detach( 'proteins', [ $tainted_entry ] );
    return;
  }

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
  $entry =~ s/^(.{10})\.\d+$/$1/;

  $c->log->debug( "Protein::begin: looking up sequence '$entry'" )
    if $c->debug;

  # retrieve the data for this sequence entry
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#---------------------------------------

=head2 old_protein : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_protein : Path( '/protein' ) {
  my ( $this, $c ) = @_;

  if ( $c->stash->{param_entry} =~ m/\,/ ) {
    $c->log->debug( 'Protein::old_protein: got multiple accessions; detaching to "proteins"' )
      if $c->debug;
    $c->detach( 'proteins', [ $c->stash->{param_entry} ] );
    return;
  }
  else {
    $c->log->debug( 'Protein::old_protein: single accession; redirecting to "protein"' )
      if $c->debug;

    delete $c->req->params->{id};
    delete $c->req->params->{acc};
    delete $c->req->params->{entry};

    $c->res->redirect( $c->uri_for( '/protein', $c->stash->{param_entry}, $c->req->params ) );
  }
}

#-------------------------------------------------------------------------------

=head2 protein_end : Chained

The entry point when the accession/ID is given as an argument on the URL,
e.g. /protein/VAV_HUMAN.

=cut

sub protein_end : Chained( 'protein' )
                  PathPart( '' )
                  Args( 0 ) {
  my ( $this, $c ) = @_;

  # see what we're outputting...
  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Protein::protein_end: emitting XML' ) if $c->debug;

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Protein::protein_end: there was an error: |' .
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
  elsif( $c->stash->{output_pfamalyzer} ) {
    $c->log->debug( 'Protein::protein_end: emitting text for PfamAlyzer' )
      if $c->debug;

    # get the Pfam-A and Pfam-B regions on this sequence
    my @pfamA_regions = $c->model('PfamDB::PfamaRegFullSignificant')
                          ->search( { 'me.pfamseq_acc' => $c->stash->{pfamseq}->pfamseq_acc,
                                      in_full      => 1 },
                                    { prefetch => [ 'pfama_acc', 'pfamseq' ],
                                      order_by => [ 'seq_start' ] } );


    my $regions;
    foreach my $region ( @pfamA_regions ) {
      push @{ $regions->{ $region->pfamseq_acc } }, $region;
    }

    $c->stash->{regions} = $regions;
    $c->stash->{template} = 'rest/protein/entry_pfamalyzer.tt';
  }
  else {
    $c->log->debug( 'Protein::protein_end: emitting HTML' ) if $c->debug;

    # we're going to need to add extra data to the stash, data that is
    # only used in the HTML templates, not the XML templates. Only
    # attempt this if we actually have a sequence to work with
    if ( $c->stash->{pfamseq} ) {
      if (defined $c->stash->{uniprot}) {
          $c->forward('get_annseq_uniprot');
          $c->forward('get_mapping_uniprot');
          $c->forward('get_summary_data_uniprot');
      } else {
        $c->forward('get_annseq');
        $c->forward('get_mapping');
        $c->forward('get_summary_data');
      }

    }
  }

}

#-------------------------------------------------------------------------------

=head2 graphic : Chained( 'protein' ) PathPart( 'graphic' )

Returns just the JSON string describing the domain graphic for this sequence.

=cut

sub graphic : Chained( 'protein' )
              PathPart( 'graphic' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # the earlier methods in the chain get the row for the protein itself, but
  # we need to add the storable for the architecture here explicitly
  $c->forward('get_annseq');

  $c->res->content_type( 'application/json' );
  $c->res->body( $c->stash->{layout} );
}

#-------------------------------------------------------------------------------

=head2 proteins : Chained

Returns a list of the details of the specified proteins, along with the regions
found on them, all in a format specific to PfamAlyzer.

=cut

sub proteins : Chained( 'protein' )
               PathPart( '' )
               Args( 1 ) {
  my ( $this, $c, $tainted_accs ) = @_;

  my @accs;
  foreach my $acc ( split m/\,/, $tainted_accs ) {
    next unless $acc =~ m/^\w+$/;
    push @accs, $acc;
  }

  # get the Pfam-A and Pfam-B regions on this sequence
  my @pfamA_regions = $c->model('PfamDB::PfamaRegFullSignificant')
                        ->search( { 'me.pfamseq_acc' => \@accs,
                                    in_full      => 1 },
                                  {
                                    prefetch => [ qw( pfamseq pfama_acc ) ],
                                    order_by => [ qw( seq_start ) ] } );


  my $regions;
  foreach my $region ( @pfamA_regions ) {
    push @{ $regions->{ $region->pfamseq_acc } }, $region;
  }

  $c->stash->{regions} = $regions;
  $c->stash->{template} = 'rest/protein/entry_pfamalyzer.tt';
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

  if (defined($c->stash->{uniprot}) ){
      $c->log->debug("get_data Arrived from Jump.pm and uniprot flag received")
        if $c->debug;
  }
  # look for the entry itself
  $c->stash->{pfamseq} = $c->model('PfamDB::Pfamseq')
                           ->search( [ { 'me.pfamseq_acc' => $entry },
                                       { pfamseq_id  => $entry } ],
                                     { prefetch => [ qw( annseqs ) ] } )
                           ->single;

  unless ( defined $c->stash->{pfamseq} ) {
    $c->log->debug( "Protein::get_data: no such entry |$entry|; checking as a secondary accession" )
      if $c->debug;

    # see if this is really a secondary accession
    $c->stash->{secondary} = $c->model('PfamDB::SecondaryPfamseqAcc')
                               ->search( { secondary_acc => $entry },
                                         { prefetch      => [ qw( pfamseq_acc ) ] } )
                               ->single;
    # set a flag to show that we got a secondary accession, so that the
    # template add a message to that effect to the page
    if ( $c->stash->{secondary} ) {
      $c->log->debug( "Protein::get_data: '$entry' looks like a secondary accession" )
        if $c->debug;
      $c->stash->{pfamseq} = $c->stash->{secondary}->pfamseq_acc;
      $c->stash->{from_secondary_acc} = $entry;
    }
  }

  #if pfamseq still hasn't been set we should check uniprot
  unless ($c->stash->{pfamseq}) {
      $c->log->debug("Protein::get_data: '$entry' searching in uniprot")
        if $c->debug;
      $c->stash->{pfamseq} = $c->model('PfamDB::UniProt')
                               ->search( [ { 'me.uniprot_acc' => $entry },
                                           { uniprot_id  => $entry } ])
                               ->single;
      # set a flag to show that this is a uniprot entry for any further
      #processing
      $c->stash->{uniprot} = 1 if $c->stash->{pfamseq};
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
             ->search( { 'me.pfamseq_acc' => $c->stash->{pfamseq}->pfamseq_acc,
                         in_full           => 1 },
                       { prefetch => [ qw( pfama_acc ) ] } );
  $c->stash->{pfama_regions} = \@pfama_regions;

  $c->log->debug( 'Protein::get_regions: found '
                  . scalar( @{ $c->stash->{pfama_regions} } ) . ' Pfam-A hits' )
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

  $c->log->debug( 'Protein::get_annseq: got a storable; encoding as JSON' )
    if $c->debug;

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
                  ->search( { 'me.pfamseq_acc' => $c->stash->{pfamseq}->pfamseq_acc,
                              'pdb_res_start'             => \'!= pdb_res_end' },
                            { prefetch => [ qw( pfama_acc
                                                pfamseq_acc
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

  # number of structures. Take directly from the mapping that we already retrieved
  my %pdb_ids = map { $_->pdb_id->pdb_id => 1 } @{ $c->stash->{pfamMaps} };

  $summaryData{numStructures} = scalar keys %pdb_ids;

  $c->stash->{summaryData} = \%summaryData;

  my @pfama_regions = $c->model('PfamDB::PfamaRegFullSignificant')
                        ->search( { 'me.pfamseq_acc' => $c->stash->{pfamseq}->pfamseq_acc,
                                    in_full => 1 },
                                  { prefetch => [ qw( pfama_acc pfamseq_acc ) ] } );

  my @other_regions = $c->model('PfamDB::OtherReg')
                        ->search( { 'me.pfamseq_acc' => $c->stash->{pfamseq}->pfamseq_acc },
                                  {} );

  my $regions;
  foreach my $region ( @pfama_regions, @other_regions ) {
    push @{ $regions->{ $region->seq_start } }, $region;
  }

  $c->stash->{regions} = $regions;

  my $ncbi_taxid = $c->stash->{pfamseq}->ncbi_taxid;

  my $cp = $c->model('PfamDB::CompleteProteomes')
             ->find( { ncbi_taxid => $ncbi_taxid } );

  if ( $cp ) {
    $c->log->debug("Protein::get_summary_data: found a complete proteome for $ncbi_taxid")
      if $c->debug;
    $c->stash->{complete_proteome} = 1;
  }

  $c->log->debug('Protein::get_summary_data: added the summary data to the stash')
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_annseq_uniprot : Private

Retrieves and stashes the Storable for the sequence annotation data structure.

=cut

sub get_annseq_uniprot : Private {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Protein::get_annseq_uniprot: adding annseq storable' ) if $c->debug;


  #$c->log->debug( 'Protein::get_annseq_uniprot: got a storable; encoding as JSON' )
  #  if $c->debug;

}

#-------------------------------------------------------------------------------

=head2 get_mapping_uniprot : Private

Gets the structure-to-sequence-to-family mapping.

=cut

sub get_mapping_uniprot : Private {
  my ( $this, $c ) = @_;

  # note the use of the ref-to-scalar for the second part of the where clause.
  # We need to make sure that that constraint gets interpreted as
  #
  #     ... where pdb_res_start != pdb_res_end and ...
  #
  # my @mapping = $c->model('PfamDB::PdbPfamaReg')
  #                 ->search( { 'me.pfamseq_acc' => $c->stash->{pfamseq}->pfamseq_acc,
  #                             'pdb_res_start'             => \'!= pdb_res_end' },
  #                           { prefetch => [ qw( pfama_acc
  #                                               pfamseq_acc
  #                                               pdb_id ) ] } );
  #
  # $c->stash->{pfamMaps} = \@mapping;
  #
  # $c->log->debug('Protein::get_mapping_uniprot: added the structure mapping to the stash')
  #   if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Gets the data items for the overview bar

=cut

sub get_summary_data_uniprot : Private {
  my ( $this, $c ) = @_;

  my %summaryData;

  # # first, the number of sequences... pretty easy...
  $summaryData{numSequences} = 1;

  # # also, the number of architectures
  $summaryData{numArchitectures} = 1;

  # # number of species
  $summaryData{numSpecies} = 1;

  # # number of structures. Take directly from the mapping that we already retrieved
  my %pdb_ids = map { $_->pdb_id->pdb_id => 1 } @{ $c->stash->{pfamMaps} };

  $summaryData{numStructures} = scalar keys %pdb_ids;

  $c->stash->{summaryData} = \%summaryData;
  #

  my @pfama_regions = $c->model('PfamDB::UniprotRegFull')
                       ->search( { 'me.uniprot_acc' => $c->stash->{pfamseq}->uniprot_acc,
                                   in_full => 1 },
                                 { prefetch => [ qw( uniprot_acc uniprot_acc ) ] } );
  my @other_regions = $c->model('PfamDB::OtherReg')
                        ->search( { 'me.pfamseq_acc' => $c->stash->{pfamseq}->uniprot_acc },
                                 {} );

  my $regions;
  foreach my $region ( @pfama_regions, @other_regions ) {
   push @{ $regions->{ $region->seq_start } }, $region;
  }

  $c->stash->{regions} = $regions;

  my $ncbi_taxid = $c->stash->{pfamseq}->ncbi_taxid;

  my $cp = $c->model('PfamDB::CompleteProteomes')
              ->find( { ncbi_taxid => $ncbi_taxid } );

  if ( $cp ) {
     $c->log->debug("Protein::get_summary_data_uniprot: found a complete proteome for $ncbi_taxid")
      if $c->debug;
     $c->stash->{complete_proteome} = 1;
  }

  $c->log->debug('Protein::get_summary_data_uniprot: added the summary data to the stash')
   if $c->debug;

   #create a quick and dirty uniprot domain graphic
   $c->forward('_drawUniProt');
}

sub _drawUniProt : Private {
  my ( $this, $c ) = @_;

  # #copied in from PfamLib::Pfam::ViewProcess::Storable (pt1)
  # my $nestings;
  # foreach my $n ( @{ $self->pfamdb->getAllNestedDomains } ) {
  #   my $npfamA =
  #     $self->pfamdb->getSchema->resultset('PfamA')->find( { pfama_acc => $n->nests_pfama_acc->pfama_acc } )
  #     ->pfama_acc;
  #
  #   my $pfamA = $n->pfama_acc->pfama_acc;
  #   $nestings->{$pfamA}->{$npfamA}++;
  # }
  # #copied in from PfamLib::Pfam::ViewProcess::Storable (pt2)
  # #It nests
  # $regions[$#regions]->end( $pfamaRegionsRef->[$j]->seq_start - 1 );
  # $regions[$#regions]->aliEnd( $pfamaRegionsRef->[$j]->seq_start - 1 );
  # push(
  #   @regions,
  #   Bio::Pfam::Sequence::Region->new(
  #     {
  #       start       => $pfamaRegionsRef->[$j]->seq_end + 1,
  #       end         => $region->seq_end,
  #       aliStart    => $pfamaRegionsRef->[$j]->seq_end + 1,
  #       aliEnd      => $region->ali_end,
  #       modelStart  => $region->model_start,
  #       modelEnd    => $region->model_end,
  #       modelLength => $region->model_length,
  #       metadata    => Bio::Pfam::Sequence::MetaData->new(
  #         {
  #           accession   => $region->pfama_acc,
  #           identifier  => $region->pfama_id,
  #           type        => $region->type,
  #           description => $region->description,
  #           score       => $region->domain_evalue_score,
  #           scoreName   => 'e-value',
  #           start       => $region->seq_start,
  #           end         => $region->seq_end,
  #           aliStart    => $region->ali_start,
  #           aliEnd      => $region->ali_end,
  #           database    => 'pfam'
  #         }
  #       ),
  #       type => 'pfama'
  #     }
  #   )
  # );


  $c->log->debug( 'Protein::_drawUniProt: drawing uniprot domain graphic' ) if $c->debug;
  my $meta = Bio::Pfam::Sequence::MetaData->new(
    {
      organism    => $c->stash->{pfamseq}->species,
      taxid       => $c->stash->{pfamseq}->ncbi_taxid,
      accession   => $c->stash->{pfamseq}->uniprot_acc,
      identifier  => $c->stash->{pfamseq}->uniprot_id,
      description => $c->stash->{pfamseq}->description,
      database    => 'uniprot'
    }
  );

  my (@regions, @markups, %nestingRegions, @sortedRegions, $containerRegion, @topLevelRegions, %regionArrangement);
  #1. ensure that domains are sorted so that nested domains are always seen within the containing domain
  my @sortedRegionStarts = sort{$a <=> $b} keys(%{$c->stash->{regions}});
  foreach my $regionStart (@sortedRegionStarts) {
    foreach my $region (@{$c->stash->{regions}->{$regionStart}}) {
      push(@sortedRegions, $region);
      my @nested_domains = $c->model('PfamDB::NestedDomains')
                           ->search( { 'me.pfama_acc' => $region->pfama_acc->pfama_acc }, {} );
      foreach my $nested_domain (@nested_domains) {
          $nestingRegions{$nested_domain->nests_pfama_acc->pfama_acc} = $region->pfama_acc->pfama_acc;
      }
    }
  }

  #2. make a %regionArrangement hash structure of regions which nest other regions
  for(my $i=0; $i < scalar @sortedRegions; $i++) {
    my $currRegion = $sortedRegions[$i];
    if ($i > 0 && !defined $containerRegion) {
      $containerRegion = $sortedRegions[$i-1];
    }

    #previous region is a container region and current is nested
    if (defined $containerRegion
        && $nestingRegions{$currRegion->pfama_acc->pfama_acc} eq $containerRegion->pfama_acc->pfama_acc
        && $currRegion->seq_start >= $containerRegion->seq_start
        && $currRegion->seq_end <= $containerRegion->seq_end) {
          push(@{$regionArrangement{$containerRegion->pfama_acc->pfama_acc}}, $currRegion);
    } else {
      #current region is either a self contained region or a container
      $regionArrangement{$currRegion->pfama_acc->pfama_acc} = [];
      $containerRegion = undef;
      push(@topLevelRegions, $currRegion);
    }
  }

  #3. create objects for web display
  foreach my $region (@topLevelRegions) {
    if (scalar @{$regionArrangement{$region->pfama_acc->pfama_acc}} > 0) {
      my $seq_start = $region->seq_start;
      my $seq_end = $region->seq_end;
      my $ali_start = $region->ali_start;
      my $ali_end = $region->ali_end;

      foreach my $nestedRegion (@{$regionArrangement{$region->pfama_acc->pfama_acc}}) {
        $seq_end = $nestedRegion->seq_start-1;
        $ali_end = $nestedRegion->seq_start-1;

        my $containerRegionObj = _drawRegion($region,
                                    $seq_start,
                                    $seq_end,
                                    $ali_start,
                                    $ali_end);
        push(@regions, $containerRegionObj);

        my $regionObj = _drawRegion($nestedRegion,
                                    $nestedRegion->seq_start,
                                    $nestedRegion->seq_end,
                                    $nestedRegion->ali_start,
                                    $nestedRegion->ali_end);
        push(@regions, $regionObj);

        $seq_start = $nestedRegion->ali_end+1;
        $ali_start = $nestedRegion->ali_end+1;

        my $markup = Bio::Pfam::Sequence::Markup->new(
          {
            start    => $nestedRegion->seq_start - 1,
            end      => $nestedRegion->seq_end + 1,
            type     => 'Nested',
            colour   => '#00ffff',
            lineColour => '#ff0000',
            metadata => Bio::Pfam::Sequence::MetaData->new(
              {
                database => 'pfam',
                start    => $nestedRegion->seq_start - 1,
                end      => $nestedRegion->seq_end + 1,
                type     => 'Link between discontinous regions'
              }
            ),
          }
        );
        push(@markups, $markup);
      }

      $seq_end = $region->seq_end;
      $ali_end = $region->ali_end;
      my $containerRegionObj = _drawRegion($region,
                                  $seq_start,
                                  $seq_end,
                                  $ali_start,
                                  $ali_end);
      push(@regions, $containerRegionObj);

    } else {
      my $regionObj = _drawRegion($region,
                                  $region->seq_start,
                                  $region->seq_end,
                                  $region->ali_start,
                                  $region->ali_end);
      push(@regions, $regionObj);
    }

  }

  my $seqObj = Bio::Pfam::Sequence->new(
    {
      metadata => $meta,
      length   => $c->stash->{pfamseq}->length,
      regions  => \@regions,
      motifs   => [],
      markups  => \@markups,
    }
  );

  $c->stash->{seqs} = [$seqObj];

  my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
  $lm->layoutSequences( $c->stash->{seqs} );

  # configure the JSON object to correctly stringify the layout manager output
  my $json = new JSON;
  $json->pretty(1);
  $json->allow_blessed;
  $json->convert_blessed;

  # encode and stash the sequences as a JSON string
  $c->stash->{layout} = $json->encode( $c->stash->{seqs} );
}

sub _drawRegion {
  my ($region, $start, $end, $ali_start, $ali_end) = @_;
  my $regionObj = Bio::Pfam::Sequence::Region->new( {
      start       => $start,
      end         => $end,
      aliStart    => $ali_start,
      aliEnd      => $ali_end,
      modelStart  => $region->model_start,
      modelEnd    => $region->model_end,
      modelLength => $region->pfama_acc->model_length,
      metadata    => Bio::Pfam::Sequence::MetaData->new( {
          accession   => $region->pfama_acc->pfama_acc,
          identifier  => $region->pfama_acc->pfama_id,
          type        => $region->pfama_acc->type,
          description => $region->pfama_acc->description,
          score       => $region->domain_evalue_score,
          scoreName   => 'e-value',
          start       => $start,
          end         => $end,
          aliStart    => $ali_start,
          aliEnd      => $ali_end,
          database    => 'pfam'
        }
      ),
      type => 'pfama'
    }
  );
  return $regionObj
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
    my $i = ( lc $a ) cmp ( lc $b );
    return $i if $i == 0;
    if ( $a eq $pref ) {
      $i = -1;
    }
    elsif ( $b eq $pref ) {
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
