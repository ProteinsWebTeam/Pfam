
# StructuralDomains.pm
# rdf 20060823 WTSI
#
# $Id: StructuralDomains.pm,v 1.7 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::StructuralDomains - controller for generating
domain graphics for Pfam, cath and scop

=cut

package PfamWeb::Controller::Structure::StructuralDomains;

=head1 DESCRIPTION

Retrieves domain information from SCOP and CATH web services and draws
separate domain graphics showing Pfam, SCOP and CATH definitions of
domains.

Generates a B<page fragment>.

$Id: StructuralDomains.pm,v 1.7 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;
use Storable qw(thaw);
use Data::Dumper;
use XML::LibXML;
use XML::LibXML::XPathContext;

use Bio::SeqFeature::Generic;

use Bio::Pfam::WebServices::Client::Scop;
use Bio::Pfam::WebServices::Client::Cath;
use Bio::Pfam::CATHRegion;
use Bio::Pfam::SCOPRegion;

use base "PfamWeb::Controller::Structure";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getStructuralDomains : Path

This method relies on the PDB ID being retrieved from the URL by the
begin method from the parent. If it's handed a valid ID, it tries to
retrieve domain information from SCOP and CATH and then renders the
domain graphics.

Picks up a URL like http://localhost:3000/structure/structuraldomains?id=1w9h

=cut

sub getStructuralDomains : Path {
  my( $this, $c ) = @_;
  $c->log->debug("Getting structural domains for:".$c->stash->{pdbId}.":");

  $c->log->debug("Contacting WebServices using ".$this->{wsProxy}." proxy");
  my $annSeqs = {};

  my $cathSuccess = 0;
  eval {
    my $cathClient = Bio::Pfam::WebServices::Client::Cath->new(-proxy => $this->{wsProxy},
                                   -pdbId => $c->stash->{pdbId});
    $cathClient->queryService;
  
    die "No valid response from CATH service" unless ref $cathClient->_response;
  
    if($cathClient->_response){
      #Step 1: get a sequence for each chain.
      $annSeqs->{CATH} = $c->forward( "getStructureSeqs", [ qw/CATH/ ] );
      #Step 2: parse the response and add the CATH domains
      $cathSuccess = $c->forward( "addCathDomains", [ $annSeqs, $cathClient ] );
    }
  };

  my $scopSuccess = 0;
  eval {
    my $scopClient = Bio::Pfam::WebServices::Client::Scop->new(-proxy => $this->{wsProxy},
                                   -pdbId => $c->stash->{pdbId});
    $scopClient->queryService;
    
    die "No valid response from CATH service" unless ref $scopClient->_response;
  
    if($scopClient->_response){
      #Step 1: get a sequence for each chain.
      $annSeqs->{SCOP} = $c->forward( "getStructureSeqs", [ qw/SCOP/ ] );
      #Step 2: parse the response and add the SCOP domains
      $scopSuccess = $c->forward( "addScopDomains", [ $annSeqs, $scopClient ] );
    }
  };

  # make sure that either Cath or SCOP gave us something sensible
  return unless( $cathSuccess or $scopSuccess );

  $c->log->debug("Finished contacting WebServices");

  #Now we need to get the Pfam seqs
  $annSeqs->{PFAM} = $c->forward( "getPfamSeqs" );

  #Now Merge PfamSeqs and Structure Chains;
  my @seqs;

  # keep a mapping between the array index for @seqs and the database
  # name, so that we can label the graphics in the final page as SCOP
  # or CATH
  $c->stash->{seqMapping} = {};

  foreach my $pfamseqId (keys %{$annSeqs->{PFAM}}){
    $c->log->debug("Got $pfamseqId");
  
    push @seqs, $annSeqs->{PFAM}->{$pfamseqId}->{seq};
    $c->stash->{seqMapping}->{0} = "Pfam";
  
    foreach my $chain ( sort{ $a cmp $b } keys %{ $annSeqs->{PFAM}->{$pfamseqId}->{chains} } ){
      foreach my $structureDB (qw/SCOP CATH/){
        if($annSeqs->{$structureDB}->{$pfamseqId}->{$chain}->{seq}){
          $c->log->debug("Got $pfamseqId, $chain, $structureDB bioperl object");
          push @seqs, $annSeqs->{$structureDB}->{$pfamseqId}->{$chain}->{seq};
          $c->stash->{seqMapping}->{scalar @seqs - 1} = $structureDB;
        }
      }
    }
  }

  # render All the sequences
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->scale_x( $this->{scale_x} );
  $layout->scale_y( $this->{scale_y} );

  my %regionsAndFeatures = ( PfamA      => 1,
               SCOP       => 1,
               CATH       => 1,
               Disordered => 1,
               noFeatures => 0 );

  $layout->layout_sequences_with_regions_and_features( \@seqs,
                             \%regionsAndFeatures );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );
  foreach my $image ($imageset->each_image){
    $image->print_image;
  }

  $c->stash->{domainImages} = $imageset;
}

#-------------------------------------------------------------------------------

=head2 end : Private

Overrides the end method from Section.pm so that we can redirect
explicitly to error message templates if required.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  # check for errors
  if ( scalar @{ $c->error } ) {
    $c->forward( "/reportError" );
    $c->stash->{template} = "components/blocks/structure/domainsError.tt";
  } else {
    $c->stash->{template} = "components/blocks/structure/domains.tt";
  }

  # and render the page - need to make sure the templates tell the
  # wrapper not to add the header/footer, using the META tag at the top
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->clear_errors;

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getStructureSeqs : Private

=cut

sub getStructureSeqs : Private {
  my( $this, $c, $name ) = @_;

  my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
  my $seqs = {};
  my @rs = $c->model("PfamDB::Pdb_residue")->search({auto_pdb => $c->stash->{pdb}->auto_pdb},
                          {join     => [ qw/pfamseq/ ],
                           prefetch => [ qw/pfamseq/ ]});
  foreach my $r (@rs){
    if($r->chain =~ /\S{1}/){
      unless($seqs->{$r->pfamseq_id}->{$r->chain}){
        $seqs->{$r->pfamseq_id}->{$r->chain}->{seq} = $fac->createAnnotatedSequence();
        $seqs->{$r->pfamseq_id}->{$r->chain}->{seq}->id($name.":".$c->stash->{pdbId}.":".$r->chain);
        $seqs->{$r->pfamseq_id}->{$r->chain}->{seq}->length($r->length);
        #Make an array of the sequence and make every residue
        for(my $n = 1; $n <= $r->length; $n++){
          $seqs->{$r->pfamseq_id}->{$r->chain}->{disordered}->[$n] = 1;
        }
      }
      if($r->pdb_seq_number && $r->pfamseq_seq_number){
        $seqs->{$r->pfamseq_id}->{$r->chain}->{disordered}->[$r->pfamseq_seq_number]=0;
        $seqs->{$r->pfamseq_id}->{$r->chain}->{mapping}->{$r->pdb_seq_number} = 
          $r->pfamseq_seq_number;
      }
    } else {
      unless($seqs->{$r->pfamseq_id}->{"_"}){
        $seqs->{$r->pfamseq_id}->{"_"}->{seq} = $fac->createAnnotatedSequence();
        $seqs->{$r->pfamseq_id}->{"_"}->{seq}->id($name.":".$c->stash->{pdbId}.":_");
        $seqs->{$r->pfamseq_id}->{"_"}->{seq}->length($r->length);
        for(my $n = 1; $n <= $r->length; $n++){
          $seqs->{$r->pfamseq_id}->{"_"}->{disordered}->[$n] = 1;
        }
    
        #$c->log->debug("Before:".Dumper(@{$seqs->{$r->pfamseq_id}->{"_"}->{disordered}}));
      }
      if($r->pdb_seq_number  && $r->pfamseq_seq_number ){
        $seqs->{$r->pfamseq_id}->{"_"}->{disordered}->[$r->pfamseq_seq_number] = 0;
        $seqs->{$r->pfamseq_id}->{"_"}->{mapping}->{$r->pdb_seq_number} = $r->pfamseq_seq_number;
      }
    }
    #$c->log->debug("After:".Dumper(@{$seqs->{$r->pfamseq_id}->{"_"}->{disordered}}));
  }

  #Now make disordered regions
  foreach my $seqId (keys %$seqs){
    foreach my $chain (keys %{$seqs->{$seqId}}){
      $c->log->debug("Building chains for $chain");
      my ($ds, $de);
      for(my $m = 1; $m <= $seqs->{$seqId}->{$chain}->{seq}->length; $m++){
        if(($ds and $de) && ($seqs->{$seqId}->{$chain}->{disordered}->[$m] == 1)){
          $de=$m;
        }elsif(($ds and $de) && ($seqs->{$seqId}->{$chain}->{disordered}->[$m] == 0)){
          #new disorder region, add to the annotated sequence object.
          $seqs->{$seqId}->{$chain}->{seq}->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $seqs->{$seqId}->{$chain}->{seq}->id,
                                                  '-FROM' => $ds,
                                                  '-TO' => $de,
                                                  '-TYPE' => "Disordered",
                                                  '-SOURCE' => "structure"));
          $de = 0;
          $ds = 0;
        }elsif($seqs->{$seqId}->{$chain}->{disordered}->[$m] == 1){
          #start new disordered region
          $ds=$de=$m;
        }
      }
      if($de and $ds){
      $seqs->{$seqId}->{$chain}->{seq}->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $seqs->{$seqId}->{$chain}->{seq}->id,
                                                '-FROM' => $ds,
                                                '-TO' => $de,
                                                '-TYPE' => "Disordered",
                                                '-SOURCE' => "structure"));
      }
    }
  }
  return($seqs);
}

#-------------------------------------------------------------------------------

=head2 getPfamSeqs : Private

Retrieves the sequence data for the required PDB entry

=cut

sub getPfamSeqs : Private {
  my ($this, $c) = @_;

  my $seqs = {};

  # retrieve the Storable with the data for this sequence
  my @pfamseqs = $c->model('PfamDB::Pfamseq')
                   ->search( { 'pdb_residue.auto_pdb' => $c->stash->{pdb}->auto_pdb },
                             { select   => [ {distinct => 'pdb_residue.chain' }, 
                                             'pfamseq_id' ],
                               as       => [ qw( chain pfamseq_id ) ],
                               join     => [ qw( annseq pdb_residue ) ],
                               prefetch => [ qw( annseq ) ] } );

  foreach my $pfamseq (@pfamseqs){

  $seqs->{$pfamseq->get_column("pfamseq_id")}->{seq} = thaw( $pfamseq->annseq_storable )
    unless $seqs->{$pfamseq->get_column("pfamseq_id")}->{seq};

    my $chain = ($pfamseq->get_column("chain") =~ /(\S+)/) ? $1 : "_";

    $seqs->{ $pfamseq->get_column("pfamseq_id") }->{chains}->{$chain} = 1;
  }

  $c->log->debug( "found " . scalar(keys %$seqs) . " storables" );

  return ($seqs);
}

#-------------------------------------------------------------------------------

=head2 addScopDomains : Private

Tries to retrieve the SCOP domain definitions from the SCOP web
service. If it gets a valid SOAP response, it walks the XML and
extracts what it can...

=cut

sub addScopDomains : Private {
  my ($this, $c, $annSeqs, $scopClient) = @_;
  my $ns = "http://scop.mrc-lmb.cam.ac.uk/test1/";
  my $xc = XML::LibXML::XPathContext->new($scopClient->_response);
  $xc->registerNs( "scop" => $ns );
  foreach my $pdbIdNode ($xc->findnodes("scop:document/scop:list-of-scop-domains/scop:pdbEntry")) {
    if ($pdbIdNode->hasAttribute("pdbid")) {
      my $pdbId = $pdbIdNode->getAttribute("pdbid");
      $xc = XML::LibXML::XPathContext->new($pdbIdNode);
      $xc->registerNs( "scop" => $ns );
      foreach my $domainNode ($xc->findnodes("scop:domain")) {
        $xc = XML::LibXML::XPathContext->new($domainNode);
        $xc->registerNs( "scop" => $ns );
        my $nameNode = $xc->findnodes("scop:attribute/scop:node")->shift;
        my $sunid = $nameNode->getAttribute("sunid");
        my $name = $nameNode->textContent;
  
  
        my $previous_end;
        my $annRegions = {};
        my @features;
        #Multiple regions on a single domain indicate nested or discontinuous domain
        foreach my $region ($xc->findnodes("scop:region")) {
        my $chain = $region->getAttribute("chain");
        my (%mapping, @range);
        foreach my $pfamseqId (keys %{$annSeqs->{SCOP}}) {
          if ($annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{mapping}) {
            %mapping = %{$annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{mapping}};
            foreach (sort{$a <=> $b} keys %mapping) {
              if ($annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{disordered}->[$mapping{$_}] == 0) {
                push(@range, $_);
              }
            }
            last;
          }
        }
        #Find the sequence in the list
        my $start = $range[0];
        my $end = $range[$#range];
        my $start_frag=0;
        my $end_frag=0;
        if ($region->getAttribute("start")) {
          my $s = $region->getAttribute("start");
          $start = $mapping{$s} if($mapping{$s});
        }
        if ($region->getAttribute("stop")) {
          my $e = $region->getAttribute("stop");
          $end = $mapping{$region->getAttribute("stop")} if($mapping{$region->getAttribute("stop")});
        }
  
        if ($previous_end && $start) {
          #add feature as must be fragment of the domain;
          push(@features, (Bio::SeqFeature::Generic->new('-start' => $previous_end,
                                 '-end' => $start,
                                 '-primary' => "nested")));
          $annRegions->{$chain}->[$#{$annRegions->{$chain}}]->end_frag(1);
          $start_frag=1;
        }
  
        $previous_end = $end;
        push(@{$annRegions->{$chain}}, Bio::Pfam::SCOPRegion->new('-scop_name' => $name,
                                      '-scop_id' => $sunid,
                                      '-type' => "SCOP",
                                      '-from' => $start,
                                      '-to' => $end,
                                      '-start_frag' => $start_frag,
                                      '-end_frag' => $end_frag
                                     ));
        }
        foreach my $chain (keys %$annRegions) {
          foreach my $pfamseqId (keys %{$annSeqs->{SCOP}}) {
            if ($annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{seq}) {
              foreach my $r (@{$annRegions->{$chain}}) {
                $annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{seq}->addAnnotatedRegion($r);
              }
              foreach my $f (@features) {
                $annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{seq}->addFeature($f);
              }
            }
          }
        }
      }
    }
  }
  return 1;
}

#-------------------------------------------------------------------------------

=head2 addCathDomains : Private

Tries to retrieve the CATH domain definitions from the CATH web
service. If it gets a valid CATH response, it walks the XML and
extracts what it can...

=cut

sub addCathDomains : Private {
  my ($this, $c, $annSeqs, $cathClient) = @_;

  foreach my $pdbIdNode ( $cathClient->_response->findnodes("document/cath_pdb_query") ) {
    if ($pdbIdNode->hasAttribute("pdb_code")) {
      my $chain;
      if ($pdbIdNode->getAttribute("chain_code") =~ /(\S+)/) {
        $chain = $1;
      } else {
        $chain = "_";
      }
      my $pdbId = $pdbIdNode->getAttribute("pdb_code");
      foreach my $domainNode ($pdbIdNode->findnodes("cath_domain")) {
        my $id = $domainNode->getAttribute("domain_id");
        my $cathcode = $domainNode->findnodes("cath_code")->shift;
        my $cathcodeString = $cathcode->getAttribute("class_code").".".$cathcode->getAttribute("arch_code").".".$cathcode->getAttribute("top_code").".".$cathcode->getAttribute("homol_code");
        
        my $previous_end;
        my $annRegions = {};
        my @features;
        #Multiple regions on a single domain indicate nested or discontinuous domain
        foreach my $region ($domainNode->findnodes("segments/segment")) {
          $c->log->debug("Working on segments");
          my (%mapping, @range);
          foreach my $pfamseqId (keys %{$annSeqs->{CATH}}) {
            if ($annSeqs->{CATH}->{$pfamseqId}->{$chain}->{mapping}) {
              %mapping = %{$annSeqs->{CATH}->{$pfamseqId}->{$chain}->{mapping}};
              @range = sort{$a <=> $b} keys %mapping;
              last;
            }
          }
          #Find the sequence in the list
          my $start = $range[0];
          my $end = $range[$#range];
          my $start_frag=0;
          my $end_frag=0;
          $start = $mapping{$region->getAttribute("pdb_start")} if ($region->getAttribute("pdb_start"));
          $end = $mapping{$region->getAttribute("pdb_stop")} if ($region->getAttribute("pdb_stop"));
          if ($previous_end && $start) {
            #add feature as must be fragment of the domain;
            push(@features, (Bio::SeqFeature::Generic->new('-start' => $previous_end,
                                     '-end' => $start,
                                     '-primary' => "nested")));
            $annRegions->{$chain}->[$#{$annRegions->{$chain}}]->end_frag(1);
            $start_frag=1;
          }
          $previous_end = $end;
          push(@{$annRegions->{$chain}}, Bio::Pfam::CATHRegion->new('-cath_name' => $id,
                                      '-cath_id' => $cathcodeString,
                                      '-type' => "CATH",
                                      '-from' => $start,
                                      '-to' => $end,
                                      '-start_frag' => $start_frag,
                                      '-end_frag' => $end_frag));
        }
        foreach my $chain (keys %$annRegions) {
          foreach my $pfamseqId (keys %{$annSeqs->{CATH}}) {
            if ($annSeqs->{CATH}->{$pfamseqId}->{$chain}->{seq}) {
              foreach my $r (@{$annRegions->{$chain}}) {
              $annSeqs->{CATH}->{$pfamseqId}->{$chain}->{seq}->addAnnotatedRegion($r);
              }
              foreach my $f (@features) {
              $annSeqs->{CATH}->{$pfamseqId}->{$chain}->{seq}->addFeature($f);
              }
            }
          }
        }
      }
    }
  }
  return 1;
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
