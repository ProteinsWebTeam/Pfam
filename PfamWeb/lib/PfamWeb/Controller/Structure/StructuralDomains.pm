# StructuralDomains.pm
# rdf 20060823 WTSI
#
# $Id: StructuralDomains.pm,v 1.2 2006-09-28 14:25:59 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::StructuralDomains - controller for generating
domain graphics for Pfam, cath and scop

=cut

package PfamWeb::Controller::Structure::StructuralDomains;

=head1 DESCRIPTION

Controller to build a set of domain graphics for a given Pdb


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



=head2 default : Path

Picks up a URL like http://localhost:3000/structure/graphics?ids=Q6FRP6

=cut

sub getStructuralDomains : Path {
  my( $this, $c ) = @_;
  $c->log->debug("Getting structural domains for:".$c->stash->{pdbId}.":");

  $c->log->debug("Contacting WebServices using ".$this->{wsProxy}." proxy");
  my $cathClient = Bio::Pfam::WebServices::Client::Cath->new(-proxy => $this->{wsProxy},
							     -pdbId => $c->stash->{pdbId});
  $cathClient->queryService;
  my $scopClient = Bio::Pfam::WebServices::Client::Scop->new(-proxy => $this->{wsProxy},
							     -pdbId => $c->stash->{pdbId});
  $scopClient->queryService;
  $c->log->debug("Finished contacting WebServices");
  #$c->log->debug("Got:".$cathClient->_response->toString(1)." for cath");
  #$c->log->debug("Got:".$scopClient->_response->toString(1)." for scop");

  my $annSeqs = {};
  if($cathClient->_response){
    #Step 1: get a sequence for each chain.
    $annSeqs->{CATH} = getStructureSeqs($this, $c, "CATH");
    #Step 2: parse the response and add the CATH domains
    addCathDoamins($this, $c, $annSeqs, $cathClient);
}

  if($scopClient->_response){
    #Step 1: get a sequence for each chain.
    $annSeqs->{SCOP} = getStructureSeqs($this, $c, "SCOP");
    #Step 2: parse the response and add the SCOP domains
    addScopDoamins($this, $c, $annSeqs, $scopClient);
}

  #Now we need to get the Pfam seqs
  $annSeqs->{PFAM} = getPfamSeqs($this, $c);
  
  #Now Merge PfamSeqs and Structure Chains;
  my @seqs;
  foreach my $pfamseqId (keys %{$annSeqs->{PFAM}}){
     $c->log->debug("Got $pfamseqId");
     push(@seqs, $annSeqs->{PFAM}->{$pfamseqId}->{seq});
     foreach my $chain (sort{$a cmp $b} keys %{$annSeqs->{PFAM}->{$pfamseqId}->{chains}}){
       foreach my $structureDB (qw/SCOP CATH/){
	 if($annSeqs->{$structureDB}->{$pfamseqId}->{$chain}->{seq}){
	   $c->log->debug("Got $pfamseqId, $chain, $structureDB bioperl object");
	    push(@seqs,$annSeqs->{$structureDB}->{$pfamseqId}->{$chain}->{seq});
	  }
       }
     }
   }
  
  # render All the sequences
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->scale_x( $this->{scale_x} );
  $layout->scale_y( $this->{scale_y} );


  my %regionsAndFeatures = ( "PfamA"      => 1, "SCOP" => 1, "CATH" => 1, "Disordered" => 1, "noFeatures" => 0 );

  $layout->layout_sequences_with_regions_and_features( \@seqs,
						       \%regionsAndFeatures );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );
  foreach my $image ($imageset->each_image){
    $image->print_image;
  }
 



#   # get the PDB chain/uniprot mapping from the cache
#   my $chainsMapping = $c->cache->get( "chain_mapping" );

#   # build a chains-to-UniProt ID mapping
#   my %chainsToUnp;
#   my $chains;
#   foreach my $unp ( keys %$chainsMapping ) {

# 	# each key in %chains is a uniprot ID, pointing to an anonymous
# 	# hash that has the PDB chain ID as keys and "" as values
# 	my $chains = join ", ", sort keys %{$chainsMapping->{$unp}};
	
# 	$chainsToUnp{$chains} = $unp;
#   }

#   # build a UniProt ID-to-image mapping
#   my %unpToImage;
#   my $unp;
#   foreach my $image ( $imageset->each_image ) {
# 	$unp = $image->image_name;
# 	$unpToImage{$unp} = $image;

# 	# while we're iterating over all images, print them...
# 	$image->print_image;
#   }

#   # and put both of those mappings into the stash for the template...
#   $c->stash->{chainsToUnp} = \%chainsToUnp;
#   $c->stash->{unpToImage}  = \%unpToImage;
  $c->stash->{domainImages} = $imageset;

  # set the template to render
  $c->stash->{template} = "components/blocks/structure/domains.tt";

}


#-------------------------------------------------------------------------------


sub getStructureSeqs {
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
	$seqs->{$r->pfamseq_id}->{$r->chain}->{mapping}->{$r->pdb_seq_number} = $r->pfamseq_seq_number;
      }
    }else{
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

sub getPfamSeqs{
  my ($this, $c) = @_;

  my $seqs = {};

  # retrieve the Storable with the data for this sequence
  my @pfamseqs = $c->model("PfamDB::Pfamseq")->search( { "pdb_residue.auto_pdb" => $c->stash->{pdb}->auto_pdb },
						       { select => [ {distinct => "pdb_residue.chain"}, "pfamseq_id" ],
							 as => [ qw/chain pfamseq_id/ ],
							 join => [ qw/pdb_residue annseq/ ],
							 prefetch => [ qw/annseq/]} );

  foreach my $pfamseq (@pfamseqs){
    # thaw it out
    unless( $seqs->{$pfamseq->get_column("pfamseq_id")}->{seq}){
      $seqs->{$pfamseq->get_column("pfamseq_id")}->{seq} = thaw( $pfamseq->annseq_storable );
    }
    my $c;
    if($pfamseq->get_column("chain") =~ /(\S+)/){
      $c = $1;
    }else{
      $c = "_";
    }
    $seqs->{$pfamseq->get_column("pfamseq_id")}->{chains}->{$c} = 1;
  }
  $c->log->debug( "found " . scalar(keys %$seqs) . " storables" );
  return ($seqs);
}


sub addScopDoamins{
  my ($this, $c, $annSeqs, $scopClient) = @_;
  my $ns = "http://scop.mrc-lmb.cam.ac.uk/test1/";
  my $xc = XML::LibXML::XPathContext->new($scopClient->_response);
  $xc->registerNs( "scop" => $ns );
  foreach my $pdbIdNode ($xc->findnodes("scop:document/scop:list-of-scop-domains/scop:pdbEntry")){
    if($pdbIdNode->hasAttribute("pdbid")){
       my $pdbId = $pdbIdNode->getAttribute("pdbid");
       $xc = XML::LibXML::XPathContext->new($pdbIdNode);
       $xc->registerNs( "scop" => $ns );
       foreach my $domainNode ($xc->findnodes("scop:domain")){
	 $xc = XML::LibXML::XPathContext->new($domainNode);
	 $xc->registerNs( "scop" => $ns );
	 my $nameNode = $xc->findnodes("scop:attribute/scop:node")->shift;
	 my $sunid = $nameNode->getAttribute("sunid");
	 my $name = $nameNode->textContent;


	my $previous_end;
	my $annRegions = {};
	my @features;
	#Multiple regions on a single domain indicate nested or discontinuous domain
	foreach my $region ($xc->findnodes("scop:region")){
	    my $chain = $region->getAttribute("chain");
	    my (%mapping, @range);
	    foreach my $pfamseqId (keys %{$annSeqs->{SCOP}}){
	      if($annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{mapping}){
		%mapping = %{$annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{mapping}};
		foreach (sort{$a <=> $b} keys %mapping){
		  if($annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{disordered}->[$mapping{$_}] == 0){
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
	    if ($region->getAttribute("start")){
	      my $s = $region->getAttribute("start");
	      $start = $mapping{$s} if($mapping{$s});
	    }
	    if ($region->getAttribute("stop")){
	      my $e = $region->getAttribute("stop");
	      $end = $mapping{$region->getAttribute("stop")} if($mapping{$region->getAttribute("stop")});
	    }

	    if($previous_end && $start){
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
	foreach my $chain (keys %$annRegions){
	  foreach my $pfamseqId (keys %{$annSeqs->{SCOP}}){
	    if($annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{seq}){
	      foreach my $r (@{$annRegions->{$chain}}){
		$annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{seq}->addAnnotatedRegion($r);
	      }
	      foreach my $f (@features){
		$annSeqs->{SCOP}->{$pfamseqId}->{$chain}->{seq}->addFeature($f);
	      }
	    }
	  }
	}
       }
     }
   }
 }

sub addCathDoamins{
  my ($this, $c, $annSeqs, $cathClient) = @_;

  foreach my $pdbIdNode ($cathClient->_response->findnodes("document/cath_pdb_query")){
    if($pdbIdNode->hasAttribute("pdb_code")){
      my $chain;
      if($pdbIdNode->getAttribute("chain_code") =~ /(\S+)/){
	$chain = $1;
      }else{
	$chain = "_";
      }
      my $pdbId = $pdbIdNode->getAttribute("pdb_code");
      foreach my $domainNode ($pdbIdNode->findnodes("cath_domain")){
	my $id = $domainNode->getAttribute("domain_id");
	my $cathcode = $domainNode->findnodes("cath_code")->shift;
	my $cathcodeString = $cathcode->getAttribute("class_code").".".$cathcode->getAttribute("arch_code").".".$cathcode->getAttribute("top_code").".".$cathcode->getAttribute("homol_code");

	my $previous_end;
	my $annRegions = {};
	my @features;
	#Multiple regions on a single domain indicate nested or discontinuous domain
	foreach my $region ($domainNode->findnodes("segments/segment")){
	  $c->log->debug("Working on segments");
	  my (%mapping, @range);
	  foreach my $pfamseqId (keys %{$annSeqs->{CATH}}){
	    if($annSeqs->{CATH}->{$pfamseqId}->{$chain}->{mapping}){
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
	  if($previous_end && $start){
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
	foreach my $chain (keys %$annRegions){
	  foreach my $pfamseqId (keys %{$annSeqs->{CATH}}){
	    if($annSeqs->{CATH}->{$pfamseqId}->{$chain}->{seq}){
	      foreach my $r (@{$annRegions->{$chain}}){
		$annSeqs->{CATH}->{$pfamseqId}->{$chain}->{seq}->addAnnotatedRegion($r);
	      }
	      foreach my $f (@features){
		$annSeqs->{CATH}->{$pfamseqId}->{$chain}->{seq}->addFeature($f);
	      }
	    }
	  }
	}
      }
    }
  }
}
=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
