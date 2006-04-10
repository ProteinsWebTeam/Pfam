
# Summary.pm
# jt6 20060316 WTSI
#
# Controller to build the main Pfam family page. Still a test-bed of
# sorts.
#
# $Id: Summary.pm,v 1.8 2006-04-10 10:58:35 rdf Exp $

package PfamWeb::Controller::Summary;

use strict;
use warnings;
use Data::Dumper;
use base "Catalyst::Controller";


# pick up a URL like http://localhost:3000/summary/PF00067

sub getacc : LocalRegex( '^(PF\d{5})' ) {
  my( $this, $c ) = @_;

  my $acc = $c->req->snippets->[0];
  $c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $acc } );

  $c->stash->{template} = "pages/error.tt"
	unless defined $c->stash->{pfam};

  #$c->log->info( "getacc: Pfam object: |", $c->stash->{pfam}, "|" );
}


# pick up URLs like
#   http://localhost:3000/summary?acc=PF00067
# or
#   http://localhost:3000/summary?id=p450

sub default : Private {
  my( $this, $c ) = @_;

#   if( defined $c->session->{layout} ) {
# 	$c->stash->{layout} = $c->session->{layout};
# 	$c->log->debug( "added layout object to stash" );
#   }

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(PF\d{5})$/;
	$c->log->info( "found accession |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param("id") ) {

	$c->req->param("id") =~ m/(^\w+$)/;
	$c->log->info( "found ID |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_id => $1 } )
	  if defined $1;

  }

  $c->stash->{template} = "pages/error.tt"
	unless defined $c->stash->{pfam};

  $c->log->info( "default: Pfam object: |", $c->stash->{pfam}, "|" );

}


# hand off to TT to render the page. Default to "view.tt", which is
# the standard summary page right now. If actions have had problems,
# they'll have switched to point to "error.tt"

sub end : Private {
  my( $this, $c ) = @_;

  # see if a view was specified
  my $view;
  if( $c->req->param( "view" ) ) {
	( $view = $c->req->param( "view" ) ) =~ s/^([A-Za-z_]+).*$/$1/;

	# set the view
	$c->stash->{template} ||= "pages/" . $this->{views}->{$view}
  }

  # pick a tab
  if( $c->req->param( "tab" ) ) {
	$c->req->param( "tab" ) =~ /^([A-Za-z]+).*$/;
	$c->stash->{selectedTab} = $1;
  }

  #----------------------------------------------------------------------
  # add domain architectures to the stash

  my @seqs_acc;
  my $acc = $c->stash->{pfam}->pfamA_acc;
  my $auto_pfam = $c->stash->{pfam}->auto_pfamA; 
  my $no_archs;
  foreach my $arch ( PfamWeb::Model::PfamA_architecture->search( {'auto_pfamA' => $auto_pfam },
	  							 { join        => [qw/ arch /],
								   prefetch    => [qw/ arch /],
							     	   order_by    => "arch.no_seqs DESC"})) {
      $no_archs++;
      push @seqs_acc, $arch->pfamseq_acc;
  }

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->scale_x("0.2"); #0.33
  $layout->scale_y("0.5"); #0.45

  #my %order = map{$_ => 1 }$layout->region_order;
  my %order = ( "pfama" => 1);

  my $seqs = PfamWeb::Model::GetBioObjects::getAnnseq( \@seqs_acc, \%order );

  $layout->layout_sequences( @$seqs );

  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images($layout->layout_to_XMLDOM);

  $c->stash->{images} = $imageset;

  #----------------------------------------------------------------------
  # add the species tree

  my $tree = PfamWeb::Model::GetSpeciesTree::getTree($acc);

  my $js;
  $tree->convert_to_js(\$js);
  $c->stash->{tree} = $js;

  #----------------------------------------------------------------------
  # get the PDB details

  my @maps = PfamWeb::Model::PdbMap->search( { auto_pfam   => $auto_pfam,
					       pfam_region => 1 },
					     { join        => [qw/ pdb / ],
					       prefetch    => [qw/ pdb /]}
														);
  $c->stash->{pfamMaps} = \@maps;

  #----------------------------------------------------------------------
  # Build the summary details

  my %summaryData;

  #Number or architectures....
  $summaryData{numArchitectures} = $no_archs;

  #Number of sequences in full alignment
  $summaryData{numSequences} = $c->stash->{pfam}->num_full;

  #Number of structures known for the domain 
  my %pdb_unique = map{$_->pdb_id => 1} @maps;
  $summaryData{numStructures} = scalar(keys %pdb_unique);

  #Number of species
  my @species = PfamWeb::Model::PfamA_reg_full->search({ auto_pfamA => $auto_pfam,
						         in_full    => 1 },
						       { join       => [ qw/pfamseq/ ],
						         prefetch   => [ qw/pfamseq/ ]});
  

  my %species_unique = map{$_->species => 1}@species;
  $summaryData{numSpecies} = scalar(keys %species_unique);

  # HACK:hardcoded interactions number added here...
  $summaryData{numIpfam} = 7;

  $c->stash->{summaryData} = \%summaryData;

  #----------------------------------------------------------------------
  #Build up the set of database cross references 
  #There are many different ways of getting the DBXREFS.

# +-----------------+
# | db_id           | Notes      
# +-----------------+
# | PRC             | Internal
# | IPFAM           | Internal
# | SCOP            | Structure 
# | HOMSTRAD        | Structure
# | PROSITE         | Motif
# | MIM             | Mutation
# | TC              | Motif
# | PRINTS          | Motif
# | SMART           | Motif
# | CAZY            | Motif
# | URL             | ?
# | PROSITE_PROFILE | Motif
# | LOAD            | Motif
# | MEROPS          | Motif
# | PFAMB           | Internal
# | COGS            | Motif
# | PUBMED          | Lit
# | PDB             | Structure
# | INTERPRO        | Motif/Lit
# | GO              | Function
# +-----------------+

  #Lets do the ones we already have data pulled out for......
  my (%atobPRODOM, %Xrefs); 
  #Interpro  
  if ($c->stash->{pfam}->interpro_id){
      push(@{$Xrefs{interpro}}, $c->stash->{pfam}->interpro_id);
  }
   $c->log->debug(Dumper(%Xrefs));
  #pdb
  my @pdb_ids = keys (%pdb_unique); 
  if(scalar(@pdb_ids )){
      $Xrefs{pdb} = \@pdb_ids;
  }

  
  foreach my $xref ($c->stash->{pfam}->pfamA_database_links){
      if($xref->db_id eq "PFAMB"){
	  $atobPRODOM{$xref->db_link} = $xref;
      }else{
	  push(@{$Xrefs{$xref->db_link}}, $xref);
      }
  }  

  #Get PfamA to PfamA links based on PRC
  my @atoaPRC = PfamWeb::Model::PfamA2pfamA_PRC_results->search( {"pfamA1.pfamA_acc" => $acc,
							          "evalue" => { "<=", "0.01"}  },
								 {join               => [qw/pfamA1 pfamA2/],
								  select             => [ "pfamA1.pfamA_id", "pfamA2.pfamA_id" ],
								  as                 => [qw/l_pfamA_id r_pfamA_id /],
								  prefetch           => [qw/pfamA2 pfamA1/]});
     
  if(scalar(@atoaPRC)){
      $Xrefs{atobPRC} = \@atoaPRC;
  }

  #Get PfamB to PfamA links based on PRC
  my @atobPRC = PfamWeb::Model::PfamB2pfamA_PRC_results->search( {pfamA_acc => $acc},
								 {join      => [qw/pfamA pfamB/],
								  prefetch  => [qw/pfamA pfamB/]});
  
  #Now go through the various operations to find the union between PRC and PRODOM pfamB links
  my %atobPRC = map{$_->pfamB_acc => $_ }@atobPRC;
  my( %atobBOTH);
  foreach ( keys %atobPRC, keys %atobPRODOM ) {
      $atobBOTH{$_} = $atobPRC{$_} if( exists( $atobPRC{$_} ) and exists( $atobPRODOM{$_} ) );
  }

  # and then prune out those accessions that are in both lists
  foreach ( keys %atobPRC ) {
      delete $atobPRC{$_} if exists $atobBOTH{$_};
  }
  
  foreach ( keys %atobPRODOM ) {
      delete $atobPRODOM{$_} if exists $atobBOTH{$_};
  }
  
  # Now populate the hash of Xrefs;
  my @atobPRC_pruned = values %atobPRC;
  $Xrefs{atobPRC} = \@atobPRC_pruned if(scalar(@atobPRC_pruned));
  
  my @atobPRODOM = values %atobPRODOM;
  $Xrefs{atobPRODOM} = \@atobPRODOM if(scalar(@atobPRODOM));
  
  my @atobBOTH = values %atobBOTH;
  $Xrefs{atobBOTH} = \@atobBOTH if(scalar(@atobBOTH));
  
  $c->stash->{xrefs} = \%Xrefs;

  
  #----------------------------------------------------------------------
  # make sure there's a template defined ultimately
  $c->stash->{template} ||= "pages/" . $this->{views}->{default};

  # and use it
  $c->forward( "PfamWeb::View::TT" );
}

1;
