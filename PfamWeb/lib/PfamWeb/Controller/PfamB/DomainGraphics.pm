
# DomainGraphics.pm
# jt6 20061023 WTSI
#
# Controller to build a set of domain graphics for a given PfamB.
#
# $Id: DomainGraphics.pm,v 1.2 2006-10-27 08:53:53 rdf Exp $

package PfamWeb::Controller::PfamB::DomainGraphics;

use strict;
use warnings;
use Data::Dump qw( dump );
use Storable qw(thaw);

use base "PfamWeb::Controller::PfamB";


# pick up a URL like http://localhost:3000/pfamB/domaingraphics?acc=PB000067

sub getData : Path {
  my( $this, $c ) = @_;
  my $auto = $c->stash->{pfam}->auto_pfamB;


  #Get a list of all the sequneces and unique archiectures for this family;
  my( $arch ) = $c->req->param( "arch" ) =~ /^(\d+|nopfama)$/i;


  my (@seqs, %seenArch, %example , @architectures);
  if($arch){
    $arch = lc($arch);
    if($arch eq "nopfama"){
      ###hmmm we are going to have to get everything and throw away stuff with no auto_architecture.....
      @architectures =
	$c->model("PfamDB::PfamB_reg")->search( { auto_pfamB => $auto },
						{join      => [ qw/ pfamseq_architecture annseq pfamseq/],
						 prefetch  => [ qw/ pfamseq_architecture annseq pfamseq/]
						});
      foreach my $arch ( @architectures ) {
	if($arch->auto_architecture !~ /\d+/){
	  push @seqs, thaw( $arch->annseq_storable );
	}
      }
    }else{
      
      @architectures =
	$c->model("PfamDB::PfamB_reg")->search( { auto_pfamB => $auto,
						  auto_archiecture => $arch },
						{join      => [ qw/ pfamseq_architecture annseq pfamseq/],
						 prefetch  => [ qw/ pfamseq_architecture annseq pfamseq/]
						});
      #query has already done the selection
      foreach my $arch ( @architectures ) {
	push @seqs, thaw( $arch->annseq_storable );
      }
    }#End if/else
  }else{
    @architectures =
      $c->model("PfamDB::PfamB_reg")->search( { auto_pfamB => $auto },
					      {join      => [ qw/ pfamseq_architecture annseq pfamseq/],
					       prefetch  => [ qw/ pfamseq_architecture annseq pfamseq/]
					      });

    $c->log->debug( "found " . scalar @architectures . " architectures" );
    foreach my $arch ( @architectures ) {
      if(!$seenArch{$arch->auto_architecture}){
	push @seqs, thaw( $arch->annseq_storable );
	$example{$arch->pfamseq_acc} = $arch->auto_architecture;
      }
      $seenArch{$arch->auto_architecture}++;
    }
  }
  $c->log->debug( "found " . scalar @seqs . " storables" );

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  my %regionsAndFeatures = ( "PfamA"      => 1,
			     "PfamB"      => 1,
			     "noFeatures" => 1 );
  $layout->layout_sequences_with_regions_and_features( \@seqs, \%regionsAndFeatures );
  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images( $layout->layout_to_XMLDOM );


  $c->log->debug(dump(\%seenArch));

  $c->stash->{images} = $imageset;
  $c->stash->{archExamples} = \%example;
  $c->stash->{counts} = \%seenArch;
  #   # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/family/domainSummary.tt";



}

#-------------------------------------------------------------------------------

1;
