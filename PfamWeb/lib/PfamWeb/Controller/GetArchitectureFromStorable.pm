package PfamWeb::Controller::GetArchitectureFromStorable;

use strict;
use warnings;

use Storable qw(thaw);
use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::Pfam::SeqPfam;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::Pfam::Drawing::Image::Image;

use base "Catalyst::Controller";


# pick up a URL like http://localhost:3000/getarchitecture/PF00571

sub getArchStore : LocalRegex( '^(\S+)' ) {
  my( $this, $c ) = @_;
  my $acc = $c->req->snippets->[0];


  my @architectures = PfamWeb::Model::PfamA_architecture->search( {pfamA_acc => $acc},
								  {
								      join => [qw/arch pfam/],
								      order_by =>"arch.no_seqs DESC",
								  });
  
  my @seqs;
  eval{
      foreach my $arch (@architectures){
	  push(@seqs, thaw($arch->annseq_storable));
	  	  
      }
  };
  if($@){
      warn "Error thawing storables:[$@]\n";
  }
   
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  my $x = $layout->scale_x("0.2"); #0.33
  $layout->scale_y("0.5"); #0.45
  my %regionsAndFeatures = ( "PfamA"      => 1,
			     "noFeatures" => 1 );
  $layout->layout_sequences_with_regions_and_features(\@seqs, \%regionsAndFeatures);
  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images($layout->layout_to_XMLDOM);
 
  $c->stash->{images} = $imageset;
  $c->stash->{architectures} = @architectures;

  if($c->stash->{images}){
      $c->stash->{template} = "pages/swissPfam.tt";
  }else{
      $c->stash->{template} = "pages/error.tt";
  }
  
  $c->forward( "PfamWeb::View::TT" );

}

1;
