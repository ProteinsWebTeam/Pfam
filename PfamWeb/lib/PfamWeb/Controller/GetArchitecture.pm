package PfamWeb::Controller::GetArchitecture;

use strict;
use warnings;

use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::Pfam::SeqPfam;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::Pfam::Drawing::Image::Image;

use base "Catalyst::Controller";


# pick up a URL like http://localhost:3000/getarchitecture/PF00571

sub getseq : LocalRegex( '^(\S+)' ) {
  my( $this, $c ) = @_;
  my $acc = $c->req->snippets->[0];
  my @seqs_acc;

  #Probably want to put this in the stash.....
  foreach my $arch ( $c->model("PfamDB::PfamA_architecture")->search( {'pfamA_acc' => $acc},
							    {
								join => [qw/ arch pfam/],
								prefetch => [qw/arch/],
								order_by =>"arch.no_seqs DESC",
							    })){
      push(@seqs_acc, $arch->pfamseq_acc);   
  }



 
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  my $x = $layout->scale_x("0.2"); #0.33
  $layout->scale_y("0.5"); #0.45

  #my %order = map{$_ => 1 }$layout->region_order;
  my %order = ( "pfama" => 1);
 
  #This step is v.slow
  my $seqs = PfamWeb::Model::GetBioObjects::getAnnseq(\@seqs_acc, \%order);
  
  $layout->layout_sequences(@$seqs);
  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images($layout->layout_to_XMLDOM);
  #print STDERR $layout->layout_toString;
  #foreach my $im ($imageset->each_image){
     #$im->print_image;
  #}

  $c->stash->{images} = $imageset;
#$c->stash->{images} =1;

if($c->stash->{images}){
  $c->stash->{template} = "pages/swissPfam.tt";
}else{
  $c->stash->{template} = "pages/error.tt";
}

  $c->forward( "PfamWeb::View::TT" );

}
1;
