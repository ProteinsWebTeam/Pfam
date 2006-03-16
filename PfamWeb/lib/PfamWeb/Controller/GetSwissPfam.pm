package PfamWeb::Controller::GetSwissPfam;

use strict;
use warnings;

use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::Pfam::SeqPfam;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::Pfam::Drawing::Image::Image;

use base "Catalyst::Controller";


# pick up a URL like http://localhost:3000/getswisspfam/P00789

sub getseq : LocalRegex( '^(\S+)' ) {
  my( $this, $c ) = @_;
  my $acc = $c->req->snippets->[0];
  my @seqs_acc;
  push(@seqs_acc, $acc);
  my $seqs = PfamWeb::Model::GetBioObjects::getAnnseq(\@seqs_acc);

  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->layout_sequences(@$seqs);
  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images($layout->layout_to_XMLDOM);

  foreach my $im ($imageset->each_image){
     $im->print_image;
  }

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
