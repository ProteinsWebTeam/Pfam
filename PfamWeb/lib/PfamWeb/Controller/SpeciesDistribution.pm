package PfamWeb::Controller::SpeciesDistribution;

use strict;
use warnings;
use JSON;
use Data::Dumper;
use base "Catalyst::Controller";

sub getSpeciesDistribution : LocalRegex( '^(P\S{1}\d{5,7})' ) {
  my ($this, $c) = @_;
  my $acc = $c->req->snippets->[0];

$c->stash->{acc} = $acc;
if($acc =~ /PF/){
  $c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $acc } );
}elsif($acc =~ /PB/){
  $c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $acc } );
}

  my $tree = PfamWeb::Model::GetSpeciesTree::getTree($acc);
  
  my $js;
  $tree->convert_to_js(\$js);
  $c->stash->{tree} = $js;

  $c->log->debug($c->stash->{tree});
  if($c->stash->{tree} && $c->stash->{pfam}){
      $c->stash->{template} = "pages/speciesTree.tt";
  }else{
      $c->stash->{template} = "pages/error.tt";
  }
  
  $c->forward( "PfamWeb::View::TT" );
}



1;
