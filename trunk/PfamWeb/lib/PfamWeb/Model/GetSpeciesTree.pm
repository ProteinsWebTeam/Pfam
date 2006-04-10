package PfamWeb::Model::GetSpeciesTree;

use strict;
use warnings;
use Bio::Pfam::Web::Tree;


sub getTree{
    my $family_acc = shift;
    my $treeData = &_getTreeData($family_acc);
    my $tree = &_constructTree($treeData); 
    return ($tree);
}

sub _getTreeData{
    my $family_acc = shift;
    my @treeData;
    #The family id could be either pfamB or pfamA.
    if($family_acc =~ /^PB\d+$/) {
	#We seem to have a Pfam-B entry
	foreach my $region ( PfamWeb::Model::PfamB_reg->search( pfamB_acc => $family_acc)){
	    
	}
    }elsif($family_acc =~ /^PF\d+$/){
	#Looks like a Pfam-A entry as it does not match a Pfam-B i 
	
	foreach my $region ( PfamWeb::Model::PfamA_reg_full->search( {"pfamA.pfamA_acc" => $family_acc,
								      "in_full"         => 1}, 
								     { join             => [qw/ pfamA pfamseq /],
								      # prefetch         => [qw/ pfamA pfamseq /]
								       })){
	    push(@treeData, $region->taxonomy);
	    
	    
	}
	
    }else{
	warn "Unrecognised family accc, $family_acc, does not look like pfamA or pfamB.\n";
    }
    return \@treeData;
    
}

sub _constructTree{
    my $treeData = shift;
    my $depth = shift;
    $depth = 100 if(!$depth);
    my $tree = Bio::Pfam::Web::Tree->new();
    $tree->grow_tree ( $treeData, ';' );
    $tree = $tree->clear_root();
    return ($tree);
}





1;
