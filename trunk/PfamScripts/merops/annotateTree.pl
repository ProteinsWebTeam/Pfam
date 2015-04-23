#!/usr/bin/env perl

use strict;
use warnings;
use DBI;

use lib "/homes/jaina/perl5/lib/perl5";
use Bio::Phylo::Forest::NodeRole;
use Bio::Phylo::IO;
use Bio::Phylo::Forest::TreeRole;

my $treeFile="RAxML_labelledTree.insert";

#Read tree file
my $newickTree;
open(TREE, $treeFile) or die "Couldn't open fh to $treeFile, $!";
while(<TREE>) {
  chomp $_;
  $newickTree.=$_;
}
close TREE;
my $tree = Bio::Phylo::IO->parse(-string => $newickTree,-format => 'newick')->first;

#Lets get a mapping of holotype to merops code
my (%meropsCode, %codeCount);
open(CODE, ">meropsCode.txt") or die "Couldn't open meropsCode.txt for writing, $!";
my $dbh = DBI->connect("dbi:mysql:database=meropsweb;host=mysql-merops-curation:port=4408", "admin", "gCox9MH5");
my $sth=$dbh->prepare("select code from domain where sequence_id=? and start=? and end=?");
open(SEED, "SEED") or die "Couldn't open fh to SEED, $!";
while(<SEED>) {
  if(/^H\|((\S+)\/(\d+)-(\d+))/) {
    my ($acc_se, $holotype_id, $start, $end)=($1, $2, $3, $4);
    $sth->execute($holotype_id, $start, $end) or die "Couldn't execute statement ".$sth->errstr."\n";
    my $code = $sth->fetchrow;
    die "No merops code for $holotype_id" unless($code);
    $meropsCode{$acc_se}=$code;
    $codeCount{$code}=1;
    print CODE "$holotype_id\t$code\n";
  }
}
close SEED;
close CODE;

#Open outfile, ready for writing
my $outfile = "homologueAnnotation.txt";
my ($fh, %done);
open($fh, ">$outfile") or die "Couldn't open fh to outfile, $!";

if(keys %codeCount==1) { #If there is only one merops code in the whole tree then we can annotate all homologues with this code
  print STDERR "Only one merops code in the whole tree, so all homologues will get annotated with this code\n";
  printAnnotation($tree->get_terminals, \%codeCount, \%done, $fh);
}
else { #Go through each terminal node, walk up the tree, and annotate with the merops code for the closest holotype(s)
  foreach my $node (@{$tree->get_terminals}) {
    next if($node->get_name =~ /^H\|/ or $node->get_name =~ /^MER/); #Ignore seed sequences
    next if(exists($done{$node->get_name}));

    #Keep walking up tree until we find the closest holotype(s), then store the merops code
    my $currentNode=$node;
    my %holotypeCode; 
    until(keys %holotypeCode) {
      $currentNode=traverseUp($currentNode, \%holotypeCode, \%meropsCode);
    }
    
    #Print annotation to outfile
    if(keys %holotypeCode == 1) {  #If only one holotypeCode in the subtree, then we can annotate all terminal nodes in the subtree with the holotypeCode annotation
      printAnnotation($currentNode->get_terminals, \%holotypeCode, \%done, $fh);
    }
    else { #Otherwise just annotate this node
      my @nodesToAnnotate=$node;
      printAnnotation(\@nodesToAnnotate, \%holotypeCode, \%done, $fh); 
    }
  }
}
close $fh;
  
sub traverseUp {
  my ($node, $holotype, $meropsCode) = @_;
  
  my $parent=$node->get_parent;
  
  foreach my $terminal (@{$parent->get_terminals}) {
    if($terminal->get_name =~ /^H\|(\S+)$/) {
      my $code=$meropsCode->{$1};  #eg. code=M11.001
      $holotype->{$code}=1;  
    }
  }
  return $parent;
}

sub printAnnotation {
  my ($nodesToAnnotate, $holotypeCode, $done, $fh) = @_;
  
  foreach my $node (@{$nodesToAnnotate}) {
    next if($node->get_name =~ /^[MER|H\|]/);
    next if(exists($done->{$node->get_name}));    
    
    my $id;
    if($node->get_name =~ /QUERY___(\S+)$/) {
      $id=$1;
    }
    else {
      die "Couldn't parse name from ".$node->get_name.", expected it to be in format 'QUERY___id\n";
    }
    
    my $annotation;
    foreach my $code (sort keys %{$holotypeCode}) {
      $annotation.="," if($annotation); #Separate merops codes with ','
      $annotation.=$code;
    }
    print $fh "$id\t$annotation\n";
    $done->{$node->get_name}=1;
  }
}
