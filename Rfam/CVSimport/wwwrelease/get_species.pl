#!/usr/local/bin/perl

use lib '/nfs/disk100/pubseq/Pfam/scripts/Modules';

use strict;
use Bio::Pfam::Tree::GSI;
use Bio::Pfam::Tree;
use Bio::Pfam::Tree::BioSeqFormat;
use vars qw($opt_a $opt_i $opt_h);
use Getopt::Std;

getopts('i:ha');

###########################################################
# Help
if ($opt_h) {
print <<END;

pfam2pnh.pl - buids a pseudo-NewHamphire file of species distribution

Usage: pfam2pnh.pl [-a] -i <pfamseq_index> <Pfam-A.full> <Pfam-B> > out_file

Option:
-a   Build pnh using Pfam Accesion names

END
die "\nHave a nice day\n";
}

###########################################################
# Initialize
#die "You must specify a GSI index (-i gsi_file)\n" if (!$opt_i);
my $index = $opt_i;
my ($domainID, $domainAC, %proteinID);
my $time = time;
#GSI::openGSI($index);

###########################################################

#open(_TMP, ">tmp.txt");

# Retrive taxonomy and build trees
while (<>) { 
 CASE:	{
    #   print "EX: $_";
    # Get domain ID and proteins IDs
    if (/\#\s+STOCKHOLM/) {
      next;
    }
    if (/\#=GF ID\s+(\S+)/){ 
      $domainID = $1; print _FILE "ID $domainID\n";  sleep 3; last CASE
    }
    if (/\#=GF AC\s+(\S+)/){
      %proteinID = undef;
      $domainAC = $1;  
      close(_FILE);
      open(_FILE, ">$1.species");      
      last CASE 
    }
    #   next if ($_ !~ /[A-Z]|[a-z]|[0-9]/);
    if (/\#=GS\s+([^\/]+)\/\S+\s+AC/){ 
      $proteinID{$1}++; last CASE 
    }
    
    if ( ($_ !~ /\#=/) && ($_ !~ /^\/\//) ){
      my ($id, @junk) = split(/\//, $_);
      #   print "ID: $id \n";
      $proteinID{$id}++; last CASE
    }
    
    
    if ($_ =~ /^\/\//)   {
      my @branch;
      print STDERR "Processing $domainID:$domainAC\n";
   #   next if ( ($domainAC ne "RF00022") && ($domainAC ne "RF00023") && ($domainAC ne "RF00024") && ($domainAC ne "RF00025") && ($domainAC ne "RF00004") && ($domainAC ne "RF00012")  );
      
      # Get taxonomy of protein
      foreach my $proteinID (sort keys %proteinID) {
	next if ($proteinID !~ /[A-Z]|[a-z]|[0-9]/);
	print _FILE "$proteinID";
#	print _TMP "emblrelease:$proteinID\n";
#	print _TMP "emblrelease:$proteinID\n";
	my ($os, $oc, $tax);
	open(_GETZ, "getz -e '[emblrelease-acc:$proteinID]' |");
	#print STDERR "$_ \n";
	my $got_os = 0;
	while(<_GETZ>) {
	  # print "$_";
	  if ($_ =~ /^OS\s+(.*)/) {
	    
	    if (!$got_os) {
	      
	      $os = $1;
	      print _FILE "~" . $os . "~";
	      #   print "OS: $os \n";
	      $got_os = 1;
	    } else {
	      print STDERR "$proteinID :: $_ multi \n";
	      last;
	    }
	  }
	  if ($_ =~ /^OC\s+(.*)/) {
	    $oc .= $1;
	    #	      print "OC: $1 \n"; 
	  }
	  #    print "$_ \n";
	  
	} #/ end WHILE getz
	
	$oc =~  s/;[^;]+$/;/;
	print _FILE "$oc\n";
	$tax = $oc;
	$tax .=  $os;
	
	
      } #/ end foreach proteinID

#      close(_TMP);
#      my $str = "getz -e \@tmp.txt |";
#      print STDERR "STR: $str \n";
#      open(_GETZ, "$str");

#      while(<_GETZ>) {
##	print STDERR "GETZ: $_ ";
	
#      }

#      close(_GETZ);



    }
    
  }
  
}

  





$time = time - $time;
print STDERR "(execution time $time)\n";
	
1;
