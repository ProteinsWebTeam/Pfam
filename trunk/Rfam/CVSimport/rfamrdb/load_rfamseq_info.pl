#!/usr/local/bin/perl
use lib '/nfs/WWWdev/SANGER_docs/cgi-bin/Rfam';

use RfamWWWConfig;
use EntryWWW;

use lib '/nfs/team71/pfam/mm1/rfam/scripts/Modules';

require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/UpdateRDB.pm";
require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/Bio/Rfam.pm";

#use lib '/nfs/disk100/pubseq/Pfam/scripts/Modules';#

use strict;
#use Bio::Pfam::Tree::GSI;
#use Bio::Pfam::Tree;
#use Bio::Pfam::Tree::BioSeqFormat;
#use vars qw($opt_a $opt_i $opt_h);
use Getopt::Long;


#my($dir, $file_type);

#&GetOptions(  'dir=s' => \$dir,
#	      'file_type=s' => \$file_type );

#getopts('i:ha');



#####################################################
# This iterates through all Rfam.full
# for each sequence does a getz on emblrelease and loads fields into RDB:
#
#  -  ACC
#  -  ID
#  -  DESCRIPTION
#  -  SPECIES
#  -  TAXONOMY
#
#
#######################################################


###########################################################
# Help
#if ($opt_h) {
#print <<END;

#pfam2pnh.pl - buids a pseudo-NewHamphire file of species distribution

#Usage: pfam2pnh.pl [-a] -i <pfamseq_index> <Pfam-A.full> <Pfam-B> > out_file

#Option:
#-a   Build pnh using Pfam Accesion names

#END
#die "\nHave a nice day\n";
#}

###########################################################
# Initialize
#die "You must specify a GSI index (-i gsi_file)\n" if (!$opt_i);
#my $index = $opt_i;
#my ($domainID, $domainAC, %proteinID);
#my $time = time;
#GSI::openGSI($index);

###########################################################

#die "need --dir \n" if (!$dir);
#die "need --file_type \n" if (!$file_type);


##open(_TMP, ">tmp.txt");

#my $id;
#my $pwd=`pwd`;
# chomp $pwd;

#print "DIR: $pwd\n";
##exit(0);
#my $seq_file  = $pwd . "/" . $file_type ."_rfamseq.dat";
#my $regions_file = $pwd . "/" . $file_type . "_regions.dat";


#open(_SEQ, ">$seq_file");
#open(_REGIONS, ">$regions_file");

#my $got_cons = 0;
#my %arr = undef;
#my %rfamseq;

## Retrive taxonomy and build trees
#my $count = 0;
# open(_RFAM, "$dir/Rfam." . $file_type );
#while (<_RFAM>) { 
##  print "$_ \n";
# # sleep 1;
# CASE:	{
#  #  print "$_ \n";
#    #   print "EX: $_";
#    # Get domain ID and proteins IDs
#    if (/\#\s+STOCKHOLM/) {
#      next;
#    }
#    if (/\#=GF\s+ID\s+(\S+)/){ 
#      $domainID = $1; last CASE;   #print _FILE "ID $domainID\n"; 
#    }
#    if (/\#=GF\s+AC\s+(\S+)/){
#      print "$domainAC: $count \n";

  

#      $count = 0;
#      $got_cons = 0;
#      %proteinID = undef;
#      $domainAC = $1;  
#      %arr = undef;
# #     close(_FILE);
# #     open(_FILE, ">$1.species");      
#      last CASE 
#    }
#    #   next if ($_ !~ /[A-Z]|[a-z]|[0-9]/);
#    if (/\#=GS\s+([^\/]+)\/\S+\s+AC/){ 
#      $proteinID{$1}++; last CASE 
#    }

#  #  last if  ($_ =~ /^\/\//);

#    if ($_ =~ /^\#=GC/) {
#  #   print "GOT: $got_cons $count\n";
#      $got_cons = 1;
#      last CASE
#    }

# #   if ( ($_ !~ /\#=/)  && ($_ !~ /^\/\//) ){
##      $id = $1 if ($_ =~ /^(\S+)\//);
##      print "ID: $id \n";
##    }

#    if  ( ($_ =~ /\#=G[R|S]\s+(\S+)\/(\d+)-(\d+)/) || ($_ =~ /^(\S+)\/(\d+)-(\d+)/)  ){
#      my $test = $1 . "/" . $2 . "-" . $3;
#      if (defined($arr{$test})) {
##	print "SAME $test \n";
#      } else {
#	$arr{$test} = $test;
#	$count++;
#	if (!$got_cons) {
#	  if (defined($rfamseq{$1})) {
#	  } else {
#	    print _SEQ "emblrelease:$1";
#	   # print _SEQ "\t$id" if ($file_type =~ /full/i);
#	    print _SEQ "\n";
#	    $rfamseq{$1} = $1;
#	  }
#	  print _REGIONS "$domainAC\t$1\t$2\t$3\n";
#	}


#      }

     
#     # my ($acc, @junk) = split(/\//, $_);
#      #   print "ID: $id \n";
#  #    if ($1 ne $id ) {
##	print "ACC: $1 L ID: $id : START: $2 END: $3 \n";
##	sleep 1;
#   #   }

#      $proteinID{$id}++; last CASE
#    }
    
    

    
##    if ($_ =~ /^\/\//)   {
##      my @branch;
##      print STDERR "Processing $domainID:$domainAC\n";
##   #   next if ($domainAC ne "RF00011");
      
##      # Get taxonomy of protein
##      foreach my $proteinID (sort keys %proteinID) {
##	next if ($proteinID !~ /[A-Z]|[a-z]|[0-9]/);
##	print _FILE "$proteinID";
###	print _TMP "emblrelease:$proteinID\n";
###	print _TMP "emblrelease:$proteinID\n";
##	my ($os, $oc, $tax);
##	open(_GETZ, "getz -e '[emblrelease-acc:$proteinID]' |");
##	#print STDERR "$_ \n";
##	my $got_os = 0;
##	while(<_GETZ>) {
##	  # print "$_";
##	  if ($_ =~ /^OS\s+(.*)/) {
	    
##	    if (!$got_os) {
	      
##	      $os = $1;
##	      print _FILE "~" . $os . "~";
##	      #   print "OS: $os \n";
##	      $got_os = 1;
##	    } else {
##	      print STDERR "$proteinID :: $_ multi \n";
##	      last;
##	    }
##	  }
##	  if ($_ =~ /^OC\s+(.*)/) {
##	    $oc .= $1;
##	    #	      print "OC: $1 \n"; 
##	  }
##	  #    print "$_ \n";
	  
##	} #/ end WHILE getz
	
##	$oc =~  s/;[^;]+$/;/;
##	print _FILE "$oc\n";
##	$tax = $oc;
##	$tax .=  $os;
	
	
##      } #/ end foreach proteinID

###      close(_TMP);
###      my $str = "getz -e \@tmp.txt |";
###      print STDERR "STR: $str \n";
###      open(_GETZ, "$str");

###      while(<_GETZ>) {
####	print STDERR "GETZ: $_ ";
	
###      }

###      close(_GETZ);



##    }
    
#  }
  
#}

  
#close(_SEQ);
#close(_REGIONS);


my  $rdb = Bio::Rfam->rdb_update();

#if ($file_type =~ /seed/) {

#  $rdb->add_rfamseq(  "/nfs/team71/pfam/mm1/scripts/Rfam/seed_upload_rfamseq.dat");
#  $rdb->update_rfam_reg_seed_from_file( "/nfs/team71/pfam/mm1/scripts/Rfam/seed_regions.dat" );

#} else {
#  $rdb->add_rfamseq( "/nfs/team71/pfam/mm1/scripts/Rfam/upload_rfamseq.dat");
  $rdb->update_rfam_reg_full_from_file( "/nfs/team71/pfam/mm1/scripts/Rfam/full_regions.dat" );
 

#}



#$time = time - $time;
#print STDERR "(execution time $time)\n";
	
1;
