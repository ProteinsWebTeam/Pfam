#!/usr/bin/perl -T
#
# Author:        jaina
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       18 June, 2020
# Last Modified: $Date$

#This needs to be configured in apache ideally, but it works and so I am going with this......
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamLib';
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata';
use lib '/nfs/production/xfam/pfam/Bio-HMM-Logo-master/blib/arch';
use lib '/nfs/production/xfam/pfam/Bio-HMM-Logo-master/blib/lib';
use lib '/nfs/production/xfam/pfam/perl5/lib/perl5';
use lib '/nfs/production/xfam/pfam/perl5/lib/perl5/x86_64-linux-thread-multi';

use strict;
use warnings;
$ENV{'PFAM_CONFIG'} = '/nfs/production/xfam/pfam/software/Conf/pfam_svn.conf';  
use CGI;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::PfamQC;

my $q = CGI->new;
my ($ncbi_tax) = $q->param('ncbi_taxid') =~ /^(\d+)$/;
unless($ncbi_tax) { 
  bail("No ncbi_taxid specified");
}

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#Get species name from db
my $st_tax = $dbh->prepare("select species from complete_proteomes where ncbi_taxid = '$ncbi_tax'");
$st_tax->execute or bail("Couldn't execute statement ".$st_tax->errstr."\n");

my ($species) = $st_tax->fetchrow;

my $summary; #Text for summary file
if($species) {
  $summary = "Species name for NCBI taxid $ncbi_tax is '$species'\n";
}
else {
  bail("Couldn't find the ncbi taxid |$ncbi_tax| in the database");
}


#Find out which ones have a Pfam-A match
my $st2 = $dbh->prepare("select p.pfamseq_acc from pfamseq p, pfamA_reg_full_significant r where p.pfamseq_acc=r.pfamseq_acc and in_full=1 and ncbi_taxid=$ncbi_tax");
$st2->execute() or bail("Couldn't execute statement ". $st2->errstr."\n");

my $pfamA_data = $st2->fetchall_arrayref;
my %pfamA_proteins;
foreach my $row(@$pfamA_data) {
  $pfamA_proteins{$row->[0]}=1;
}
my $pfamA_count = keys %pfamA_proteins;


#Get total number of proteins in proteome, and info to make fasta file from
my $st3 = $dbh->prepare("select pfamseq_acc, seq_version, description, sequence from pfamseq where ncbi_taxid=$ncbi_tax");
$st3->execute() or die bail("Couldn't execute statement ".$st3->errstr."\n");

my $all_data = $st3->fetchall_arrayref;
my $protein_count=0;
my $fa;
foreach my $row(@$all_data) {
  my ($pfamseq_acc, $seq_version, $desc, $sequence) = ($row->[0], $row->[1], $row->[2], $row->[3]);

  $protein_count++;

  unless(exists($pfamA_proteins{$pfamseq_acc})) {
    $fa .= ">$pfamseq_acc" . "." . "$seq_version $desc\n$sequence\n";
  }
}


$summary .= "$protein_count proteins in proteome\n";
my $percentage1 = ($pfamA_count/$protein_count)*100;
$percentage1 = sprintf("%.1f", $percentage1);

my $no_pfamA = $protein_count - $pfamA_count;
my $percentage2 = ($no_pfamA/$protein_count)*100;
$percentage2 = sprintf("%.1f", $percentage2);

$summary .= "$pfamA_count/$protein_count ($percentage1\%) proteins have a match to a Pfam-A domain\n";
$summary .= "$no_pfamA/$protein_count ($percentage2\%) proteins do not match a Pfam-A domain\n";

my $all_lines = $summary . "End of summary\n";  #This signifies the enof the summary, and is used in a reg ex in the proteome_jackhmmer.cgi script
$all_lines .= $fa;

#Return lines all the lines
print $q->header(), $all_lines;


exit;

sub bail {
  my $msg = shift;
  print $q->header( -status => '400 Bad Request' ), $msg;
  exit 1;
}
