#!/usr/local/bin/perl
#TODO:
#Logfile
# useage
#
use warnings;
use strict;

use Cwd;

use File::Copy;
use File::Basename;
use Getopt::Long;
use Config::General;
use Data::Dumper;
#use lib "/homes/swb/bin/Working/GenomeScan";
#use lib "/homes/swb/bin/Modules";
use DBI;
use Bio::SeqFeature::Generic;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::Infernal;
use Bio::Rfam::Utils;

use Bio::Easel::MSA;
use Bio::Easel::SqFile;
use Bio::Easel::Random;

my $rdb_host = "mysql-rfam-stage";
my $rdb_driver = "mysql";
my $rdb_user = "rfamro";
my $rdb_pass = "";
my $rdb_port= "4409";
my $rdb_name = "rfam_live";
# Create a connection to the database.
my $rfdbh = DBI->connect(
	"dbi:mysql:$rdb_name:$rdb_host:$rdb_port",$rdb_user, $rdb_pass, {
	PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	RaiseError => 1
	}    );
    
 my $query = qq( select rfam_acc, rfam_id, f.type, fr.type, f.description, rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score, group_concat(distinct concat(dl.db_id,":",dl.db_link)) as dbxrefs, group_concat(distinct concat("PMID:",fl.pmid)) as PMIDS from family f join full_region fr using (rfam_acc) join rfamseq rs using (rfamseq_acc) join database_link dl using (rfam_acc) join family_literature_reference fl using (rfam_acc) where f.type like 'Gene%' group by rfam_acc, rfam_id, f.type, fr.type, f.description,rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score ;); 
	
#my $query = qq( select rfam_acc, rfam_id, f.type, fr.type, f.description, rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score, group_concat(distinct concat(dl.db_id,":",dl.db_link)) as dbxrefs, group_concat(distinct concat("PMID:",fl.pmid)) as PMIDS from family f join full_region fr using (rfam_acc) join rfamseq rs using (rfamseq_acc) join database_link dl using (rfam_acc) join family_literature_reference fl using (rfam_acc) where f.rfam_acc =  'RF01888' group by rfam_acc, rfam_id, f.type, fr.type, f.description,rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score ;); 
	
my $sthQuery = $rfdbh->prepare($query);#

$sthQuery->execute();
              
my %classes = (
	"Gene; rRNA;" => "rRNA",                    
	"Gene; snRNA; splicing;" => "snRNA",         
	"Gene; tRNA;" => "tRNA",                    
	"Gene;" => "other",                          
	"Gene; ribozyme;" => "ribozyme",               
	"Gene; snRNA; snoRNA; CD-box;" => "snoRNA",  
	"Gene; sRNA;" => "other",                   
	"Gene; miRNA;" => "miRNA",        
	"Gene; antisense;" => "antisense_RNA",              
	"Gene; snRNA; snoRNA; HACA-box;" => "snoRNA",
	"Gene; snRNA;" => "snRNA",                  
	"Gene; antitoxin;" => "other",               
	"Gene; snRNA; snoRNA; scaRNA;" => "snoRNA",   
	"Gene; lncRNA;" => "lncRNA",                  
	"Gene; CRISPR;" => "other",
);

my %types = (
	"Gene; tRNA;" => "tRNA",
	"Gene; rRNA;" => "rRNA",
);

my %class_exceptions = (
	"RF00006" =>		"vault_RNA",
	"RF00024" =>		"telomerase_RNA",
	"RF00025" =>		"telomerase_RNA",
	"RF01050" =>		"telomerase_RNA",
	"RF00009" =>		"RNase_P_RNA",
	"RF00010" =>		"RNase_P_RNA",
	"RF00011" =>		"RNase_P_RNA",
	"RF00373" =>		"RNase_P_RNA",
	"RF00030" =>		"RNase_MRP_RNA",
	"RF00008" =>		"hammerhead_ribozyme",
	"RF00163" =>		"hammerhead_ribozyme",
	"RF00017" =>		"SRP_RNA",
	"RF00169" =>		"SRP_RNA",
	"RF01502" =>		"SRP_RNA",
	"RF01854" =>		"SRP_RNA",
	"RF01855" =>		"SRP_RNA",
	"RF01856" =>		"SRP_RNA",
	"RF01857" =>		"SRP_RNA",
);
print "RFAM_ACC\tRFAM_ID\tRNA_TYPE\tncRNA_CLASS\tALIGNMENT\tDESCRIPTION\tNCBI_ID\tSEQACC\tSEQ_START\tSEQ_END\tBITSCORE\tDBXREFS\tPMIDS\n";

my $res = $sthQuery->fetchall_arrayref;
for my $result (@$res) {
	my ($rfam_acc, $rfam_id, $rfam_type, $align_type, $rfam_description, $ncbi_id, $seq_acc, $seq_start, $seq_end, $bits, $dbxref,$pmids) = @$result;
	my $insdc_type = ($types{$rfam_type}) ? $types{$rfam_type} :"ncRNA";
	my $insdc_class = ($class_exceptions{$rfam_acc}) ? $class_exceptions{$rfam_acc} : $classes{$rfam_type};
	
	
	print "$rfam_acc\t$rfam_id\t$insdc_type\t$insdc_class\t$align_type\t$rfam_description\t$ncbi_id\t$seq_acc\t$seq_start\t$seq_end\t$bits\t$dbxref\t$pmids\n";
}


