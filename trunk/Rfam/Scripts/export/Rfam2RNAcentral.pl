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

my $config = Bio::Rfam::Config->new;
#my $rfamdb = $config->rfamlive;  #use this when converting query to DBIx class
my $schema = $config->rfamlive;
my $rfdbh    = $schema->storage->dbh;

my $query1 = qq(select rfam_acc from family;);

my $query2 = qq( select rfam_acc, rfam_id, f.type, fr.type, f.description, rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score, group_concat(distinct concat(dl.db_id,":",dl.db_link)) as dbxrefs, group_concat(distinct concat("PMID:",fl.pmid)) as PMIDS from family f join full_region fr using (rfam_acc) join rfamseq rs using (rfamseq_acc) join database_link dl using (rfam_acc) join family_literature_reference fl using (rfam_acc) where f.type like 'Gene%' and f.rfam_acc = ? and fr.is_significant="1" group by rfam_acc, rfam_id, f.type, fr.type, f.description,rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score;);

#my $query = qq( select rfam_acc, rfam_id, f.type, fr.type, f.description, rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score, group_concat(distinct concat(dl.db_id,":",dl.db_link)) as dbxrefs, group_concat(distinct concat("PMID:",fl.pmid)) as PMIDS from family f join full_region fr using (rfam_acc) join rfamseq rs using (rfamseq_acc) join database_link dl using (rfam_acc) join family_literature_reference fl using (rfam_acc) where f.rfam_acc =  'RF01888' group by rfam_acc, rfam_id, f.type, fr.type, f.description,rs.ncbi_id, fr.rfamseq_acc, fr.seq_start, fr.seq_end, fr.bit_score ;); 
	
my $sthQuery1 = $rfdbh->prepare($query1);
my $sthQuery2 = $rfdbh->prepare($query2);#

my %families;
              
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


$sthQuery1->execute();
my $res1 = $sthQuery1->fetchall_arrayref;
foreach my $row1 (@$res1){
    $families{$row1->[0]}=1;
}

foreach my $accn (keys %families){

    $sthQuery2->execute($accn);

    my $res = $sthQuery2->fetchall_arrayref;
    for my $result (@$res) {
	my ($rfam_acc, $rfam_id, $rfam_type, $align_type, $rfam_description, $ncbi_id, $seq_acc, $seq_start, $seq_end, $bits, $dbxref,$pmids) = @$result;
	my $insdc_type = ($types{$rfam_type}) ? $types{$rfam_type} :"ncRNA";
	my $insdc_class = ($class_exceptions{$rfam_acc}) ? $class_exceptions{$rfam_acc} : $classes{$rfam_type};
	
	
	print "$rfam_acc\t$rfam_id\t$insdc_type\t$insdc_class\t$align_type\t$rfam_description\t$ncbi_id\t$seq_acc\t$seq_start\t$seq_end\t$bits\t$dbxref\t$pmids\n";
    }
}

