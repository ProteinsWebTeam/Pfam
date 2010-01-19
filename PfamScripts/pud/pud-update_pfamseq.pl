#! /software/bin/perl -w

use strict;
use Log::Log4perl qw(:easy);
use Config::General qw(SaveConfig);
use Cwd;
use File::Copy;
use Getopt::Long;
use Digest::MD5 qw(md5_hex);
use Text::Wrap;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


$Text::Wrap::columns = 60;

my($statusdir, $pfamseq_dir);

&GetOptions( "status_dir=s"   => \$statusdir, 
	     "pfamseq_dir=s"  => \$pfamseq_dir);


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

unless($statusdir and -e $statusdir) {
    $logger->logdie("Status directory [$statusdir] does not exist");
}
unless($pfamseq_dir and -e $pfamseq_dir) {
    $logger->logdie("Pfamseq direcory [$pfamseq_dir] does not exist");
}

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

my $dbh = $pfamDB->getSchema->storage->dbh;


#First make a file of old pfamseq accs and md5s
if(-s "$statusdir/old_pfamseq.md5") {
    $logger->info("Already made a file of accessions and md5s from old pfamseq, this will be used during seed surgery\n");
}
else {
    $logger->info("Making a file of accessions and md5s from old pfamseq, this will be used during seed surgery\n");
    my $st_md5 = $dbh->prepare("select pfamseq_acc, md5 from pfamseq, pfamA_reg_seed where pfamseq.auto_pfamseq = pfamA_reg_seed.auto_pfamseq");
    $st_md5->execute() or $logger->logdie("Couldn't select pfamseq_acc and md5 from pfamseq table ".$st_md5->errstr."\n");
    
    my $md5_data = $st_md5->fetchall_arrayref;
    open(MD5, "> $statusdir/old_pfamseq.md5") or $logger->logdie("Couldn't open filehandle $!\n");
    foreach my $row (@$md5_data) {
	print MD5 "$row->[0]\t$row->[1]\n";
    }
    close MD5;

}

chdir($pfamseq_dir) or $logger->logdie("Couldn't change directory into $pfamseq_dir $!\n");


#Set UniProtKB location
my $uniprot_url = "ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete";


#Get reldate.txt from EBI ftp site
if(-s "reldate.txt") {
    $logger->debug("Already downloaded\n");
}
else {
    $logger->debug("Downloading reldate.txt from ebi ftp site\n");
    system ("wget $uniprot_url/reldate.txt") and $logger->logdie("Couldn't obtain reldate.txt from EBI:[$!]\n")
}


if(-e "$statusdir/updated_version") { 
    $logger->debug("Already updated rdb with swiss prot and trembl version\n");
} 
else {
    $logger->debug("Updating rdb with swiss prot and trembl version\n");
    my ($swiss_prot_rel, $trembl_rel);
    open(REL, "reldate.txt") or  $logger->logdie("Couldn't open reldate.txt");
    while(<REL>) {
	if(/swiss_prot_version:\s+(\S+)/) {
	    $swiss_prot_rel = $1;
	}
	elsif(/trembl_version:\s+(\S+)/) {
	    $trembl_rel = $1;
	}
    }
    close REL;

    my $st_version = $dbh->prepare("update VERSION set swiss_prot_version = \"$swiss_prot_rel\"");
    $st_version->execute() or $logger->logdie("Failed to update version table with swiss prot version ". $st_version->errstr."\n");
    my $st_version2 = $dbh->prepare("update VERSION set trembl_version = \"$trembl_rel\"");
    $st_version2->execute() or $logger->logdie("Failed to update version table with trembl version ". $st_version2->errstr."\n");

    system("touch $statusdir/updated_version") and $logger->logdie("Couldn't touch $statusdir/updated_version:[$!]\n");
}



#Get UniProtKB from EBI ftp site 
my @files = qw(uniprot_sprot.dat uniprot_trembl.dat);
foreach my $file (@files) {
    if(-s "$file.gz") {
	$logger->debug("Already downloaded $file.gz");
    } 
    else {
	$logger->debug("Downloading $file.gz from EBI ftp site\n");
	system ("wget $uniprot_url/$file.gz") and $logger->logdie("Couldn't download $file.gz from EBI:[$!]\n");
	$logger->logdie("Couldn't download $file.gz from EBI:[$!]\n") unless(-s "$file.gz");
    }
}


#Retrieve data from files
my $num_seq;
if(-e "$statusdir/parsed_uniprotkb") { 
    $logger->debug("Already parsed UniProtKB\n");
} 
else {
    #Get taxomony data
    my @result = $pfamDB->getSchema->resultset('NcbiTaxonomy')->search({});
    
    my %rdb_taxonomy;
    foreach my $row (@result) {
	$rdb_taxonomy{$row->ncbi_taxid}=1;
    }


    open(FASTA, ">pfamseq") or  $logger->logdie("Failed to open filehandle:[$!]\n");
    open(PFAMSEQ, ">pfamseq.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");
    open(DISULPHIDE, ">disulphide.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");
    open(ACT_METAL, ">active_site_metal.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");
    open(SEC_ACC, ">secondary_acc.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");


    foreach my $file (@files) { 
	
	$logger->debug("Parsing $file\n");
	
	$/= "//\n";
	open(FH, "gunzip -c $file.gz |") or $logger->logdie("Failed to gunzip $file.gz:[$!]\n");
	while(<FH>) {
	    
	    my @entry = split(/\n/, $_);
	    
	my %record;
	    foreach my $line (@entry) {
		if( $line =~ /^AC\s+(\S+);(.+)?/) {
		    my $secAcc;
		    if($record{'AC'}){
			$secAcc .= "$1;"
		    } 
		    else {
			$record{'AC'} = $1 
		    }
		    
		    if($2) {
			$secAcc .= $2;
			#remove all white space;
			$secAcc =~ s/\s+//g;
			#split on ; to get a arrayRef of the results.
			push(@{$record{'SEC_AC'}}, split(/;/, $secAcc));
		    }

		}
		elsif( $line =~ /^ID\s+(\S+)/){
		    $record{'ID'} = $1;  
		}
		elsif( $line =~ /^DT\s+.+sequence\s+version\s+(\d+)/) {
		    $record{'SEQ_VER'} = $1;
		} 
		elsif( !$record{'DE_CONTAINS'} and $line =~ /^DE\s+RecName\:\s+(Full|Short)=(.+);$/){
		    $record{'DE_REC'} .= "$2";
		    if($record{'DE_REC'} =~ /(.+)\n\$/) {
			$record{'DE_REC'} = "$1";
		    }

		    $record{'DE_REC'} .= " ";

		}
		elsif( $line =~ /^DE\s+(EC=\S+);$/){
		    $record{'DE_EC'} .= "$1 ";
		}  
		elsif( !$record{'DE_CONTAINS'} and $line =~ /^DE\s+SubName\:\s+Full=(.+);$/){
		    $record{'DE_SUB'} = "$1 "; 
		}
		elsif( $line =~ /DE\s+Contains/) {
		    $record{'DE_CONTAINS'}=1;
		}
		elsif( $line =~ /^DE\s+Flags\:\s+(.+);$/){
		    if($1 =~ /Fragment/){
			$record{'DE_FLAG'} = "(Fragment)";
		    }
		}
		elsif( $line =~ /^OS\s+(.+)$/){
		    $record{'OS'} .= "$1 ";  
		}
		elsif( $line =~ /^OC\s+(.+)$/){	      
		    $record{'OC'} .= "$1 ";  
		}       
		elsif( $line =~ /^OX\s+NCBI_TaxID=(\d+);$/){
		    if(exists($rdb_taxonomy{$1})) {
			$record{'NCBI_TAX'} = $1;  
		    } 
		    else {
			$record{'NCBI_TAX'} = "0";
		    }
		}
		elsif( $line =~ /^SQ\s+SEQUENCE\s+(\d+)\sAA;\s+\d+\sMW;\s+(\S+)\s+CRC64;$/){
		    $record{'SEQ_LEN'} = $1;
		    $record{'CRC64'} = $2;
		}
		elsif( $line =~ /^^\s+(.*)$/){
		    my $seq = $1;
		    $seq =~ s/\s+//g;
		    $record{'SEQ'} .= $seq;
		}       
		elsif( $line =~ /^PE\s+(\d+)\:/) {
		    $record{'PE'}=$1;  
		}   
		elsif( $line =~ /^FT\s+ACT_SITE\s+(\d+)\s+\d+\s+(.+)$/ or $line =~ /^FT\s+ACT_SITE\s+(\d+)\s+\d+/ ) {
		    my ($res, $annotation) = ($1, $2);
		    if($annotation) {
			$record{'AS'}{$res}=$annotation; 
			if($annotation =~ /\.$/) {
			    chop $record{'AS'}{$res}; #Remove the trailing '.'
			} 
			else {
			    $record{'AS_FT'} = $res;
			} 
		    } 
		    else {
			$record{'AS'}{$1}="";
		    }
		}
		elsif( defined($record{'AS_FT'}) and $line =~ /^FT\s+(.+)$/) {
		    my $r = $record{'AS_FT'};
		    $record{'AS'}{$r} .= " $1";
		    if($record{'AS'}{$r} =~ /\.$/) {
			chop $record{'AS'}{$r};	       
			$record{'AS_FT'}=undef;
		    }
		}
		elsif( $line =~ /^FT\s+METAL\s+(\d+)\s+\d+\s+(.+)$/ or $line =~ /^FT\s+METAL\s+(\d+)\s+\d+$/  ) { 
		    my ($res, $annotation) = ($1, $2);
		    if($annotation) {
			$record{'ME'}{$res}=$annotation;
			if($annotation =~ /\.$/) {
			    chop $record{'ME'}{$res};
			}
			else {
			    $record{'ME_FT'} = $res;
			}
		    }
		    else {
			$record{'ME'}{$1}="";
		    }
		}
		elsif( defined($record{'ME_FT'}) and $line =~ /^FT\s+(.+)$/) {
		    my $r = $record{'ME_FT'};
		    $record{'ME'}{$r} .= " $1";
		    if($record{'ME'}{$r} =~ /\.$/) {
			chop $record{'ME'}{$r};	       
			$record{'ME_FT'}=undef;
		    }
		    
		}
		elsif( $line =~ /^FT\s+DISULFID\s+(\d+)\s+(\d+)/) { 
		    $record{'DS'}{$1}=$2;
		}
		elsif( $line =~ /^FT\s+NON_CONS/) { 
		    $record{'NON_CONS'}=1;
		    last; # We don't do non consecutive   
		}
		
	    } 
	    
	    next if($record{'NON_CONS'});
	    
	    unless($record{'AC'}) {
		$logger->logdie("No accession found in record [$.] [@entry]  $file\n");
	    } 
	    
	    my $crc64 = _crc64($record{'SEQ'});
	    unless($crc64 eq $record{'CRC64'}) {
		$logger->logdie ("Incomplete sequence or crc64 is incorrect for $record{'AC'} \ncrc64:[$crc64]\nseq: $record{'SEQ'}\n");
	    }
	    
	    $record{'MD5'} = md5_hex($record{'SEQ'});
	    
	    my $description;
	    
	    if(exists($record{'DE_REC'})){
		$description = $record{'DE_REC'};
		$description .= $record{'DE_EC'} if($record{'DE_EC'});
		$description .= $record{'DE_FLAG'} if($record{'DE_FLAG'});
	    }
	    elsif( exists($record{'DE_SUB'})) {
		$description = $record{'DE_SUB'};
		$description .= $record{'DE_EC'} if($record{'DE_EC'});
		$description .= $record{'DE_FLAG'} if($record{'DE_FLAG'});
	    }
	    else {
		$logger->logdie("No DE line info, something has gone wrong for $record{'AC'}\n");
	    }
	    
	    #There will be a \s at the end of these variables so lets remove it 
	    chop $description unless($record{'DE_FLAG'}); 
	    chop $record{'OS'};
	    chop $record{'OC'};
	    
	    chop $record{'OS'} if($record{'OS'} =~ /\.$/); #Remove trailing '.'
	    
	    print FASTA ">$record{'AC'}" . "." . $record{'SEQ_VER'} . " $record{'ID'} $description\n";
	    print FASTA wrap("", "", $record{'SEQ'}) ."\n";
	    
	    my $is_frag;
	    if($record{'DE_FLAG'}) {
		$is_frag = 1;
	    } 
	    else {
		$is_frag = 0;
	    }
	    
	    print PFAMSEQ "$record{'ID'}\t$record{'AC'}\t$record{'SEQ_VER'}\t$record{'CRC64'}\t$record{'MD5'}\t$description\t$record{'PE'}\t$record{'SEQ_LEN'}\t$record{'OS'}\t$record{'OC'}\t$is_frag\t$record{'SEQ'}\t\\N\t'NOW()'\t$record{'NCBI_TAX'}\t\\N\t\\N\t\\N\n";
	    
	    
	    if(exists($record{'SEC_AC'})) {
		foreach my $sec_acc (@{$record{'SEC_AC'}}) {
		    print SEC_ACC "$record{'AC'}\t$sec_acc\n";
		}
	    }
	    
	    if($record{'AS'}) {
		foreach my $residue (keys %{$record{'AS'}}) {
		    if( lc($record{'AS'}{$residue}) =~ /(potential|probable|similarity)/) {
			print ACT_METAL "$record{'AC'}\t3\t$residue\t$record{'AS'}{$residue}\n";
		    }
		    else {
			print ACT_METAL "$record{'AC'}\t1\t$residue\t$record{'AS'}{$residue}\n";
			
		    } 
		}
	    }
	    
	    
	    if($record{'ME'}) {
		foreach my $residue (keys %{$record{'ME'}}) {
		    if( lc($record{'ME'}{$residue}) =~ /(potential|probable|similarity)/) {
			print ACT_METAL "$record{'AC'}\t5\t$residue\t$record{'ME'}{$residue}\n";
		    }
		    else {
			print ACT_METAL "$record{'AC'}\t4\t$residue\t$record{'ME'}{$residue}\n";
			
		    } 
		}
	    }
	    
	    if($record{'DS'}) {
		foreach my $residue (keys %{$record{'DS'}}) {
		    print DISULPHIDE "$record{'AC'}\t$residue\t$record{'DS'}{$residue}\n";
		}
	    }
	    
	    $num_seq++;
	    
	}
    }

    close FASTA;
    close DISULPHIDE;
    close ACT_METAL;
    close PFAMSEQ; 
    close SEC_ACC;

    system("touch $statusdir/parsed_uniprotkb") and $logger->logdie("Couldn't touch $statusdir/parsed_uniprotkb:[$!]\n");
}


#Make DBSIZE file
if(-s "DBSIZE") {
    $logger->debug("Already made DBSIZE file\n");
}
else {
    $logger->debug("Making DBSIZE file\n");
    open(DBSIZE, ">DBSIZE") or $logger->logdie("Couldn't open filehandle to DBSIZE:[$!]\n");
    print DBSIZE "$num_seq";
    close DBSIZE;
}

#Make NCBI and WU-blast indices
if(-e "$statusdir/made_easel_indices") {
    $logger->debug("Already made easel indices for pfamseq\n");
} 
else {
    $logger->debug("Making easel indices for pfamseq\n");
    system ("esl-sfetch --index pfamseq") and $logger->logdie("Couldn't make easel indices for pfamseq:[$!]");
    system("touch $statusdir/made_easel_indices") and $logger->logdie("Couldn't touch $statusdir/made_easel_indices:[$!]\n");
}
										 
if(-e "$statusdir/made_ncbi_indices") {
    $logger->debug("Already made NCBI indices for pfamseq\n");
}
else {
    $logger->debug("Making NCBI indices for pfamseq\n");
    system ("formatdb -p T -i pfamseq") and $logger->logdie("Couldn't make NCBI-blast indices for pfamseq:[$!]");
    system("touch $statusdir/made_ncbi_indices") and $logger->logdie("Couldn't touch $statusdir/made_ncbi_indices:[$!]\n");
}
 

#Make tmp_pfamseq table in rdb
if(-e "$statusdir/created_tmp_pfamseq") {
  $logger->info("Already created table tmp_pfamseq\n");
}
else {
  $logger->info("Creating table tmp_pfamseq\n");
    
  my $st = $dbh->prepare("create table tmp_pfamseq (\
  pfamseq_id varchar(12) NOT NULL,\
  pfamseq_acc varchar(6) NOT NULL,\
  seq_version tinyint(4) NOT NULL,\
  crc64 varchar(16) NOT NULL,\
  md5 varchar(32) NOT NULL,\
  description text NOT NULL,\
  evidence tinyint(4) NOT NULL,\
  length mediumint(8) NOT NULL default '0',\
  species text NOT NULL,\
  taxonomy mediumtext,\
  is_fragment tinyint(1) default NULL,\
  sequence blob NOT NULL,\
  updated timestamp NOT NULL default CURRENT_TIMESTAMP,\
  created datetime default NULL,\
  ncbi_taxid int(10) unsigned default '0',\
  genome_seq tinyint(1) default '0',\
  auto_architecture int(10) default NULL,\
  treefam_acc varchar(8) default NULL,\
  PRIMARY KEY  (pfamseq_acc),\
  KEY pfamseq_acc_version (pfamseq_acc,seq_version) ) ENGINE=InnoDB");

  $st->execute() or $logger->logdie("Failed to create table ".$st->errstr."\n");
  system("touch $statusdir/created_tmp_pfamseq") and $logger->logdie("Couldn't touch $statusdir/created_tmp_pfamseq:[$!]\n"); 

}

my $cwd = getcwd;

#Upload pfamseq data to temporary table
if(-e "$statusdir/uploaded_pfamseq") {
    $logger->info("Already uploaded $cwd/pfamseq.dat to tmp_pfamseq\n");
}
else {
    $logger->info("Uploading $cwd/pfamseq.dat to tmp_pfamseq\n");
    
    system("scp $cwd/pfamseq.dat ".$config->pfamliveAdmin->{host}.":/tmp/pfamseq.dat" ) and 
      $logger->logdie("Could not scp pfamseq.dat to database host");
    my $st_upload = $dbh->prepare("load data infile '/tmp/pfamseq.dat' into table tmp_pfamseq");
    $st_upload->execute() or $logger->logdie("Failed to upload pfamseq.dat".$st_upload->errstr."\n");
    system("touch $statusdir/uploaded_pfamseq") and $logger->logdie("Couldn't touch $statusdir/uploaded_pfamseq:[$!]\n"); 
}


#Delete obsolete data from pfamseq
if(-e "$statusdir/delete_pfamseq_obsolete") {
    $logger->info("Already deleted sequences in pfamseq that have been deleted from UniProtKB, or have changed in sequence version\n");
}
else {
    $logger->info("Deleting sequences in pfamseq that have been deleted from UniProtKB, or have changed in sequence version\n");
    my $delete_pfamseq = $dbh->prepare("delete pfamseq.* from pfamseq left join tmp_pfamseq on pfamseq.pfamseq_acc=tmp_pfamseq.pfamseq_acc and tmp_pfamseq.seq_version=pfamseq.seq_version and pfamseq.md5=tmp_pfamseq.md5 and pfamseq.crc64=tmp_pfamseq.crc64 where tmp_pfamseq.pfamseq_acc is null");
    $delete_pfamseq->execute() or $logger->logdie("Failed to delete rows from pfamseq table ".$delete_pfamseq->errstr."\n");
    system("touch $statusdir/delete_pfamseq_obsolete") and $logger->logdie("Couldn't touch $statusdir/delete_pfamseq_obsolete:[$!]\n"); 
}


#Update changed data to pfamseq
if(-e "$statusdir/update_pfamseq_changed") {
    $logger->info("Already updated pfamseq with changed data\n");
}
else {
    $logger->info("Updating pfamseq with changed data\n");
    my $changed_pfamseq = $dbh->prepare("update pfamseq, tmp_pfamseq set pfamseq.pfamseq_id = tmp_pfamseq.pfamseq_id, pfamseq.description = tmp_pfamseq.description, pfamseq.evidence = tmp_pfamseq.evidence,  pfamseq.species = tmp_pfamseq.species, pfamseq.taxonomy=tmp_pfamseq.taxonomy, pfamseq.is_fragment = tmp_pfamseq.is_fragment, pfamseq.updated = NOW(), pfamseq.ncbi_taxid = tmp_pfamseq.ncbi_taxid  where tmp_pfamseq.pfamseq_acc=pfamseq.pfamseq_acc");
    $changed_pfamseq->execute() or $logger->logdie("Failed to update pfamseq with changed data ".$changed_pfamseq->errstr."\n");
    system("touch $statusdir/update_pfamseq_changed") and $logger->logdie("Couldn't touch $statusdir/update_pfamseq_changed:[$!]\n"); 
}


#Insert new data to pfamseq
if(-e "$statusdir/pfamseq_new") {
    $logger->info("Already updated pfamseq with new data\n");
}
else {
    $logger->info("Updating pfamseq with new data\n");
    my $pfamseq_new = $dbh->prepare("insert into pfamseq (pfamseq.pfamseq_id, pfamseq.pfamseq_acc, pfamseq.seq_version, pfamseq.crc64, pfamseq.md5, pfamseq.description, pfamseq.length, pfamseq.species, pfamseq.taxonomy, pfamseq.is_fragment, pfamseq.sequence, pfamseq.created, pfamseq.ncbi_taxid) (select tmp_pfamseq.pfamseq_id, tmp_pfamseq.pfamseq_acc, tmp_pfamseq.seq_version, tmp_pfamseq.crc64, tmp_pfamseq.md5, tmp_pfamseq.description, tmp_pfamseq.length, tmp_pfamseq.species, tmp_pfamseq.taxonomy, tmp_pfamseq.is_fragment, tmp_pfamseq.sequence, tmp_pfamseq.created, tmp_pfamseq.ncbi_taxid from tmp_pfamseq left join pfamseq on tmp_pfamseq.pfamseq_acc=pfamseq.pfamseq_acc and tmp_pfamseq.seq_version=pfamseq.seq_version where pfamseq.pfamseq_acc is null)");
    $pfamseq_new->execute() or $logger->logdie("Failed to update pfamseq with new data ".$pfamseq_new->errstr."\n");
    system("touch $statusdir/pfamseq_new") and $logger->logdie("Couldn't touch $statusdir/pfamseq_new:[$!]\n"); 
}


#Check pfamseq in rdb is the correct size;
my $dbsize;
if(-e "$statusdir/check_pfamseq_size") {
    open(DB, "DBSIZE") or $logger->logdie("Could not open DBSIZE:[$!]\n");
    while(<DB>){
      ($dbsize) = $_ =~/(\S+)/;
      last;
    }
    close(DB);
    $logger->info("Already checked the number of sequences [$dbsize] in rdb is the same as in the pfamseq fasta file\n");
}
else {
    $logger->info("Checking the number of sequences in rdb is the same as in the pfamseq fasta file\n");
    open(DB, "DBSIZE") or $logger->logdie("Couldn't open file handle to DBSIZE");
    while(<DB>) {
	if(/(\S+)/) {
	    $dbsize = $1;
	    last;
	}
    }
    close DB;

    my $pfamseq_qc = $dbh->prepare("select count(*) from pfamseq");

    $pfamseq_qc->execute or $logger->logdie("Failed to query pfamseq for size of pfamseq ".$pfamseq_qc->errstr."\n");

    my $rdb_size = $pfamseq_qc->fetchrow;

    unless($rdb_size == $dbsize) {
	$logger->logdie("Mis-match between [$rdb_size] sequences in rdb, and $dbsize sequences in the pfamseq fasta file\n");
    }else{
      system("touch $statusdir/check_pfamseq_size") and $logger->logdie("Could not touch $statusdir/check_pfamseq_size:[$!]");
    }
}

my %acc2auto;


#Transform active site and metal ion binding data
if(-e "active_site_metal.auto.dat ") {
    $logger->info("Already transformed active_site_metal.dat file\n");
}
else {
    $logger->info("Transforming active_site_metal.dat file\n");
    acc2auto_mapping(\%acc2auto) unless(scalar keys %acc2auto);
    acc2auto("active_site_metal.dat", "active_site_metal.auto.dat", \%acc2auto);
}


#Delete old active site and metal ion binding data
if(-e "$statusdir/delete_active_metal") {
    $logger->info("Already deleted old active site and metal ion binding data from pfamseq_markup\n");
}
else {
    $logger->info("Deleting old active site and metal ion binding data from pfamseq_markup\n");
    my $delete_act_metal = $dbh->prepare("delete from pfamseq_markup");
    $delete_act_metal->execute() or $logger->logdie("Failed to delete old data from pfamseq_markup ".$delete_act_metal->errstr."\n");
    system("touch $statusdir/delete_active_metal") and $logger->logdie("Couldn't touch $statusdir/delete_active_metal:[$!]\n"); 
}


#Upload new active site and metal ion binding data
if(-e "$statusdir/upload_active_metal") {
    $logger->info("Already uploaded $cwd/active_site_metal.dat to pfamseq_markup\n");
}
else {
    $logger->info("Uploading $cwd/active_site_metal.auto.dat to pfamseq_markup\n");
    
    system("scp $cwd/active_site_metal.auto.dat ".$config->pfamliveAdmin->{host}.":/tmp/active_site_metal.auto.dat" ) and
    $logger->logdie('Failed to scp active_site_metal.auto.dat to database host');
my $act_metal_new = $dbh->prepare("load data infile '/tmp/active_site_metal.auto.dat' into table pfamseq_markup (auto_pfamseq, auto_markup, residue, annotation) ");
$act_metal_new->execute() or $logger->logdie("Failed to upload active site and metal ion binding data to pfamseq_markup ".$act_metal_new->errstr."\n");
    system("touch $statusdir/upload_active_metal") and $logger->logdie("Couldn't touch $statusdir/upload_active_metal:[$!]\n"); 
}


#Transform disulphide bond binding data
if(-e "disulpide.auto.dat") {
    $logger->info("Already transformed disulpide.dat file\n");
}
else {
    $logger->info("Transforming disulpide.dat file\n");
    acc2auto_mapping(\%acc2auto) unless(scalar keys %acc2auto);
    acc2auto("disulphide.dat", "disulphide.auto.dat", \%acc2auto);
}




#Delete old disulphide bond data
if(-e "$statusdir/delete_disulphide") {
    $logger->info("Already deleted old disulphide bond data\n");
}
else {
    $logger->info("Deleting old disulphide bond data\n");
    my $delete_disulphide = $dbh->prepare("delete from pfamseq_disulphide");
    $delete_disulphide->execute() or $logger->logdie("Failed to delete old data from pfamseq_disulphide ".$delete_disulphide->errstr."\n");
    system("touch $statusdir/delete_disulphide") and $logger->logdie("Couldn't touch $statusdir/delete_disulphide:[$!]\n"); 
}


#Upload new disulphide bond data
if(-e "$statusdir/upload_disulphide") {
   $logger->info("Already uploaded $cwd/disulphide.dat to pfamseq_disulphide\n");
}
else {
    $logger->info("Uploading $cwd/disulphide.auto.dat to pfamseq_disulphide\n");
    system("scp $cwd/disulphide.auto.dat ".$config->pfamliveAdmin->{host}.":/tmp/disulphide.auto.dat" ) and $logger->logdie("Could not scp disulphide.auto.dat to database host"); 
    my $disulphide_new = $dbh->prepare("load data infile '/tmp/disulphide.auto.dat' into table pfamseq_disulphide (auto_pfamseq, bond_start, bond_end)");
    $disulphide_new->execute() or $logger->logdie("Failed to upload disulphide bond data to pfamseq_disulphide ".$disulphide_new->errstr."\n");
    system("touch $statusdir/upload_disulphide") and $logger->logdie("Couldn't touch $statusdir/upload_disulphide:[$!]\n"); 
}


#Transform secondary acc data
if(-e "disulpide.auto.dat") {
    $logger->info("Already transformed secondary_acc.dat file\n");
}
else {
    $logger->info("Transforming secondary accession data\n");
    acc2auto_mapping(\%acc2auto) unless(scalar keys %acc2auto);
    acc2auto("secondary_acc.dat", "secondary_acc.auto.dat", \%acc2auto);
}


#Delete old secondary accession data
if(-e "$statusdir/delete_secondary_acc") {
    $logger->info("Already deleted old secondary accession data\n");
}
else {
    $logger->info("Deleting old secondary accession data\n");
    my $delete_sec_acc = $dbh->prepare("delete from secondary_pfamseq_acc");
    $delete_sec_acc->execute() or $logger->logdie("Failed to delete old data from secondary_pfamseq_acc ".$delete_sec_acc->errstr."\n");
    system("touch $statusdir/delete_secondary_acc") and $logger->logdie("Couldn't touch $statusdir/delete_secondary_acc:[$!]\n"); 
}


#Upload new secondary accesion data
if(-e "$statusdir/upload_secondary_acc") {
    $logger->info("Already uploading $cwd/secondary_acc.dat to secondary_pfamseq_acc\n");
}
else {
    $logger->info("Uploading $cwd/secondary_acc.auto.dat to secondary_pfamseq_acc\n");
    system("scp $cwd/secondary_acc.auto.dat ".$config->pfamliveAdmin->{host}.":/tmp/secondary_acc.auto.dat" ) and $logger->logdie("Could not scp secondary_acc.auto.dat to database host"); 
    my $sec_acc_new = $dbh->prepare("load data infile '/tmp/secondary_acc.auto.dat' into table secondary_pfamseq_acc (auto_pfamseq, secondary_acc)");

    $sec_acc_new->execute() or $logger->logdie("Failed to upload secondary pfamseq accessions to secondary_pfamseq_acc ".$sec_acc_new->errstr."\n");
    system("touch $statusdir/upload_secondary_acc") and $logger->logdie("Couldn't touch $statusdir/upload_secondary_acc:[$!]\n"); 
}


#Delete old active site alignments
if(-e "$statusdir/delete_active_site_alignments") {
    $logger->info("Already deleted old active site alignments\n");
}
else {
    $logger->info("Deleting old active site alignments\n");
    my $as_aln = $dbh->prepare("delete from _active_site_alignments");
    $as_aln->execute() or $logger->logdie("Failed to delete old data from _active_site_alignments");
    system("touch $statusdir/delete_active_site_alignments") and $logger->logdie("Couldn't touch $statusdir/delete_active_site_alignments:[$!]\n"); 
}

$logger->info("All the data from the new UniProtKB has been uploaded to the rdb\n");


#Copy pfamseq to nfs
if(-e "$statusdir/copied_pfamseq") {
    $logger->info("Already copied pfamseq to nfs directory\n");
}
else {
    $logger->info("Copying pfamseq to nfs directory\n");
    my $pfamseq_nfs = $config->{pfamseq}->{location};

    unlink glob("$pfamseq_nfs/*") or $logger->logdie("Problem deleting files in $pfamseq_nfs\n");
    my @pfamseq_files = qw(pfamseq pfamseq.pal pfamseq.ssi);
    open(PAL, "pfamseq.pal") or $logger->logdie("Could not open pfamseq.pal:[$!]");
    while(<PAL>){
      if(/^DBLIST\s+(.*)/){
        my $l = $1; #$l contains the list of virtual databases;
        foreach my $bit (split(/\s+/, $l)){
          foreach my $ext (qw(phr pin psq)){
            push(@pfamseq_files, $bit.".".$ext);
          }
        }
      }
    }
    close(PAL);

    foreach my $f (@pfamseq_files) {
        $logger->debug("Copying $f");
        copy($f, $pfamseq_nfs."/".$f) or $logger->logdie("Copy $f to $pfamseq_nfs failed: $!");
    }
    system("touch $statusdir/copied_pfamseq") and $logger->logdie("Couldn't touch $statusdir/copied_pfamseq");
}


#Change PFAM_CONFIG
if(-e "$statusdir/changed_pfam_config") {
    $logger->info("Already changed pfam config file\n");
}
else {
    $logger->info("Changing pfam config file\n");

    my $c = new Config::General($ENV{PFAM_CONFIG}); 
    my %ac = $c->getall; 
    
    my $pfam_config = $ENV{PFAM_CONFIG};
    move($pfam_config, "$pfam_config.old") or $logger->logdie("Could not move config file");;
    $ac{pfamseq}->{dbsize} = $dbsize; 
  
    SaveConfig($pfam_config, \%ac);

    my $newConfig;
    eval { 
	$newConfig = Bio::Pfam::Config->new; 

    };
    if($@) { 
	$logger->logdie("Problem modifying the pfam_config ($pfam_config) file");
    }
    $config = $newConfig;

  system("touch $statusdir/changed_pfam_config") and $logger->logdie("Couldn't touch $statusdir/changed_pfam_config:[$!]\n"); 
}











sub _crc64 {
  my $text = shift;
  use constant EXP => 0xd8000000;
  my @highCrcTable = 256;
  my @lowCrcTable  = 256;
  my $initialized  = ();
  my $low          = 0;
  my $high         = 0;

  unless($initialized) {
    $initialized = 1;
    for my $i(0..255) {
      my $low_part  = $i;
      my $high_part = 0;
      for my $j(0..7) {
        my $flag = $low_part & 1; # rflag ist f\u0178r alle ungeraden zahlen 1
        $low_part >>= 1;# um ein bit nach rechts verschieben
        $low_part |= (1 << 31) if $high_part & 1; # bitweises oder mit 2147483648 (), wenn $parth ungerade
        $high_part >>= 1; # um ein bit nach rechtsverschieben
        $high_part ^= EXP if $flag;
      }
      $highCrcTable[$i] = $high_part;
      $lowCrcTable[$i]  = $low_part;
    }
  }

  foreach (split '', $text) {
    my $shr = ($high & 0xFF) << 24;
    my $tmph = $high >> 8;
    my $tmpl = ($low >> 8) | $shr;
    my $index = ($low ^ (unpack "C", $_)) & 0xFF;
    $high = $tmph ^ $highCrcTable[$index];
    $low  = $tmpl ^ $lowCrcTable[$index];
  }
  return sprintf("%08X%08X", $high, $low);
}# end crc64



sub acc2auto {

    my ($infile, $outfile, $auto_hash) = @_;

    open(INFILE, $infile) or $logger->logdie("Couldn't open file handle to $infile $!\n");

    open(OUTFILE, "> $outfile") or $logger->logdie("Couldn't open $outfile for writing $!\n");

    while(<INFILE>) {
 
        if(/^(\S+)\s+(.+)/) {
            my ($acc, $rest) = ($1, $2);
 
            unless(exists($$auto_hash{$acc})) {
                next;
                $logger->logdie("This acc [$acc] is not in pfamseq!\n");
            }

            print OUTFILE "$$auto_hash{$acc}\t$rest\n";
        }
    }
    close INFILE;
    close OUTFILE;
}


sub acc2auto_mapping {

    my ($hash) = @_;

    #Query pfamseq table for accessions and auto numbers
    $logger->info("Querying pfamseq table for accessions and auto numbers\n");
    my $acc2auto = $dbh->prepare("select pfamseq_acc, auto_pfamseq from pfamseq");
    $acc2auto->execute or $logger->logdie("Failed to query pfamseq table for accessions and auto numbers\n");
    my $acc2auto_data = $acc2auto->fetchall_arrayref;
        
    
    my %acc2auto;
    foreach my $row (@{$acc2auto_data}) {
	$$hash{$row->[0]}= $row->[1];
    }
    
}
