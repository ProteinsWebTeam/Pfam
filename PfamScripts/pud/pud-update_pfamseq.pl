#! /usr/bin/env perl 

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Config::General qw(SaveConfig);
use Cwd;
use File::Copy;
use Getopt::Long;
use Digest::MD5 qw(md5_hex);
use Text::Wrap;
use Mail::Mailer;
# use LSF::Job;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


$Text::Wrap::columns = 60;

my($status_dir, $pfamseq_dir);

&GetOptions( "status_dir=s"   => \$status_dir, 
  "pfamseq_dir=s"  => \$pfamseq_dir);


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

unless($status_dir and -e $status_dir) {
  help();
}
unless($pfamseq_dir and -e $pfamseq_dir) {
  help();
}

my $cwd = cwd();
$logger->debug("Using work directory: $cwd\n");

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

my $dbh = $pfamDB->getSchema->storage->dbh;

#set uniprot location
my $uniprot_location = $config->uniprotPrivateLoc;


#Get reldate.txt from UniProt ftp directory
if(-s "$pfamseq_dir/reldateRP.txt") {
  $logger->debug("Already copied reldate.txt\n");
}
else {
  $logger->debug("Copying reldate.txt from uniprot\n");
  # copy("$uniprot_location/reldate.txt", "$pfamseq_dir/reldateRP.txt") or $logger->logdie("Could not copy reldate.txt [$!]\n");

  # my $cp_data = LSF::Job->submit(-q => "datamover", -o => "/dev/null", -J => "reldate_pfamseq", "cp $uniprot_location/reldate.txt $pfamseq_dir/reldateRP.txt");

  system("sbatch --job-name=reldate_pfamseq --time=1:00:00 --mem=1G -p datamover -o '/dev/null' -e '/dev/null' --wrap=\"cp $uniprot_location/reldate.txt $pfamseq_dir/reldateRP.txt\" ");

  $logger->info("Waiting for reldateRP.txt to be copied from uniprot reldate.txt");
  until(-s "$pfamseq_dir/reldateRP.txt") {
    sleep 30;
  }
}



if(-e "$status_dir/updated_reference_proteome_version") { 
  $logger->debug("Already updated rdb with reference proteome version\n");
} 
else {
  $logger->debug("Updating rdb with refprot version\n");
  my ($swiss_prot_rel, $trembl_rel);
  open(REL, "$pfamseq_dir/reldateRP.txt") or  $logger->logdie("Couldn't open reldateRP.txt");
  while(<REL>) {
    if(/Swiss-Prot Release\s+(\S+)/) {
      $swiss_prot_rel = $1;
    }
    elsif(/TrEMBL Release\s+(\S+)/) {
      $trembl_rel = $1;
    }
  }
  close REL;
  unless(defined($trembl_rel) and defined($swiss_prot_rel)){
    $logger->logdie("Failed to get release information for reldate.txt.");  
  }
  #Reference protoeme version will be same as the sprot/trembl version in the reldate.txt file
  my $st_version = $dbh->prepare("update version set reference_proteome_version = \"$swiss_prot_rel\"");
  $st_version->execute() or $logger->logdie("Failed to update version table with reference proteome version ". $st_version->errstr."\n");
  chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");
  system("touch $status_dir/updated_reference_proteome_version");
}

chdir($pfamseq_dir) or $logger->logdie("Couldn't change directory into $pfamseq_dir $!\n");

#Get a copy of refprot
my @files = qw(uniprot_reference_proteomes.dat.gz);

foreach my $file (@files) {
  if(-s $file) {
    $logger->debug("Already copied $file");
  }
  else {
    $logger->debug("Copying $uniprot_location/internal/$file\n");
    # copy("$uniprot_location/internal/$file", "$file") or $logger->logdie("Could not copy $file [$!]\n");
    # $logger->logdie("Couldn't copy $file from $uniprot_location/internal/$file:[$!]\n") unless(-s "$file");

    # my $cp_data = LSF::Job->submit(-q => "datamover", -o => "/dev/null", -J => 'cp_pfamseq', "cp $uniprot_location/internal/$file $file");
    # my $cp_data2 = LSF::Job->submit(-q => "datamover", -o => "/dev/null", -w => "done($cp_data)", -J => 'cp_pfamseq_done', "touch cp_${file}_done");

    my $jobid;
    my $job_res = `sbatch --mem=1GB --time=1:00:00 -J cp_pfamseq -p datamover -o '/dev/null' -e '/dev/null' --wrap="cp $uniprot_location/internal/$file $file"`;

    if ($job_res =~ /^Submitted batch job (\d+)/ ) {
      $jobid = $1;
    }
    system("sbatch --job-name=cp_pfamseq_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '/dev/null' -e '/dev/null' --wrap=\"touch cp_${file}_done\" ");

    $logger->debug("Waiting for copy job to complete...");
    until(-e "cp_${file}_done") {
      sleep 60;
    }
    $logger->debug("Completed copying $file");
    unlink("cp_${file}_done");
  }
}


#$logger->logdie("Check that NcbiTaxonomy has been populated, then remove this line!");
#Retrieve data from files
my $num_seq;
chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");
if(-e "$status_dir/parsed_refprot") { 
  $logger->debug("Already parsed refprot\n");
} 
else {
  #Get taxomony data
  my @result = $pfamDB->getSchema->resultset('NcbiTaxonomy')->search({});

  my %rdb_taxonomy;
  foreach my $row (@result) {
    $rdb_taxonomy{$row->ncbi_taxid}=1;
  }

  chdir($pfamseq_dir) or $logger->logdie("Couldn't change directory into $pfamseq_dir $!\n");

  open(PFAMSEQ, ">pfamseq.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");
  open(FASTA, ">pfamseq.fasta") or $logger->logdie("Failed to open filehandle:[$!]\n");
  open(DISULPHIDE, ">disulphide.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");
  open(ACT_METAL, ">active_site_metal.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");
  open(SEC_ACC, ">secondary_acc.dat") or  $logger->logdie("Failed to open filehandle:[$!]\n");

  foreach my $file (@files) { 

    $logger->debug("Parsing $file\n");

    local $/= "//\n";
    open(FH, "gunzip -c $file |") or $logger->logdie("Failed to gunzip $file:[$!]\n");

#adding counts for debugging
    my $count1=0;
    my $count2=0;

    while(<FH>) {

      my @entry = split(/\n/, $_);
      my $AS_res=undef; #Active site residue
      my $ME_res=undef; #Metal binding 

#count for debugging
      $count1++;
      my %record;
      foreach my $line (@entry) {

        #If last line was parsing active site data, see if 
        #active site annoation has finished
        if($AS_res) {
            if($line =~ /^FT\s{3}\w/) { #Next feature
                  $AS_res=undef;
            }
        }

        #If last line was parsing metal binding data, see if 
        #metal binding annotation has finished
        if($ME_res) {
            if($line =~ /^FT\s{3}\w/) { #Next feature
                $ME_res=undef;
            }
        }

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
        elsif( $line =~ /^ID\s+(\S+)\s+(\S+)/){
          $record{'ID'} = $1;  
          if($2 eq 'Reviewed;') {
            $record{'SWISSPROT'}=1;
          }
          else {
            $record{'SWISSPROT'}=0;
          }
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
        elsif( $line =~ /^OX\s+NCBI_TaxID=(\d+)/) {
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
		elsif($line =~ /^FT\s+ACT_SITE\s+(\d+)/)  {  #FT   ACT_SITE        238
			$record{'AS'}{$1}=undef;
			$AS_res=$1;
		}
		elsif($AS_res) {
			if($line =~ /^FT\s+(.+)/) {   #FT                   /note="Charge relay system; for serine protease NS3
				$record{'AS'}{$AS_res} .= " " if(defined($record{'AS'}{$AS_res}));
				$record{'AS'}{$AS_res} .= $1;
			}
		}
		elsif($line =~ /^FT\s+METAL\s+(\d+)/) {  #FT   METAL           52
			$record{'ME'}{$1}=undef;
			$ME_res=$1;
		}
		elsif($ME_res) {
			if($line =~ /^FT\s+(.+)/) {
				$record{'ME'}{$ME_res} .= " " if(defined($record{'ME'}{$ME_res}));
				$record{'ME'}{$ME_res} .= $1;
			}
		}
		elsif( $line =~ /^FT\s+DISULFID\s+(\d+)\.\.(\d+)/) {  #FT   DISULFID        5..103
			$record{'DS'}{$1}=$2;
		}
		elsif( $line =~ /^FT\s+DISULFID\s+(\d+)/) {  #FT   DISULFID        30, eg interchain
			$record{'DS'}{$1}="\\N";
		}
        elsif( $line =~ /^FT\s+NON_CONS/) { 
          $record{'NON_CONS'}=1;
          last; # We don't do non consecutive   
        }
        elsif ($line =~ /^KW.+Complete\sproteome.+Reference\sproteome/){ #KW line contains complete and reference
          $record{'Complete_proteome'}=1;
          $record{'Reference_proteome'}=1;
        }
        elsif( $line =~ /^KW.+Reference\sproteome/){ #is it from a reference proteome?
          $record{'Reference_proteome'}=1;
        }
        elsif ($line =~ /^KW.+Complete\sproteome/){ #is it from a complete proteome?
          $record{'Complete_proteome'}=1;
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

      my $is_frag=0;
      $is_frag = 1 if($record{'DE_FLAG'});

      #This will be uploaded into the tmp_pfamseq table.
      print PFAMSEQ "$record{'AC'}\t$record{'ID'}\t$record{'SEQ_VER'}\t$record{'CRC64'}\t$record{'MD5'}\t$description\t$record{'PE'}\t$record{'SEQ_LEN'}\t$record{'OS'}\t$record{'OC'}\t$is_frag\t$record{'SEQ'}\t\\N\t\\N\t$record{'NCBI_TAX'}\t\\N\t\\N\t$record{'SWISSPROT'}\n";
      print FASTA ">$record{'AC'}.$record{'SEQ_VER'} $record{'ID'} $description\n$record{'SEQ'}\n";

#count for debugging
      $count2++;

      if(exists($record{'SEC_AC'})) {
        foreach my $sec_acc (@{$record{'SEC_AC'}}) {
          print SEC_ACC "$record{'AC'}\t$sec_acc\n";
        }
      }

####need to parse out evidence tags here###	
	  foreach my $residue (sort keys %{$record{'AS'}}) {
		  my $annotation = $record{'AS'}{$residue};

		  if(!$annotation) {
			  print ACT_METAL "$record{'AC'}\t3\t$residue\t\\N\t\\N\n"; #auto_markup 3 = uniprot predicted active site
			  next;
		  }

		  $annotation =~ s/\///g;  #remove / (eg /note="Nucleophile" /evidence="ECO:0000255|HAMAP-Rule:MF_00807")

		  #ECO:0000269 - Experimental evidence
          #ECO:0000305 - Curator inference evidence
          #ECO:0000303 - Non-traceable author statement evidence

          if($annotation =~ /ECO:0000269/ or $annotation =~ /ECO:0000305/ or $annotation =~ /ECO:0000303/) {
             print ACT_METAL "$record{'AC'}\t1\t$residue\t$annotation\t\\N\n"; #auto_markup 1 = expermentally determined active site
          }
          else {
             print ACT_METAL "$record{'AC'}\t3\t$residue\t$annotation\t\\N\n"; #auto_markup 3 = uniprot predicted active site
          }
      }



####need to parse out evidence tags here###	    
      foreach my $residue (sort keys %{$record{'ME'}}) {
        my $annotation = $record{'ME'}{$residue};

        if(!$annotation) {
			print ACT_METAL "$record{'AC'}\t5\t$residue\t\\N\t\\N\n"; #auto_markup 5 = uniprot predicted metal ion binding
            next;
		}

		$annotation =~ s/\///g;  #remove / (eg /note="Nucleophile" /evidence="ECO:0000255|HAMAP-Rule:MF_00807")

		#ECO:0000269 - Experimental evidence
        #ECO:0000305 - Curator inference evidence
        #ECO:0000303 - Non-traceable author statement evidence

		if($annotation =~ /ECO:0000269/ or $annotation =~ /ECO:0000305/ or $annotation =~ /ECO:0000303/) {
			print ACT_METAL "$record{'AC'}\t4\t$residue\t$annotation\t\\N\n"; #auto_markup 4 = experimentally determined metal ion binding
		}
		else {
			print ACT_METAL "$record{'AC'}\t5\t$residue\t$annotation\t\\N\n"; #auto_markup 5 = uniprot predicted metal ion binding
		}
	  }

      if($record{'DS'}) {
        foreach my $residue (keys %{$record{'DS'}}) {
          print DISULPHIDE "$record{'AC'}\t$residue\t$record{'DS'}{$residue}\n";
        }
      }

      $num_seq++;

    }
#print counts for debugging
    $logger->info("$count1 entries parsing, $count2 entries added to .dat file\n");

  }

  close DISULPHIDE;
  close ACT_METAL;
  close PFAMSEQ;
  close FASTA;
  close SEC_ACC;
 
  #Check all the files have size
  foreach my $file ( qw(pfamseq.dat disulphide.dat active_site_metal.dat secondary_acc.dat) ) {
    unless(-s $file) {
        $logger->logdie("$file did not get generated");
    }
  }
  chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");
  system("touch $status_dir/parsed_refprot") and $logger->logdie("Couldn't touch $status_dir/parsed_refprot:[$!]\n");

}


#Make DBSIZE_pfamseq_preAntifam file
if(-s "$pfamseq_dir/DBSIZE_pfamseq_preAntifam") {
  $logger->debug("Already made DBSIZE_pfamseq_preAntifam file\n");
}
else {
  $logger->debug("Making DBSIZE_pfamseq_preAntifam file\n");
  open(DBSIZE, ">$pfamseq_dir/DBSIZE_pfamseq_preAntifam") or $logger->logdie("Couldn't open filehandle to DBSIZE_pfamseq_preAntifam:[$!]\n");
  print DBSIZE "$num_seq";
  close DBSIZE;
}


$dbh = $pfamDB->getSchema->storage->dbh; #Do this again as connection will drop 

#Make tmp_pfamseq table in rdb
if(-e "$status_dir/created_tmp_pfamseq") {
  $logger->info("Already created table tmp_pfamseq\n");
}
else {
  $logger->info("Creating table tmp_pfamseq\n");

  $dbh->do("drop table if exists tmp_pfamseq");

  my $st = $dbh->prepare("create table tmp_pfamseq (\
    pfamseq_acc varchar(10) NOT NULL,\
    pfamseq_id varchar(16) NOT NULL,\
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
    auto_architecture bigint(11) unsigned DEFAULT NULL, \
    treefam_acc varchar(8) DEFAULT NULL, \
    swissprot tinyint(1) DEFAULT '0', \
    PRIMARY KEY  (pfamseq_acc),\
    KEY pfamseq_acc_version (pfamseq_acc,seq_version) ) ENGINE=InnoDB");

  $st->execute() or $logger->logdie("Failed to create table ".$st->errstr."\n");
  system("touch $status_dir/created_tmp_pfamseq") and $logger->logdie("Couldn't touch $status_dir/created_tmp_pfamseq:[$!]\n"); 

}


#Upload pfamseq data to temporary table
if ( -e "$status_dir/uploaded_pfamseq" ) {
  $logger->info("Already uploaded $cwd/$pfamseq_dir/pfamseq.dat to tmp_pfamseq\n");
}
else {
  $logger->info("Uploading $cwd/$pfamseq_dir/pfamseq.dat to tmp_pfamseq\n");

  my $sth_setup1 = $dbh->prepare('set foreign_key_checks=0;');
  $sth_setup1->execute() or $logger->logdie("Failed to set foreign_key_checks for LOAD DATA ".$sth_setup1->errstr."\n");

  my $sth_setup2 = $dbh->prepare('set unique_checks=0;');
  $sth_setup2->execute() or $logger->logdie("Failed to set unique_checks for LOAD DATA ".$sth_setup2->errstr."\n");

  my $sth_load = $dbh->prepare("LOAD DATA LOCAL INFILE '$cwd/$pfamseq_dir/pfamseq.dat' INTO TABLE tmp_pfamseq;");
  $sth_load->execute() or $logger->logdie("Failed to set up for LOAD DATA ".$sth_load->errstr."\n");

  # my $sth = $dbh->prepare( 'INSERT into tmp_pfamseq (pfamseq_acc, pfamseq_id, seq_version, crc64, md5, description, evidence, length, species, taxonomy, is_fragment, sequence, created, ncbi_taxid, auto_architecture, treefam_acc, swissprot) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'); 
  # _loadTable( $dbh, "$cwd/$pfamseq_dir/pfamseq.dat", $sth, 17 ); 
  
  system("touch $status_dir/uploaded_pfamseq")
    and $logger->logdie("Couldn't touch $status_dir/uploaded_pfamseq:[$!]\n");
}


#Delete obsolete data from pfamseq
if(-e "$status_dir/delete_pfamseq_obsolete") {
  $logger->info("Already deleted sequences in pfamseq that have been deleted from UniProtKB, or have changed in sequence version\n");
}
else {
  $logger->info("Deleting sequences in pfamseq that have been deleted from reference proteomes, or have changed in sequence version\n");
  my $delete_pfamseq = $dbh->prepare("delete pfamseq.* from pfamseq left join tmp_pfamseq on pfamseq.pfamseq_acc=tmp_pfamseq.pfamseq_acc and tmp_pfamseq.seq_version=pfamseq.seq_version and pfamseq.md5=tmp_pfamseq.md5 and pfamseq.crc64=tmp_pfamseq.crc64 where tmp_pfamseq.pfamseq_acc is null");
  $delete_pfamseq->execute() or $logger->logdie("Failed to delete rows from pfamseq table ".$delete_pfamseq->errstr."\n");
  system("touch $status_dir/delete_pfamseq_obsolete") and $logger->logdie("Couldn't touch $status_dir/delete_pfamseq_obsolete:[$!]\n"); 
}


#Update changed data to pfamseq ###added complete and reference proteome flags to update here ####
if(-e "$status_dir/update_pfamseq_changed") {
  $logger->info("Already updated pfamseq with changed data\n");
}
else {
  $logger->info("Updating pfamseq with changed data\n");
  my $changed_pfamseq = $dbh->prepare("update pfamseq, tmp_pfamseq set pfamseq.pfamseq_id = tmp_pfamseq.pfamseq_id, pfamseq.description = tmp_pfamseq.description, pfamseq.evidence = tmp_pfamseq.evidence,  pfamseq.species = tmp_pfamseq.species, pfamseq.taxonomy=tmp_pfamseq.taxonomy, pfamseq.is_fragment = tmp_pfamseq.is_fragment, pfamseq.updated = NOW(), pfamseq.ncbi_taxid = tmp_pfamseq.ncbi_taxid, pfamseq.auto_architecture=tmp_pfamseq.auto_architecture, pfamseq.treefam_acc=tmp_pfamseq.treefam_acc, pfamseq.swissprot = tmp_pfamseq.swissprot where tmp_pfamseq.pfamseq_acc=pfamseq.pfamseq_acc"); 
  $changed_pfamseq->execute() or $logger->logdie("Failed to update pfamseq with changed data ".$changed_pfamseq->errstr."\n");
  system("touch $status_dir/update_pfamseq_changed") and $logger->logdie("Couldn't touch $status_dir/update_pfamseq_changed:[$!]\n"); 
}


#Insert new data to pfamseq ####added complete and reference proteome flags here ####
if(-e "$status_dir/pfamseq_new") {
  $logger->info("Already updated pfamseq with new data\n");
}
else {
  $logger->info("Updating pfamseq with new data\n");
  my $pfamseq_new = $dbh->prepare("insert into pfamseq (pfamseq.pfamseq_id, pfamseq.pfamseq_acc, pfamseq.seq_version, pfamseq.crc64, pfamseq.md5, pfamseq.description, pfamseq.evidence, pfamseq.length, pfamseq.species, pfamseq.taxonomy, pfamseq.is_fragment, pfamseq.sequence, pfamseq.created, pfamseq.ncbi_taxid, pfamseq.auto_architecture, pfamseq.treefam_acc, pfamseq.swissprot) (select tmp_pfamseq.pfamseq_id, tmp_pfamseq.pfamseq_acc, tmp_pfamseq.seq_version, tmp_pfamseq.crc64, tmp_pfamseq.md5, tmp_pfamseq.description, tmp_pfamseq.evidence, tmp_pfamseq.length, tmp_pfamseq.species, tmp_pfamseq.taxonomy, tmp_pfamseq.is_fragment, tmp_pfamseq.sequence, tmp_pfamseq.created, tmp_pfamseq.ncbi_taxid, tmp_pfamseq.auto_architecture, tmp_pfamseq.treefam_acc, tmp_pfamseq.swissprot from tmp_pfamseq left join pfamseq on tmp_pfamseq.pfamseq_acc=pfamseq.pfamseq_acc and tmp_pfamseq.seq_version=pfamseq.seq_version where pfamseq.pfamseq_acc is null)");
  $pfamseq_new->execute() or $logger->logdie("Failed to update pfamseq with new data ".$pfamseq_new->errstr."\n");
  system("touch $status_dir/pfamseq_new") and $logger->logdie("Couldn't touch $status_dir/pfamseq_new:[$!]\n"); 
}

#Check pfamseq in rdb is the correct size;
my $dbsize;
if(-e "$status_dir/check_pfamseq_size") {
  open(DB, "$pfamseq_dir/DBSIZE_pfamseq_preAntifam") or $logger->logdie("Could not open $pfamseq_dir/DBSIZE_pfamseq_preAntifam:[$!]\n");
  while(<DB>){
    ($dbsize) = $_ =~/(\S+)/;
    last;
  }   
  close(DB);
  $logger->info("Already checked the number of sequences [$dbsize] in rdb is the same as in the pfamseq fasta file\n");
}
else {
  $logger->info("Checking the number of sequences in rdb is the same as in the pfamseq fasta file\n");
  open(DB, "$pfamseq_dir/DBSIZE_pfamseq_preAntifam") or $logger->logdie("Could not open $pfamseq_dir/DBSIZE_pfamseq_preAntifam:[$!]\n");
  while(<DB>){
    ($dbsize) = $_ =~/(\S+)/;
    last;
  }   
  close(DB);

  my $pfamseq_qc = $dbh->prepare("select count(*) from pfamseq");

  $pfamseq_qc->execute or $logger->logdie("Failed to query pfamseq for size of pfamseq ".$pfamseq_qc->errstr."\n");

  my $rdb_size = $pfamseq_qc->fetchrow;

  unless($rdb_size == $dbsize) {
    $logger->logdie("Mis-match between [$rdb_size] sequences in rdb, and $dbsize sequences in the pfamseq fasta file\n");
  }else{
    system("touch $status_dir/check_pfamseq_size") and $logger->logdie("Could not touch $status_dir/check_pfamseq_size:[$!]");
  }   
}

#Delete old active site and metal ion binding data
if(-e "$status_dir/delete_active_metal") {
  $logger->info("Already deleted old active site and metal ion binding data from pfamseq_markup\n");
}
else {
  $logger->info("Deleting old active site and metal ion binding data from pfamseq_markup\n");
  my $delete_act_metal = $dbh->prepare("delete from pfamseq_markup");
  $delete_act_metal->execute() or $logger->logdie("Failed to delete old data from pfamseq_markup ".$delete_act_metal->errstr."\n");
  system("touch $status_dir/delete_active_metal") and $logger->logdie("Couldn't touch $status_dir/delete_active_metal:[$!]\n"); 
}


#Upload new active site and metal ion binding data
if ( -e "$status_dir/upload_active_metal" ) {
  $logger->info(
    "Already uploaded $cwd/$pfamseq_dir/active_site_metal.dat to pfamseq_markup\n");
}
else {
  $logger->info("Uploading $cwd/$pfamseq_dir/active_site_metal.dat to pfamseq_markup\n");
  my $sth = $dbh->prepare('INSERT into pfamseq_markup VALUES (?,?,?,?,?)');
  _loadTable( $dbh, "$cwd/$pfamseq_dir/active_site_metal.dat", $sth, 5 ); 
  system("touch $status_dir/upload_active_metal")
    and $logger->logdie("Couldn't touch $status_dir/upload_active_metal:[$!]\n");
}

#Delete old disulphide bond data
if(-e "$status_dir/delete_disulphide") {
  $logger->info("Already deleted old disulphide bond data\n");
}
else {
  $logger->info("Deleting old disulphide bond data\n");
  my $delete_disulphide = $dbh->prepare("delete from pfamseq_disulphide");
  $delete_disulphide->execute() or $logger->logdie("Failed to delete old data from pfamseq_disulphide ".$delete_disulphide->errstr."\n");
  system("touch $status_dir/delete_disulphide") and $logger->logdie("Couldn't touch $status_dir/delete_disulphide:[$!]\n"); 
}


#Upload new disulphide bond data
if ( -e "$status_dir/upload_disulphide" ) {
  $logger->info("Already uploaded $cwd/$pfamseq_dir/disulphide.dat to pfamseq_disulphide\n");
}
else {
  $logger->info("Uploading $cwd/$pfamseq_dir/disulphide.dat to pfamseq_disulphide\n"); #changed logger message for mySQL / schema changes
  #There are 3 rows in the the pfamseq_disulphide
  my $sth = $dbh->prepare('INSERT into pfamseq_disulphide VALUES (?,?,?)');
  _loadTable( $dbh, "$cwd/$pfamseq_dir/disulphide.dat", $sth, 3 ); #changed as mySQL insert statement now uses disulphide.dat instead of disulphide_auto.dat
  system("touch $status_dir/upload_disulphide") and $logger->logdie("Couldn't touch $status_dir/upload_disulphide:[$!]\n"); 
}


#Delete old secondary accession data
if(-e "$status_dir/delete_secondary_acc") {
  $logger->info("Already deleted old secondary accession data\n");
}
else {
  $logger->info("Deleting old secondary accession data\n");
  my $delete_sec_acc = $dbh->prepare("delete from secondary_pfamseq_acc");
  $delete_sec_acc->execute() or $logger->logdie("Failed to delete old data from secondary_pfamseq_acc ".$delete_sec_acc->errstr."\n");
  system("touch $status_dir/delete_secondary_acc") and $logger->logdie("Couldn't touch $status_dir/delete_secondary_acc:[$!]\n"); 
}


#Upload new secondary accesion data
if(-e "$status_dir/upload_secondary_acc") {
  $logger->info("Already uploading $cwd/secondary_acc.dat to secondary_pfamseq_acc\n");
}
else {
  $logger->info("Uploading $cwd/$pfamseq_dir/secondary_acc.dat to secondary_pfamseq_acc\n"); #changed file name to reflect file used for mySQL insert for new db schema
  #There are 2 columsn in the the secondary_pfamseq_acc table
  my $sth = $dbh->prepare('INSERT INTO secondary_pfamseq_acc VALUES (?,?)');
  _loadTable($dbh, "$cwd/$pfamseq_dir/secondary_acc.dat", $sth, 2); #changed file uploaded from secondary_acc.auto.dat for mySQL insert statement for new schema
  system("touch $status_dir/upload_secondary_acc") and $logger->logdie("Couldn't touch $status_dir/upload_secondary_acc:[$!]\n");
}


#Delete old active site alignments
if(-e "$status_dir/delete_active_site_hmm_positions") {
  $logger->info("Already deleted old active site hmm positions\n");
}
else {
  $logger->info("Deleting old active site hmm positions\n");
  my $as_aln = $dbh->prepare("delete from _active_site_hmm_positions");
  $as_aln->execute() or $logger->logdie("Failed to delete old data from _active_site_hmm_positions");
  system("touch $status_dir/delete_active_site_hmm_positions") and $logger->logdie("Couldn't touch $status_dir/delete_active_site_hmm_positions:[$!]\n"); 
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
  my ($infile, $outfile, $auto_hash, $dbh) = @_;

  my $sth = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_acc = ?");

  open(INFILE, $infile) or $logger->logdie("Couldn't open file handle to $infile $!\n");

  open(OUTFILE, "> $outfile") or $logger->logdie("Couldn't open $outfile for writing $!\n");

  while(<INFILE>) {

    if(/^(\S+)\s+(.+)/) {
      my ($acc, $rest) = ($1, $2);

      unless(exists($$auto_hash{$acc})) {
        $sth->execute($acc);
        my $row = $sth->fetchrow_arrayref;
        if(!$row){
          $logger->logdie("This acc [$acc] is not in pfamseq!\n");
        }else{
          $auto_hash->{$acc}=$row->[0];
        }
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

sub _loadTable {
  my ( $dbh, $file, $sth, $cols ) = @_;

  my $batchsize = 5000;
  my $report    = 100000;
  my $reportNo  = 1000000;
  my $count     = 0;


  $dbh->begin_work;    # start a transaction

  open( my $input, '<', $file ) or die "Could not open $file:[$!]";

  print STDERR "\nProgress: ";
  while ( my $record = <$input> ) {
    chomp $record;
    my @values = split( /\t/, $record );
    for ( my $i = 0 ; $i < $cols ; $i++ ) {
      $values[$i] = undef if ( !defined($values[$i]) or $values[$i] eq '\N');
    }
    $sth->execute(@values);

    $count += 1;
    if ( $count % $batchsize == 0 ) {
      $dbh->commit;    # doublecheck the commit statement too
      if ( $count % $report == 0 ) {
        if ( $count % $reportNo == 0 ) {
          print STDERR "$count";
        }
        else {
          print STDERR ".";
        }
      }

      $dbh->begin_work;

    }
  }
  $dbh->commit;
  print STDERR "\n\n Uploaded $count records\n";
}


sub help {
  print STDERR << "EOF";

This script copies the current reference proteome set and updates
pfamlive with it.  A temporary table is first created in pfamlive, 
and the new sequence data uploaded to it.  The script deletes all 
obsolete data from pfamseq, and updates any changed sequences, 
and finally uploads new sequences to pfamseq.  

The script also updates the active site data, metal binding data and
disulphide bond data. The script generates a DBSIZE_pfamseq_preAntifam file
in the pfamseq_dir.

Usage:

  $0 -status_dir <status_dir> -pfamseq_dir <pfamseq_dir>

Both the status directory and pfamseq_directory must already exist.
pfamseq_dir is the directory where the uniprot data will be downloaded
to, and where the files to be uploaded to the database will be
generated. The status directory is where a log of the progress of 
the script is recorded.

EOF

  exit (0);

}
