#! /usr/bin/env perl 

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Config::General qw(SaveConfig);
use Cwd;
use File::Copy;
use Getopt::Long;
use Digest::MD5 qw(md5_hex);
use LSF::Job;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;

use Smart::Comments;

my($status_dir, $pfamseq_dir);

&GetOptions( "status_dir=s"  => \$status_dir, 
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
if(-s "$pfamseq_dir/reldate.txt") {
  $logger->debug("Already downloaded reldate.txt\n");
}
else {
  $logger->debug("Copying reldate.txt from uniprot $uniprot_location\n");
  # copy("$uniprot_location/reldate.txt", "$pfamseq_dir/reldate.txt") or $logger->logdie("Could not copy $uniprot_location/reldate.txt to $pfamseq_dir [$!]\n");

  my $cp_rel = LSF::Job->submit(-q => "datamover", -o => "/dev/null", -J => "reldate_uniprot", "cp $uniprot_location/reldate.txt $pfamseq_dir/reldate.txt");

  $logger->info("Waiting for reldate.txt to be copied from uniprot reldate.txt");
  until(-s "$pfamseq_dir/reldate.txt") {
    sleep 30;
  }
}

#Update swiss prot and tremble version in the db
if(-e "$status_dir/updated_uniprot_version") {
  $logger->debug("Already updated rdb with swiss prot and trembl version\n");
}
else {
  $logger->debug("Updating rdb with swiss prot and trembl version\n");
  my ($swiss_prot_rel, $trembl_rel);
  open(REL, "$pfamseq_dir/reldate.txt") or  $logger->logdie("Couldn't open $pfamseq_dir/reldate.txt");
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
    $logger->logdie("Faied to get release information for reldate.txt.");  
  }
  my $st_version = $dbh->prepare("update version set swiss_prot_version = \"$swiss_prot_rel\"");
  $st_version->execute() or $logger->logdie("Failed to update version table with swiss prot version ". $st_version->errstr."\n");
  my $st_version2 = $dbh->prepare("update version set trembl_version = \"$trembl_rel\"");
  $st_version2->execute() or $logger->logdie("Failed to update version table with trembl version ". $st_version2->errstr."\n");

  system("touch $status_dir/updated_uniprot_version") and $logger->logdie("Couldn't touch $status_dir/updated_version:[$!]\n");
}

chdir($pfamseq_dir) or $logger->logdie("Couldn't change directory into $pfamseq_dir $!\n");

#Get copy of latest uniprot
my @files = qw(uniprot_sprot.dat.gz uniprot_trembl.dat.gz);

foreach my $file (@files) {
  if(-s $file) {
    $logger->debug("Already copied $file");
  }
  else {
    $logger->debug("Copying $uniprot_location/$file\n");
    # copy("$uniprot_location/$file.gz", "$pfamseq_dir/$file.gz") or $logger->logdie("Could not copy $uniprot_location/$file.gz to $pfamseq_dir [$!]\n");
    # $logger->logdie("Couldn't copy $file.gz from $uniprot_location:[$!]\n") unless(-s "$pfamseq_dir/$file.gz");

    my $cp_data = LSF::Job->submit(-q => "datamover", -o => "/dev/null", -J => 'cp_uniprot', "cp $uniprot_location/$file $file");
    $logger->debug("bsub cmd: cp $uniprot_location/$file $file");
    ### $cp_data
    my $cp_data2 = LSF::Job->submit(-q => "production", -o => "/dev/null", -w => "done($cp_data)", -J => 'cp_uniprot_done', "touch cp_${file}_done");

    $logger->debug("Waiting for copy job to complete...");
    until(-e "cp_${file}_done") {
      sleep 60;
    }
    $logger->debug("Completed copying $file");
    unlink("cp_${file}_done");
  }
}

#$logger->logdie("Check that NcbiTaxonomy has been populated, then remove this line!");
#Parse files
my $num_seq;
chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");
if(-e "$status_dir/parsed_uniprotkb") { 
  $logger->debug("Already parsed UniProtKB\n");
} 
else {
  #Get taxomony data
  my @result = $pfamDB->getSchema->resultset('NcbiTaxonomy')->search({});

  my %rdb_taxonomy;
  foreach my $row (@result) {
    $rdb_taxonomy{$row->ncbi_taxid}=1;
  }
  open(UNIPROT, ">$pfamseq_dir/uniprot.dat") or  $logger->logdie("Failed to open filehandle to $pfamseq_dir/uniprot.dat:[$!]");
  open(FASTA, ">$pfamseq_dir/uniprot.fasta") or $logger->logdie("Failed to open filehandle to $pfamseq_dir/uniprot.fasta:[$!]");
  foreach my $file (@files) { 

    $file =~ s/.gz$//;
    $logger->debug("Parsing $file\n");

    $/= "//\n";
    open(FH, "gunzip -c $pfamseq_dir/$file.gz |") or $logger->logdie("Failed to gunzip $pfamseq_dir/$file.gz:[$!]\n");

    #adding counts for debugging
    my $count1=0;
    my $count2=0;
    while(<FH>) {

      my @entry = split(/\n/, $_);

      #count for debugging
      $count1++;
      my %record;
      foreach my $line (@entry) {
        if( $line =~ /^AC\s+(\S+);(.+)?/ and !$record{'AC'}) {
          $record{'AC'} = $1 
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
        elsif( $line =~ /^OX\s+NCBI_TaxID=(\d+)/){
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

      my $is_frag;
      if($record{'DE_FLAG'}) {
        $is_frag = 1;
      } 
      else {
        $is_frag = 0;
      }

      my $complete;
      my $reference;
      if($record{'Reference_proteome'}) {
        $reference = 1;
      } 
      else {
        $reference = 0;
      }

      if($record{'Complete_proteome'}) { #This isn't uploaded to db, but keeping it here in case we want to in future
        $complete = 1;
      } 
      else {
        $complete = 0;

      }
      print UNIPROT "$record{'AC'}\t$record{'ID'}\t$record{'SEQ_VER'}\t$record{'CRC64'}\t$record{'MD5'}\t$description\t$record{'PE'}\t$record{'SEQ_LEN'}\t$record{'OS'}\t$record{'OC'}\t$is_frag\t$record{'SEQ'}\t\\N\t$record{'NCBI_TAX'}\t$reference\t$complete\t\\N\t0\t0\t0\t0\n";
      print FASTA ">$record{'AC'}.$record{'SEQ_VER'} $record{'ID'} $description\n$record{'SEQ'}\n";
      
      #count for debugging
      $count2++;

      $num_seq++;

    }
    #print counts for debugging
    $logger->info("$count1 entries parsing, $count2 entries added to .dat file\n");

  }

  close UNIPROT; 
  close FASTA;

  system("touch $status_dir/parsed_uniprotkb") and $logger->logdie("Couldn't touch $status_dir/parsed_uniprotkb:[$!]\n");

}

#Make DBSIZE_uniprot_preAntifam file
if(-s "$pfamseq_dir/DBSIZE_uniprot_preAntifam") {
  $logger->debug("Already made DBSIZE_uniprot_preAntifam file\n");
}
else {
  $logger->debug("Making DBSIZE_uniprot_preAntifam file\n");
  open(DBSIZE, ">$pfamseq_dir/DBSIZE_uniprot_preAntifam") or $logger->logdie("Couldn't open filehandle to DBSIZE_uniprot_preAntifam:[$!]\n");
  print DBSIZE "$num_seq";
  close DBSIZE;
}

$dbh = $pfamDB->getSchema->storage->dbh; #Do this again as connection will drop 

#Upload to uniprot table
if ( -e "$status_dir/uploaded_uniprot" ) { 
  $logger->info("Already uploaded $pfamseq_dir/uniprot.dat to database\n");
}
else {

  #Delete old uniprot data
  $logger->info("Deleting old data from uniprot table");
  my $sth_delete = $dbh->prepare("delete from uniprot");
  $sth_delete->execute() or $logger->logdie("Failed to delete old data from uniprot ".$sth_delete->errstr."\n");


  $logger->info("Uploading $pfamseq_dir/uniprot.dat to database\n");
  my $sth = $dbh->prepare('INSERT into uniprot (uniprot_acc, uniprot_id, seq_version, crc64, md5, description, evidence, length, species, taxonomy, is_fragment, sequence, created, ncbi_taxid, ref_proteome, complete_proteome, treefam_acc, rp15, rp35, rp55, rp75) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)');
  _loadTable( $dbh, "$pfamseq_dir/uniprot.dat", $sth, 21 );  
  
  system("touch $status_dir/uploaded_uniprot")
    and $logger->logdie("Couldn't touch $status_dir/uploaded_uniprot:[$!]\n");
}


#Check uniprot in rdb is the correct size;
my $dbsize;
if(-e "$status_dir/check_uniprot_size") {
  open(DB, "$pfamseq_dir/DBSIZE_uniprot_preAntifam") or $logger->logdie("Could not open $pfamseq_dir/DBSIZE_uniprot_preAntifam:[$!]\n");
  while(<DB>){
    ($dbsize) = $_ =~/(\S+)/;
    last;
  }   
  close(DB);
  $logger->info("Already checked the number of sequences [$dbsize] in rdb is the same as in the DBSIZE_uniprot_preAntifam file\n");
}
else {
  $logger->info("Checking the number of sequences in rdb is the same as in the DBSIZE_uniprot_preAntifam file\n");
  open(DB, "$pfamseq_dir/DBSIZE_uniprot_preAntifam") or $logger->logdie("Could not open $pfamseq_dir/DBSIZE_uniprot_preAntifam:[$!]\n");
  while(<DB>){
    ($dbsize) = $_ =~/(\S+)/;
    last;
  }   
  close(DB);

  my $pfamseq_qc = $dbh->prepare("select count(*) from uniprot");

  $pfamseq_qc->execute or $logger->logdie("Failed to query uniprot table for size of uniprot ".$pfamseq_qc->errstr."\n");

  my $rdb_size = $pfamseq_qc->fetchrow;

  unless($rdb_size == $dbsize) {
    $logger->logdie("Mis-match between [$rdb_size] sequences in rdb, and $dbsize sequences in the uniprot fasta file\n");
  }else{
    system("touch $status_dir/check_uniprot_size") and $logger->logdie("Could not touch $status_dir/check_uniprot_size:[$!]");
  }   
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

This script copies the current UniProt set and updates
pfamlive with it.  

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
