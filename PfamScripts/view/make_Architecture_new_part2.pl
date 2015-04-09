#!/usr/bin/env perl

#make architectures - part 2.
#loops through all seqs in a chunk and calculates architectures
#options - -fileonly only creates and sorts files
#          -uploadonly only uploads data (files already created)
#Default - both creates files and uploads.
#Triggered as farm jobs by make_Architecture_new_part1.pl  
#Run make_Architecture_new_part3.pl after this.        

use strict;
use warnings;
use DDP;
use Log::Log4perl qw(:easy);
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use File::Slurp;
use String::CRC32;
use Getopt::Long;

#set up db stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#get the logger

Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my ($chunk, $file, $upload);
&GetOptions(
    'chunk=i' => \$chunk,
    'fileonly' => \$file,
    'uploadonly' => \$upload,
);

if ($file && $upload){
    die "-fileonly and -uploadonly options are incompatible\n";
}

#file names will be chunk -1 as they start at 0
$chunk--;

if ($chunk < 10){
    $chunk = 0 . $chunk;
}

$logger->debug("Working on chunk $chunk");

my $pfamseq_upload = "PFAMSEQ_UPLOAD_" . $chunk;
my $outfile = "ARCH_SCORE_" . $chunk;

#section for file only
unless ($upload){

#get pfamA, hmm_length and put in hash
    $logger->debug("Fetching PfamA accessions and HMM lengths");
    my %pfamA_hmm;
    my $st1 = $dbh->prepare("select pfamA_acc, model_length from pfamA") or $logger->logdie("Cannot prepare statement");
    $st1->execute;
    my $arrayref1=$st1->fetchall_arrayref();
    foreach my $row1 (@$arrayref1){
        $pfamA_hmm{$row1->[0]}=$row1->[1];
    }

    my $file = 'pfamseq_dump_' . $chunk;

#for each chunk:
    open (OUTFILE, ">$outfile") or $logger->logdie("Cannot open file to write $!");
    open (PFAMSEQ_UPLOAD, ">$pfamseq_upload") or $logger->logdie("Cannot open file to write $!");
    $logger->debug("Working on file $file");
    my @lines = read_file($file);

    foreach my $line (@lines){
        my @data = split(/\s+/, $line);
        my $seq = $data[0];
        #skip header
        if ($seq eq 'pfamseq_acc'){
            next;
        }
        #change is fragment to is complete for purpose of sorting
        my $is_complete;
        if ($data[1]==0){
            $is_complete = 1;
        } elsif ($data[1]==1){
            $is_complete = 0;
        }

        my($md5, $archAcc, $archId, @archStringAcc, @archStringId, @model_start_end);
        my $domOrder=1;
        my $score = 0;
        
#   get pfam domains 
        my $pfamaRegionsRef = $pfamDB->getPfamRegionsForSeq( $seq );

#   if no domains = NULL architecture - DON'T REALLY NEED TO PRINT THIS TO FILE
        if (!$pfamaRegionsRef){
            $archAcc="NULL";
        }  else { 
#   if domains exist:
#   sort by start position
            foreach my $region ( sort { $a->seq_start <=> $b->seq_start } @$pfamaRegionsRef )
                {
                 $region->update( { domain_order => $domOrder } );
                 push( @archStringAcc, $region->pfama_acc->pfama_acc );
                 push( @archStringId,  $region->pfama_acc->pfama_id );
                 my $model_s_e = $region->model_start . "_" . $region->model_end;
                 push( @model_start_end, $model_s_e );
                 $domOrder++;
                 }
            $archAcc = join( ' ', @archStringAcc );
            $archId = join('~', @archStringId);
            $md5 = crc32($archAcc);

#   score the architecture 
            my $count = @archStringAcc;
            for (my $i=0; $i<$count; $i++){
                my $pfama = $archStringAcc[$i];
                my $model_length = $pfamA_hmm{$pfama};
                my ($start, $end)=split('_', $model_start_end[$i]);
                if ($start == 1){
                    $score++;
                }
                if ($end >= $model_length){
                    $score++;
                }

            } #end of scoring

#   write to file ARCH_SCORE: MD5 of accn string, score, is_complete, accn_string, id_string, pfamseq_acc,
            print OUTFILE "$md5\t$score\t$is_complete\t$archAcc\t$archId\t$seq\n";
            print PFAMSEQ_UPLOAD "$md5\t$seq\n";
   
        } #end of if/else for seqs with archs that exist
    } #end of loop through each line/sequence in the file   

close (OUTFILE);
close (PFAMSEQ_UPLOAD);

} #end of first file only section

#section for upload only
unless ($file){
#use _loadTable to update pfamseq
    $logger->debug("Loading auto_architectures into pfamseq table");
    my $stp = $dbh->prepare("UPDATE pfamseq SET auto_architecture = ? WHERE pfamseq_acc = ?") or $logger->logdie("Cannot prepare statement");
    _loadTable( $dbh, $pfamseq_upload, $stp, 2 ); 

}#end of first upload only section

my $topscores_upload = "ARCH_TOPSCORES_upload_" . $chunk;

#second file only section
unless ($upload){
    $logger->debug("Sorting $outfile");
#TODO file names
    my $sorted_file = "ARCH_SCORE_sorted_" . $chunk;
    system ("sort -nrk1 $outfile > $sorted_file") and $logger->logdie("Could not sort file $!");

#   read/write top score per md5 - we want the top line per md5 and can ignore the NULLs
    $logger->debug("Finding top scores for each architecture in $sorted_file");

#TODO file names - next 2 lines
    my $topscore_file = "ARCH_TOPSCORE_" . $chunk;

    open (TOP, ">$topscore_file") or $logger->logdie("Can't open file to write $!");
    open (TOP_UPLOAD, ">$topscores_upload") or $logger->logdie("Can't open file to write $!");

    my @archdata = read_file($sorted_file);
    my %md5_seen;
    foreach my $line (@archdata){
    #the ones which are NULL start with characters so regex below won't catch
        $line =~ /^(\d+)\t+\d+\t\d+\t(.*)\t(.*)\t\w+/;
        my $md5 = $1;
        my $accs = $2;
        my $ids = $3;
        unless ($md5_seen{$md5}){
            print TOP $line;
            print TOP_UPLOAD "$md5\t$ids\t$accs\n";
            $md5_seen{$md5}=1;      
        }
    }
    close (TOP);
    close (TOP_UPLOAD);

} #end of second file only section

#second upload only section
unless ($file){
#use _loadTable to upload architectures

    my $sta = $dbh->prepare("INSERT into architecture (auto_architecture, architecture, architecture_acc) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE architecture = values(architecture), architecture_acc = values(architecture_acc);") or $logger->logdie("Can't prepare statement");

    eval{
        _loadTable( $dbh, $topscores_upload, $sta, 3 ); 
    };

    if ($@){
        $logger->warn(Problem loading file $topscores_upload into the architecture table);
    }

}#end of second upload only section


###########################################
#_loadTable - taken from pud scripts but with batch size increased 
sub _loadTable {
  my ( $dbh, $file, $sth, $cols ) = @_;

  my $batchsize = 10000;
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
