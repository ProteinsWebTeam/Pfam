#!/usr/bin/env perl

#make architectures - part 3.
#To run after all farm jobs from make_Architecture_new_part2.pl  are finished
#concatenate top scoring arch files and sort a final time
#upload type examples
#options - -fileonly only creates and sorts files
#          -uploadonly only uploads data (files already created)
#Default - both creates files and uploads.

use strict;
use warnings;
use DDP;
use Log::Log4perl qw(:easy);
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use File::Slurp;
use String::CRC32;
use Cwd;
use Getopt::Long;

my ($file, $upload);
&GetOptions(
    'fileonly' => \$file,
    'uploadonly' => \$upload,
);

if ($file && $upload){
    die "-fileonly and -uploadonly options are incompatible\n";
}



#set up db stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#get the logger

Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $dir = getcwd;

my $uploadfile = "TYPE_EX";

#files section - only if no $upload
unless ($upload){
    $logger->debug("Concatenating files");

#create list of files
    my @filesall=read_dir($dir);
    my @files;
    foreach my $file (@filesall){
      if ( $file =~ /ARCH_TOPSCORE_\d+/ ){
           push ( @files, $file );
     }
    }

#concatenate
    my $file_list = join (' ', @files);
    system ( "cat $file_list > TOPARCH_ALL" ) and $logger->logdie("Can't concatenate files");

#sort
    $logger->debug("Sorting file");
    my $sorted = "TOPARCH_ALL_SORTED";
    system ( "sort -nrk1 TOPARCH_ALL > $sorted" ) and $logger->logdie("Can't sort file");

#get top score
    my %seen;
    my @sorted_data = read_file($sorted);
    open (UPLOAD, ">$uploadfile") or $logger->logdie("Can't open file to write");

    foreach my $line (@sorted_data){
        $line =~ /^(\d+)\t+\d+\t\d+\t.*\t.*\t(\w+)/;
        my $md5 = $1;
        my $seq = $2;
        unless ($seen{$md5}){
            print UPLOAD "$seq\t$md5\n";
            $seen{$md5}=1;
        }
    }
    close(UPLOAD);
} #end of files section

#uploads section - only if no $file

unless ($file){
#TODO upload type example to db
    $logger->debug("Uploading type examples");
    my $stt = $dbh->prepare("UPDATE architecture set type_example = ? where auto_architecture = ?") or $logger->logdie("Cannot prepare statement");
    _loadTable( $dbh, $uploadfile, $stt, 2 );

#populate no_seqs in architecture and number_archs in pfamA
    $logger->debug("Calcuating no_seqs for each architecture");
    my $sts = $dbh->prepare("UPDATE architecture a SET no_seqs = (select count(*) from pfamseq s where s.auto_architecture=a.auto_architecture)") or $logger->logdie("Cannot prepare statement");
    $sts->execute();

    $logger->debug("Finding auto_architectures for each pfamA");
    my $host = $pfamDB->{host};
    my $user = $pfamDB->{user};
    my $pass = $pfamDB->{password};
    my $port = $pfamDB->{port};
    my $db = $pfamDB->{database};
    my $archfile = "PFAMA_ARCH_DUMP";
    my $cmd = "mysql -h $host -u $user -p$pass -P $port $db --skip-column-names --quick -e \"SELECT DISTINCT r.pfamA_acc, auto_architecture FROM pfamA_reg_full_significant r, pfamseq s WHERE s.pfamseq_acc=r.pfamseq_acc AND in_full=1 \" > $archfile";
    system($cmd) and die "Could not obtain auto_architectures from database";
    $logger->debug("Uploading to pfamA_architecture");

    my $sta = $dbh->prepare("INSERT into PfamA_architecture (pfamA_acc, auto_architecture) VALUES (?, ?)") or $logger->logdie("Cannot prepare statement");
    _loadTable( $dbh, $archfile, $sta, 2 );


    $logger->debug("Calcuating number_archs for each Pfam");
    my $stn = $dbh->prepare("UPDATE pfamA a set number_archs=(select count(*) from pfamA_architecture p where p.pfamA_acc=a.pfamA_acc)") or $logger->logdie("Cannot prepare statement");
    $stn->execute();

} #end of uploads section


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

