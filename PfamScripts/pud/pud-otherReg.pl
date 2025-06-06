#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Cwd;
use IO::File;
use Net::SCP;
use Getopt::Long;
use File::Touch;
use Data::Printer;
use Data::Dumper;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Get the current working directory
my $pwd = getcwd;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my ( $statusdir, $pfamseqDir, $split );

&GetOptions(
  "statusdir=s"  => \$statusdir,
  "pfamseqdir=s" => \$pfamseqDir,
  "split=i"      => \$split
) or $logger->logdie("Invalid option!");

$split //= 1000;

#Get the connection to the pfam database.
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh    = $pfamDB->getSchema->storage->dbh;

unless ($pfamseqDir) {
  $logger->logdie("You need to supply the pfamseq directory name:[$!]");
}
unless ($statusdir) {
  $logger->logdie("You need to supply the status directory name:[$!]");
}
#-------------------------------------------------------------------------------
#Hash of sub refs that maps region data to a parsing sub routine.
my %subs = (
  ncoils  => \&parseNcoils,
  seg     => \&parseSeg,
  phobius => \&parsePhobius,
  iupred  => \&parseIupred
);


#-------------------------------------------------------------------------------
unless ( -d "$statusdir/otherReg" ) {
  mkdir("$statusdir/otherReg")
    or $logger->logdie("Could not make directory $statusdir/otherReg: [$!]");
}

#Make and change and into the otherReg dir in the lastest pfamseqdir
unless ( -d "$pfamseqDir/otherReg" ) {
  mkdir("$pfamseqDir/otherReg")
    or $logger->logdie("Could not make directory $pfamseqDir/otherReg: [$!]");
}

#-------------------------------------------------------------------------------
#Open and spit pfamseq into bits;
my $n = 0;
unless ( -e "$statusdir/otherReg/doneSplit" ) {
  my $dbsize;
  if(-e "$pfamseqDir/DBSIZE"){
    open(D, "<", "$pfamseqDir/DBSIZE") or $logger->logdie("Could not open $pfamseqDir/DBSIZE");
    while(<D>){
      if(/(\d+)/){
        $dbsize = $1;
        last;
      }
    }
    close(D);
  }else{
    my $rs = $pfamDB->getSchema->resultset('Pfamseq')->search({});
    $dbsize = $rs->count;
  }

  my $bit = int( ( $dbsize ) / $split );
  $bit++;

  $logger->debug(
    "Going to split pfamseq into $split files containing $bit sequences");

  open( B, ">$pfamseqDir/otherReg/pfamseq.$n" )
    or $logger->logdie("Could not open $pfamseqDir/otherReg/pfamseq.$n:[$!]\n");
  open( P, "$pfamseqDir/pfamseq" )
    or $logger->logdie("Could not open $pfamseqDir/pfamseq:[$!]\n");

  my $c;
  while (<P>) {
    if (/^>/) {
      $c++;
      if ( $c > $bit ) {
        close(B);
        $n++;
        $c = 0;
        open( B, ">$pfamseqDir/otherReg/pfamseq.$n" );
      }
    }
    print B $_;
  }
  close(B);
  close(P);
  $logger->info("Made $n files");
  touch("$statusdir/otherReg/doneSplit");
}else{
  my @files = glob("$pfamseqDir/otherReg/pfamseq.*");
  $n = scalar(@files); 
}
$logger->info("There are $n files to process!");

##-------------------------------------------------------------------------------
##Now submit the searches to the farm

unless ( -e "$statusdir/otherReg/doneFarm" ) {
  $logger->info("Submitting jobs to the farm");

  #submit jobs to farm
  my $fh = IO::File->new("> otherreg.sbatch");

  $fh->print( "#!/bin/bash
#SBATCH --job-name=otherreg
#SBATCH --output=$statusdir/otherReg/otherreg.%a.log
#SBATCH --error=$statusdir/otherReg/otherreg.%a.log
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --time=12:00:00
#SBATCH --array=0-999

");


  #Change into the directory containing the shattered pfamseq files
  $fh->print("cd $pfamseqDir/otherReg\n");

#To calculate these other regions
#segmasker (replaces seg)
#ncoils - This needs the environment variable COILSDIR to be set. Done via cshrc.pfam
#phobius
#iupred - This needs the environment variable IUPred_PATH to be set. Done via cshrc.pfam


  $fh->print( "
ncoils -c < pfamseq.\$SLURM_ARRAY_TASK_ID > ncoils.\$SLURM_ARRAY_TASK_ID
segmasker -in pfamseq.\$SLURM_ARRAY_TASK_ID -out seg.\$SLURM_ARRAY_TASK_ID
phobius.pl pfamseq.\$SLURM_ARRAY_TASK_ID > phobius.\$SLURM_ARRAY_TASK_ID
iupred_multifasta pfamseq.\$SLURM_ARRAY_TASK_ID long > iupred.\$SLURM_ARRAY_TASK_ID

echo \"\$SLURM_ARRAY_TASK_ID Successfully completed\"

");

  $fh->close;

  my $jobid;
  my $job_res = `sbatch otherreg.sbatch`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  unlink("otherreg.sbatch");

  #Touch file in log dir when regions jobs have finished
  system("sbatch --job-name=otherreg_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '/dev/null' -e '/dev/null' --wrap=\"touch $statusdir/otherReg/doneFarm\" ");

  #have jobs finished?
  until(-e "$statusdir/otherReg/doneFarm"){
    $logger->info("OtherReg farm jobs still running - checking again in 10 minutes\n");
    sleep(600);
  }
  $logger->info("OtherReg jobs have completed");

#-------------------------------------------------------------------------------


#Now we should have every thing back to be joined together and uploaded.
  $logger->info("Checking all of the files have been retrieved from the farm");
  my $error;
  for ( my $i = 0 ; $i < $n ; $i++ ) {
    foreach my $f (keys %subs) {
      unless ( -s "$pfamseqDir/otherReg/$f.$i" ) {
        $logger->warn("$pfamseqDir/otherReg/$f.$i is missing");
        $error++;
      }
    }
  }

  if ($error) {
    unlink("$statusdir/otherReg/doneFarmCheck");
    $logger->logdie('Some of the files are other region files are missing')
  }
}

#-------------------------------------------------------------------------------
#Join and upload

my $orDir = "$pfamseqDir/otherReg";


#-------------------------------------------------------------------------------
#Now parse the data and write to the file
if(-s "$orDir/allOtherReg.dat"){
  $logger->info("Already made database upload file.");
}else{
  my $fhOut;
  open( $fhOut, ">$orDir/allOtherReg.dat" )
    or $logger->logdie("Could not open allOtherReg.dat");

  for ( my $m = 0 ; $m < $n ; $m++ ) {
#    my $pfamseq = getAutos($m, $orDir, $dbh);
    foreach my $f (keys %subs) {
      $logger->info("Parsing $f.$m");
      $subs{$f}( $orDir, "$f.$m", $fhOut );
    }
  }
  close $fhOut;
}


#-------------------------------------------------------------------------------
#Copy file to the mysql instance so that it can be uploaded
#

$pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
$dbh    = $pfamDB->getSchema->storage->dbh;


if(-e "$statusdir/otherReg/doneUpload"){
  $logger->info("Already uploaded the database");
}else{
  #Now load the file....
  $logger->info('preparing to upload the file');
  $dbh->do("delete from other_reg");

  my $sthInsert = $dbh->prepare("INSERT INTO other_reg (pfamseq_acc, 
    seq_start, 
    seq_end, 
    type_id, 
    source_id, 
    score, 
    orientation) VALUES ( ?,?,?,?,?,?,?)"); #mySQL statement updated due to db schema change - replaced auto_pfamseq with pfamseq_acc

  _loadTable($dbh, "$orDir/allOtherReg.dat" , $sthInsert, 7);

  system("touch $statusdir/otherReg/doneUpload");

}

#-------------------------------------------------------------------------------
#Subroutines that parse each file type......
#

#getAutos *****no longer used*****
sub getAutos {
  my ($m, $orDir, $dbh) = @_;  
  my $sth = $dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc=?") or
  $logger->logdie("Error preparing statement:". $dbh->errstr); #updated mySQL statement for db schema change - I assume this should still be used as a sanity check

  my $pfamseq = {};
  open(P, "<", $orDir."/pfamseq.".$m) or $logger->logdie("Failed to open $orDir/pfamseq.$m :[$!]");
  while(<P>){
    if(/^>(\S+)\.\d+/){
      $sth->execute($1);
      my $row = $sth->fetchrow_arrayref;
      $pfamseq->{$1} = $row->[0];  
    }
  }
  close(P);
  return($pfamseq); 
}


sub parseNcoils {
  my ($dir, $file, $fh) = @_;
  # print STDERR "parseNcoils: $dir/$file\n";

  $/ = "\n>";
  open( COILS, "$dir/$file" ) or $logger->logdie("Could not open $dir/$file:[$!]\n");
  while (<COILS>) {
    chomp;
    my @entry = split( /\n/, $_ );
    if (@entry) {
      my $acc = $1 if ( $entry[0] =~ /^>?(\S{6,10})\.\d+/ );
      if ( !$acc ) {
        warn "Could not find id or entry for $_\n";
      }
      else {
        foreach my $region ( @entry[ 1 .. $#entry ] ) {
          if ($region =~ /(\d+)\s+(\d+)/ ) {
            my $start = $1;
            my $end = $2;
            print $fh "\\N\t$acc\t$start\t$end\tcoiled_coil\tncoils\t\\N\t\\N\n";
          }
        }
      }
    }
  }
  $/ = "\n";
}

sub parseSeg {
  my ( $dir, $file, $fh ) = @_;
#split on \n>
  $/ = "\n>";
  open ( SEG, "$dir/$file" ) or $logger->logdie("Could not open $dir/$file:[$!]\n");
  while (<SEG>) {
    my @entry = split( /\n/, $_ );
#parse out acc
    my $acc;
    if ($entry[0] =~ /(\w{6,10})\.\d+/){
      $acc = $1;
    } else {
      print "parseSeg: can't parse acc\n"
    }
    foreach my $line (@entry){
      if ($line =~/\w{6,10}\.\d+/){
        next;
      }
      if ($line =~/^(\d+)\s+-\s+(\d+)/){
        my $start = $1;
        my $end = $2;
        $start++;  #segmasker results are 0 indexed so add one to start and end
        $end++; 
        print $fh "\\N\t$acc\t$start\t$end\tlow_complexity\tsegmasker\t\\N\t\\N\n";
      } 

    }

  }
  close (SEG);
}

sub parsePhobius {
  my ( $dir, $phobius_file, $fh ) = @_;
#split on //\n
  $/ = "//\n";
  open( PHOB, "$dir/$phobius_file" ) or $logger->logdie("Could not open $dir/$phobius_file:[$!]\n");
  while (<PHOB>){
    my @entry = split( /\n/, $_ );
#parse out acc
    my $acc;
    if ($entry[0] =~ /ID\s+(\w{6,10})\.\d+/){
      $acc = $1;
    } else {
      print "parsePhobius: can't parse acc\n"
    }
    foreach my $line (@entry){
      if ($line =~/ID\s+\w{6,10}\.\d+/){
        next;
      }
      if ($line =~ /^FT\s+SIGNAL\s+(\d+)\s+(\d+)/){
        my $start = $1;
        my $end   = $2;
        print $fh "\\N\t" . $acc . "\t$start\t$end\tsig_p\tPhobius\t\\N\t\\N\n";
      } elsif ($line =~ /^FT\s+TRANSMEM\s+(\d+)\s+(\d+)/){
        my $start = $1;
        my $end   = $2;
        print $fh "\\N\t" . $acc . "\t$start\t$end\ttransmembrane\tPhobius\t\\N\t\\N\n";
      }
    }

  }
  close (PHOB);
}

sub parseIupred {
  my ( $dir, $iupred_file, $fh, $pfamseq_auto ) = @_;
#split on // and new line
  $/ = "//\n";
  open( DIS, "<", "$dir/$iupred_file" )  or $logger->logdie("Failed to open $dir/$iupred_file for reading:[$!]");
  while (<DIS>) {
    my @entry_c = split( /\n/, $_ );
#get rid of comments
    my @entry = grep {!/^#/} @entry_c;
    my $acc;
    if ($entry[0] =~ /ID\s+(\w{6,10})\.\d+/){
      $acc = $1;
    } else {
      print "parseIupred: can't parse acc\n";
    }
    foreach my $line (@entry){
      if ($line =~/ID\s+\w{6,10}\.\d+/){
        next;
      }
      if ($line =~ /^FT\s+IUPred\s+(\d+)\s+(\d+)/){
        my $start = $1;
        my $end = $2;
        print $fh "\\N\t" . $acc . "\t$start\t$end\tdisorder\tIUPred\t\\N\t\\N\n";
      }
    }

  }

  close (DIS);
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
    shift(@values); #Take the first element off.
    for ( my $i = 0; $i < $cols ; $i++ ) {
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
