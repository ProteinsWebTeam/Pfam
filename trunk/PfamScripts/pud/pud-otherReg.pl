#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Cwd;
use IO::File;
use Sys::Hostname;
use Data::UUID;
use Net::SCP;
use Getopt::Long;
use File::Touch;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Get the current working directory
my $pwd = getcwd;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my ( $statusdir, $pfamseqDir, $split );
$split = 500;

&GetOptions(
  "statusdir=s"  => \$statusdir,
  "pfamseqdir=s" => \$pfamseqDir,
  "split=i"      => \$split
  ) or $logger->logdie("Invalid option!");

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
chdir("$pfamseqDir/otherReg")
  or
  $logger->logdie("Could not change into directory $pfamseqDir/otherReg: [$!]");

#-------------------------------------------------------------------------------
#Open and spit pfamseq into bits;
my $n = 1;
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

  open( B, ">pfamseq.$n" )
    or $logger->logdie("Could not open pfamseq.$n:[$!]\n");
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
        open( B, ">pfamseq.$n" );
      }
    }
    print B $_;
  }
  close(B);
  close(P);
  $logger->info("Made $n files");
  touch("$statusdir/otherReg/doneSplit");
}else{
  my @files = glob("pfamseq.*");
  $n = scalar(@files); 
}
$logger->info("There are $n files to process!");

##-------------------------------------------------------------------------------
##Now submit the searches to the farm
my $farmConfig = $config->farm;
my $phost      = hostname;
my $ug         = new Data::UUID;
my $uuid       = $ug->to_string( $ug->create() );
my $fh         = IO::File->new();
my $user       = $ENV{USER};

unless ( -e "$statusdir/otherReg/doneFarm" ) {
  $logger->info("Submitting jobs to the farm");

  #Set up the job and copy the files over;
  $fh->open( "| bsub -q "
      . $farmConfig->{lsf}->{queue}
      . " -R \"select[mem>4000] rusage[mem=4000]\" -M 4000000 -o $statusdir/otherReg/$uuid.log  -JotherRegs\"[1-$n]\" " );
 
  #Change into the directory containing the shattered pfamseq files
  $fh->print("cd $pfamseqDir/otherReg\n");

#To calculate these other regions
#seg
#ncoils - This needs the environment variable COILSDIR to be set. Done via cshrc.pfam
#phobius
#iupred - This needs the environment variable IUPred_PATH to be set. Done via cshrc.pfam
 
  $fh->print(
    "ncoils -f < pfamseq.\$\{LSB_JOBINDEX\} > ncoils.\$\{LSB_JOBINDEX\}\n");
  $fh->print("seg pfamseq.\$\{LSB_JOBINDEX\} -l > seg.\$\{LSB_JOBINDEX\}\n");
  $fh->print(
    "phobius.pl pfamseq.\$\{LSB_JOBINDEX\} > phobius.\$\{LSB_JOBINDEX\}\n");
  $fh->print(
"iupred_multifasta pfamseq.\$\{LSB_JOBINDEX\} long > iupred.\$\{LSB_JOBINDEX\}\n"
  );
  $fh->close;

  sleep(60);    #Give them change to get on to the farm queue!
  system("touch $statusdir/otherReg/doneFarm");
}

#-------------------------------------------------------------------------------
##Have all of the jobs finished
if(-e "$statusdir/otherReg/doneFarmCheck"){
  $logger->info("Already checked the all jobs have completed successfully");
}else{
$logger->info("Waiting for jobs to finish on the farm");
my $finished = 0;
while ( !$finished ) {
  open( FH, "bjobs -JotherRegs|" );
  my $jobnum;
  while (<FH>) {
    if (/^\d+/) {
      $jobnum++;
    }
  }
  close FH;

  if ($jobnum) {
    $logger->info(
      "Will not continue until your $jobnum outstanding jobs have completed."
        . "Will check again in ten minutes" );
    sleep(600);
  }
  else {
    $finished = 1;
  }
}


#-------------------------------------------------------------------------------


#Now we should have every thing back to be joined together and uploaded.
$logger->info("Checking all of the files have been retieved from the farm");
my $error;
for ( my $i = 1 ; $i <= $n ; $i++ ) {
  foreach my $f (keys %subs) {
    unless ( -s "$pfamseqDir/otherReg/$f.$i" ) {
      $logger->warn("$pfamseqDir/otherReg/$f.$i is missing");
      $error++;
    }
  }
}

  $logger->logdie('Some of the files are other region files are missing')
    if ($error);
  
  system("touch $statusdir/otherReg/doneFarmCheck");
}
#-------------------------------------------------------------------------------
#Join and upload

my $orDir = "$pfamseqDir/otherReg";

#-------------------------------------------------------------------------------
#Now parse the data and write to the file
if(-e "$orDir/allOtherReg.dat"){
  $logger->info("Already made database upload file.");
}else{
  my $fhOut;
  open( $fhOut, ">$orDir/allOtherReg.dat" )
    or $logger->logdie("Could not open allOtherReg.dat");

  for ( my $m = 1 ; $m <= $n ; $m++ ) {
    my $pfamseq = getAutos($m, $orDir, $dbh);
  
    foreach my $f (keys %subs) {
      $logger->info("Parsing $f.$m");
      $subs{$f}( $orDir, "$f.$m", $fhOut, $pfamseq );
    }
  }
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

  my $sthInsert = $dbh->prepare("INSERT INTO other_reg (auto_pfamseq, 
                                                seq_start, 
                                                seq_end, 
                                                type_id, 
                                                source_id, 
                                                source, 
                                                orientation) VALUES ( ?,?,?,?,?,?,?)");

  _loadTable($dbh, "$orDir/allOtherReg.dat" , $sthInsert, 7);
}
#-------------------------------------------------------------------------------
#Subroutines that parse each file type......
#

sub getAutos {
  my ($m, $orDir, $dbh) = @_;  
  my $sth = $dbh->prepare("select auto_pfamseq from pfamseq where pfamseq_acc=?") or
          $logger->logdie("Error preparing statement:". $dbh->errstr);
  
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
  my ( $dir, $file, $fh, $pfamseq_autos ) = @_;

  #print STDERR scalar(@{$coilsAR})."$dir, $file\n";

  $/ = "\n>";
  open( COILS, "$dir/$file" ) || die "Could not open $dir/$file:[$!]\n";
  while (<COILS>) {
    chomp;
    my @entry = split( /\n/, $_ );
    if (@entry) {
      my $acc = $1 if ( $entry[0] =~ /^>?(\S{6})\.\d+/ );
      my $seq = join( "", @entry[ 1 .. $#entry ] );
      if ( !$acc || !$seq ) {
        warn "Could not find id or entry for $_\n";
      }
      else {

        #print STDERR "Finding coil\n";
        &findCoils( $$pfamseq_autos{$acc}, $seq, $fh );
      }
    }

    #print STDERR scalar(@{$coilsAR})."$dir, $file\n";
  }
  $/ = "\n";
}

sub findCoils {
  my ( $acc_auto, $seq, $fh ) = @_;
  my $start;
  my $end;
  my $prev = -1;
  while ( $seq =~ m/x/g ) {
    my $x_pos = pos $seq;
    if ( ( $prev + 1 ) == $x_pos ) {

      #$end = $x_pos;
    }
    else {
      $end = $prev;
      if ($start) {

        #print "\t$acc_auto\t$start\t$end\tcoiled_coil\tncoils\t\t\n";
        print $fh "\\N\t$acc_auto\t$start\t$end\tcoiled_coil\tncoils\t\\N\t\\N\n";
      }
      $start = $x_pos;
    }
    $prev = $x_pos;
  }
  if ($start) {
    print $fh "\\N\t$acc_auto\t$start\t$prev\tcoiled_coil\tncoils\t\\N\t\\N\n";
  }
}

sub parseSeg {
  my ( $dir, $file, $fh, $pfamseq_autos ) = @_;
  open( SEG, "$dir/$file" ) || die "Could not open $dir/$file:[$!]\n";
  while (<SEG>) {
    if (/^>(\S+)\.\d+\((\d+)\-(\d+)\)\s+complexity=(\S+)/) {
      my $acc   = $1;
      my $start = $2;
      my $end   = $3;
      my $score = $4;
      print $fh
"\\N\t$$pfamseq_autos{$acc}\t$start\t$end\tlow_complexity\tseg\t$score\t\\N\n";
    }
  }
  close(SEG);
}

sub parsePhobius {
  my ( $dir, $phobius_file, $fh, $pfamseq_auto ) = @_;
  open( PHOB, "$dir/$phobius_file" )
    || die "Could not open $dir/$phobius_file:[$!]\n";

  my ($acc);
  my $last_membrane = 0;
  while (<PHOB>) {
    if (/^ID\s+(\S+)\.\d+/) {
      $acc = $1;
      unless ( exists( $pfamseq_auto->{$acc} ) ) {
        $logger->logdie("$acc is not in the pfamseq mapping!");
      }
      
    }
    elsif (/^FT\s+SIGNAL\s+(\d+)\s+(\d+)/) {
      my $start = $1;
      my $end   = $2;
      print $fh "\\N\t"
        . $pfamseq_auto->{$acc}
        . "\t$start\t$end\tsig_p\tPhobius\t\\N\t\\N\n";
    }
    elsif (/^FT\s+TRANSMEM\s+(\d+)\s+(\d+)/) {
      my $start = $1;
      my $end   = $2;
      print $fh
        "\\N\t".$pfamseq_auto->{$acc}."\t$start\t$end\ttransmembrane\tPhobius\t\\N\t\\N\n";
    }
    elsif (/^\/\//) {
      $acc = "";
    }
  }
}

sub parseIupred {
  my ( $dir, $iupred_file, $fh, $pfamseq_auto ) = @_;

  open( DIS, "<", $iupred_file )
    or $logger->logdie("Failed to open $iupred_file for reading:[$!]");

  my ($acc);

  #Need to parse something like this:

  #ID Q6GZX3.1
  #FT  IUPred     218     224  DISORDERED
  #FT  IUPred     261     298  DISORDERED
  #//

  while (<DIS>) {
    if (/^ID\s+(\S+)\.\d+/) {
      $acc = $1;
      unless ( exists( $pfamseq_auto->{$acc} ) ) {
        $logger->logdie("$acc is not in the pfamseq mapping!");
      }
    }
    elsif (/^FT\s+IUPred\s+(\d+)\s+(\d+)/) {
      my $start = $1;
      my $end   = $2;
      print $fh "\\N\t"
        . $pfamseq_auto->{$acc}
        . "\t$start\t$end\tdisorder\tIUPred\t\\N\t\\N\n";
    }
    elsif (/^\/\//) {
      $acc = "";
    }
    elsif (/^#/) {

      #comment block
      next;
    }
    else {
      $logger->logdie("Failed to parse line, $_");
    }
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