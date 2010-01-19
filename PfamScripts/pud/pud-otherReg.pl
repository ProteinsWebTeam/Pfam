#!/usr/local/bin/perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Cwd;
use IO::File;
use Sys::Hostname;
use Data::UUID;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Get the current working directory
my $pwd = getcwd;

$ENV{'COILSDIR'} = '/usr/local/ensembl/data/coils';

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh    = $pfamDB->getSchema->storage->dbh;

my $pfamseqDir = shift;

unless ($pfamseqDir) {
  $logger->logdie("You need to supply the directory name:[$!]");
}

#-------------------------------------------------------------------------------
#Make and change and into the otherReg dir in the lastest pfamseqdir
unless ( -e "$pfamseqDir/otherReg" ) {
  mkdir("$pfamseqDir/otherReg")
    or $logger->logdie("Could not make directory $pfamseqDir/otherReg: [$!]");
}
chdir("$pfamseqDir/otherReg")
  or $logger->logdie(
  "Could not change into directory $pfamseqDir/otherReg: [$!]");

#-------------------------------------------------------------------------------
#Open and spit pfamseq into bits;

#my $bit = int( ( $config->dbsize ) / 100 );
#$bit++;
#
#$logger->debug(
#  "Going to split pfamseq into 100 files containing $bit sequences");
#my $n = 1;
#
#open( B, ">pfamseq.$n" )
#  or $logger->logdie("Could not open pfamseq.$n:[$!]\n");
#open( P, "$pfamseqDir/pfamseq" )
#  or $logger->logdie("Could not open $pfamseqDir/pfamseq:[$!]\n");
#
#my $c;
#while (<P>) {
#  if (/^>/) {
#    $c++;
#    if ( $c > $bit ) {
#      close(B);
#      $n++;
#      $c = 0;
#      open( B, ">pfamseq.$n" );
#    }
#  }
#  print B $_;
#}
#close(B);
#close(P);
#$logger->info("Made $n files");
#
##-------------------------------------------------------------------------------
##Now submit the searches to the farm
#my $farmConfig = $config->farm;
#my $phost      = hostname;
#my $ug         = new Data::UUID;
#my $uuid       = $ug->to_string( $ug->create() );
#my $fh         = IO::File->new();
#
#$logger->info("Submitting jobs to the farm");
#
##Set up the job and copy the files over;
#$fh->open( "| bsub -q "
#    . $farmConfig->{lsf}->{queue}
#    . " -o /tmp/$$.log  -JotherRegs\"[1-$n]\" " );
#$fh->print( "mkdir " . $farmConfig->{lsf}->{scratch} . "/$uuid\n" );
#$fh->print( "cd " . $farmConfig->{lsf}->{scratch} . "/$uuid\n" );
#$fh->print(
#      "/usr/bin/scp -p $phost:$pfamseqDir/otherReg/pfamseq.\$\{LSB_JOBINDEX\} "
#    . $farmConfig->{lsf}->{scratch}
#    . "/$uuid/pfamseq.\$\{LSB_JOBINDEX\}\n" );
#
##To calculate these other regions
##seg
##ncoils - This needs the environment variable COILSDIR to be set.
##phobius
#$fh->print(
#  "ncoils -f < pfamseq.\$\{LSB_JOBINDEX\} > ncoils.\$\{LSB_JOBINDEX\}\n");
#$fh->print("seg pfamseq.\$\{LSB_JOBINDEX\} -l > seg.\$\{LSB_JOBINDEX\}\n");
#$fh->print(
#  "phobius.pl pfamseq.\$\{LSB_JOBINDEX\} > phobius.\$\{LSB_JOBINDEX\}\n");
#
##Now bring back all of the files
#foreach my $f (qw(ncoils seg phobius)) {
#  $fh->print(
#"/usr/bin/scp -p $f.\$\{LSB_JOBINDEX\} $phost:$pfamseqDir/otherReg/$f.\$\{LSB_JOBINDEX\}\n"
#  );
#}
#$fh->print("rm -f pfamseq.\$\{LSB_JOBINDEX\}\n");
#$fh->close;
#
##-------------------------------------------------------------------------------
##Have all of the jobs finished
#$logger->info("Waiting for jobs to finish on the farm");
#my $finished = 0;
#while ( !$finished ) {
#  open( FH, "bjobs -JotherRegs|" );
#  my $jobnum;
#  while (<FH>) {
#    if (/^\d+/) {
#      $jobnum++;
#    }
#  }
#  close FH;
#
#  if ($jobnum) {
#    $logger->info(
#"Will not continue until your $jobnum outstanding jobs have completed. Will check again in five minutes"
#    );
#    sleep(300);
#  }
#  else {
#    $finished = 1;
#  }
#}

my $n = 100;
#-------------------------------------------------------------------------------
#Now we should have every thing back to be joined together and uploaded.
$logger->info("Checking all of the files have been retieved from the farm");
my $error;
for ( my $i = 1 ; $i <= $n ; $i++ ) {
  foreach my $f (qw(seg phobius ncoils)) {
    unless ( -s "$pfamseqDir/otherReg/$f.$i" ) {
      $logger->warn("$pfamseqDir/otherReg/$f.$i is missing");
      $error++;
    }
  }
}

$logger->logdie('Some of the files are other region files are missing')
  if ($error);

#-------------------------------------------------------------------------------
#Join and upload

my $orDir = "$pfamseqDir/otherReg";

#-------------------------------------------------------------------------------
#Get auto_pfamase mapping
$logger->info("Getting pfamseq accessions/auto_pfamseq");

my $newSeqSth =
  $dbh->prepare("select pfamseq_acc, auto_pfamseq from pfamseq")
  or $logger->logdie('select prepare failed:'. $dbh->errstr );
$newSeqSth->execute or $logger->logdie('select execution failed:'. $dbh->errstr );
my $res = $newSeqSth->fetchall_arrayref;
my $newSeqs;
foreach my $r (@$res) {
  $newSeqs->{ $r->[0] } = $r->[1];
}

#-------------------------------------------------------------------------------
#Now parse the data and write to the file

my %subs = ( ncoils  => \&parseNcoils,
             seg     => \&parseSeg,
             phobius => \&parsePhobius );
my ($fh);
open($fh, ">$orDir/allOtherReg.dat") or $logger->logdie("Could not open allOtherReg.dat");

for( my $m =1; $m <= $n; $m++){
  foreach my $f (qw(ncoils phobius seg)){
    $logger->info("Parsing $f.$m");
    $subs{$f}( $orDir, "$f.$m", $fh, $newSeqs );
  }
}

#-------------------------------------------------------------------------------
#Copy file to the mysql instance so that it can be uploaded
#

$logger->info("scp data file to instance");
system("scp $orDir/allOtherReg.dat ".$config->pfamliveAdmin->{host}.":/tmp/allOtherReg.dat") 
  and $logger->logdie("Could not scp file ($orDir/allOtherReg.dat) to ".
    $config->pfamliveAdmin->{host}." because:[$!]");

$logger->info('preparing to upload the file');
$dbh->do("delete from other_reg");

my $sth = $dbh->prepare( "LOAD DATA INFILE '/tmp/allOtherReg.dat' INTO TABLE other_reg" ) 
   or $logger->logdie('select prepare failed:'. $dbh->errstr );

$sth->execute or $logger->logdie('select execution failed:'. $dbh->errstr ); 

#-------------------------------------------------------------------------------
#Subroutines that parse each file type......
#

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

sub findCoils{
    my($acc_auto, $seq, $fh) = @_;
    my $start;
    my $end;
    my $prev = -1;
    while( $seq =~ m/x/g){
        my $x_pos = pos $seq;
        if(($prev + 1) == $x_pos){
            #$end = $x_pos;
        }else{
            $end = $prev;
            if($start){
                #print "\t$acc_auto\t$start\t$end\tcoiled_coil\tncoils\t\t\n";
                print $fh "\t$acc_auto\t$start\t$end\tcoiled_coil\tncoils\t\t\n";
            }
            $start = $x_pos;
        }
        $prev = $x_pos;
    }
    if($start){
        print $fh "\t$acc_auto\t$start\t$prev\tcoiled_coil\tncoils\t\t\n";     
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
        "\t$$pfamseq_autos{$acc}\t$start\t$end\tlow_complexity\tseg\t$score\t\n";
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
    }
    elsif (/^FT\s+SIGNAL\s+(\d+)\s+(\d+)/) {
      my $start = $1;
      my $end   = $2;
      print $fh 
        "\t" . $$pfamseq_auto{$acc} . "\t$start\t$end\tsig_p\tPhobius\t\t\n";
    }
    elsif (/^FT\s+TRANSMEM\s+(\d+)\s+(\d+)/) {
      my $start = $1;
      my $end   = $2;
      print $fh
        "\t$$pfamseq_auto{$acc}\t$start\t$end\ttransmembrane\tPhobius\t\t\n";
    }
    elsif (/^\/\//) {
      $acc = "";
    }
  }
}
