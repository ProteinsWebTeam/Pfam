package Bio::Pfam::ViewProcess::HHsearch;

use strict;
use warnings;
use File::Copy;
use POSIX qw(ceil);
use Moose;
use Moose::Util::TypeConstraints;
use Data::Printer;
use Getopt::Long qw(GetOptionsFromArray);
use File::Temp qw(tempdir);
use File::Slurp qw(read_file append_file);

extends 'Bio::Pfam::ViewProcess::Architecture';

has '+statusFile' => (
  default => 'hhsearch',
);

has 'match' => (
  is      => 'rw',
  isa     => 'Int',
  default => 50,
);

has 'identity' => (
  is      =>  'rw',
  isa     =>  'Int',
  default => 100,
);

has 'upload' => (
  is      =>  'rw',
  isa     =>  'Bool',
  default =>  0,
);

has 'evalThres' => (
  is   => 'rw',
  isa  => 'Str',
  default => '0.01'
);



sub makeHHMFromAccAndSearch {
  my ($self, $seed, $name) = @_;
  my $hhmfile = $self->makeHHMFromAcc($seed, $name);
  my $results = $self->searchModel($hhmfile);
  unlink($hhmfile);
  return($results);
}

sub makeHHMFromFileAndSearch {
  my ($self, $seed, $name) = @_;
  my $hhmfile = $self->makeHHMFromFile($seed, $name);
  my $results = $self->searchModel($hhmfile);
  unlink($hhmfile);
  return($results);
}

sub makeHHMFromFile {
  my ($self, $seed, $name) = @_;
  
  unless(-e $seed){
    $self->logger->logdie("The alignment, $seed, does not exist.");
  }
  
  my $tempDir = tempdir( CLEANUP => 0 );
  my $hhname;
  if($name){
    $hhname = $name.'.hhm';
  }
  my $hhmfile = $self->_buildHMM($seed, $name, $hhname, 0, $tempDir);
  
  return($hhmfile);
}

sub makeHHMFromAcc {
  my ($self, $acc) = @_;
 
  my $a = $self->pfamdb->getSchema->resultset('Pfama')->find({pfama_acc => $acc});
  unless($a and defined($a->pfama_acc)){
    $self->logger->warn("Could not find accession $acc in the database.");
  }
  $self->logger->debug("Working on ".$a->pfama_acc."\n");
  $self->_getSEED($a->auto_pfama);
  my $hhm = $self->makeHHMFromFile("SEED", $acc);
  return($hhm);
}  

sub searchModel {
  my($self, $hhm, $hmmlib, $output) = @_;
  
  #If we have not been given an hhlib, then use the default 
  if(!$hmmlib){
    $hmmlib = $self->config->hhsearchLibDir."/Pfam-A.hhm";
  }
  
  #If no ouput file name specified, make one up, mocking what hhsearch does.
  if(!$output){
    if($hhm =~ /(.*)\.hhm/){
      $output= "$1.hhr";
    }else{
      $output = "$hhm.hhr";
    }
  }
  #Run the search
  my $cmd = $self->config->hhsearchBin."/hhsearch -v 0 -i $hhm -d $hmmlib -o $output";
  system($cmd) == 0
    or $self->logger->logdie("Failed to run hhsearch");
  
  #Return the name of the output file.
  return($output);
}

sub makeHHLib {
  my ( $self ) = @_;

  my $hmmLib = $self->config->hhsearchLibDir."/Pfam-A.hhm";
  if(!$hmmLib and !$self->options->{newlib}){
    $self->logger->logdie("Could not find ".$self->config->hhLibDir."/Pfam-A.hhm, try building it by using -newlib option.");
  }
  if($self->options->{newlib}){
    $self->logger->debug("Going to make a new hhm formated HMM library");
    my $tempDir = tempdir( CLEANUP => 0 );
    my $newHmmLib = $tempDir."/Pfam-A.hhm";

    my @pfamAs = $self->pfamdb->getSchema->resultset('Pfama')->search({});
    foreach my $a (@pfamAs){
      $self->logger->debug("Working on ".$a->pfama_acc."\n");
      my $row = $self->pfamdb->getSchema->resultset('PfamaInternal')->find( { auto_pfama => $a->auto_pfama } );
      $row->seed_uncompressed($tempDir);
      $self->_buildHMM($tempDir."/SEED", 
                       $a->pfama_acc, 
                       $newHmmLib,
                       1, #append 
                       $tempDir);
      unlink($tempDir.'/SEED');
      unlink($tempDir.'/SEED.fasta');
    }
    
    #finally copy it across to the library.
    copy($newHmmLib, $hmmLib) or $self->logger->logdie("Failed to copy new HHlib: $!");
  }
}
  

sub submitToFarm {
  my ($self, $noJobs) = @_;
  
  my $rs = $self->pfamdb->getSchema->resultset('Pfama')->search({});
  my $chunkSize = ceil($rs->count/$noJobs);
  
  #Now submit the jobs
  my $queue = 'normal';
  my $resource = "-M3500000 -R'select[mem>3500 && mypfamlive2<500] rusage[mypfamlive2=10:mem=3500]'";
  my $memory = 3500000;  
  my $fh = IO::File->new();

  my $options = '';
  $options .= ' -upload ' if ($self->upload);
  $options .= " -evalue ".$self->evalThres;;

  $fh->open( "| bsub -q $queue  ".$resource." -o ".
              $self->options->{statusdir}."/hhsearch.\%J.\%I.log  -JHHsearch\"[1-$noJobs]\"");
  $fh->print( "performHHsearch.pl $options -chunk \$\{LSB_JOBINDEX\} -chunkSize $chunkSize -statusdir ".$self->options->{statusdir}."\n");
  $fh->close;
  $self->logger->debug("Status is:".$self->statusFile."\n");
  while(! $self->statusCheck($self->statusFile, $noJobs)){
    $self->logger->info('Waiting for jobs to complete.');
    sleep(600);
  }
}

sub searchRange {
  my ( $self ) = @_;
  
  my $chunk = $self->options->{chunk};
  my $chunkSize = $self->options->{chunkSize};

  if(!$self->statusCheck($self->statusFile.".$chunk.done")){
  
  my(@pfamA);
  if($chunk and $chunkSize ) {
    $self->logger->debug("Calculating PfamA paging");

    my $pfamAll = $self->pfamdb->getSchema->resultset('Pfama')->search(
      {},
      {
      page => 1,
      rows => $chunkSize
    });

    my $pager = $pfamAll->pager;
    $self->logger->info( "Working on page $chunk out of " . $pager->last_page );

    @pfamA = $pfamAll->page($chunk)->all;
  }
  
  $self->logger->debug("Searching pfamA range, chunk $chunk.");
  
  my $filename = $self->statusFile.".$chunk";
  
  $self->touchStatus($filename); 
  open(S, '+<', $self->options->{statusdir}.'/'.$filename) or 
      $self->logger->logdie("Could not open status file:[$!]"); 
  
  my %done;
  while(<S>){
    if(/^(\S+)/){
      $done{$1}++;
    }
  }
  my $filenameRes = $self->options->{statusdir}."/$filename.res";
  my $tempDir = tempdir( CLEANUP => 0 );
  chdir($tempDir);
  foreach my $a (@pfamA){
    next if(exists($done{$a->pfama_acc}));
    $self->logger->debug("Working on ".$a->pfama_acc);
    $self->_getSEED($a->auto_pfama);
    my $results = $self->makeHHMFromFileAndSearch("SEED", $a->pfama_acc);
    if($self->upload){
      $self->uploadResult($results, $a);
    }
    my @data = read_file($results);
    append_file( $filenameRes, @data );
    unlink($results);
    
    print S $a->pfama_acc."\n";
  }
  close(S);
  $self->touchStatus($self->statusFile.".$chunk.done");
  }
}

sub processOptions {
  my ( $self ) = @_;
  my ($statusDir, $acc, $seed, 
  $file, $name, $onlylib, $upload, $evalue, $newlib,  $help, 
  $chunk, $chunkSize, $all);
  my $options = {};

  my @opts = @ARGV;
  #Get and check the input parameters
  GetOptionsFromArray(
    \@opts, 
    "statusdir=s" => \$statusDir,
    "acc=s"       => \$acc,
    "seed"        => \$seed,
    "file=s"      => \$file,
    "name=s"      => \$name,
    "newlib"      => \$newlib,
    "onlylib"     => \$onlylib,
    "upload"      => \$upload,
    "evalue=s"    => \$evalue,
    "allVsall"    => \$all,
    "chunk=i"     => \$chunk,
    "chunkSize=i" => \$chunkSize,
    "h|help"      => \$help
  );

  if ($help) {
    $options->{help} = 1;
    $self->options($options);
    return;
  }

  if($all or $chunkSize){
    if ( !$statusDir or !-d $statusDir) {
      $self->logger->logdie("No status directory passed in or it is not present $statusDir.....");
    }
  }
  
  if($onlylib){
    $newlib = 1;
  }
  
  my $check = 0;
  $check++ if($seed);
  $check++ if($acc);
  $check++ if($file);
  
  if($check >1){
    $self->logger->logdie("You can only specific one of --seed, --acc <accession or --file <name>");
  }
  
  if($evalue){
    $self->evalThres($evalue);
  }
  
  if($upload){
    $self->upload(1);
  }
  
  if($acc){
    if($acc !~ /PF\d{5}/){
      $self->logger->logdie("That does not look like a Pfam accession [$acc].");
    }
  }
  $options->{acc}       = $acc;
  $options->{file}      = $file;
  $options->{seed}      = $seed;
  $options->{name}      = $name;
  $options->{onlylib}   = $onlylib;
  $options->{newlib}    = $newlib;
  $options->{statusdir} = $statusDir;
  $options->{chunk}     = $chunk;
  $options->{chunkSize} = $chunkSize;
  $options->{allVsall}  = $all;

  $self->options($options);
}

sub uploadResult {
  my ($self, $file, $pfamRow) = @_;
  
  if(!$pfamRow or !$pfamRow->auto_pfama){
    $self->logger->logdie("did not get a pfama dbix row passed in");
  }
  open(R, '<', $file) or $self->logger->logdie("Could not open $file:[$!]");
  
  #Delete all regions belonging to this row.
  $self->logger->debug("Deleting rows for ".$pfamRow->pfama_acc);
  $self->pfamdb->getSchema->resultset('Pfama2pfamaHhsearchResults')
                  ->search({auto_pfama1 => $pfamRow->auto_pfama})->delete;
  
  my @rows;
  
  while(<R>){
    if(/^No\s+\d+/){
      #We have hit the end of the summary block.
      last;
    }elsif(/^\s+\d+\s+PF\d{5}/){
      #line should look like this:
# No Hit                             Prob E-value P-value  Score    SS Cols Query HMM  Template HMM
#  1 PF10000                        100.0 2.2E-43 1.8E-47  216.0   0.0   72    1-72      1-72  (72)
#  2 PF00717                         78.9     0.2 1.6E-05   26.5   0.0   16   12-27      5-20  (70)
#  3 PF09866                         64.0    0.79 6.5E-05   23.6   0.0   14   17-30      1-14  (42)
      my @r = split(/\s+/, $_);
      my $pfam_acc = $r[2];
      next if($pfam_acc eq $pfamRow->pfama_acc); #Do not stor self self matches
      my $evalue   = $r[4];
      if($evalue <= $self->evalThres){
        my $b = $self->pfamdb->getSchema->resultset('Pfama')->find({pfama_acc => $pfam_acc});
        if($b and $b->pfama_acc){
          push(@rows, { auto_pfama1 => $pfamRow->auto_pfama,
                        auto_pfama2 => $b->auto_pfama,
                        evalue      => $evalue });
        }
      }
    }
    #$self->logger->debug($_);
  }
  close(R);
  if(scalar(@rows)){
    $self->pfamdb->getSchema->resultset('Pfama2pfamaHhsearchResults')->populate(\@rows);
  }
}


sub _getSEED {
  my ($self, $auto) = @_;
  
  my $row = $self->pfamdb->getSchema->resultset('PfamaInternal')->find( { auto_pfama => $auto } );
  $row->seed_uncompressed();
  
}

sub _buildHMM {
  my ($self, $seedFile, $name, $hmmfilename, $append, $tempDir) = @_;
  
  my $nameCmd = '';
  if($name){
    $nameCmd = " -name $name ";
  }
  if(!defined($tempDir)){
    $tempDir = ".";
  }
  if(!$hmmfilename){
    $hmmfilename = 'HHM'
  }
  
  my $outputCmd;
  if($append){
    $outputCmd = " -a  $hmmfilename";
  }else{
    $outputCmd = " -o  $hmmfilename";
  }
  
  my $cmd = $self->config->hmmer3bin."/esl-reformat --replace .:- afa $seedFile > $tempDir/$$.fasta";
  system($cmd) == 0
    or $self->logger->logdie("Failed to run esl-reformat on $seedFile");
      
  $cmd = $self->config->hhsearchBin."/hhmake -M ".$self->match." -id ".$self->identity." $nameCmd -i $tempDir/$$.fasta $outputCmd > /dev/null 2>&1 ";
  system($cmd) == 0
    or $self->logger->logdie("Failed to run hhmake on $tempDir/$$.fasta [based on $seedFile]");

  unlink("$tempDir/$$.fasta");
  return($hmmfilename);
}

1;
