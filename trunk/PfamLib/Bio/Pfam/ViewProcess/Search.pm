package Bio::Pfam::ViewProcess::Search;

use strict;
use warnings;
use File::Copy;
use Moose;
use Moose::Util::TypeConstraints qw(enum);

use File::Temp qw(tempdir);
use File::Slurp qw(read_file append_file);

extends 'Bio::Pfam::ViewProcess::Architecture';

has '+statusFile' => (
  default => 'search',
);

has 'upload' => (
  is      =>  'rw',
  isa     =>  'Bool',
  default =>  0,
);

has 'database' => (
  is      => 'rw',
  isa     => enum( [ qw( metaseq ncbi pfamseq )]),
  default => 'pfamseq',
);

has 'databaseList' => (
  is => 'ro',
  isa     => 'ArrayRef',
  default => sub{ [ qw( metaseq ncbi pfamseq )] },
);

has 'databaseToTag' => (
  is => 'ro',
  isa     => 'HashRef',
  default => sub{ { metaseq => 'meta',
                    ncbi    => 'ncbi',
                    pfamseq => 'full' } },
);

has 'cpus' => (
 is      => 'rw',
 isa     => 'Int',
 default => 4
);

sub search {
  my ( $self, $acc, $tmpdir ) = @_;
  if(!defined($tmpdir)){
    $tmpdir = tempdir( CLEANUP => 1 );
  }
  unless(-d $tmpdir){
    $self->logger->logdie("Failed to get a temporary directory.");
  }
  my $pfam = $self->pfamdb->getSchema-resultset('Pfama')->find({pfama_acc => $acc});
  unless($pfam){
    $self->logger->logdie("Failed to get a Pfam row for $acc.");
  }
  #Get the stockholm markup HMM, we need this as it has the GAs
  my $result = $self->pfamdb->getSchema->resultset('PfamaHmm')->search({ auto_pfama => $pfam->auto_pfamA});
  $result->getHMMAsFile( $acc.".ann", $tmpdir );
  #Now perform the search
  my $database = $self->config->{$self->database}->location.'/'.$self->database;
  my $alifile =  "$tmpdir/$acc.".$self->database.".ali";
  my $cpu = '';
  $cpu = ' --cpu '.$self->cpu if($self->cpus > 1);
  system($self->config->hmmer3bin."/hmmsearch $cpu -A $alifile --cut_ga --notextw $tmpdir/$acc.hmm $database > /dev/null")
    and $self->logger->logdie("Failed to run hmmsearch with $acc.hmm against $database:[$!]");
  
  $self->pfam($pfam);
  my $GFAnn = $self->getGFAnnotations();
  my $filename = $self->writeAnnotateAlignment("$alifile", $GFAnn);
  #
  if($self->upload){
    $self->uploadTreesAndAlign($filename, $self->datbaseToTag->{$self->database});
  }
}

sub searchAll {
  my ($self, $acc, $dbs, $tmpdir) = @_;
  
  foreach my $db (@$dbs){
    $self->database($db);
    $self->search($acc, $tmpdir);
  }
}

sub processOptions {
  my ( $self ) = @_;
  my ($statusDir, $acc, $upload, @dbs, $dir, $chunk, $chunkSize, $help, $cpu, $all);
  my $options = {};

  my @opts = @ARGV;
  #Get and check the input parameters
  GetOptionsFromArray(
    \@opts, 
    "statusdir=s" => \$statusDir,
    "acc=s"       => \$acc,
    "upload"      => \$upload,
    "db=s"        => \@dbs,
    "chunk=i"     => \$chunk,
    "chunkSize=i" => \$chunkSize,
    "cpu=i"       => \$cpu,
    "all"         => \$all,
    "h|help"      => \$help
  );

  if ($help) {
    $self->options($options);
    return;
  }

  if($chunkSize){
    if ( !$statusDir or !-d $statusDir) {
      $self->logger->logdie("No status directory passed in or it is not present $statusDir.....");
    }
  }
  
  if($upload){
    $self->upload(1);
  }
  if($cpu){
    $self->cpus($cpu);
  }
  
  if($acc){
    if($acc !~ /PF\d{5}/){
      $self->logger->logdie("That does not look like a Pfam accession [$acc].");
    }
  }
  
  if(!scalar(@dbs)){
    $self->logger->logdie("You need to specifiy one or more databases to search");
  }
  
  $options->{acc}       = $acc;
  $options->{dbs}       = \@dbs;
  $options->{statusdir} = $statusDir;
  $options->{chunk}     = $chunk;
  $options->{chunkSize} = $chunkSize;
  $options->{all}       = $all;
  
  $self->options($options);
}
sub submitToFarm {
  my ($self, $noJobs) = @_;
  
  my $rs = $self->pfamdb->getSchema->resultset('PfamA')->search({});
  my $chunkSize = ceil($rs->count/$noJobs);
  
  $self->logger->debug("Calculating PfamA paging");
  my $pfamAll = $self->pfamdb->getSchema->resultset('PfamA')->search(
      {},
      {
      page => 1,
      rows => $chunkSize
    });
  
  my $pager = $pfamAll->pager;
    
  for(my $i = 1; $i<= $pager->last_page; $i++){
    my @pfamA = $pfamAll->page($i)->all;
    #Determine the max model length
    my $max= 0;
    foreach my $a (@pfamA){
      $max = $a->model_length if($a->model_length > $max);
    }
  
    #Now submit the jobs, determinig roughly how much memory we are going to use.
    my $memory_gb = ceil(($max * 40000 * 48 * $self->cpus)/1000000000); 
    $memory_gb  += 1; #Add another Gig for the alignment overhead.
    my $queue = 'long'; #searches may go over 8 hours. 
    my $memory_mb=$memory_gb*1000;
    my $memory_kb=$memory_mb*1000;
    my ($options, $resource);
    if($self->upload){
      $resource = " -M$memory_kb -R'span[hosts=1] select[mem>$memory_mb && mypfamlive2<500] rusage[mypfamlive2=10:mem=$memory_mb]'";
      $options .= ' -upload ';
    }else{
      $resource = " -M$memory_kb -R'span[hosts=1] select[mem>$memory_mb] rusage[mem=$memory_mb]'"
    }
    
    $resource .= " -cpu ".$self->cpu;
    
    $options .= join(' -db ', @{$self->options->{dbs}});
    if($self->options->{dir}){
      $options .= ' -dir '.$self->options->{dir};
    }
    $options .= "-statusdir ".$self->options->{statusdir};
    
    my $fh = IO::File->new();
    $fh->open( "| bsub -G pfam-grp -q $queue  ".$resource." -o ".$self->options->{statusdir}."/search.$i.log");
    $fh->print( "performOtherSearch.pl $options -chunk $i  -chunkSize $chunkSize\n");
    $fh->close;
  }
  
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

    my $pfamAll = $self->pfamdb->getSchema->resultset('PfamA')->search(
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
  my $tempDir = tempdir( CLEANUP => 0 );
  chdir($tempDir);
  foreach my $a (@pfamA){
    next if(exists($done{$a->pfama_acc}));
    $self->logger->debug("Working on ".$a->pfama_acc);
    $self->searchAll($a->pfam_acc, $self->options->{dbs}, $tempDir);
    print S $a->pfama_acc."\n";
  }
  close(S);
  $self->touchStatus($self->statusFile.".$chunk.done");
  }
}
1;