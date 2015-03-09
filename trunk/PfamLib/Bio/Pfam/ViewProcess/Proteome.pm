package Bio::Pfam::ViewProcess::Proteome;

use strict;
use warnings;
use File::Copy;
use Getopt::Long qw(GetOptionsFromArray);
use Log::Log4perl qw(:easy);
use IO::Compress::Gzip qw(gzip $GzipError);
use Data::Printer;
use POSIX qw(ceil);
use Moose;
use Moose::Util::TypeConstraints qw(enum);

extends 'Bio::Pfam::ViewProcess::Architecture';

has '+statusFile' => ( default => 'proteomeTSV', );

has 'pfamA' => (
  is  => 'rw',
  isa => 'HashRef'
);

has 'clanMap' => (
  is  => 'rw',
  isa => 'HashRef'
);

has 'release' => ( 
  is => 'rw',
 isa => 'Str' );

sub BUILD {
  my ( $self, $args ) = @_;

  $self->{config} = Bio::Pfam::Config->new;
  $self->{jobdb} =
    Bio::Pfam::PfamJobsDBManager->new( %{ $self->config->pfamjobs } );
  unless ( $self->{jobdb} ) {
    $self->mailPfam( "Failed to run view process",
      "Could not get connection to the pfam_jobs database" );
  }

  Log::Log4perl->easy_init($DEBUG);
  my $logger = get_logger();

  $self->{logger} = $logger;

  $self->logger->debug("Got pfam_job db connection");
  $self->{pfamdb} =
    Bio::Pfam::PfamLiveDBManager->new( %{ $self->config->pfamliveAdmin } );

  unless ( $self->{pfamdb} ) {
    $self->mailPfam( "Failed to run view process",
      "View process failed as we could not connect to pfamlive" );
  }
  $self->logger->debug("Got pfamlive database connection");
  $self->logger->debug('Processing options');
  $self->processOptions;
  $self->getPfamData;
  $self->getClanData;

  my $version = $self->pfamdb->getSchema->resultset('Version')->search->first;
  $self->release( $version->pfam_release );

  if(!-d $self->options->{releasedir}."/proteomes"){
    mkdir($self->options->{releasedir}."/proteomes");
  }

  return ($self);
}

sub getPfamData {
  my ($self) = @_;

  my @pfamA = $self->pfamdb->getSchema->resultset('PfamA')->search( {}, );

  my $pfamA;
  foreach my $row (@pfamA) {
    $pfamA->{ $row->pfama_acc }->{pfamA_acc}    = $row->pfama_acc;
    $pfamA->{ $row->pfama_acc }->{pfamA_id}     = $row->pfama_id;
    $pfamA->{ $row->pfama_acc }->{model_length} = $row->model_length;
    $pfamA->{ $row->pfama_acc }->{type}         = $row->type;
  }

  $self->pfamA($pfamA);
}

sub getClanData {
  my ($self) = @_;

  unless ( $self->pfamA ) {
    $self->getPfamData;
  }

  my @clan = $self->pfamdb->getSchema->resultset("ClanMembership")->search(
    {},
    {
      join     => [qw/clan_acc/],
      prefetch => [qw/clan_acc/]
    }
  );

  #Add clans to hash
  my $clan;
  foreach my $row (@clan) {
    $clan->{ $row->pfama_acc->pfama_acc } = $row->clan_acc->clan_acc;
  }

  #Add No_clan to hash for pfamA with no clan
  foreach my $pfamA_acn ( keys %{ $self->pfamA } ) {
    unless ( exists( $clan->{$pfamA_acn} ) ) {
      $clan->{$pfamA_acn} = "No_clan";
    }
  }
  $self->clanMap($clan);
}

#Takes an array of taxids and make a TSV for each one!
sub makeTSV {
  my ( $self, $ncbi_taxid ) = @_;

  my $dbh = $self->pfamdb->getSchema->storage->dbh;

  #Set up query for retrieving domain info from pfamA_reg_full_significant
  my $st_regions = $dbh->prepare(
    "SELECT r.pfamseq_acc, 
                                         ali_start, 
                                         ali_end, 
                                         seq_start, 
                                         seq_end, 
                                         pfamA_acc, 
                                         model_start, 
                                         model_end, 
                                         domain_bits_score, 
                                         domain_evalue_score 
                                   FROM pfamA_reg_full_significant as r
                                   LEFT JOIN proteome_pfamseq as p ON p.pfamseq_acc=r.pfamseq_acc
                                   WHERE auto_proteome = ? 
                                   AND in_full=1"
  ) or $self->logger->logdie( "Failed to prepare statement:" . $dbh->errstr );

  #Go through each sequence for the taxid and get Pfam-A regions
  my $proteome =
    $self->pfamdb->getSchema->resultset('CompleteProteome')
    ->find( { ncbi_taxid => $ncbi_taxid } );

  #Set up filehandle for printing

  my $outfile = $self->options->{releasedir} . "/proteomes/" . $ncbi_taxid . ".tsv";
  p($outfile);
  if ( -s "$outfile.gz" ) {
    $self->logger->info(
      "Already done ncbi taxid $ncbi_taxid '" . $proteome->species . "'" );
    return;
  }
  open( OUT, ">$outfile" )
    or $self->logger->logdie("Couldn't open fh to $outfile, $!");

  #Retrieve domain info for proteome
  $self->logger->info( "Retrieving domains for ncbi taxid $ncbi_taxid '"
      . $proteome->species
      . "'" );
  $st_regions->execute( $proteome->auto_proteome )
    or $self->logger->logdie(
    "Couldn't execute statement " . $st_regions->errstr );
  my $pfamA_domains = $st_regions->fetchall_arrayref();

  #Print header
  print OUT "#Pfam-A regions from Pfam version "
    . $self->release
    . " for ncbi taxid $ncbi_taxid '"
    . $proteome->species . "'\n";
  print OUT "#Total number of proteins in proteome: "
    . $proteome->total_genome_proteins . "\n";
  print OUT "#<seq id> <alignment start> <alignment end> <envelope start> "
    . "<envelope end> <hmm acc> <hmm name> <type> <hmm start> <hmm end> "
    . "<hmm length> <bit score> <E-value> <clan>\n";

  my $pfamA = $self->pfamA;
  my $clan  = $self->clanMap;

  #Go through each domain and print info
  foreach my $domain (@$pfamA_domains) {
    my (
      $pfamseq_acc, $ali_start, $ali_end,
      $seq_start,   $seq_end,   $pfamA_acc,
      $model_start, $model_end, $domain_bits_score,
      $domain_evalue_score
      )
      = (
      $domain->[0], $domain->[1], $domain->[2], $domain->[3], $domain->[4],
      $domain->[5], $domain->[6], $domain->[7], $domain->[8], $domain->[9]
      );

    my $line = join( "\t",
      $pfamseq_acc,
      $ali_start,
      $ali_end,
      $seq_start,
      $seq_end,
      $pfamA->{$pfamA_acc}->{pfamA_acc},
      $pfamA->{$pfamA_acc}->{pfamA_id},
      $pfamA->{$pfamA_acc}->{type},
      $model_start,
      $model_end,
      $pfamA->{$pfamA_acc}->{model_length},
      $domain_bits_score,
      $domain_evalue_score,
      $clan->{$pfamA_acc} );
    print OUT "$line\n";

  }
  close OUT;

  #gzip the file
  my $gzip = $outfile . ".gz";
  gzip $outfile => $gzip
    or $self->logger->logdie("Problem with gzip: $GzipError");
  unlink($outfile);
}

sub submitToFarm {
  my ($self, $noJobs) = @_;
  
  my $rs = $self->pfamdb->getSchema->resultset('CompleteProteome')->search({});
  my $chunkSize = ceil($rs->count/$noJobs);
  
  #Now submit the jobs
  my $queue = 'production-rh6';
  my $resource = "-M3500 -R rusage[mem=3500]";
  my $memory = 3500;  
  my $fh = IO::File->new();

  $fh->open( "| bsub -q $queue  ".$resource." -o ".
              $self->options->{statusdir}."/protTSV.\%J.\%I.log  -JprotTSV\"[1-$noJobs]\"");
  $fh->print( "makeProteomeTSV.pl -chunk \$\{LSB_JOBINDEX\} -chunkSize $chunkSize ".
              " -releasedir ".$self->options->{releasedir}.
              " -statusdir ".$self->options->{statusdir}."\n");
  $fh->close;
  $self->logger->debug("Status is:".$self->statusFile."\n");
  while(! $self->statusCheck($self->statusFile, $noJobs)){
    $self->logger->info('Waiting for jobs to complete.');
    sleep(600);
  }
}

sub proteomeRange {
  my ($self)    = @_;
  my $chunk     = $self->options->{chunk};
  my $chunkSize = $self->options->{chunkSize};
 
  if ( !$self->statusCheck( $self->statusFile . ".$chunk.done" ) ) {

    if ( $chunk and $chunkSize ) {
      $self->logger->debug("Calculating proteome paging");

      my $proteomesAll =
        $self->pfamdb->getSchema->resultset('CompleteProteome')->search(
        {},
        {
          page => 1,
          rows => $chunkSize
        }
        );

      my $pager = $proteomesAll->pager;
      $self->logger->info(
        "Working on page $chunk out of " . $pager->last_page );

      my @proteomes = $proteomesAll->page($chunk)->all;

      $self->logger->debug("Searching proteome range, chunk $chunk.");

      my $filename = $self->statusFile . ".$chunk";

      $self->touchStatus($filename);
      open( S, '+<', $self->options->{statusdir} . '/' . $filename )
        or $self->logger->logdie("Could not open status file:[$!]");

      my %done;
      while (<S>) {
        if (/^(\S+)/) {
          $done{$1}++;
        }
      }
      p(%done);
      foreach my $p (@proteomes) {
        next if ( exists( $done{ $p->ncbi_taxid } ) );
        $self->logger->debug( "Working on " . $p->ncbi_taxid );
        $self->makeTSV( $p->ncbi_taxid );
        print S $p->ncbi_taxid . "\n";
      }
      close(S);
      $self->touchStatus( $self->statusFile . ".$chunk.done" );
    }
  }
}

sub speciesToTaxId {
  my ($self) = @_;

  my $row =
    $self->pfamdb->getSchema->resultset('CompleteProteome')
    ->search( { species => $self->options->{species} } );
  if ( $row and $row->ncbi_taxid ) {
    $self->options->{taxid} = $row->ncbi_taxid;
  }
  else {
    $self->logger->logdie( "Could not fines complete proteome for species "
        . $self->options->{species} );
  }
}

sub processOptions {
  my ( $self ) = @_;
  my ($statusDir, $species, $taxid, $releaseDir, $help, $chunk, $chunkSize, $all);
  my $options = {};

  my @opts = @ARGV;
  #Get and check the input parameters
  GetOptionsFromArray(
    \@opts, 
    "statusdir=s"  => \$statusDir,
    "releasedir=s" => \$releaseDir,
    "taxid=i"      => \$taxid,
    "species=s"    => \$species,
    "allVsall"     => \$all,
    "chunk=i"      => \$chunk,
    "chunkSize=i"  => \$chunkSize,
    "h|help"       => \$help
  );

  if ($help) {
    $self->options($options);
    return;
  }

  if($all or $chunkSize){
    if ( !$statusDir or !-d $statusDir) {
      $self->logger->logdie("No status directory passed in or it is not present $statusDir.....");
    }
  }
  
  if(!$releaseDir){
    $releaseDir = ".";
  }
  
  my $check = 0;
  $check++ if($taxid);
  $check++ if($species);
  
  if($check >1){
    $self->logger->logdie("You can only specify one of --species <species name> or --taxid <name>");
  }
  

  $options->{species}    = $species; 
  $options->{taxid}      = $taxid;
  $options->{statusdir}  = $statusDir;
  $options->{releasedir} = $releaseDir;
  $options->{chunk}      = $chunk;
  $options->{chunkSize}  = $chunkSize;
  $options->{allVsall}   = $all;

  $self->options($options);
}

1;
