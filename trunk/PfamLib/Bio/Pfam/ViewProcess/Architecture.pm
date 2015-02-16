package Bio::Pfam::ViewProcess::Architecture;

use strict;
use warnings;
use Data::Printer;
use POSIX qw(ceil);
use Getopt::Long qw(GetOptionsFromArray);
use Log::Log4perl qw(:easy);
use Moose;
use Moose::Util::TypeConstraints;

use base 'Bio::Pfam::ViewProcess';

has 'cachedRef' => (
  is    => 'rw',
  isa   => 'HashRef',
  default => sub{ {} },
);

has 'statusFile' => (
  is  => 'ro',
  isa => 'Str',
  default => 'calArch',
);

sub BUILD {
  my( $self, $args ) = @_;
  
  
  $self->{config} = Bio::Pfam::Config->new;
  $self->{jobdb}  = Bio::Pfam::PfamJobsDBManager->new( %{ $self->config->pfamjobs } );
  unless($self->{jobdb}){
    $self->mailPfam( "Failed to run view process", "Could not get connection to the pfam_jobs database" );
  }
  
  Log::Log4perl->easy_init($DEBUG);
  my $logger = get_logger();
  
  $self->{logger} = $logger;

  $self->logger->debug("Got pfam_job db connection");
  $self->{pfamdb} = Bio::Pfam::PfamLiveDBManager->new( %{ $self->config->pfamliveAdmin } );
  
  unless ($self->{pfamdb}) {
    $self->mailPfam( "Failed to run view process", "View process failed as we could not connect to pfamlive" );
  }
  $self->logger->debug("Got pfamlive database connection");
  $self->logger->debug('Processing options');  
  $self->processOptions;
  return($self);
}

sub updateSingleFamily {
  my($self) = @_;
  my $pfamA  = $self->pfamdb->getPfamData($self->options->{acc});
  my @seqsRS = $self->pfamdb->getSchema->resultset('Pfamseq')->search(
    {
      "pfama_reg_full_significants.auto_pfamA" => $pfamA->auto_pfama,
      "pfama_reg_full_significants.in_full"    => 1
    },
    { join => [qw(pfama_reg_full_significants)] }
  );

  my ( @archs, %oldArchs );
  foreach my $seq (@seqsRS) {
    unless ( exists( $oldArchs{ $seq->auto_architecture } ) ) {
      $oldArchs{ $seq->auto_architecture }++;
    }
  }
  
  $self->debug(p(%oldArchs));
  $self->updateArchiectures( \@seqsRS );

  foreach my $auto_arch ( keys(%oldArchs) ) {
    my $arch = $self->pfamdb->getSchema->resultset('Architecture')->find($auto_arch);
    next unless ($arch);

    #Check that the type exmaple is still valid.
    my $seq =
      $self->pfamdb->getSchema->resultset('Pfamseq')->find( $arch->type_example );
    if ( $seq->auto_architecture != $arch->auto_architecture ) {
      my $row =
        $self->pfamdb->getSchema->resultset('Pfamseq')
        ->search( { auto_architecture => $arch->auto_architecture },
        { order_by => 'is_fragment' } )->first;
      if ( $row and $row->auto_pfamseq ) {
        $arch->update( { type_example => $row->auto_pfamseq } );
      }
      else {
        #$arch->delete;
        $self->logger->debug( "Delete " . $arch->auto_architecture );
      }
    }
  }

  my %pfamAs;
  my $dbh = $self->pfamdb->getSchema->storage->dbh;

  die;
#This takes ages and needs to be reworked!!!!
  my $pfamArchSth = $dbh->prepare(
        "REPLACE INTO pfamA_architecture (auto_pfamA, auto_architecture) "
      . " SELECT DISTINCT r.auto_pfamA, auto_architecture FROM pfamA_reg_full_significant r, pfamseq s "
      . " WHERE s.auto_pfamseq=r.auto_pfamseq AND in_full=1 AND auto_pfamA= ? "
    )
    or $self->logger->logdie(
    "Failed to prepare statment to update pfamA_architectures:"
      . $dbh->errstr );

  my $deleteArchSth =
    $dbh->prepare("DELETE FROM pfamA_architecture where auto_pfamA=?");

  foreach my $seq (@seqsRS) {
    my @r =
      $self->pfamdb->getSchema->resultset("PfamaRegFullSignificant")
      ->search( { auto_pfamseq => $seq->auto_pfamseq, in_full => 1 } );
    if (@r) {
      foreach my $d (@r) {
        unless ( exists( $pfamAs{ $d->auto_pfama } ) ) {
          $self->logger->debug(
            "Updaing architecture for " . $d->pfama_id . "," . $d->in_full );
          $deleteArchSth->execute( $d->auto_pfama );
          $pfamArchSth->execute( $d->auto_pfama );
          $pfamAs{ $d->auto_pfama }++;
        }
      }
    }
  }
  
  $dbh->do(
"UPDATE architecture a SET no_seqs = (select count(*) from pfamseq s where s.auto_architecture=a.auto_architecture)"
  );
  $dbh->do(
"UPDATE pfamA a set number_archs=(select count(*) from pfamA_architecture p where p.auto_pfamA=a.auto_pfamA)"
  );
}


sub updateSeqRange {
  my( $self ) = @_;
  my $chunk = $self->options->{chunk};
  my $chunkSize = $self->options->{chunkSize};
  
  if(!$self->statusCheck($self->statusFile.".$chunk")){
  
  my $rangeFrom = ( ( $chunk - 1 ) * $chunkSize ) + 1;
  my $rangeTo   = ( ($chunk) * $chunkSize );
  $self->logger->debug(
    "Calculating architectures in the range of $rangeFrom to $rangeTo.");

#-------------------------------------------------------------------------------

  my $currentSeq = $rangeFrom;
  while ( $currentSeq < $rangeTo ) {
    $self->pfamdb->getSchema->txn_begin;
    my $nextCurrentSeq =
      ( $currentSeq + 1000 ) > $rangeTo ? $rangeTo : $currentSeq + 1000;
    $self->logger->debug("Working on $currentSeq to $nextCurrentSeq");
        my @seqsRS = $self->pfamdb->getSchema->resultset('Pfamseq')->search(
      {
        'me.auto_pfamseq' =>
          [ -and => { '>=', $currentSeq }, { '<=', $nextCurrentSeq } ]
      }
    );
    $currentSeq = $nextCurrentSeq;
    $self->updateArchiectures( \@seqsRS );
    $self->pfamdb->getSchema->txn_commit;
  }
  #Note if you change the file name, fix the submit to farm
  $self->touchStatus($self->statusFile.".$chunk");
  }
  $self->touchStatus($self->statusFile.".$chunk.done");
}

sub clearAllArchitecture {
  my ($self) = @_;
  #Need set status direct
  if(!$self->statusCheck('clearedArchitecture')){
    $self->pfamdb->getSchema->resultset('Architecture')->delete;
    $self->touchStatus('clearedArchitecture');
  }
}

sub updateAllArchitecture {
  my ( $self ) = @_;
  
  my $dbh = $self->pfamdb->getSchema->storage->dbh;
  
  #Get the counts for everything
  $dbh->do(
    "UPDATE architecture a SET a.no_seqs = (select count(*) from pfamseq s where s.auto_architecture=a.auto_architecture)"
  );
  
  #Remove any architectures that are obsolete, i.e. have zero sequences.
  $dbh->do( "DELETE FROM architecture WHERE no_seqs = 0" );
  
  
   $dbh->do(
        "REPLACE INTO pfamA_architecture (auto_pfamA, auto_architecture) "
      . " SELECT DISTINCT r.auto_pfamA, auto_architecture FROM pfamA_reg_full_significant r, pfamseq s "
      . " WHERE s.auto_pfamseq=r.auto_pfamseq AND in_full=1"
    );
  
  #Now update the counts.
  $dbh->do( "UPDATE pfamA a set number_archs=(select count(*) from pfamA_architecture p where p.auto_pfamA=a.auto_pfamA)");
}


sub updateCountsForCached {
  my ($self) = @_;

  #Need to update the architecture table names as well as remove architectures that
  #do not match anything in pfamseq any more....

  my $dbh = $self->pfamdb->getSchema->storage->dbh;
  
  my $archCountSth =
    $dbh->prepare(
      "UPDATE architecture SET no_seqs = (select count(*) from pfamseq s where s.auto_architecture=?) where auto_architecture=?"
    );

  foreach my $archStr ( keys %{$self->cachedRef} ) {
    $archCountSth->execute(
      $self->cachedRef->{$archStr}->auto_architecture,
      $self->cachedRef->{$archStr}->auto_architecture
    );
  }
}

sub updateArchiectures {
  my ( $self, $modSeqsRef) = @_;

  my $pfamDB = $self->pfamdb;
  foreach my $seq (@$modSeqsRef) {

    #$logger->debug("Working on sequence:".$seq->pfamseq_acc);
    #PfamA region statement
    my $pfamaRegionsRef = $pfamDB->getPfamRegionsForSeq( $seq->pfamseq_acc );
    unless ($pfamaRegionsRef) {
      $seq->update( { auto_architecture => 0 } );
      next;
    }

    #$self->logger->debug("Got regions for sequence");
    #Detterming the architecture....Also add kristoffers bit about domain order!
    my @archStringAcc;
    my @archStringId;
    my $domOrder = 1;
    foreach
      my $region ( sort { $a->seq_start <=> $b->seq_start } @$pfamaRegionsRef )
    {
      $region->update( { domain_order => $domOrder } );
      push( @archStringAcc, $region->pfama_acc );
      push( @archStringId,  $region->pfama_id );
      $domOrder++;
    }

    my $arch;
    my $archStringAcc = join( ' ', @archStringAcc );
    if ( $self->cachedRef->{$archStringAcc} ) {
      $arch = $self->cachedRef->{$archStringAcc};
    }

    $arch =
      $pfamDB->getSchema->resultset('Architecture')
      ->find( { architecture_acc => join( ' ', @archStringAcc ) } );

    if ($arch) {
      unless ( $seq->is_fragment ) {

        #is the type example a fragment?
        my $archSeq =
          $pfamDB->getSchema->resultset('Pfamseq')
          ->find( { auto_pfamseq => $arch->type_example } );
        if ( !defined($archSeq) or $archSeq->is_fragment ) {

          #Yes, replace type example
          $arch = $arch->update( { type_example => $seq->auto_pfamseq } );
        }

        #if so update it the type example
      }
      unless ( defined( $seq->auto_architecture )
        and $seq->auto_architecture eq $arch->auto_architecture )
      {
        $seq->update( { auto_architecture => $arch->auto_architecture } );
      }
    }
    else {

      $pfamDB->getSchema->txn_commit;
      $arch = $pfamDB->getSchema->resultset('Architecture')->find_or_create(
        {
          architecture     => join( '~', @archStringId ),
          no_seqs          => 1,
          architecture_acc => $archStringAcc
        }
      );
      if ( !defined( $arch->type_example ) or $arch->type_example == 0 ) {
        $arch->update( { type_example => $seq->auto_pfamseq } );
      }
      $pfamDB->getSchema->txn_begin;

      #$logger->debug( "Got architecture|" . $arch->auto_architecture . "|" );
      $seq->update( { auto_architecture => $arch->auto_architecture } );
    }
    $self->cachedRef->{$archStringAcc} = $arch;
  }
}

sub updateAllClanArchitectures {
  my ($self) = @_;
  my @clanMembers = $self->pfamdb
                          ->getSchema
                            ->resultset('ClanMembership')
                              ->search( {},
                                        { columns  => [qw(me.auto_clan)],
                                          distinct => 1 });
  $self->_clanArchitecture(\@clanMembers);
}

sub updateClanArchitectures {
  my ($self, $autoPfamAs) = @_;
  my @clanMembers = $self->pfamdb
                          ->getSchema
                            ->resultset('ClanMembership')
                              ->search( { 'auto_pfama' => [ @$autoPfamAs ] },
                                        {  columns     => [qw(me.auto_clan)],
                                           distinct    => 1 });
  $self->_clanArchitecture(\@clanMembers);
}

sub _clanArchitecture {
  my ($self, $clans) = @_;
  
  my $dbh = $self->pfamdb->getSchema->storage->dbh;  
  my $clanArchSth = $dbh->prepare(
 "INSERT INTO clan_architecture (auto_clan, auto_architecture) ".
 " SELECT DISTINCT c.auto_clan, auto_architecture from clan_membership c, pfamA_reg_full_significant r, pfamseq s ".
 " WHERE s.auto_pfamseq=r.auto_pfamseq AND c.auto_pfamA=r.auto_pfamA AND in_full=1 AND c.auto_clan= ? ");

  my $clanUpdateNumArch = $dbh->prepare(
  "UPDATE clans c SET number_archs = (SELECT COUNT(DISTINCT auto_architecture) FROM clan_architecture a ".
  " WHERE c.auto_clan=a.auto_clan) where c.auto_clan= ? ");
  
  my $clanUpdateNumStructures = $dbh->prepare(
  "UPDATE clans c SET number_structures = (SELECT SUM(number_structures) ".
  "FROM clan_membership m , pfamA a WHERE m.auto_pfamA=a.auto_pfamA AND auto_clan=c.auto_clan) ".
  "WHERE c.auto_clan=?;");

  #Update the number of pdb_regions and arch in the clan table!
  foreach my $c (@$clans){
    my $auto_clan = $c->auto_clan->auto_clan;
    $self->pfamdb->getSchema->resultset('ClanArchitecture')->search({ auto_clan => $auto_clan })->delete;
    $clanArchSth->execute($auto_clan);
    $clanUpdateNumArch->execute($auto_clan);
    $clanUpdateNumStructures->execute($auto_clan);
  }
}

sub submitToFarm {
  my ($self, $noJobs) = @_;
  
  my $rs = $self->pfamdb->getSchema->resultset('Pfamseq')->search({});
  my $max = $rs->get_column('auto_pfamseq')->max;
  my $chunkSize = ceil($max/$noJobs);
  
  #Now submit the jobs
  my $queue = 'production-rh6';
  my $resource = "rusage[mem=2500000]";
  my $memory = 2500000;  
  my $fh = IO::File->new();
  $fh->open( "| bsub -q $queue  -M $memory -R $resource -o ".
              $self->options->{statusdir}."/arch.\%J.\%I.log  -Jarch\"[1-$noJobs]%70\"");
  $fh->print( "makeArchitecture.pl -chunk \$\{LSB_JOBINDEX\} -chunkSize $chunkSize -statusdir ".$self->options->{statusdir}."\n");
  $fh->close;
  $self->logger->debug("Status is:".$self->statusFile."\n");
  while(! $self->statusCheck($self->statusFile, $noJobs)){
    $self->logger->info('Waiting for jobs to complete.');
    sleep(600);
  }
}

sub processOptions {
  my ( $self ) = @_;
  my ($statusDir, $acc, $help, $chunk, $chunkSize );
  my $options = {};

  my @opts = @ARGV;
  #Get and check the input parameters
  GetOptionsFromArray(
    \@opts, 
    "statusdir=s" => \$statusDir,
    "acc=s"       => \$acc,
    "chunk=i"     => \$chunk,
    "chunkSize=i" => \$chunkSize,
    "h|help"      => \$help
  );

  if ($help) {
    $self->options($options);
    return;
  }

  if ( !$statusDir or !-d $statusDir) {
    $self->logger->logdie("No status directory passed in or it is not present $statusDir.....");
  }

  $options->{acc}       = $acc;
  $options->{statusdir} = $statusDir;
  $options->{chunk}     = $chunk;
  $options->{chunkSize} = $chunkSize;

  $self->options($options);
}

1;
