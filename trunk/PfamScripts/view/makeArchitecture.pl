#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(get_logger :levels);
use Data::Dump qw(dump);
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess;

Log::Log4perl->init(
  \<<EOF
log4perl.rootLogger=DEBUG, SCREEN
# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);


my ($chunk, $chunkSize, $acc);
GetOptions( "chunk=i" => \$chunk,
            "chunkSize=i" => \$chunkSize,
            "acc=s" => \$acc) or die "Invalid option\n";



my $logger = get_logger();
$logger->level($DEBUG);

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}
my $dbh = $pfamDB->getSchema->storage->dbh;
$logger->debug("Got pfamlive database connection");


my %cachedArchs;
if($acc and $acc =~ /PF\d{5}/){
  my $pfamA = $pfamDB->getPfamData($acc);
  my @seqsRS = $pfamDB->getSchema->resultset('Pfamseq')->search(
    {
      "pfama_reg_full_significants.auto_pfamA" => $pfamA->auto_pfama,
      "pfama_reg_full_significants.in_full" => 1
    },
    {
      join => [qw(pfama_reg_full_significants)]
    }
    );
  
  my (@archs, %oldArchs);
  foreach my $seq (@seqsRS){
    unless(exists($oldArchs{ $seq->auto_architecture })){
      $oldArchs{$seq->auto_architecture}++;    
    }    
  }   
  print dump(%oldArchs);
  &updateArchiectures( \@seqsRS, $pfamDB, $logger, \%cachedArchs );
  
  foreach my $auto_arch (keys(%oldArchs)){
    my $arch = $pfamDB->getSchema->resultset('Architecture')->find( $auto_arch);
    next unless($arch);
    #Check that the type exmaple is still valid.
    my $seq = $pfamDB->getSchema->resultset('Pfamseq')->find( $arch->type_example);
    if($seq->auto_architecture != $arch->auto_architecture){
       my $row = $pfamDB->getSchema->resultset('Pfamseq')->search( { auto_architecture => $arch->auto_architecture},
                                                         { order_by => 'is_fragment' }  )->first;
       if($row and $row->auto_pfamseq){
          $arch->update({type_example => $row->auto_pfamseq});  
       }else{
          #$arch->delete;
          $logger->debug("Delete ".$arch->auto_architecture);
       }
    }
  }
  
  my %pfamAs;
  my $dbh = $pfamDB->getSchema->storage->dbh;
  
  my $pfamArchSth = $dbh->prepare(
     "REPLACE INTO pfamA_architecture (auto_pfamA, auto_architecture) "
    . " SELECT DISTINCT r.auto_pfamA, auto_architecture FROM pfamA_reg_full_significant r, pfamseq s "
    . " WHERE s.auto_pfamseq=r.auto_pfamseq AND in_full=1 AND auto_pfamA= ? " )
  or $logger->logdie(
    "Failed to prepare statment to update pfamA_architectures:" . $dbh->errstr );
 
  my $deleteArchSth = $dbh->prepare("DELETE FROM pfamA_architecture where auto_pfamA=?");

  foreach my $seq (@seqsRS){
      my @r = $pfamDB->getSchema->resultset("PfamaRegFullSignificant")->search( { auto_pfamseq => $seq->auto_pfamseq, in_full => 1} );
      if(@r){
        foreach my $d (@r){
          unless(exists($pfamAs{$d->auto_pfama})){
            $logger->debug("Updaing architecture for ".$d->pfama_id. ",".$d->in_full);
            $deleteArchSth->execute($d->auto_pfama);
            $pfamArchSth->execute($d->auto_pfama);          
            $pfamAs{$d->auto_pfama}++;
          }  
        }
      }    
  }
  $dbh->do("UPDATE architecture a SET no_seqs = (select count(*) from pfamseq s where s.auto_architecture=a.auto_architecture)");
  $dbh->do("UPDATE pfamA a set number_archs=(select count(*) from pfamA_architecture p where p.auto_pfamA=a.auto_pfamA)");
  exit;
}elsif($chunk and $chunkSize){

  my $rangeFrom = (($chunk-1) * $chunkSize)+1; 
  my $rangeTo   = (($chunk) * $chunkSize); 
  $logger->debug("Calculating architectures in the range of $rangeFrom to $rangeTo.");
#-------------------------------------------------------------------------------

my $currentSeq = $rangeFrom;
while($currentSeq < $rangeTo){ 
  $pfamDB->getSchema->txn_begin;
  my $nextCurrentSeq = ($currentSeq + 1000) > $rangeTo ? $rangeTo : $currentSeq + 1000; 
  $logger->debug("Working on $currentSeq to $nextCurrentSeq");
  my @seqsRS = $pfamDB->getSchema->resultset('Pfamseq')->search(
    {'me.auto_pfamseq' => [ -and => {'>=', $currentSeq },{'<=',$nextCurrentSeq }]}
      );   
  $currentSeq = $nextCurrentSeq;  
  &updateArchiectures( \@seqsRS, $pfamDB, $logger, \%cachedArchs );
  $pfamDB->getSchema->txn_commit;
}
}else{
   die "No range or accession entered.\n";
}


sub updateArchiectures {
  my ( $modSeqsRef, $pfamDB, $logger, $cachedArchsRef ) = @_;

  foreach my $seq (@$modSeqsRef) {
    #$logger->debug("Working on sequence:".$seq->pfamseq_acc);
    #PfamA region statement
    my $pfamaRegionsRef = $pfamDB->getPfamRegionsForSeq( $seq->pfamseq_acc );
    unless($pfamaRegionsRef){
      $seq->update( {auto_architecture => 0});
      next;
    }
    #$logger->debug("Got regions for sequence");
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
    if ( $cachedArchsRef->{$archStringAcc} ) {
      $arch = $cachedArchsRef->{$archStringAcc};
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
        if (!defined($archSeq) or $archSeq->is_fragment ) {

          #Yes, replace type example
          $arch = $arch->update(
            { type_example => $seq->auto_pfamseq } );
        }

        #if so update it the type example
      }
      unless ( defined( $seq->auto_architecture )
        and $seq->auto_architecture eq $arch->auto_architecture )
      {
        $seq->update(
          { auto_architecture => $arch->auto_architecture } );
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
      if(!defined($arch->type_example) or $arch->type_example == 0){
        $arch->update({type_example => $seq->auto_pfamseq});
      }
      $pfamDB->getSchema->txn_begin;

      #$logger->debug( "Got architecture|" . $arch->auto_architecture . "|" );
      $seq->update(
        { auto_architecture => $arch->auto_architecture } );
    }
    $cachedArchsRef->{$archStringAcc} = $arch;
  }
}


#Need to update the architecture table names as well as remove architectures that
#do not match anything in pfamseq any more....
  
my $archCountSth = $dbh->prepare("UPDATE architecture SET no_seqs = (select count(*) from pfamseq s where s.auto_architecture=?) where auto_architecture=?");

foreach my $archStr ( keys %cachedArchs){
  $archCountSth->execute($cachedArchs{$archStr}->auto_architecture, $cachedArchs{$archStr}->auto_architecture);  
}
