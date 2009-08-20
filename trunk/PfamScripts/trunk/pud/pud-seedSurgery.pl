#!/software/bin/perl 
#
# The aim of this program is to identify any SEED alignments that have out-of-date
# seqeunces in them and will try and fix the SEED alignments in the cases where the sequences
# has changed.  It checks every family out into the same directory
#

use strict;
use warnings;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use File::Copy;
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::AlignPfam;
use Bio::LocatableSeq;

my ( $help, $surgeryDir, $famDir, $oldSeedSeqMd5s );

GetOptions(
  "help"       => \$help,
  "surgery=s"  => \$surgeryDir,
  "families=s" => \$famDir,
  "md5file=s"  => \$oldSeedSeqMd5s
);

help() if ($help);

my $config = Bio::Pfam::Config->new;

Log::Log4perl->init( $config->productionLoc . "/Logs/log4perl.conf" );
my $logger = Log::Log4perl->get_logger('fileLogger');

$logger->debug( "Using database:" . $config->pfamlive->{database} );
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

my $pwd = getcwd;

unless ( $famDir and -d $famDir ) {
  $logger->logdie("You need to provde a surgery dir or it does not exist");
}

unless ( $surgeryDir and -d $surgeryDir ) {
  $logger->logdie("You need to provde a surgery dir or it does not exist");
}

#-------------------------------------------------------------------------------
# Read in the old md5file;

my $oldMd5s;

open( MD5, $oldSeedSeqMd5s )
  or $logger->logdie("Could not open $oldSeedSeqMd5s:[$!]");
while (<MD5>) {
  my ( $k, $v ) = split( /\s+/, $_ );
  $oldMd5s->{$k} = $v;
}
close(MD5);

my $dbh = $pfamDB->getSchema->storage->dbh;

#-------------------------------------------------------------------------------
# Which families need surgery?
$logger->info('Getting list of families that need to be updated.');

my $pfamSth = $dbh->prepare(
  'select auto_pfamA, pfamA_acc from  pfamA a 
where num_seed != (
select count(auto_pfamA) from pfamA_reg_seed s where a.auto_pfamA=s.auto_pfamA)'
);

$pfamSth->execute or $logger->logdie( $dbh->errstr );
my $res = $pfamSth->fetchall_arrayref;

my %pfamA;
foreach my $r (@$res) {
  $pfamA{ $r->[1] } = $r->[0];
}
$logger->info("Finished getting list of Pfam families");

#-------------------------------------------------------------------------------
#Prepare the statemnet that we use to get the sequence information
my $seqSth =
  $dbh->prepare("select pfamseq_acc, seq_version, sequence from pfamseq where pfamseq_acc = ?")
  or die $dbh->errstr;

my $seedSth = $dbh->prepare(
  "select distinct pfamseq_acc from pfamseq s, pfamA_reg_seed r 
where s.auto_pfamseq=r.auto_pfamseq and auto_pfamA=?"
) or die $dbh->errstr;

#-------------------------------------------------------------------------------
#Get all secondary accessions
$logger->info("Getting secondary accessions");

my $secSth = $dbh->prepare(
"select pfamseq_acc, seq_version, secondary_acc, md5 from pfamseq, secondary_pfamseq_acc 
where pfamseq.auto_pfamseq = secondary_pfamseq_acc.auto_pfamseq"
) or die $dbh->errstr;

$secSth->execute or $dbh->errstr;

my $secAccs;
$res = $secSth->fetchall_arrayref;
foreach my $r (@$res) {
  $secAccs->{ $r->[2] }->{acc}      = $r->[0];
  $secAccs->{ $r->[2] }->{version}  = $r->[1];
  $secAccs->{ $r->[2] }->{md5}      = $r->[3];
}
$logger->info("Finished secondary accession query");

#-------------------------------------------------------------------------------
#Get a list of new pfamseq Accessions and their md5 chechsums.
$logger->info("Getting (new) pfamseq accessions");

my $newSeqSth =
  $dbh->prepare("select pfamseq_acc, seq_version, md5 from pfamseq")
  or die $dbh->errstr;
$newSeqSth->execute or die $dbh->errstr;
$res = $newSeqSth->fetchall_arrayref;
my $newSeqs;

foreach my $r (@$res) {
  $newSeqs->{ $r->[2] }->{version} = $r->[1];
  $newSeqs->{ $r->[2] }->{acc}     = $r->[0];
}

$logger->info("Finished getting (new) pfamseq accessions");

#-------------------------------------------------------------------------------

my $familyIO = Bio::Pfam::FamilyIO->new;

FAM:
foreach my $fam ( sort { $a cmp $b } keys %pfamA ) {
  $logger->debug("$fam needs seed surgery");

  $seedSth->execute( $pfamA{$fam} ) or die $dbh->errstr;
  my $seedSeqs = $seedSth->fetchall_arrayref;
  my $seedSeqsHash;
  foreach my $r (@$seedSeqs) {
    $seedSeqsHash->{ $r->[0] }++;
  }

  if ( -d $surgeryDir . '/' . $fam ) {
    $logger->warn("$fam looks like it is already undergoing surgery");
  }

  unless ( -d $famDir . '/' . $fam ){
    $logger->warn("$fam does not exist in the $famDir");
    next;
  }
  move( $famDir . "/$fam", $surgeryDir . "/$fam" ) or 
    $logger->logdie("Failed to move $fam in to $surgeryDir:[$!]");
  chdir("$surgeryDir/$fam")
    or $logger->logdie( "Could not change into $surgeryDir/$fam:[$!]" );

  my $descObj;
  eval{
    $descObj = $familyIO->parseDESC('DESC');
  };
  if($@){
    system('touch FailedToParseDESC');
    next;  
  }
  $logger->debug("reseeding $fam ...\n");

  #Read in the old seed alignment
  my $oldseed = Bio::Pfam::AlignPfam->new;
  open( SEED, "SEED" ) || $logger->logdie( "$fam/SEED can't be opened: [$!]" );
  $oldseed->read_selex( \*SEED );    # read_Pfam barfs at #=RF line
  close(SEED);

  #Move the SEED alignment sideways
  move( "SEED", "SEEDXSUR" ) or $logger->logdie( "Could not move SEED to SEEDXSUR:[$!]");

  #Make some more alignment objects
  my $newseed   = Bio::Pfam::AlignPfam->new;
  my $finalseed = Bio::Pfam::AlignPfam->new;

  #Find out which sequnces have changed in the alignment.
  my $needHMMAlign =
    verifySeedSeqs( $oldseed, $newseed, $seedSeqsHash, $newSeqs, $oldMd5s,
    $secAccs, $descObj );

  #Those where the sequence has changed, the sequences need to aligned back
  appendNewSequences( $newseed, $finalseed ) if ($needHMMAlign);

  #Write out the new seed alignment
  open( NS, ">SEED" ) or $logger->logdie( "Could not open SEED:[$!]" );
  if ( $needHMMAlign and ( $finalseed->no_sequences > 1 ) ) {
    my $no_gaps = $finalseed->allgaps_columns_removed;
    $no_gaps->write_Pfam( \*NS );
  }
  else {
    my $no_gaps = $newseed->allgaps_columns_removed;
    $no_gaps->write_Pfam( \*NS );
  }
  close NS;

#-------------------------------------------------------------------------------
#Need to double check these families.
  if ( defined( $descObj->NESTS ) ) {
    system "add_match_states" and warn "Failed to add match stated to family\n";
    $logger->info(
      "*** You should check the RF line to make sure all is OK ***");
  }

#-------------------------------------------------------------------------------
#Move those families that do not need surgery (as only names have changes into the non-surgery directory.
  if ( !$needHMMAlign and !defined( $descObj->NESTS ) ) {
    $logger->debug("Moving $fam to $famDir");
    move( "$surgeryDir/$fam", "$famDir/$fam" )
      or die "Failed to move $fam dir into $famDir\n";
  }
}

sub verifySeedSeqs {
  my ( $oldseed, $newseed, $seedSeqHash, $newSeqs, $oldMd5s, $secAccs,
    $descObj ) = @_;
  my $needHMMAlign = 0;

  my $nse;    #List of nse....

  #Log file per family to record what happens during seed surgery
  open( LOG, ">seed_surgery.log" )
    || $logger->logdie( "Failed to open seed_surgery.log file:[$!]");

  #Go through each sequence and see if it is still in pfamseq.
  foreach my $seq ( $oldseed->each_seq ) {

    my ( $newsubseq, $deleted, $realign );

    #Check that sequence is still in pfamseq;
    if ( $seedSeqHash->{ $seq->id } ) {
      $logger->debug($seq->id." unchanged");
      #This sequence is still okay so add to the hash
      $newsubseq = Bio::Pfam::SeqPfam->new(
        '-seq'     => $seq->seq,
        '-id'      => $seq->id,
        '-version' => $seq->seq_version,
        '-start'   => $seq->start,
        '-end'     => $seq->end,
        '-type'    => 'aligned',
        '-names'   => { 'acc' => $seq->accession_number() }
      );

    } #Look to see if there is a sequence that is the same based on MD5 checksum.
    elsif ( $oldMd5s->{ $seq->id } and $newSeqs->{ $oldMd5s->{ $seq->id } } ) {
      my $newAcc = $newSeqs->{ $oldMd5s->{ $seq->id } }->{acc};
      my $newVer = $newSeqs->{ $oldMd5s->{ $seq->id } }->{version};

      $newsubseq = Bio::Pfam::SeqPfam->new(
        '-seq'     => $seq->seq,
        '-id'      => $newAcc,
        '-version' => $newVer,
        '-start'   => $seq->start,
        '-end'     => $seq->end,
        '-type'    => 'aligned',
        '-names'   => { 'acc' => $newAcc }
      );
      $logger->debug($seq->id." changed accession");
      
    }
    elsif ( $secAccs->{ $seq->id } ) {

      #Has the seequence accession just become as secondary accession?
      #Check that the regions are the same sequence
      if ( $oldMd5s->{ $seq->id } eq $secAccs->{ $seq->id }->{md5} ) {

        $newsubseq = Bio::Pfam::SeqPfam->new(
          '-seq'     => $seq->seq,
          '-id'      => $secAccs->{ $seq->id }->{acc},
          '-version' => $secAccs->{ $seq->id }->{version},
          '-start'   => $seq->start,
          '-end'     => $seq->end,
          '-type'    => 'aligned',
          '-names'   => { 'acc' => $secAccs->{ $seq->id }->{acc} }
        );
        $logger->debug($seq->id." changed accession");
      
      }    #Sequence is different, going to align
      else {
        
        print LOG "Differnet MD5s, going to perform SW alignment with "
          . $secAccs->{ $seq->id }->{acc} . "\n";
        $seqSth->execute( $secAccs->{ $seq->id }->{acc} );
        my $row = $seqSth->fetchrow_arrayref;
        if ( !$row ) {
          print LOG "Failed to fetch seqeuence "
            . $secAccs->{ $seq->id }->{acc} . "\n";
          $logger->debug($seq->id." deleted");
            
        }
        else {
          replaceSequence( $seq->seq, $row, $nse );
          $needHMMAlign++;
          $realign = $row->[0] . "." . $row->[1];
          $logger->debug($seq->id." replaced by updated sequence");
      
        }
      }
    }    #Look to see if the version has been incremented
    else {
      $seqSth->execute( $seq->id );
      my $row = $seqSth->fetchrow_arrayref;
      if ( !$row ) {
        print LOG "Failed to find replacement seqeuence for "
          . $seq->id
          . ", must be deleted\n";
        $logger->debug($seq->id." deleted");
          
        $deleted++;
      }
      else {
        print LOG "Sequence change - Performing SW for " . $seq->id . "\n";
        replaceSequence( $seq->seq, $row, $nse );
        $logger->debug($seq->id." replaced by updated sequence");
        $needHMMAlign++;
        $realign = $row->[0] . "." . $row->[1];
      }
    }

  #Now add the new sequence object to the newseed unless it already contains it.
    if ($newsubseq) {
      my $uid =
          $newsubseq->id . "."
        . $newsubseq->version . "/"
        . $newsubseq->start . "-"
        . $newsubseq->end;
      if ( $nse->{$uid} ) {
        print LOG "$uid already in the SEED alignment, skipping\n";
      }
      else {
        $newseed->add_seq($newsubseq);
        $nse->{$uid}++;
      }
    }

    if ( defined( $descObj->NESTS ) ) {
      foreach my $n ( @{ $descObj->NESTS } ) {
        if ( $n->{seq} =~ /(\S+)\.(\d+)/ ) {
          my $nSeq = $1;
          my $nVer = $2;
          if ( $nSeq eq $seq->id ) {
            if ($deleted) {
              open( FN, ">>fixNestedLocation" )
                or $logger->logdie("Could not open fixNestedLocation:[$1]");
              print FN $seq->id . " deleted\n";
              close(FN);
            }
            elsif ($realign) {
              open( FN, ">>fixNestedLocation" )
                or $logger->logdie("Could not open fixNestedLocation:[$1]");
              print FN $seq->id . " replaced with $realign\n";
              close(FN);
            }
          }
        }
        else {
          $logger->logdie( "Did not regex " . $n->{seq} );
        }
      }
    }
  }
  close(LOG);
  return ($needHMMAlign);
}

#-------------------------------------------------------------------------------
#In theory nothing needs changing.

sub replaceSequence {
  my ( $oldseq, $newSeqRowRef, $nse ) = @_;

  #Old sequence is aligned, so we need to remove .|-;
  $oldseq = uc($oldseq);    #Make upper case
  $oldseq =~ s/[^A-Z]//g;   #remove non amino-acid.

  #Write out the sequecnes so we can make an alignment out of them
  open( SEQA, ">$$.seqa" ) || $logger->logdie("Could not open $$.seqa:[$!]");
  open( SEQB, ">$$.seqb" ) || $logger->logdie("Could not open $$.seqb:[$!]");
  print SEQA ">oldseq\n" . $oldseq . "\n";    #prints out the domain sequence
  print SEQB ">newseq\n"
    . $newSeqRowRef->[2]
    . "\n";    #prints out the whole of the new sequence.
  close(SEQA);
  close(SEQB);

  #Now perform the SW alingment - uses the EMBOSS binary.
  system(
"/software/pubseq/bin/emboss/bin/water -asequence $$.seqa -bsequence $$.seqb -gapopen 10.0 -gapextend 0.5 -outfile $$.water"
  ) and $logger->logdie("Failed to run smith-waterman:[$!]");

  #Now parse the results of SW. We want the start end points of the new sequence
  my $start = 9999999999;
  my $end   = 0;
  open( WATER, "$$.water" )
    || $logger->logdie( "could not open the smith-waterman result file:[$!]" );
  while (<WATER>) {
    if (/^newseq\s+(\d+)\s+\S+\s+(\d+)/) {
      my $this_start = $1;
      my $this_end   = $2;
      $end   = $this_end   if ( $this_end > $end );
      $start = $this_start if ( $this_start < $start );
    }
  }
  close(WATER);

  #Make NSE once as we will using it several times.
  my $newNse = $newSeqRowRef->[0] . "." . $newSeqRowRef->[1] . "/$start-$end";

#If this sequence is not already in the alignment, add it in directly to the fasta file that
#will be aligned back!
  if ( !$nse->{$newNse} ) {
    my $newDomSeq =
      substr( $newSeqRowRef->[2], $start - 1, ( $end - $start + 1 ) );
    open( NEW, ">>$$.newseq.fasta" );
    print NEW ">$newNse\n$newDomSeq\n";
    close(NEW);

    # add it to the list of sequences in the alignment
    $nse->{"$newNse"} = 1;
  }

  #Now remove the sequence files and alignment file
  unlink("$$.seqa")  || $logger->logdie("Could not unlink $$.seqa:[$!]\n");
  unlink("$$.seqb")  || $logger->logdie("Could not unlink $$.seqb:[$!]\n");
  unlink("$$.water") || $logger->logdie("Could not unlink $$.water[$!]\n");
}

#-------------------------------------------------------------------------------
#In theory nothing needs changing.

sub appendNewSequences {
  my ( $ali1, $ali2 ) = @_;

  open( S, ">SEED.moretoadd" ) || $logger->logdie("Could not open SEED.moretoadd");
  $ali1->write_Pfam( \*S );
  close(S);
  
  #At the moment H£ does not support the --with
  unless(-e "HMM.2"){
   system( $config->hmmer3bin."/hmmconvert -2 HMM > HMM.2") 
      and $logger->logdie("Could not convert HMM to HMMER2 format");
  }
  
  if ( -s "$$.newseq.fasta" && -s "SEED.moretoadd" ) {
    system(
$config->hmmer2bin."/hmmalign -q --withali SEED.moretoadd -o SEED.addedmore HMM.2 $$.newseq.fasta"
    ) and $logger->logdie("hmmalign failied");
    open( S, "SEED.addedmore" ) or $logger->logdie("Could not open SEED.addedmore:[$!]");
    $ali2->read_selex( \*S );
    close(S);
    $ali2->map_chars( '\-', '.' );
    $ali2->uppercase();
  }
  elsif ( -s "$$.newseq.fasta" ) {
    system($config->hmmer2bin."/hmmalign -q -o SEED.addedmore HMM.2 $$.newseq.fasta")
      and $logger->logdie("hmmalign failied to run:[$!]");
    open( S, "SEED.addedmore" ) or $logger->logdie("Could not open SEED.addedmore:[$!]");
    $ali2->read_selex( \*S );
    close(S);
    $ali2->map_chars( '\-', '.' );
    $ali2->uppercase();
  }
  else {
    $logger->warn("Failed to make new SEED");
  }

}

sub help {
  print <<EOF;
	
	The aim of this program is to identify any SEED alignments that have out-of-date
	seqeunces in them and will try and fix the SEED alignments in the cases where the sequences
	has changed.  It checks every family out into the same directory
 
	Usage $0 -h pfamdb2a -u pfamro -P 3303 -p pass -sd /lustre/pfam/pfam/Production/SURGERY -nsd /lustre/pfam/pfam/Production/NON_SURGERY
	
Where:	
	-help 	=> help, 
	-u   => pfamdb user,
	-h   => pfamdb_host,
	-P   => pfamdb_port,
	-sd  => surgery directory including path,
	-nsd => non-surgery directory
	
EOF
  exit(1);

}

