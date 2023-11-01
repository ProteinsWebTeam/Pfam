#!/usr/bin/env perl 
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

#Log::Log4perl->init( $config->productionLoc . "/Logs/log4perl.conf" );
#my $logger = Log::Log4perl->get_logger('fileLogger');
Log::Log4perl->easy_init($DEBUG);
my $logger = Log::Log4perl->get_logger();


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

my $pfamASth = $dbh->prepare(
  'select pfamA_acc from  pfamA a 
where num_seed != (
select count(pfamA_acc) from pfamA_reg_seed s where a.pfamA_acc=s.pfamA_acc)'
); 

$pfamASth->execute or $logger->logdie( $dbh->errstr );
my $res = $pfamASth->fetchall_arrayref;

my %pfamA;
foreach my $r (@$res) {
  $pfamA{ $r->[0] } = 1;
}
$logger->info("Finished getting list of Pfam families");

#-------------------------------------------------------------------------------
#Prepare the statement that we use to get the sequence information
my $pfamseqSth =
  $dbh->prepare("select pfamseq_acc, seq_version, sequence from pfamseq where pfamseq_acc = ?")
  or die $dbh->errstr;

my $pfamseqSeedSth = $dbh->prepare(
  "select distinct s.pfamseq_acc from pfamseq s, pfamA_reg_seed r 
where s.pfamseq_acc=r.pfamseq_acc and s.md5=r.md5 and pfamA_acc=?"
) or die $dbh->errstr; 

#-------------------------------------------------------------------------------
#Get all secondary accessions
$logger->info("Getting secondary accessions");

my $secSth = $dbh->prepare(
"select pfamseq.pfamseq_acc, seq_version, secondary_acc, md5 from pfamseq, secondary_pfamseq_acc 
where pfamseq.pfamseq_acc = secondary_pfamseq_acc.pfamseq_acc"
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
$logger->info("Preparing the pfamseq and uniprot queries");

my $newSeqSthPfamseq =
  $dbh->prepare("select pfamseq_acc, seq_version from pfamseq where md5 = ?")
  or die $dbh->errstr;


#-------------------------------------------------------------------------------
#Uniprot queries
my $uniprotSth = $dbh->prepare("select uniprot_acc, seq_version, sequence from uniprot where uniprot_acc = ?") or $logger->logdie($dbh->errstr);
my $newSeqSthUniprot = $dbh->prepare("select uniprot_acc, seq_version from uniprot where md5 = ?") or $logger->logdie($dbh->errstr);
my $uniprotSeedSth = $dbh->prepare("select distinct uniprot_acc from uniprot u, pfamA_reg_seed p where p.pfamseq_acc=u.uniprot_acc and p.md5=u.md5 and pfamA_acc=?");

$logger->info("Getting list of families that have mixed seeds");
#Get list of families that have mixed seeds
my $rpSeed = $dbh->prepare("select pfamA_acc from pfamA where rp_seed=0");
$rpSeed->execute() or die "Couldn't execute statement ".$rpSeed->errstr."\n";
my (%mixedSeed, $pfamA_acc);
$rpSeed->bind_columns(\$pfamA_acc);
while ($rpSeed->fetch()) {
  $mixedSeed{$pfamA_acc}=1;
}


my $familyIO = Bio::Pfam::FamilyIO->new;

FAM:
foreach my $fam ( sort { $a cmp $b } keys %pfamA ) {

  if(-d "$surgeryDir/$fam" or -e "$famDir/$fam/seed_surgery.log") {
    $logger->debug("$fam already undergone surgery");
    next;
  }
  $logger->debug("$fam needs seed surgery");

  #Get hash of seed seqs that have the same md5 in pfamA_reg_seed and pfamseq
  $pfamseqSeedSth->execute( $fam ) or die $dbh->errstr; 
  my $seedSeqsPfamseq = $pfamseqSeedSth->fetchall_arrayref;
  my $seedSeqsPfamseqHash;
  foreach my $r (@$seedSeqsPfamseq) {
    $seedSeqsPfamseqHash->{ $r->[0] }++;
  }

  #Get hash of seed seqs that have the same md5 in pfamA_reg_seed and uniprot;
  $uniprotSeedSth->execute( $fam ) or die $dbh->errstr;
  my $seedSeqsUniprot = $uniprotSeedSth->fetchall_arrayref;
  my $seedSeqsUniprotHash;
  foreach my $r (@$seedSeqsUniprot) {
    $seedSeqsUniprotHash->{ $r->[0] }++;
  }

  unless ( -d $famDir . '/' . $fam ){
    $logger->warn("$fam does not exist in the $famDir");
    next;
  }
  system( "mv $famDir/$fam $surgeryDir" ) and 
    $logger->logdie("Failed to move $famDir/$fam to  $surgeryDir:[$!]");
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

  my $mixedSeed=0;
  $mixedSeed=1 if(exists($mixedSeed{$fam}));

  #Find out which sequnces have changed in the alignment.
  my $needHMMAlign =
    verifySeedSeqs( $oldseed, $newseed, $seedSeqsPfamseqHash, $newSeqSthPfamseq, $oldMd5s,
    $secAccs, $descObj, $seedSeqsUniprotHash, $newSeqSthUniprot, $mixedSeed );

  #Those where the sequence has changed, the sequences need to aligned back
  appendNewSequences( $newseed, $finalseed ) if ($needHMMAlign);

  #Write out the new seed alignment
  open( NS, ">SEED" ) or $logger->logdie( "Could not open SEED:[$!]" );
  if ( $needHMMAlign and ( $finalseed->num_sequences > 1 ) ) {
    my $no_gaps = $finalseed->allgaps_columns_removed;
    $no_gaps->write_Pfam( \*NS );
  }
  else {
    my $no_gaps = $newseed->allgaps_columns_removed;
    $no_gaps->write_Pfam( \*NS );
  }
  close NS;

  if(-e "$surgeryDir/$fam/fixNestedLocation") {
      $logger->debug("$fam needs its nested location fixed, moving to $surgeryDir/fixNested");
      unless(-d "$surgeryDir/fixNested") {
          mkdir("$surgeryDir/fixNested", 0755) or $logger->logdie("Cannot mkdir $surgeryDir/fixNested, $!");
      }
      system("mv $surgeryDir/$fam $surgeryDir/fixNested") and $logger->logdie("Failed to move $fam dir into $surgeryDir/fixNested");
  }

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
    if(-s "$surgeryDir/$fam/SEED") {
      chdir($surgeryDir);
      $logger->debug("Moving $fam to $famDir");
      system( "mv $surgeryDir/$fam $famDir" ) and $logger->logdie("Failed to move $fam dir into $famDir");
    }
    else { #Seed is empty so move it to a separate dir
      $logger->debug("$fam has no sequences left in SEED, moving to $surgeryDir/noSEED");
      unless(-d "$surgeryDir/noSEED") {
        mkdir("$surgeryDir/noSEED", 0755) or $logger->logdie("Cannot mkdir $surgeryDir/noSEED, $!");
      }
      system("mv $surgeryDir/$fam $surgeryDir/noSEED") and $logger->logdie("Failed to move $fam dir into $surgeryDir/noSEED");
    }
  }
}

sub verifySeedSeqs {
  my ( $oldseed, $newseed, $seedSeqPfamseqHash, $newPfamseqSth,  $oldMd5s, $secAccs, $descObj, 
  $seedSeqsUniprotHash, $newUniprotSth, $mixedSeed) = @_;
  my $needHMMAlign = 0;

  my $nse;    #List of nse....

  #Log file per family to record what happens during seed surgery
  open( LOG, ">seed_surgery.log" )
    || $logger->logdie( "Failed to open seed_surgery.log file:[$!]");

  foreach my $seq ( $oldseed->each_seq ) {

    #Find seq in pfamseq with same md5
    my ( $newsubseq, $deleted, $realign, $pfamseqMd5Map );
    if($oldMd5s->{ $seq->id }){
      $newPfamseqSth->execute($oldMd5s->{ $seq->id });
      $pfamseqMd5Map = $newPfamseqSth->fetchrow_arrayref;  
    }


    #Check that sequence with same md5 is still in pfamseq;
    if ( $seedSeqPfamseqHash->{ $seq->id } ) {
      $logger->debug($seq->id." unchanged in pfamseq");
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
    elsif ( $oldMd5s->{ $seq->id } and defined($pfamseqMd5Map) ) {
      my $newAcc = $pfamseqMd5Map->[0];
      my $newVer = $pfamseqMd5Map->[1];

      $newsubseq = Bio::Pfam::SeqPfam->new(
        '-seq'     => $seq->seq,
        '-id'      => $newAcc,
        '-version' => $newVer,
        '-start'   => $seq->start,
        '-end'     => $seq->end,
        '-type'    => 'aligned',
        '-names'   => { 'acc' => $newAcc }
      );
      $logger->debug($seq->id." changed accession in pfamseq");
      
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
        $logger->debug($seq->id." changed accession in pfamseq");
      
      }
      else {
        if($mixedSeed) { #If mixed seed and sec acc have diff md5 then look in uniprot table for same md5
          $newsubseq=lookInUniprot($seq, $seedSeqsUniprotHash, $newSeqSthUniprot, $oldMd5s);
        }
        if(!$newsubseq) { #If not a mixed seed, or failed to find in uniprot, try and align with changed sec acc
        
          print LOG "Different MD5s, going to perform SW alignment with "
          . $secAccs->{ $seq->id }->{acc} . " from pfamseq\n";
          $pfamseqSth->execute( $secAccs->{ $seq->id }->{acc} );
          my $row = $pfamseqSth->fetchrow_arrayref;
          if ( !$row ) {
            print LOG "Failed to fetch seqeuence "
            . $secAccs->{ $seq->id }->{acc} . "\n";
            $logger->debug($seq->id." deleted");
            $deleted++;
          }
          else {
            replaceSequence( $seq->seq, $row, $nse );
            $needHMMAlign++;
            $realign = $row->[0] . "." . $row->[1];
            $logger->debug($seq->id." replaced by updated pfamseq sequence");
          }
        }
      }
    }    #Look to see if the version has been incremented
    else {
      if($mixedSeed) { #If mixed seed, look in uniprot for same md5 first
        $newsubseq=lookInUniprot($seq, $seedSeqsUniprotHash, $newSeqSthUniprot, $oldMd5s);
      }        
      if(!$newsubseq) {  #If not a mixed seed, or failed to find in uniprot, try and align updated seqeunce

        $pfamseqSth->execute( $seq->id );
        my $row = $pfamseqSth->fetchrow_arrayref;
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
          $logger->debug($seq->id." replaced by updated uniprot sequence");
          $needHMMAlign++;
          $realign = $row->[0] . "." . $row->[1];
        }
      }
    }
    
    if(!$newsubseq and $mixedSeed) { #Finally, if we get here, look for updated seq in uniprot and align
    
      $uniprotSth->execute( $seq->id );

      my $row = $uniprotSth->fetchrow_arrayref;
      if ( !$row ) { 
        print LOG "Failed to find replacement seqeuence for "
        . $seq->id
        . " in uniprot, must be deleted\n";
        $logger->debug($seq->id." deleted");

        $deleted++;
      }   
      else {
        print LOG "Sequence change - Performing SW for " . $seq->id . "\n";
        replaceSequence( $seq->seq, $row, $nse );
        $logger->debug($seq->id." replaced by updated uniprot sequence");
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
"water -asequence $$.seqa -bsequence $$.seqb -gapopen 10.0 -gapextend 0.5 -outfile $$.water"
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

sub appendNewSequences {
  my ( $ali1, $ali2 ) = @_;

  open( S, ">SEED.moretoadd" ) || $logger->logdie("Could not open SEED.moretoadd");
  $ali1->write_Pfam( \*S );
  close(S);
  
  #At the moment H3 does not support the --withali
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


sub lookInUniprot {
  my ($seq, $seedSeqsUniprotHash, $newSeqSthUniprot, $oldMd5s) = @_;

  my ($uniprotMd5Map);

  if ( $seedSeqsUniprotHash->{ $seq->id } ) { #Seq is unchanged in uniprot
    $logger->debug($seq->id." unchanged in uniprot");
    #This sequence is still okay so add to the hash
    my $newseq = Bio::Pfam::SeqPfam->new(
      '-seq'     => $seq->seq,
      '-id'      => $seq->id,
      '-version' => $seq->seq_version,
      '-start'   => $seq->start,
      '-end'     => $seq->end,
      '-type'    => 'aligned',
      '-names'   => { 'acc' => $seq->accession_number() }
    );
    return($newseq);
  }
  elsif($oldMd5s->{ $seq->id }){  #Look to see if there is a seq in uniprot with the same md5 as the old one
      $newSeqSthUniprot->execute($oldMd5s->{ $seq->id });
      $uniprotMd5Map = $newSeqSthUniprot->fetchrow_arrayref;
    
      if (defined($uniprotMd5Map) ) {
        my $newAcc = $uniprotMd5Map->[0];
        my $newVer = $uniprotMd5Map->[1];

        my $newseq = Bio::Pfam::SeqPfam->new(
          '-seq'     => $seq->seq,
          '-id'      => $newAcc,
          '-version' => $newVer,
          '-start'   => $seq->start,
          '-end'     => $seq->end,
          '-type'    => 'aligned',
          '-names'   => { 'acc' => $newAcc }
       );
       $logger->debug($seq->id." changed accession in uniprot");
       return($newseq); 
     }
   } 
   else {
     return 0;
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

