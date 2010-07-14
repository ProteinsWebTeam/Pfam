#!/software/bin/perl

use strict;
use warnings;
use Getopt::Long;
use Compress::Zlib;
use File::Copy;
use Cwd;
use LWP::UserAgent;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::PfamJobsDBManager;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $config = Bio::Pfam::Config->new;

my ( $help, $newRelease, $oldRelease, $relDir );

GetOptions(
  "new=s"    => \$newRelease,
  "old=s"    => \$oldRelease,
  "relDir=s" => \$relDir,
  "h"        => \$help
);

if ( !$newRelease || !$oldRelease ) {
  &help;
}

unless ($relDir) {
  $relDir = '.';
}

#This takes the input release numbers and checks that the are sensible.
#If they are, then it make the release directory.
$logger->info("Checking release numbers");
my ( $major, $point, $old_major, $old_point ) =
  checkReleaseNumbers( $newRelease, $oldRelease );
$logger->info("Making release dir");
unless ( -e "$relDir/$major.$point" ) {
  mkdir("$relDir/$major.$point")
    or
    $logger->logdie("Could not make release directory $relDir/$major.$point");
}
my $thisRelDir = "$relDir/$major.$point";
$logger->info("Made $thisRelDir");

#Can we connect to the PfamLive database?
$logger->info("Going to connect to the live database\n");

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}

$logger->debug("Got pfamlive database connection");
my $dbh = $pfamDB->getSchema->storage->dbh;

my $jobsDB = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );
unless ($jobsDB) {
  Bio::Pfam::ViewProcess::mailPfam( "Failed to run view process",
    "Could not get connection to the pfam_jobs database" );
}

#Now make sure that all of the view processes have completed
my $noUnFinishedJobs = $jobsDB->getSchema->resultset('JobHistory')->search(
  {
    -and => [
      status   => { '!=', 'DONE' },
      status   => { '!=', 'KILL' },
      job_type => [qw(clan family)]
    ],
  }
);

if ( $noUnFinishedJobs != 0 ) {
  $logger->logdie("There are currently $noUnFinishedJobs in the jobs database");
}
else {
  $logger->info("All view processes are complete!");
}

#Get the latest userman.txt and relnotes.txt from the ftp site.
unless ( -s "$thisRelDir/relnotes.txt" and -s "$thisRelDir/userman.txt" ) {
  $logger->info(
    "Getting text files from $old_major.$old_point from the ftp site");
  getTxtFiles($thisRelDir);
}
$logger->info("Got relnotes and userman");

#Get the pfamseq files required for the release
# 1. pfamseq
# 2. sprot.dat
# 3. trembl.dat
# 4. genpept.fa
# 5. metaseq
# 6. ncbi

my $pfamseqdir = "/lustre/pfam/pfam/Production/pfamseq";
my ( $numSeqs, $numRes );
foreach my $f (qw(pfamseq uniprot_sprot.dat uniprot_trembl.dat metaseq ncbi)) {
  unless ( -s "$thisRelDir/$f" ) {

#($numSeqs, $numRes) = checkPfamseqSize($pfamseqdir, $pfamDB) if($f eq 'pfamseq');
    $logger->info("Fetching the the sequence files");
    getPfamseqFiles( $thisRelDir, $pfamseqdir );
  }
}

unless ( -s "$thisRelDir/Pfam-A.full" and -s "$thisRelDir/Pfam-A.seed" ) {
  #makePfamAFlat( $thisRelDir, $pfamDB );
}

#foreach my $f ( "$thisRelDir/Pfam-A.seed", "$thisRelDir/Pfam-A.full" ) {
#  $logger->info("Checking format of $f");
#  checkflat($f);
#}
#
unless ( -s "$thisRelDir/Pfam-B" and -s "$thisRelDir/Pfam-B.fasta" ) {

  #TODO - uncomment this section
  makePfamBFlat( $thisRelDir, $pfamDB );
}

#foreach my $f ("$thisRelDir/Pfam-B") {
#  $logger->info("Checking format of $f");
#  checkflat_b($f);
#}

#Make the stats for the release
unless ( -s "$thisRelDir/stats.txt" ) {
  makeStats("$thisRelDir");
}
$logger->info("Maded stats!");

#Make the indexes HMMs
foreach my $f (qw(Pfam-A.hmm)) {
  $logger->info("Checking HMM flatfile $f");
  my $version = $pfamDB->getSchema->resultset('Version')->search()->first;
  open( HMM,
        "checkhmmflat.pl -v -f $thisRelDir/$f -hmmer "
      . $version->hmmer_version
      . "|" )
    or $logger->logdie( "Could not run checkhmmfalt.pl -v -file $f -hmmer "
      . $version->hmmer_version
      . "[$!]" );
  my $error;
  while (<HMM>) {
    $error = 1;
    $logger->warn("ERROR with format of $f:[$!]");
  }

  $logger->logdie("Error found in $f") if ($error);
  unless ( -s "$thisRelDir/$f.bin" ) {

    #Press and convert the files.
    $logger->debug("Making Pfam-A.hmm.bin");

    system( $config->hmmer3bin . "/hmmpress -f $thisRelDir/$f" )
      and $logger->logdie("Failed to run hmmpress:[$!]");

    system( $config->hmmer3bin
        . "/hmmconvert -b $thisRelDir/$f > $thisRelDir/$f.bin " )
      and $logger->logdie("Failed to run hmmconvert:[$!]");
  }

  unless ( -e "$thisRelDir/$f.2" ) {
    system( $config->hmmer3bin
        . "/hmmconvert -2 $thisRelDir/$f > $thisRelDir/$f.2 " )
      and $logger->logdie("Failed to run hmmconvert:[$!]");

  }

  $logger->info("Checked $f and made $f.bin");
}

#Make Pfam-A.scan.dat
unless ( -s "$thisRelDir/Pfam-A.hmm.dat" ) {
  $logger->info("Making Pfam-A.hmm.dat");
  &makePfamAScanFile( "$thisRelDir", $pfamDB );
}
$logger->info("Made Pfam-A.scan.dat");

unless ( -s "$thisRelDir/active_site.dat" ) {
  $logger->info("Making active_site.dat");
  &makeActiveSiteDat( "$thisRelDir", $pfamDB );
}
$logger->info("Made activesite.dat");

unless ( -s "$thisRelDir/Pfam-B.hmm" ) {
  $logger->info("Making Pfam-B.hmm");
  &makePfamBHmms("$thisRelDir");
}

$logger->info("Made Pfam-B.hmm and Pfam-B.hmm.dat");

#Index the fasta files!
foreach my $f (qw(Pfam-A.fasta Pfam-B.fasta)) {
  unless ( -s "$thisRelDir/$f.phr" ) {
    my $pwd = cwd;
    chdir("$thisRelDir")
      || $logger->logdie("Could not change into $thisRelDir:[$!]");
    $logger->info("Indexing $f (formatdb)");
    system("formatdb -p T -i $f")
      and $logger->logdie("Failed to index $f:[$!]");
    chdir("$pwd") || $logger->logdie("Could not change into $pwd:[$!]");
    $logger->info("");
  }
  $logger->info("Indexed $f");
}

#Make the Pfam-C file......
unless ( -s "$thisRelDir/Pfam-C" ) {
  $logger->info("Making Pfam-C");
  system("make_pfamC.pl")
    and $logger->logdie("Failed to run make_pfamC.pl:[$!]");
}
$logger->info("Made Pfam-C");

$logger->info(
"Run the following SQL statment: select pfamseq_acc, seq_version, crc64, md5, pfamA_acc,"
    . " seq_start, seq_end from pfamA a, pfamA_reg_full_significant r, pfamseq s "
    . " where s.auto_pfamseq=r.auto_pfamseq and a.auto_pfamA=r.auto_pfamA and in_full=1"
    . "  into outfile '/tmp/pdbeDataFile1.txt'" );

$logger->info(
"Run the following SQL statment:select pfamA_acc, clan_acc, clan_id, pfamA_id, description"
    . " from pfamA a  left join clan_membership m on a.auto_pfamA=m.auto_pfamA "
    . "left join clans c on m.auto_clan=c.auto_clan into outfile '/tmp/pdbeDataFile2.txt'"
);

unless ( -s "$thisRelDir/swisspfam" ) {
  $logger->info("Going to make swisspfam\n");
  makeSwissPfam( $thisRelDir, $pfamseqdir );
}

exit;    #TODO - remove this exit

#Dump the database.
$logger->info("Finished with the database lock");

exit;

sub checkPfamseqSize {
  my ( $pfamseqdir, $pfamDB ) = @_;
  my ( $residues, $sequences );

  $logger->info("Running seqstat on pfamseq");

  # Use seqstat to make stats!
  open( TMP, "seqstat $pfamseqdir/pfamseq |" )
    or $logger->logdie("Failed to run seqstat on $pfamseqdir/pfamseq:[$!]");
  while (<TMP>) {
    if (/^Total \# residues:\s+(\d+)/)   { $residues  = $1; }
    if (/^Number of sequences:\s+(\d+)/) { $sequences = $1; }
  }

  my $sequencesRDB = $pfamDB->getSchema->resultset('Pfamseq')->search( {} );

  unless ( $sequencesRDB == $sequences ) {
    $logger->logdie(
"Number of sequences in the database [$sequencesRDB] and in the pfamseq [$sequences] file do not tally"
    );
  }
  $logger->info("Number of sequences on disk and in the RDB are the same!");

  my $residuesRDB = $pfamDB->getSchema->resultset('Pfamseq')->search(
    {},
    {
      select => [ { sum => 'length' } ],
      as     => ['number_residues']
    }
  );

  unless ( $residues == $residuesRDB->first->get_column('number_residues') ) {
    $logger->logdie( "Number of residues in the database ["
        . $residuesRDB->first->get_column('number_residues')
        . "] and in the pfamseq [$residues] file do not tally" );
  }
  $logger->info("Number of residues on disk and in the RDB are the same!");
  return ( $sequences, $residues );
}

sub makeStats {
  my $releasedir = shift;
  $logger->info("Making stats.\n");
  system(
"flatfile_stats.pl $releasedir/Pfam-A.full $releasedir/Pfam-B > $releasedir/stats.txt"
    )
    and $logger->logdie(
"Failed to run flatfile_stats.pl $releasedir/Pfam-A.full $releasedir/Pfam-B:[$!]"
    );
  if ( -s "$releasedir/stats.txt" ) {
    $logger->info("Made stats");
  }
  else {
    $logger->logdie(
      "Run flatfile_stats.pl, but stats.txt does not exist or has no file size"
    );
  }
}

sub makePfamAScanFile {
  my ( $releasedir, $pfamDB ) = @_;

  open( PFAMSCAN, ">$releasedir/Pfam-A.hmm.dat" )
    or $logger->logdie("Could not open $releasedir/Pfam-A.scan.dat:[$!]");

  my @AllFamData =
    $pfamDB->getSchema->resultset("Pfama")
    ->search( undef, { order_by => \'me.pfama_id ASC' } );

  my %auto2id;
  foreach my $fam (@AllFamData) {
    $auto2id{ $fam->auto_pfama } = $fam->pfama_id;
  }

  foreach my $fam (@AllFamData) {
    print PFAMSCAN "# STOCKHOLM 1.0\n";
    print PFAMSCAN "#=GF ID   " . $fam->pfama_id . "\n";
    print PFAMSCAN "#=GF AC   " . $fam->pfama_acc .".".$fam->version . "\n";
    print PFAMSCAN "#=GF DE   " . $fam->description . "\n";
    print PFAMSCAN "#=GF GA   "
      . $fam->sequence_ga . "; "
      . $fam->domain_ga . ";\n";
    print PFAMSCAN "#=GF TP   " . $fam->type . "\n";
    print PFAMSCAN "#=GF ML   " . $fam->model_length . "\n";

    my @nested =
      $pfamDB->getSchema->resultset("NestedLocations")
      ->search( { "auto_pfama" => $fam->auto_pfama } );

    foreach my $n (@nested) {
      my $nested_id = $auto2id{ $n->nested_auto_pfama };
      print PFAMSCAN "#=GF NE   $nested_id\n";
    }

    my $clan = $pfamDB->getSchema->resultset("ClanMembership")->find(
      { auto_pfama => $fam->auto_pfama },
      {
        join     => [qw/auto_clan/],
        prefetch => [qw/auto_clan/]
      }
    );
    if ($clan) {
      print PFAMSCAN "#=GF CL   " . $clan->auto_clan->clan_acc . "\n";
    }
    print PFAMSCAN "//\n";
  }
  close(PFAMSCAN);
}

sub makeActiveSiteDat {
  my ( $releaseDir, $pfamDB ) = @_;

  my @allData =
    $pfamDB->getSchema->resultset("ActiveSiteAlignments")
    ->search( {}, { join => [qw(pfama)] } );

  my $file = $releaseDir . "/active_site.dat";
  open( FH, ">$file" ) or die "Couldn't open $file $!";

  foreach my $row (@allData) {
    my $id = $row->pfama_id;
    print FH "ID  $id\n";

    my @residues = split( /, /, $row->get_column('as_residues') );

    foreach my $r (@residues) {
      print FH "RE  $r\n";
    }

    my $al = Compress::Zlib::memGunzip( $row->get_column('alignment') );

    my @al = split( /\n/, $al );
    foreach my $row (@al) {
      if ( $row =~ /(\S+)\.\d+(\/.+)/ ) {
        print FH "AL  $1$2\n";
      }
      else {
        print FH "AL  $row\n";
      }
    }
    print FH "//\n";

  }
  close FH;

}

sub checkReleaseNumbers {
  my ( $new_release, $old_release ) = @_;

  my ( $major, $point, $major_old, $point_old );

  if ( $new_release =~ /Rel(\d+)_(\d+)/ ) {
    $major = $1;
    $point = $2;
    $logger->info("Making PFAM release $major.$point");
  }
  else {
    $logger->logdie(
"Sorry your format for new release is incorrect should be Rel2_3 for example"
    );
  }

  if ( $old_release =~ /Rel(\d+)_(\d+)/ ) {
    $old_major = $1;
    $old_point = $2;
  }
  else {
    $logger->logdie(
"Sorry your format for old release is incorrect should be Rel2_2 for example"
    );
  }

  if ( $major != $old_major and $major != ( $old_major + 1 ) ) {
    $logger->logdie(
      "Sorry your release numbers don't tally [$major] [$old_major]");
  }
  if ( $point != ( $old_point + 1 ) and $point != 0 ) {
    $logger->logdie( "Sorry your release numbers don't tally", );
  }
  return ( $major, $point, $old_major, $old_point );
}

sub getTxtFiles {
  my $relDir = shift;
  my $ua     = LWP::UserAgent->new;
  $ua->agent("MyApp/0.1 ");

  foreach my $f (qw(userman.txt relnotes.txt)) {

    # Create a request
    unless ( -e "$relDir/$f" ) {
      $logger->debug("Fetching $f\n");
      my $req =
        HTTP::Request->new(
        GET => "ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/$f" );
      my $res = $ua->request($req);

      # Check the outcome of the response
      if ( $res->is_success ) {
        open( F, ">$relDir/$f" )
          or $logger->logdie("Can not open $relDir/$f:[$!]");
        print F $res->content;
        close;
      }
      else {
        $logger->logdie( "Failed to retrieve $f from ftp site:[ "
            . $res->status_line
            . "]\n" );
      }
    }
  }
}

sub getPfamseqFiles {
  my ( $relDir, $pfamseqDir ) = @_;

  foreach my $f (qw(pfamseq uniprot_sprot.dat uniprot_trembl.dat metaseq ncbi))
  {
    unless ( -s "$relDir/$f.gz" ) {
      if ( -s "$pfamseqDir/$f.gz" ) {
        copy( "$pfamseqDir/$f.gz", "$relDir/$f.gz" )
          || $logger->logdie(
          "Could not copy $f from $pfamseqDir to $relDir:[$!]");
        next;
      }

      unless ( -s "$pfamseqDir/$f" ) {
        $logger->logdie("Could not find $f in $pfamseqDir!");
      }
      copy( "$pfamseqDir/$f", "$relDir/$f" )
        || $logger->logdie(
        "Could not copy $f from $pfamseqDir to $relDir:[$!]");
      my $pwd = getcwd;
      $logger->debug("Present working directory is:$pwd");
      chdir($relDir) or $logger->logdie("Could not cd to $relDir:[$!]");
      system("gzip $f") and $logger->logdie("Failed to gz $f:[$!]");
      chdir($pwd) or $logger->logdie("Could not cd to $pwd:[$!]");
    }
  }

}

sub makePfamAFlat {
  my ( $thiRelDir, $pfamDB ) = @_;

  #Get a list of all families
  my @families =
    $pfamDB->getSchema->resultset('Pfama')
    ->search( {}, { order_by => 'pfama_id ASC' } );

  $logger->info(
    "There will be " . scalar(@families) . " families in this release" );

  open( PFAMASEED, ">$thisRelDir/Pfam-A.seed" )
    || $logger->logdie("Could not open Pfam-A.seed");
  open( PFAMAFULL, ">$thisRelDir/Pfam-A.full" )
    || $logger->logdie("Could not open Pfam-A.full");
  open( PFAMHMM, ">$thisRelDir/Pfam.hmm" )
    || $logger->logdie("Could not open Pfam.hmm");
  open( PFAMAFA, ">$thisRelDir/Pfam-A.fasta" )
    || $logger->logdie("Could not open Pfam-A.fasta");
  open( PFAMAMETA, ">$thisRelDir/Pfam-A.full.metagenomics" )
    || $logger->logdie("Could not open Pfam-A.full.metagenomics");
  open( PFAMANCBI, ">$thisRelDir/Pfam-A.full.ncbi" )
    || $logger->logdie("Could not open Pfam-A.full.ncbi");

  #Make all of the directories for putting the trees into
  unless ( -d "$thisRelDir/trees" ) {
    $logger->info("Making tree directory");
    mkdir("$thisRelDir/trees")
      || $logger->logdie("Could not make directory $thisRelDir/trees");
  }

  #Make the directory structure for dropping the trees in to it
  foreach my $d (qw(full seed meta ncbi)) {
    unless ( -d "$thisRelDir/trees/$d" ) {
      $logger->info("Making $d sub directory within the tree directory");
      mkdir("$thisRelDir/trees/$d")
        || $logger->logdie("Could not make directory thisRelDir/trees/$d");
    }
  }

  #We are going to get the following alignments and a fasta file
  #| full |
  #| seed |
  #| ncbi |
  #| meta
  my (@errors);
  foreach my $family (@families) {
    $logger->info( "Checking " . $family->pfama_id );
    $logger->info( "Getting seed files for " . $family->pfama_id );

#------------------------------------------------------------------------------------
    ## THE SEED ALIGNMENT ##
    my $row = $pfamDB->getSchema->resultset('AlignmentsAndTrees')->find(
      {
        auto_pfama => $family->auto_pfama,
        type       => 'seed'
      }
    );

    unless ( $row and $row->auto_pfama ) {
      $logger->warn(
        "Could not find the seed row in the alignments and trees table for "
          . $family->pfama_id );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'seed',
          message =>
            'Could not find the seed row in the alignments and trees table'
        }
      );
    }
    $logger->info("Checking SEED file");

#How many regions do we expect according to pfamA_reg_seed, pfamA and the number so sequences in the
#file.
    my $seedCount =
      $pfamDB->getSchema->resultset('PfamaRegSeed')
      ->search( { auto_pfama => $family->auto_pfama } );

    #Put the seed alignment into a local file so that we can QC it.
    my $sAli = Compress::Zlib::memGunzip( $row->alignment );
    unless ( length($sAli) > 10 ) {
      $logger->warn('The seed alignment has inappropriate size');
      push(
        @errors,
        {
          family  => $family->pfama_acc,
          file    => 'seed',
          message => 'Inappropriate size'
        }
      );
    }

    open( TMP, ">/tmp/$$.seed" )
      or $logger->logdie("Could not open /tmp/$$.seed:[$!]");
    print TMP $sAli;
    close(TMP);

#Now pull out the number of sequences in the file and the number according to the SQ line.
    my ( $seedFileCount, $seedSQCount );
    open( TMP, "/tmp/$$.seed" )
      or $logger->logdie("Could not open /tmp/$$.seed:[$!]");

    while (<TMP>) {
      unless ($seedSQCount) {
        if (/^#=GF SQ   (\d+)/) {
          $seedSQCount = $1;
        }
      }
      unless (/^(#|\/\/)/) {
        $seedFileCount++;
      }
    }
    close(TMP);

    #Now cross reference all of these numbers!
    if ( $seedFileCount != $seedCount ) {
      $logger->warn(
'The number of sequences in the flat file and the number in the seed regions do not match'
      );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'seed',
          message =>
'The number of sequences in the flat file and the number in the seed regions do not match'
        }
      );
    }

    if ( $seedFileCount != $seedSQCount ) {
      $logger->warn(
'The number of sequences in #=GF line and the number of sequences in the file do not match'
      );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'seed',
          message =>
'The number of sequences in the flat file and the number in the seed regions do not match'
        }
      );

    }

    if ( $seedFileCount != $family->num_seed ) {
      $logger->warn(
'The number of sequences in num_seed and the number of sequences in the file do not match'
      );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'seed',
          message =>
'The number of sequences in the flat file and the number in the seed regions do not match'
        }
      );

    }

    unless ( length( Compress::Zlib::memGunzip( $row->jtml ) ) > 10 ) {
      $logger->warn('The html file has no size');
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'seedHTML',
          message =>
'The number of sequences in the flat file and the number in the seed regions do not match'
        }
      );

    }

    my $sTree = Compress::Zlib::memGunzip( $row->tree );
    unless ( length($sTree) > 10 ) {
      $logger->warn('The tree file has no size');
      push(
        @errors,
        {
          family  => $family->pfama_acc,
          file    => 'seedTree',
          message => 'The tree file has insufficient size'
        }
      );
    }

    #Write the SEED alignment and tree files to disk!
    open( SEEDTREE, ">$thisRelDir/trees/seed/" . $family->pfama_acc . ".tree" )
      || $logger->logdie("Error opening file");
    print SEEDTREE $sTree;
    close(SEEDTREE);
    print PFAMASEED $sAli;
    unlink("/tmp/$$.seed") or $logger->logdie("Could not remove /tmp/$$.seed");
    $sAli  = undef;
    $sTree = undef;

#------------------------------------------------------------------------------------
    ## THE FULL ALIGNMENT ##
    #Start afresh
    $row = undef;

    #Get the full alignment
    $row = $pfamDB->getSchema->resultset('AlignmentsAndTrees')->find(
      {
        auto_pfama => $family->auto_pfama,
        type       => 'full'
      }
    );
    unless ( $row and $row->auto_pfama ) {
      $logger->warn(
        "Could not find the seed row in the alignments and trees table for "
          . $family->pfama_id );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'seed',
          message =>
            'Could not find the full row in the alignments and trees table'
        }
      );
    }
    $logger->info("Checking FULL file");

    #Get the number of expected regions from pfamA_reg_full
    my $fullCount;

    #Get the number of expected regions from pfamA_insignificant
    my $fullSigCount =
      $pfamDB->getSchema->resultset('PfamaRegFullSignificant')->search(
      {
        auto_pfama => $family->auto_pfama,
        in_full    => 1
      }
      );

    #Again write it out to disk for QC-ing
    my $fAli = Compress::Zlib::memGunzip( $row->alignment );

    open( TMP, ">/tmp/$$.full" )
      or $logger->logdie("Could not open /tmp/$$.full:[$!]");
    print TMP $fAli;
    close(TMP);

    unless ( length($fAli) > 10 ) {
      $logger->warn('The seed alignment has inappropriate size');
      push(
        @errors,
        {
          family  => $family->pfama_acc,
          file    => 'full',
          message => 'Inappropriate size'
        }
      );
    }

    #No grab the regions from the file
    my ( $fullFileCount, $fullSQCount );
    open( TMP, "/tmp/$$.full" )
      or $logger->logdie("Could not open /tmp/$$.full:[$!]");
    while (<TMP>) {
      unless ($fullSQCount) {
        if (/^#=GF SQ   (\d+)/) {
          $fullSQCount = $1;
        }
      }
      unless (/^(#|\/\/)/) {
        $fullFileCount++;
      }
    }
    close(TMP);

    #Now Xref the numbers!
    if ( $fullFileCount != $fullSigCount ) {
      $logger->warn(
'The number of sequences in the flat file and the number in the full regions do not match'
      );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'full',
          message =>
'Miss-match between the number sequences in file and PfamA_reg_full_sign'
        }
      );
    }

    if ( $fullFileCount != $fullSQCount ) {
      $logger->warn(
'The number of sequences in #=GF line and the number of sequences in the file do not match'
      );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'full',
          message =>
            'Miss-match between the number sequences in file and #=GF line'
        }
      );
    }

    if ( $fullFileCount != $family->num_full ) {
      $logger->warn(
'The number of sequences in num_full and the number of sequences in the file do not match'
      );
      push(
        @errors,
        {
          family => $family->pfama_acc,
          file   => 'full',
          message =>
            'Miss-match between the number sequences in file and #=GF line'
        }
      );
    }

    unless ( length( Compress::Zlib::memGunzip( $row->jtml ) ) > 10 ) {
      $logger->warn('The html file has no size');
      push(
        @errors,
        {
          family  => $family->pfama_acc,
          file    => 'fullHTML',
          message => 'Inappropriate size'
        }
      );
    }

    my $fTree = Compress::Zlib::memGunzip( $row->tree );
    unless ( length($fTree) > 10 ) {
      $logger->warn('The tree file has no size');
      push(
        @errors,
        {
          family  => $family->pfama_acc,
          file    => 'fullTree',
          message => 'Inappropriate size'
        }
      );
    }

    #

    open( FULLTREE, ">$thisRelDir/trees/full/" . $family->pfama_acc . ".tree" )
      || $logger->logdie;
    print FULLTREE $fTree;
    close(FULLTREE);
    print PFAMAFULL $fAli;

    $fAli  = undef;
    $fTree = undef;
    $row   = undef;

#------------------------------------------------------------------------------------
    ## THE FASTA FILE ##
    $row =
      $pfamDB->getSchema->resultset('PfamaFasta')
      ->find( { auto_pfama => $family->auto_pfama } );

    unless ( $row and $row->auto_pfama ) {
      $logger->logdie(
        "Could not find the row in the fasta table for " . $family->pfama_id );
    }
    $logger->info("Checking FASTA file");
    my $fa = Compress::Zlib::memGunzip( $row->fasta );
    unless ( length($fa) > 10 ) {
      $logger->warn("Fasta file has incorrect size!");
      push(
        @errors,
        {
          family  => $family->pfama_acc,
          file    => 'fasta',
          message => 'Incorrect size of fasta file'
        }
      );
    }

    print PFAMAFA $fa;
    $row = undef;
    $fa  = undef;

#------------------------------------------------------------------------------------
    ## THE HMMS ##
    $logger->info("Checking HMM files");
    $row =
      $pfamDB->getSchema->resultset('PfamaHmm')
      ->find( { auto_pfama => $family->auto_pfama } );

    unless ( $row and $row->auto_pfama ) {
      $logger->logdie( "Could not find the row in the pfamA_HMM_ls table for "
          . $family->pfama_id );
    }

    unless ( length( $row->hmm ) ) {
      $logger->warn("hmm has no size");
      push(
        @errors,
        {
          family  => $family->pfama_acc,
          file    => 'hmm_ls',
          message => 'No size'
        }
      );
    }
    print PFAMHMM $row->hmm;

    $row = undef;

#------------------------------------------------------------------------------------
    ## THE METAGENOMICS ##
    $row = $pfamDB->getSchema->resultset('AlignmentsAndTrees')->find(
      {
        auto_pfama => $family->auto_pfama,
        type       => 'meta'
      }
    );

    #Get the number of expected regions pfamA_meta_reg
    my $metaFullSigCount =
      $pfamDB->getSchema->resultset('MetaPfamaReg')
      ->search( { auto_pfama => $family->auto_pfama, } );
    $logger->info("Got $metaFullSigCount");
    if ( $row and $row->auto_pfama and ( $metaFullSigCount > 0 ) ) {

      #Okay, looks like we have an alignments
      #PFAMAMETA
      $logger->info("Checking Metagenomics files");

      my $tree = Compress::Zlib::memGunzip( $row->tree );
      if ( $metaFullSigCount > 1 ) {
        unless ( length($tree) > 10 ) {
          $logger->warn("Metagenomics tree has incorrect size");
          push(
            @errors,
            {
              family  => $family->pfama_acc,
              file    => 'metaTree',
              message => 'No size'
            }
          );
        }

        #Write the SEED alignment and tree files to disk!
        open( METATREE,
          ">$thisRelDir/trees/meta/" . $family->pfama_acc . ".tree" )
          || $logger->logdie("Error opening file");
        print METATREE $tree;
        close(METATREE);
      }
      my $ali = Compress::Zlib::memGunzip( $row->alignment );
      unless ( length($ali) > 10 ) {
        $logger->warn("Metagenomics ali has incorrect size");
        push(
          @errors,
          {
            family  => $family->pfama_acc,
            file    => 'metaAli',
            message => 'No size'
          }
        );
      }
      print PFAMAMETA $ali;

      unless ( length( Compress::Zlib::memGunzip( $row->jtml ) ) > 10 ) {
        $logger->warn("Metagenomics html has incorrect size");
        push(
          @errors,
          {
            family  => $family->pfama_acc,
            file    => 'metaHTML',
            message => 'No size'
          }
        );
      }
    }
    $row              = undef;
    $metaFullSigCount = undef;

#------------------------------------------------------------------------------------
    ##  THE NCBI ##
    $row = $pfamDB->getSchema->resultset('AlignmentsAndTrees')->find(
      {
        auto_pfama => $family->auto_pfama,
        type       => 'ncbi'
      }
    );

    my $ncbiFullSigCount =
      $pfamDB->getSchema->resultset('NcbiPfamaReg')
      ->search( { auto_pfama => $family->auto_pfama, } );
    if ( $row and $row->auto_pfama and ( $ncbiFullSigCount > 0 ) ) {

      #Okay, looks like we have an alignments
      #PFAMAMETA
      $logger->info("Checking NCBI files");
      my $tree = Compress::Zlib::memGunzip( $row->tree );
      if ( $ncbiFullSigCount > 1 ) {
        unless ( length($tree) > 10 ) {
          $logger->warn("NCBI tree has incorrect size");
          push(
            @errors,
            {
              family  => $family->pfama_acc,
              file    => 'ncbiTree',
              message => 'No size'
            }
          );
        }

        #Write the SEED alignment and tree files to disk!
        open( NCBITREE,
          ">$thisRelDir/trees/ncbi/" . $family->pfama_acc . ".tree" )
          || $logger->logdie("Error opening file");
        print NCBITREE $tree;
        close(NCBITREE);
      }
      my $ali = Compress::Zlib::memGunzip( $row->alignment );
      unless ( length($ali) > 10 ) {
        $logger->warn("Metagenomics ali has incorrect size");
        push(
          @errors,
          {
            family  => $family->pfama_acc,
            file    => 'ncbiAli',
            message => 'No size'
          }
        );
      }
      print PFAMANCBI $ali;

      unless ( length( Compress::Zlib::memGunzip( $row->jtml ) ) > 10 ) {
        $logger->warn("Ncbi html has incorrect size");
        push(
          @errors,
          {
            family  => $family->pfama_acc,
            file    => 'NcbiHTML',
            message => 'No size'
          }
        );
      }
    }
    $row              = undef;
    $ncbiFullSigCount = undef;

  }
  if ( scalar(@errors) ) {
    open( ERRS, ">$thisRelDir/fatalErrors" )
      or $logger->logdie("Could not open $thisRelDir/fatalErrors");
    foreach my $e (@errors) {
      print ERRS $e->{family} . "\t" . $e->{file} . "\t" . $e->{message} . "\n";
    }
    close(ERRS);
    $logger->logdie(
      "SERIOUS ERROR whilst making the flatfile!!! See thisRelDir/faltErrors");
  }

  # /software/pfam/src/hmmer-3.0b2/bin/hmmconvert -2 Pfam.hmm > ! Pfam-A.h2.hmm

}

##########################################################
# Checks flatfile format, exits at first sign of trouble #
##########################################################
sub checkflat {
  my $file = shift;
  $logger->info("Checking format of $file");

  my $error;
  open( F, "checkflat.pl -v $file |" )
    || $logger->logdie("Failed to run checkflat.pl:[$!]");
  while (<F>) {
    if (/\S+/) {
      $logger->warn("ERROR with $file: [$_]");
      $error = 1;
    }
  }
  close F;

  if ($error) {
    $logger->logdie("Error in $file");
  }
}
##########################################################
# Checks flatfile format, exits at first sign of trouble #
##########################################################
sub checkflat_b {
  my $file = shift;
  $logger->info("Checking format for $file");
  my $error;
  open( F, "checkflat_pfamB.pl $file |" )
    or $logger->logdie("Failed to run checkflat_pfamB.pl:[$!]");
  while (<F>) {
    if (/\S+/) {
      $logger->warn("ERROR with $file:[$_]");
      $error = 1;
    }
  }
  close F;

  if ($error) {
    $logger->logdie("Error in $file.");
  }
}
##################
# Make swisspfam #
##################
sub makeSwissPfam {
  my ( $releasedir, $pfamseqdir ) = @_;
  $logger->info("Making swisspfam\n");

  #The next two system calls need to be reworked!
  if ( !-s "$pfamseqdir/pfamseq.len" ) {
    $logger->info("Going to needlessly waste time and run pfamseq_lengths.pl");
    system("gunzip -c /lustre/pfam/pfam/Production/pfamseq/uniprot_sprot.dat.gz /lustre/pfam/pfam/Production/pfamseq/uniprot_trembl.dat.gz | pfamseq_lengths.pl >! /lustre/pfam/pfam/Production/pfamseq/pfamseq.len"
    
    
    
    
    "pfamseq_lengths.pl $pfamseqdir/*.dat > $pfamseqdir/pfamseq.len")
      and $logger->logdie("Failed to run pfamseq_lengths.pl:[$!]");
  }

  $logger->info(
    "Going to needlessly waste some more time and run swisspfam_start_ends.pl");
  system(
"swisspfam_start_ends.pl $pfamseqdir/pfamseq.len $releasedir/Pfam-A.full $releasedir/Pfam-B > $releasedir/sw-pf"
  ) and $logger->logdie("Failed to run swusspfam_start_ends.pl\n");

  if ( !-s "$releasedir/sw-pf" ) {
    $logger->logdie( "No $releasedir/sw-pf file made!\n", 'die' );
  }

  $logger->info("Going to make swisspfam");
  system("mkswissPfam $releasedir/sw-pf > $releasedir/swisspfam")
    and $logger->logdie("Failed to run mkswissPfam:[$!]");
}

sub make_ftp {
  my $scriptdir  = shift @_;
  my $releasedir = shift @_;
  my $tmpdir     = shift @_;
  my $pfamseqdir = shift @_;
  my $release    = shift;
  &report("Making ftp files.\n");

  mkdir( "$releasedir/ftp", 0775 );
  if ( !-d "$releasedir/ftp" ) {
    &report( "Have not made ftp directory!\n", 'die' );
  }

  #	&execute("tar cf $releasedir/trees.tar $releasedir/trees");
  &execute("tar cf $releasedir/database_files.tar $releasedir/database_files");

  my @list = (
    'Pfam-B.hmm',   'Pfam-A.hmm',
    'Pfam-A.dead',  'Pfam-A.full',
    'Pfam-A.seed',  'Pfam-B',
    'swisspfam',    'diff',
    'prior.tar',    'domain.pnh',
    'Pfam-A.fasta', 'Pfam-C',
    'diff.version', "version_$release",
    'trees.tar',    'database_files.tar'
  );

  foreach my $file (@list) {
    &execute("gzip -c $releasedir/$file > $releasedir/ftp/$file.gz");
  }

  # Now need to export pfamseq each time.
  &execute("gzip -c $pfamseqdir/pfamseq > $releasedir/ftp/pfamseq.gz");
  push( @list, "pfamseq" );

  # Erik also wants the sprot.dat and uniprot.dat files.....
  &execute("gzip -c $pfamseqdir/sprot.dat > $releasedir/ftp/sprot.dat.gz");
  &execute("gzip -c $pfamseqdir/trembl.dat > $releasedir/ftp/trembl.dat.gz");
  push( @list, "sprot.dat", "trembl.dat" );

  # Test all gzipped files exist and unzip ok
  foreach my $file (@list) {
    if ( !-s "$releasedir/ftp/$file.gz" ) {
      &report( "Failed to make file $releasedir/ftp/$file.gz\n", 'die' );
    }
    &execute("gunzip -t $releasedir/ftp/$file.gz");
  }
  &execute("touch $tmpdir/made_ftp");
}

sub makePfamBHmms {
  my ($dir) = @_;

  unless ( $dir and -d $dir ) {
    $logger->logdie("Could not find the releases directory");
  }

  $logger->debug("Making Pfam-B.hmm");

  unless ( $dir . "/Pfam-B" ) {
    $logger->logdie("Could not find the Pfam-B hmms");
  }

  open( B, $dir . "/Pfam-B" )
    or $logger->logdie("Could not open Pfam-B:[$!]");
  open( B2, ">" . $dir . "/Pfam-B.top20000" )
    or $logger->logdie("Could not open Pfam-B.top20000:[$!]");

  $/ = "//";
  while (<B>) {
    print B2 $_;
    if (/#=GF ID   Pfam-B_(\d+)/) {
      if ( $1 == 20000 ) {
        print B2 "\n";
        close(B2);
        last;
      }
    }
  }
  close(B);
  $/ = "\n";

  #system($config->hmmer3bin."/hmmbuild $dir/Pfam-B.hmm.a $dir/Pfam-B.top20000")
  # and $logger->logdie("Failed to run hmmbuild:[$!]");

  $logger->debug("Making Pfam-B.hmm.dat");

  unless ( $dir . "/Pfam-B.hmm" ) {
    $logger->logdie("Could not find the Pfam-B hmms");
  }

  open( S, ">" . $dir . "/Pfam-B.hmm.dat" )
    or $logger->logdie(
    "Could not open the scan data dir in the release directory:[$!]");

  open( H, $dir . "/Pfam-B.hmm" )
    or $logger->logdie(
    "Could not open the hmm file in the release directory:[$!]");

  my @data;
  $/ = "//";
  while (<H>) {

    $/ = "\n";
    my @f = split( /\n/, $_ );

    #NAME  Pfam-B_1
    #ACC   PB000001
    #LENG  251
    my ( $name, $acc, $length );
    foreach my $l (@f) {
      if ( $l =~ /^NAME\s+(\S+)/ ) {
        $name = $1;
      }
      elsif ( $l =~ /^ACC\s+(\S+)/ ) {
        $acc = $1;
      }
      elsif ( $l =~ /LENG\s+(\d+)/ ) {
        $length = $1;
        last;
      }
    }
    if ($name) {
      push( @data, { name => $name, acc => $acc, leng => $length } );
    }
    $/ = "//";
  }

  $/ = "\n";
  foreach my $d (@data) {
    print S "# STOCKHOLM 1.0\n";
    print S "#=GF ID   " . $d->{name} . "\n";
    print S "#=GF AC   " . $d->{acc} . "\n";
    print S "#=GF ML   " . $d->{leng} . "\n";
    print S "//\n";
  }

  close(S);

  #Press and convert the files.
  $logger->debug("Making Pfam-B.hmm.bin");

  system( $config->hmmer3bin . "/hmmpress -f $dir/Pfam-B.hmm" )
    and $logger->logdie("Failed to run hmmpress:[$!]");

  system( $config->hmmer3bin
      . "/hmmconvert -b $dir/Pfam-B.hmm > $dir/Pfam-B.hmm.bin " )
    and $logger->logdie("Failed to run hmmconvert:[$!]");

}


sub help {
  print STDERR "Overrated!!!!\n";
}
