#!/software/bin/perl

use strict;
use warnings;
use Getopt::Long;
use File::Copy;
use Data::Dumper;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::ClanIO;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::Clan::Compete;
use Bio::Pfam::Clan::ClanGraphics;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::ViewProcess;
use Digest::MD5 qw(md5_hex);

#-------------------------------------------------------------------------------
#Initial things to start up - database connections and logger

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

#Lets get a new Pfam config object
my $config = Bio::Pfam::Config->new;

#Get a clan IO object
my $clanIO = Bio::Pfam::ClanIO->new;

#Can we get a PfamJobs Database
my $jobDB = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );
unless ($jobDB) {
  Bio::Pfam::ViewProcess::mailPfam( "Failed to run view process",
    "Could not get connection to the pfam_jobs database" );
}

$logger->debug("Got pfam_job db connection");
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}
$logger->debug("Got pfamlive database connection");

#-------------------------------------------------------------------------------
#Deal with the user input options
my ( $clanAcc, $help, $uid );

GetOptions(
  "clan=s" => \$clanAcc,
  "id=s"   => \$uid,
  "h"      => \$help
);

if ($help) {
  help();
  exit;
}

unless ( $clanAcc =~ /CL\d{4}/ ) {
  $logger->logdie("$clanAcc does not look like a clan accession\n");
}

if ( !$uid ) {
  if ($clanAcc) {
    Bio::Pfam::ViewProcess::mailPfam(
      "Failed to run view process for clan:$clanAcc",
      "No job id passed to $0"
      ),
      ;
  }
  else {
    Bio::Pfam::ViewProcess::mailPfam(
      "Failed to run view process for a clan",
      "No job id or clan acc passed to $0"
      ),
      ;
  }
  help();
  $logger->logdie("FATAL:No job id passed to script.....");
}

#-------------------------------------------------------------------------------
#Can we find the record of this job
my $job =
  $jobDB->getSchema->resultset('JobHistory')->find( { 'job_id' => $uid } );

unless ($job) {
  Bio::Pfam::ViewProcess::mailPfam(
    "Failed to run view process for $clanAcc",
    "Could not get job information for $uid"
  );
}
$logger->debug("Got job databse object");

#Change the status of the job from pending to running.
$job->update({status  => 'RUN',
              started => \'NOW()'});

#-------------------------------------------------------------------------------
#Make sure that the clan is in the databases.
$logger->debug("Checking clan is in the database");
my $clanData = $pfamDB->getClanData($clanAcc);

#Check that the clan is in the SVN system
$logger->debug("Checking clan is in the SVN repository");
my $client = Bio::Pfam::SVN::Client->new;
$client->checkClanExists($clanAcc);

# Check all families exist in both the database and svn
my $clanSVNObj = $clanIO->loadClanFromSVN( $clanAcc, $client );
my $clanMemRef = $pfamDB->getClanMembership($clanAcc);

# Xref clan membership in the database and svn
my $clanMemAcc;
my %clanMemAccAutoMap;
foreach my $mem (@$clanMemRef) {
  push( @$clanMemAcc, $mem->auto_pfama->pfama_acc );
  $clanMemAccAutoMap{ $mem->auto_pfama->pfama_acc } = $mem->auto_pfama;
}

# Give a clan we want to first compete all of the members within that clan.
$logger->debug("Competing the clan members");
Bio::Pfam::Clan::Compete::competeClan( $clanAcc, $pfamDB );
$clanData->update( { competed => 1 } );

#-------------------------------------------------------------------------------
# Now trigger off the view processes for all of the family members.
# Needs to happen after the competion. Will need to tweak the view process
# to make sure that the QC does not fail on alignment sizes compared with the database
my $hmmio = Bio::Pfam::HMM::HMMIO->new;

foreach my $fam (@$clanMemAcc) {

  #We are going to need the HMMs and SEED alignmetns

  #First get the HMM
  my ($hmmObj);
  eval {
    my $hmm =
      $pfamDB->getSchema->resultset('PfamaHmm')
      ->find( { auto_pfama => $clanMemAccAutoMap{$fam} } );
    $hmmObj = $hmmio->readHMM( $hmm->hmm );
  };
  if ($@) {
    Bio::Pfam::ViewProcess::mailUserAndFail( $job,
      "Problem getting HMM for $fam:[$@]" );
  }
  $logger->debug("Got hmm from $fam");

  #Set the name
  $hmmObj->name($fam);
  $hmmObj->accession($fam);

  #Write to disk
  open( H, ">HMM.3.$fam" )
    or Bio::Pfam::ViewProcess::mailUserAndFail( $job,
    "Could not open HMM.3.$fam :[$!]\n" );
  $hmmio->writeHMM( \*H, $hmmObj );
  close(H);

  #Convert to H2 format
  system( $config->hmmer3bin . "/hmmconvert -2 HMM.3.$fam > HMM.$fam" )
    and Bio::Pfam::ViewProcess::mailUserAndFail( $job,
    "Failed to run hmmconvert:[$!]" );

  my $align =
    $pfamDB->getSchema->resultset('PfamaInternal')
    ->find( { auto_pfama => $clanMemAccAutoMap{$fam} } );

  open( S, ">seed.$fam" );
  print S Compress::Zlib::memGunzip( $align->seed );
  close(S);

  unless ( -s "seed.$fam" ) {
    Bio::Pfam::ViewProcess::mailUserAndFail( $job,
      "Failed to seed alignment for $fam:[$!]" );
  }

}

#-------------------------------------------------------------------------------
# Would be good to add summary data to the clan table.

$logger->debug("Calculating the number of architectures");
#No archs
my $noArchRS = $pfamDB->getSchema->resultset('PfamaRegFullSignificant')->search( {'clan_membership.auto_clan' => $clanData->auto_clan, in_full => 1},
  { join => [qw(pfamseq clan_membership)],
    columns => [ qw(pfamseq.auto_architecture) ],
    distinct => 1 } );
my $noArch = $noArchRS->count;

$logger->debug("Calculating the number of sequences");
#No Seqs
my $noSeqsRS = $pfamDB->getSchema->resultset('PfamaRegFullSignificant')->search( {'clan_membership.auto_clan' => $clanData->auto_clan, in_full => 1},
  { join => [qw(clan_membership)],
    columns => [ qw( auto_pfamA_reg_full ) ],
    distinct => 1 } );
my $noSeqs = $noSeqsRS->count;

#No Interactions
$logger->debug("Calculating the number of interactions");
my $noIntRS = $pfamDB->getSchema->resultset('PfamaRegFullSignificant')->search( {'clan_membership.auto_clan' => $clanData->auto_clan, in_full => 1},
  { join => [qw(clan_membership interactions)],
    columns => [ qw( interactions.auto_pfamA_A interactions.auto_pfamA_B ) ] } );
my $noInt = $noIntRS->count;

#Get list of unique species
$logger->debug("Calculating the number of species");
my $noSpeciesRS = $pfamDB->getSchema->resultset('PfamaRegFullSignificant')->search( {'clan_membership.auto_clan' => $clanData->auto_clan, in_full => 1},
  { join => [qw(pfamseq clan_membership)],
    columns => [ qw(pfamseq.ncbi_taxid) ],
    distinct => 1 } );
my $noSpecies = $noSpeciesRS->count;

#Get Number of Structures;
$logger->debug("Calculating the number of sturctures");
my $noStructRS = $pfamDB->getSchema->resultset('PfamaRegFullSignificant')->search( {'clan_membership.auto_clan' => $clanData->auto_clan, in_full => 1},
  { join => [qw(pdb_pfama_regs clan_membership)],
    columns => [ qw( pdb_pfama_regs.auto_pdb_reg ) ],
    distinct => 1 } );
my $noStruct = $noStructRS->count;

$logger->debug("Uploading the summary information");
$clanData->update({ number_structures => $noStruct,
                    number_archs      => $noArch, 
                    number_species    => $noSpecies,
                    number_sequences  => $noSeqs    });


#-------------------------------------------------------------------------------

$logger->debug("Writing the clandesc file so that clan can be versioned");

$clanIO->writeCLANDESC($clanSVNObj->DESC, ".");
open(C, "CLANDESC") or die;
my @clandesc = <C>;
close(C);
#Version the clan as we do for families.

my $clanDescCksum = md5_hex(join("", @clandesc));

my $relClanVersion = $pfamDB->getSchema->resultset('ReleasedClanVersion')->find({auto_clan => $clanData->auto_clan});

my $version;
if($relClanVersion){
  $version = ($clanDescCksum eq $relClanVersion->desc_file ? $relClanVersion->version : $relClanVersion->version + 1 );   
}else{
  $version = 1;
}

$clanData->update({version => $version});

#-------------------------------------------------------------------------------
#Make Stockholm version of CLANDESC
$logger->debug("Writing Stockholm version of CLANDESC");

$clanSVNObj->DESC->AC($clanSVNObj->DESC->AC.".$version");
$clanIO->writeCLANDESC($clanSVNObj->DESC, ".");
#Write out again with the version
open(C, "CLANDESC") or die;
@clandesc = <C>;
close(C);

open(C, ">CLANDESC.sto") or die;
print C "STOCKHOLM 1.0\n";
foreach my $line (@clandesc){
  print C "#=GF ".$line;  
}
print C "//";
close(C);

open( STO, "gzip -c CLANDESC.sto|" );
my $clanDescZip = join("", <STO>);
close(STO);

$pfamDB->getSchema
        ->resultset('ClanAlignmentsAndRelationships')
          ->update_or_create({ auto_clan => $clanData->auto_clan,
                               stockholm => $clanDescZip});

#-------------------------------------------------------------------------------
# Make clan alignment and relationship images

$logger->debug("Going to run hhsearch for clan members");
my $hhScores = runHHsearch( $clanAcc, $clanMemAcc, $config, $job );

$logger->debug("Making clan alignment");
makeAlign( $hhScores, $clanMemRef, $clanAcc, $pfamDB, $clanData->auto_clan );
$logger->debug("Making clan relationship diagram");
makeGraph( $hhScores, $clanMemRef, $clanAcc, $pfamDB, $clanData->auto_clan );


#-------------------------------------------------------------------------------
$logger->debug("Initiating view process for family members");

# Set of family view processes 
my $familyIO = Bio::Pfam::FamilyIO->new;
foreach my $fam (@$clanMemAcc){
  
  my $famObj;
  eval{
    $famObj = $familyIO->loadPfamAFromSVN($fam, $client);
  };
  if($@){
    Bio::Pfam::ViewProcess::mailUserAndFail( $job,
      "Failed to $fam:[$!]" );  
  }
  $logger->debug("initiateFamily ViewProcess for $fam");
  
  Bio::Pfam::ViewProcess::initiateFamilyViewProcess($famObj, $job->user_id, $config);
}

#-------------------------------------------------------------------------------
#Set job status to be done!
 #Set the job status to be done!
  $job->update({status => 'DONE',
                closed => \'NOW()'}); 

#-------------------------------------------------------------------------------
#Subroutines

sub runHHsearch {
  my ( $clanAcc, $clanMemAcc, $config, $job ) = @_;

  copy( $config->hhsearchCal . "/cal.hhm", "cal.hhm" )
    or Bio::Pfam::ViewProcess::mailUserAndFail( $job,
    "Failed to get calibration hmm" );
  open( H, ">>cal.hhm" )
    or Bio::Pfam::ViewProcess::mailUserAndFail( $job,
    "Could not open calibration hmm for appending" );

  foreach my $acc (@$clanMemAcc) {
    open( NH, "HMM.$acc" );
    while (<NH>) {
      print H $_;
    }
    close(NH);
  }
  close(H);

  my %hhResults;
  foreach my $acc (@$clanMemAcc) {
    $logger->debug( "Going to run "
        . $config->hhsearchBin
        . "/hhsearch -i HMM.$acc -d cal.hhm -o $acc.res" );
    system( $config->hhsearchBin
        . "/hhsearch -i HMM.$acc -d cal.hhm -o $acc.res" )
      and Bio::Pfam::ViewProcess::mailUserAndFail( $job,
      "Failed ro run hhsearch on HMM.$acc:[$!]\n" );

    $logger->debug("Parsing hhsearch results for $acc");
    open( R, "$acc.res" )
      or Bio::Pfam::ViewProcess::mailUserAndFail( $job,
      "Failed to open hhsearch results for $acc" );
    while (<R>) {
      if (/\d+\s+(PF\d{5})(.{23})\s+(\S+)\s+(\S+)/) {
        $hhResults{$acc}{$1} = $4;
      }
      elsif (/^No 1/) {

        #We are on to the alignment section, so skip this!
        last;
      }
    }
    close(R);
  }
  print Dumper (%hhResults);
  return ( \%hhResults );
}

sub makeGraph {
  my ( $hhScores, $clanMemRef, $clanAcc, $pfamDB, $auto_clan ) = @_;

  # The first line of the table is a list of member names
  my @table;
  my @names;
  my @accs;

  for ( my $x = 0 ; $x < scalar(@$clanMemRef) ; $x++ ) {
    my $xAcc = $$clanMemRef[$x]->auto_pfama->pfama_acc;
    push( @accs,  $xAcc );
    push( @names, $$clanMemRef[$x]->auto_pfama->pfama_id );
    for ( my $y = 0 ; $y < scalar(@$clanMemRef) ; $y++ ) {
      my $yAcc = $$clanMemRef[$y]->auto_pfama->pfama_acc;
      if ( defined( $hhScores->{$xAcc}->{$yAcc} ) ) {
        $table[$x]->[$y] = $hhScores->{$xAcc}->{$yAcc};
      }
    }
  }

  $logger->info( "Processing table for ", $clanAcc );
  print Dumper(@table);
  $logger->info( "Processing names", $clanAcc );
  print Dumper(@names);
  $logger->info( "Processing accs", $clanAcc );
  print Dumper(@accs);

  $logger->info( "Processing ", $clanAcc );
  my $clan = Bio::Pfam::Clan::ClanGraphics->new(
    '-matrix' => [@table],
    '-names'  => [@names],
    '-accs'   => [@accs]
  ) or $logger->logdie("Couldn't create Clan object for $clanAcc!");

  unless ( $clan == -1 ) {
    my $image_map = $clan->drawGraph(
      '-file'     => "$clanAcc.png",
      '-fontpath' => '/nfs/phd/bsb/Documents/fonts/TTF',
      '-font'     => 'Arial'
    );    # or $logger->logdie("Error drawing graph for $clan_acc: $!");
    open( OUT, ">$clanAcc.html" )
      ;    #or $logger->logdie("Error creating $clan_acc.html: $!");
    print OUT $image_map;
    close(OUT);

    $logger->debug("Finished creating image and html for clan $clanAcc!");

  }
  else {

    $logger->warn("Clan $clanAcc has no connections!");

  }

  #Now upload the clan image
  my ( $clanImage, $clanIM );

  open( HTML, "gzip -c $clanAcc.html|" )
    or Bio::Pfam::ViewProcess::mailUserAndFail( $job,
    "Failed to run gzip -c $clanAcc.html:[$!]" );
  while (<HTML>) {
    $clanIM .= $_;
  }
  close(HTML);

  open( PNG, "gzip -c $clanAcc.png|" )
    or Bio::Pfam::ViewProcess::mailUserAndFail( $job,
    "Failed to run gzip -c $clanAcc.png:[$!]" );
  while (<PNG>) {
    $clanImage .= $_;
  }
  close(PNG);

  $pfamDB->getSchema->resultset('ClanAlignmentsAndRelationships')
    ->update_or_create(
    {
      auto_clan    => $auto_clan,
      image_map    => $clanIM,
      relationship => $clanImage
    }
    );

}

sub makeAlign {
  my ( $hhScoresRef, $clanMemRef, $clanAcc, $pfamDB, $auto_clan ) = @_;

  my %families_in_align;
  my %hhScores;

  foreach my $acc1 ( keys %$hhScoresRef ) {
    while ( my ( $acc2, $score ) = each %{ $hhScoresRef->{$acc1} } ) {
      $hhScores{"$acc1:$acc2"} = $score;
    }
  }

  my $align_no = 0;
  foreach my $key ( sort{ $hhScores{$a} <=> $hhScores{$b} } keys %hhScores ) {
    my ( $acc1, $acc2 ) = split( /:/, $key );
    next if ( $acc1 eq $acc2 );
    if ( !$families_in_align{$acc1} and !$families_in_align{$acc2} ) {

      #align the two seed alignments.
      $logger->debug("Aligning $acc1 && $acc2");
      system("sreformat a2m seed.$acc1 > tmp.$$.in1");
      system("sreformat a2m seed.$acc2 > tmp.$$.in2");
      system(
"muscle -quiet -profile -in1 tmp.$$.in1 -in2 tmp.$$.in2 -out tmp.$$.$align_no.align"
      ) && die;
      $families_in_align{$acc1} = "tmp.$$.$align_no.align";
      $families_in_align{$acc2} = "tmp.$$.$align_no.align";
      $align_no++;
    }
    elsif ( $families_in_align{$acc1} && !$families_in_align{$acc2} ) {
      $logger->debug("Aligning $acc2 and $families_in_align{$acc1}");
      system("sreformat a2m seed.$acc2 > tmp.$$.in2");
      system(
"muscle -quiet -profile -in1 $families_in_align{$acc1} -in2 tmp.$$.in2 -out tmp.$$.$align_no.align"
      ) && die;
      $families_in_align{$acc2} = "tmp.$$.$align_no.align";
      my $old_align = $families_in_align{$acc1};
      foreach ( keys %families_in_align ) {
        if ( $families_in_align{$_} eq $old_align ) {
          $families_in_align{$_} = "tmp.$$.$align_no.align";
        }
      }
      $align_no++;
    }
    elsif ( !$families_in_align{$acc1} && $families_in_align{$acc2} ) {
      $logger->debug("Aligning $acc1 and $families_in_align{$acc2}");
      system("sreformat a2m seed.$acc1 > tmp.$$.in2");
      system(
"muscle -quiet -profile -in1 $families_in_align{$acc2} -in2 tmp.$$.in2 -out tmp.$$.$align_no.align"
      ) && die;
      $families_in_align{$acc1} = "tmp.$$.$align_no.align";
      my $old_align = $families_in_align{$acc2};
      foreach ( keys %families_in_align ) {
        if ( $families_in_align{$_} eq $old_align ) {
          $families_in_align{$_} = "tmp.$$.$align_no.align";
        }
      }
    }
    else {
      $logger->debug("$acc1 and $acc2 is already in the alignment");

    }
  }

  #Now align all the sub alignments in the hash.
  my %tmp_align_files;
  foreach ( keys %families_in_align ) {
    print "$families_in_align{$_}\n";
    $tmp_align_files{ $families_in_align{$_} }++;
  }

  my @aligns = keys %tmp_align_files;
  my $master_align;
  if (@aligns) {
    $master_align = shift @aligns;
  }
  print "Master $master_align\n";

  foreach (@aligns) {
    system(
"muscle -quiet -profile -in2 $master_align -in1 $_ -out tmp.$$.$align_no.align"
    );
    print "Combining align $_ and $master_align into tmp.$$.$align_no.align\n";
    $master_align = "tmp.$$.$align_no.align";
    $align_no++;
  }

  #Now add any other alignments that are not already included.
  foreach my $acc ( @$clanMemAcc ) {
    next if ( $families_in_align{$acc} );
    $logger->debug("Adding orphan align $acc");
    if ( !$master_align ) {
      system(
        "sreformat a2m seed.$acc > tmp.$$.$align_no.align");
        $master_align = "tmp.$$.$align_no.align";
        $align_no++;
      }
    else {
      system(
        "sreformat a2m seed.$acc > tmp.$$.$align_no.align");
        my $in1 = "tmp.$$.$align_no.align";
        $align_no++;
        print "$in1, $master_align\n";
        system( "muscle -quiet -profile -in1 $in1  -in2 $master_align -out tmp.$$.$align_no.align"
      ) && die;
      $master_align = "tmp.$$.$align_no.align";
      $align_no++;
    }
  }

  system("sreformat --pfam stockholm $master_align > $clanAcc.sto");
  $logger->debug("Finished building clan alignment for $clanAcc");

  #Need to Swap accs for ids;
  $logger->debug("Getting regions for clan [$auto_clan]");
  my @regs = $pfamDB->getSchema
	 ->resultset('PfamaRegSeed')
	   ->search( { 'clan_membership.auto_clan' => $auto_clan },
		    { join       => [ qw (pfamseq clan_membership) ],
		      prefetch   => [ qw (pfamseq) ] });                              
 
  
  my %regs = map {$_->pfamseq_acc."/".$_->seq_start."-".$_->seq_end => $_ } @regs;
  
  $logger->debug("Making HTML aligment for clan alignment $clanAcc.sto");
  system("consensus.pl -method clustal -file $clanAcc.sto > $clanAcc.con")
      and Bio::Pfam::ViewProcess::mailUserAndFail( $job, "Failed to run consensus.pl:[$!]");
  system("clustalX.pl -a $clanAcc.sto -c $clanAcc.con -b 80 > $clanAcc.nfb.html")
     and Bio::Pfam::ViewProcess::mailUserAndFail( $job, "Failed to run clustalX.pl:[$!]" );


  my %nse;
  foreach my $acc (@$clanMemAcc){
    open(SEED, "seed.$acc") || die;
	  while(<SEED>){
	    if (/(\S+)\s+(\S+)/){
		    $nse{$1}=$acc;
	    }
	  }
	 close(SEED);  
  }
  
  open(CNFB, "$clanAcc.nfb.html") or die;
  open(CALI, ">$clanAcc.withFamBlock.html") or die;
  my $previousAssignedDiv = 0;
  my %famBlockColour;
  my $currentDiv = '';
  my $rule;
  while(<CNFB>){
    if(m|<span class="nse">(\S+)(</span>.*)|){
      my $thisNse = $1;
      my $theRest = $2;
      unless($regs{$thisNse}){
       $logger->debug("Failed to find nse for $thisNse");
       Bio::Pfam::ViewProcess::mailUserAndFail( $job, "Failed to find nse for $thisNse" ); 
      }
      my $accVerSE = $regs{$thisNse}->pfamseq_acc.".".$regs{$thisNse}->seq_version."/".$regs{$thisNse}->seq_start."-".$regs{$thisNse}->seq_end;
      
      unless(defined($famBlockColour{ $nse{$accVerSE} })){
        #Keyed of family accession
        $famBlockColour{$nse{$accVerSE}} = $previousAssignedDiv;
        $previousAssignedDiv = ($previousAssignedDiv ? 0 : 1 );
      }
      
      my $div = ($famBlockColour{$nse{$accVerSE}} ? 'evenAliLine' : 'oddAliLine');
      
      if(!defined($rule)){
        $rule = $thisNse;
      }elsif($rule eq $thisNse){
        print CALI "<div class=\"aliBlockRule\"><hr /></div>\n";  
        $currentDiv ='';
      }
      
      
      
      print CALI "<div class=\"$div\">";
      unless($currentDiv eq $div){
        print CALI "<div class=\"aliTitle\"><p>".$nse{$accVerSE}."</p></div>";       
      }
      $currentDiv=$div;
      print CALI "<span class=\"nse\">";
      print CALI $regs{$thisNse}->pfamseq_id."/".$regs{$thisNse}->seq_start."-".$regs{$thisNse}->seq_end."$theRest</div>\n";     
    }else{
      print CALI $_;  
    }   
  }
  close(CALI);
  close(CNFB);

  
  open(ALI, "gzip -c $clanAcc.withFamBlock.html |") or Bio::Pfam::ViewProcess::mailUserAndFail($job, "Failed to gzip clan alignment" );
  my $align = join("", <ALI>);
  $pfamDB->getSchema->resultset('ClanAlignmentsAndRelationships')
    ->update_or_create(
    {
      auto_clan    => $auto_clan,
      alignment    => $align
    }
    );

}

=head2 help 

  Title    : help
  Usage    : help() 
  Function : Prints the program usage
  Args     : None
  Returns  : Nothing
  
=cut

sub help {

  print <<EOF;

usage: $0 -clan <clan accession> -uid <unique id>

#-------------------------------------------------------------------------------
clan accession : Expected to be in the format CL0001
uid            : Expected in the format given by Data::UUID

This script will compete the family members with in a clan and try and do as much of the 
post-processing required for clans as possible. This will include populating the summary
information data (no. species, no. structure etc.), produce the clan alignments and clan
relationship diagrams.  Finally, it will trigger off the view processes for the clan families. 

EOF

}

