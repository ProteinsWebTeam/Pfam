#!/usr/bin/env perl

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
# use Bio::Pfam::SVN::Client;
use Bio::Pfam::ViewProcess;
use Digest::MD5 qw(md5_hex);

#-------------------------------------------------------------------------------
#Initial things to start up - database connections and logger
#Get a clan IO object
my $clanIO = Bio::Pfam::ClanIO->new;

#Can we get a Pfam Database connections
my $view = Bio::Pfam::ViewProcess->new;

# $view->logger->debug("Got pfamlive database connection");

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
  $view->logger->logdie("$clanAcc does not look like a clan accession\n");
}

if ( !$uid ) {
  if ($clanAcc) {
    $view->mailPfam(
      "Failed to run view process for clan:$clanAcc",
      "No job id passed to $0"
      ),
      ;
  }
  else {
   $view->mailPfam(
      "Failed to run view process for a clan",
      "No job id or clan acc passed to $0"
      ),
      ;
  }
  help();
  $view->logger->logdie("FATAL:No job id passed to script.....");
}

#-------------------------------------------------------------------------------
#Can we find the record of this job
my $job =
  $view->jobdb->getSchema->resultset('JobHistory')->find( { 'job_id' => $uid } );

unless ($job) {
  $view->mailPfam(
    "Failed to run view process for $clanAcc",
    "Could not get job information for $uid"
  );
}

$view->logger->debug("Starting ViewProcess for clan $clanAcc");

$view->job($job);
$view->logger->debug("Got job database object");

#Change the status of the job from pending to running.
$job->run;

#-------------------------------------------------------------------------------
#Make sure that the clan is in the databases.
$view->logger->debug("Checking clan is in the database");
my $clanData = $view->pfamdb->getClanData($clanAcc);


# Get clan and membership info
my $clan_obj = $clanIO->loadClanFromRDB($clanAcc, $view->pfamdb);
my $clanMemRef = $view->pfamdb->getClanMembership($clanAcc);

# clear members viewprocess files
foreach my $mem (@$clanMemRef) {
  $view->pfamdb->getSchema->resultset('AlignmentAndTree')->search({
      pfama_acc => $mem->pfama_acc->pfama_acc
    })->delete;
}

# Give a clan we want to first compete all of the members within that clan.
$view->logger->debug("Competing the clan members");
Bio::Pfam::Clan::Compete::competeClan( $clanAcc, $view->pfamdb );

#-------------------------------------------------------------------------------
# Now trigger off the view processes for all of the family members.
# Needs to happen after the competion. Will need to tweak the view process
# to make sure that the QC does not fail on alignment sizes compared with the database
# my $hmmio = Bio::Pfam::HMM::HMMIO->new;
# unlink("$clanAcc.lib");
# if(scalar(@$clanMemAcc) <= 40){
# foreach my $fam (@$clanMemAcc) {

#   #We are going to need the HMMs and SEED alignmetns

#   #First get the HMM
#   my ($hmmObj);
#   eval {
#     my $hmm =
#       $view->pfamdb->getSchema->resultset('PfamAHmm')
#       ->find( { pfama_acc => $clanMemAccAutoMap{$fam} } );
#     $hmmObj = $hmmio->readHMM( $hmm->hmm );
#   };
#   if ($@) {
#     $view->mailUserAndFail( "makeclanview: Problem getting HMM for $fam:[$@]" );
#   }
#   $view->logger->debug("Got hmm from $fam");

#   #Set the name
#   $hmmObj->name($fam);
#   $hmmObj->accession($fam);

#   #Set the description to undefined (if defined it goes into the hhsearch results
#   #output and screws up reg ex that reads in the hhsearch results)
#   #The HMM description will be defined if the view process has already been run
#   $hmmObj->description("");
  
#   #Write to disk
#   open( H, ">HMM.$fam" )
#     or $view->mailUserAndFail( "makeclanview: Could not open HMM.$fam :[$!]\n" );
#   $hmmio->writeHMM( \*H, $hmmObj );
#   close(H);

#   my $align =
#     $view->pfamdb->getSchema->resultset('PfamAInternal')
#     ->find( { pfama_acc => $clanMemAccAutoMap{$fam} } );

#   open( S, ">seed.$fam" );
#   print S Compress::Zlib::memGunzip( $align->seed )
#     or $view->mailUserAndFail( "makeclanview: Failed to uncompress seed alignment for $fam:[$!]" );
#   close(S);

#   unless ( -s "seed.$fam" ) {
#     $view->mailUserAndFail( "makeclanview: Wrote seed alignment for $fam but it was empty:[$!]" );
#   }
  
#   system($view->config->hmmer3bin."/esl-reformat --replace .:- --informat SELEX afa seed.$fam > seed.$fam.afa");
#   system("cat HMM.$fam >> $clanAcc.lib");
#   }
# }
#-------------------------------------------------------------------------------

$view->logger->debug("Writing the clandesc file so that clan can be versioned");

$clanIO->writeCLANDESC($clan_obj->DESC, ".");
open(C, "CLANDESC") or die;
my @clandesc = <C>;
close(C);
#Version the clan as we do for families.

my $clanDescCksum = md5_hex(join("", @clandesc));

my $relClanVersion = $view->pfamdb->getSchema->resultset('ReleasedClanVersion')->find({clan_acc => $clanData->clan_acc});

my $version;
if($relClanVersion){
  $version = ($clanDescCksum eq $relClanVersion->desc_file ? $relClanVersion->version : $relClanVersion->version + 1 );   
}else{
  $version = 1;
}

#-------------------------------------------------------------------------------
# Would be good to add summary data to the clan table.
my $noArch = 0;
my $noStruct = 0;
my $noSeqs = 0;
my $noSpecies = 0;

$view->logger->debug("Uploading the summary information and version.");

# my $dbh = $view->pfamdb->getSchema->storage->dbh;

# my $noSeqs_db = $dbh->prepare("select SUM(pa.num_full) from pfamA pa, clan_membership cm where pa.pfamA_acc = cm.pfamA_acc and cm.clan_acc='".$clanData->clan_acc."';");
# $noSeqs_db->execute or $view->logger->logdie("Failed to query pfamseq for size of pfamseq ".$noSeqs_db->errstr."\n");
# my $noSeqs = $noSeqs_db->fetchrow;

# my $noSpecies_db = $dbh->prepare("select COUNT(DISTINCT ps.ncbi_taxid) from pfamseq ps, pfamA_reg_full_significant fs, clan_membership cm  where fs.pfamA_acc = cm.pfamA_acc and ps.pfamseq_acc = fs.pfamseq_acc and fs.in_full=1 and cm.clan_acc='".$clanData->clan_acc."';");
# $noSpecies_db->execute or $view->logger->logdie("Failed to query pfamseq for size of pfamseq ".$noSpecies_db ->errstr."\n");
# my $noSpecies = $noSpecies_db->fetchrow;


$clanData = $clanData->update({ version => $version,
                    number_structures => $noStruct,
                    number_archs      => $noArch,
                    number_species    => $noSpecies,
                    number_sequences  => $noSeqs   });

#-------------------------------------------------------------------------------
#Make Stockholm version of CLANDESC
$view->logger->debug("Writing Stockholm version of CLANDESC");

$clan_obj->DESC->AC($clan_obj->DESC->AC.".$version");
$clanIO->writeCLANDESC($clan_obj->DESC, ".");
#Write out again with the version
open(C, "CLANDESC") or die;
@clandesc = <C>;
close(C);

open(C, ">CLANDESC.sto") or die;
print C "STOCKHOLM 1.0\n";
foreach my $line (@clandesc){
  print C "#=GF ".$line;  
}
print C "//\n";
close(C);

open( STO, "gzip -c CLANDESC.sto|" );
my $clanDescZip = join("", <STO>);
close(STO);

$view->pfamdb->getSchema
        ->resultset('ClanAlignmentAndRelationship')
          ->update_or_create({ clan_acc => $clanData->clan_acc,
                               stockholm => $clanDescZip,
                               alignment => undef,
                               image_map => undef,
                               relationship => undef }, { key => 'clan_acc' }
			    );

#-------------------------------------------------------------------------------
# Make clan alignment and relationship images
# Do not do for any Clan
# if(scalar(@$clanMemAcc) <= 40){ 
# $view->logger->debug("Going to run hhsearch for clan members");
# my $hhScores = runHHsearch( $clanAcc, $clanMemAcc, $view );

# $view->logger->debug("Making clan alignment");
# makeAlign( $hhScores, $clanMemRef, $clanAcc, $view, $clanData->clan_acc );
# $view->logger->debug("Making clan relationship diagram");
# makeGraph( $hhScores, $clanMemRef, $clanAcc, $view, $clanData->clan_acc );
# }

#-------------------------------------------------------------------------------
# $view->logger->debug("Initiating view process for family members");

# # Set of family view processes 
# my $familyIO = Bio::Pfam::FamilyIO->new;
# foreach my $fam (@$clanMemAcc){
#   my $dir = File::Temp->newdir( 'CLEANUP' => 1 ); 
#   my $famObj;
#   eval{
#     $famObj = $familyIO->loadPfamAFromSVN($fam, $dir, $client, 1);
#   };
#   if($@){
#     $view->mailUserAndFail( "makeclanview: Failed to $fam:[$!]" );  
#   }
#   $view->logger->debug("initiateFamily ViewProcess for $fam");
  
#   $view->initiateFamilyViewProcess($famObj, $job->user_id);
# }

#-------------------------------------------------------------------------------
#Set job status to be done!
 #Set the job status to be done!
  $job->update({status => 'DONE',
                closed => \'NOW()'}); 


$view->logger->debug("Completed successfully");
exit;



=head2 help 

  Title    : help
  Usage    : help() 
  Function : Prints the program usage
  Args     : None
  Returns  : Nothing
  
=cut

sub help {

  print <<EOF;

usage: $0 -clan <clan accession> -id <unique id>

#-------------------------------------------------------------------------------
clan accession : Expected to be in the format CL0001
uid            : Expected in the format given by Data::UUID

This script will compete the family members with in a clan and try and do as much of the 
post-processing required for clans as possible. This will include populating the summary
information data (no. species, no. structure etc.), produce the clan alignments and clan
relationship diagrams.  Finally, it will trigger off the view processes for the clan families. 

EOF

}

