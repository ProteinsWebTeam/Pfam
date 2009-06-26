#!/usr/local/bin/perl

use strict;
use warnings;

use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::SVN::Commit;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;

my($rev, $repos);
GetOptions( "rev=s"   => \$rev,
            "repos=s" => \$repos );

unless($rev and $repos){
  die "Need to define both rev and repos: try $0 -help";  
}

my %params;
$params{rev} = $rev; 
$params{repos} = $repos;

my $revlook = Bio::Pfam::SVN::Commit->new( \%params );
unless($revlook and $revlook->isa('SVN::Look')){
  die "Failed to get a SVN::Look object for rev:$rev and repos:$repos\n";
}

my $config = Bio::Pfam::Config->new;

#Now see what has just been done for this revision.
my $logMessage = $revlook->log_msg;

# The following actions will trigger a SVN post-commit
#------------------------------------------------------

# 1. Adding a new family to the repository.
#    This will add the accession to the DESC file and trigger off the view process.
# 2. Adding a family to a clan
#    This will add the family accession to the membership list in the file.
# 3. Adding a new Clan
#    This will add the accession to the CLANDESC file
# 4. Removing a family from a clan
#    This will remove the family from the clan membership
 
 
if($logMessage =~ /^PFNEWATC:(\S+)/){
  my $family = $1;
  my $client = Bio::Pfam::SVN::Client->new;
  
  
  my $connect = $config->pfamlive;
  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
    %{ $connect }
  );
  
  my $pfamA = $pfamDB->getPfamData($family);
  
  open(M, ">.defaultpfnewmove") or die "";
  print M "Moving from pending to main repository and fixing accession\n";
  close(M);
  $client->addPFNEWMOVELog;
  $client->moveNewFamily($pfamA->pfama_id, $pfamA->pfama_acc);
  
  #Now checkout and add the accession to the DESC file!
  my $tmpDir = File::Temp->newdir( 'CLEANUP' => 0 );
  my $dest = $tmpDir->dirname;
  $client->checkoutFamily($pfamA->pfama_acc, $dest);
  #parse the DESC file
  my $familyIO = Bio::Pfam::FamilyIO->new;
  
  my $descObj = $familyIO->parseDESC("$dest/DESC");
  $descObj->AC($pfamA->pfama_acc);
  $familyIO->writeDESC($descObj, $dest);
  #Commit back in
  $client->commitFamily($dest);
  
  #Now, if this family has a CL line add the accession to the CLANDESC
  if($descObj->CL){
    $revlook->updateClanMembership($pfamDB, $descObj->CL, $descObj->AC);
    addToClan($descObj->CL, $descObj->AC);  
  }

}elsif( $logMessage =~ /PFCIATC:(CL\d{4}):(PF\d{5})/ ){
  #Looks like we are trying to add a family to a clan.
  my $clan = $1;
  my $fam  = $2;
  addToClan($clan, $fam);  
}elsif($logMessage =~/CLNEW:(\S+)/){
  my $clan = $1;
  my $client = Bio::Pfam::SVN::Client->new;
  
  
  my $connect = $config->pfamlive;
  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
    %{ $connect }
  );
  
  my $clanData = $pfamDB->getClanData($clan);
  
  open(M, ">.defaultclnewmove") or die "";
  print M "Moving from pending to main repository\n";
  close(M);
  $client->addCLNEWMOVELog;
  $client->moveNewClan($clanData->clan_id, $clanData->clan_acc);
  
  open(M, ">.defaultclnewmove") or die "";
  print M "Automatically adding accession\n";
  close(M);
  $client->addCLNEWACCLog;
  
  #Now checkout and add the accession to the DESC file!
  my $tmpDir = File::Temp->newdir( 'CLEANUP' => 1 );
  my $dest = $tmpDir->dirname;
  $client->checkoutClan($clanData->clan_acc, $dest);
  #parse the DESC file
  my $clanIO = Bio::Pfam::ClanIO->new;
  
  my $descObj = $clanIO->parseCLANDESC("$dest/".$clanData->clan_acc."/CLANDESC");
  $descObj->AC($clanData->clan_acc);
  $clanIO->writeCLANDESC($descObj, $dest."/".$clanData->clan_acc);
  #Commit back in
  $client->commitClan($dest."/".$clanData->clan_acc);
  
  
}elsif($logMessage =~ /PFCIRMC:(CL\d{4})-(PF\d{5})/){
  die;
}else{
  #No other commits require post-commit processing
  ; 
}


  
sub addToClan {
  my ( $clan, $fam ) = @_;  
  my $tmpDir = File::Temp->newdir( 'CLEANUP' => 0 );
  my $dest = $tmpDir->dirname;
  
  #Check out the clan!
  my $client = Bio::Pfam::SVN::Client->new;
  
  #Check that the family and clan reside in the repository;
  $client->checkFamilyExists($fam);
  $client->checkClanExists($clan);
  
  $client->checkoutClan($clan, $dest);
  
  # Now open up the CLANDESC.  Move sideways and add the accession  
  # to the clan membership list in the CLANDESC object and write
  my $clanDir = "$dest/$clan";
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $clanObj = $clanIO->loadClanFromLocalFile($clan, $dest, "file");
  unlink("$clanDir/CLANDESC"); 
  if($clanObj->DESC->MEMB){
    push(@{ $clanObj->DESC->MEMB }, $fam );
  }else{
    $clanObj->DESC->MEMB([$fam]);
  }
  $clanIO->writeCLANDESC($clanObj->DESC, $clanDir);
  
  #Now check the clan back in, adding a automatic comment.
  $client->addAUTOMBLog($fam);
  $client->commitClan($clanDir);
}  


