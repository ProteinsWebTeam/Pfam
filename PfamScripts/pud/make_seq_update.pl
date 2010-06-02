#!/software/bin/perl

# A script for doing the Pfam sequence update. This process involves
# all families in Pfam being updated to a new version of the
# underlying sequence database.
#
# The script is written such that if anything fails halfway through it
# can be restarted without duplication of work already done.  It
# creates a log file called seq_update.log in the the status directory
# that resides in the Pfam production dir as statusdir_releasenum.
#
# There are a couple of points where the user must confirm that some
# job has been done outside the system, such as making sure all locks
# are removed and that seeds have all been checked after surgery.
#
# jm14@sanger.ac.uk, agb@sanger.ac.uk

use strict;
use warnings;
use Getopt::Long;
use File::Copy;
use Mail::Mailer;
use Cwd;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::SVN::Client;
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{  $config->pfamlive }
);
unless(-d $config->productionLoc."/Logs"){
  mkdir($config->productionLoc."/Logs") or 
    die("Could not make directory ".$config->productionLoc."/Logs because:[$!]");  
  
}

open(L, ">".$config->productionLoc."/Logs/log4perl.conf");
print L 
"log4perl.rootLogger=DEBUG, SCREEN
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=[%d{HH:mm:ss EEE MMM dd yyyy}] %6p: %4L: %M: %m%n

log4perl.logger.fileLogger=DEBUG, LOGFILE
log4perl.appender.LOGFILE=Log::Log4perl::Appender::File
log4perl.appender.LOGFILE.filename=".$config->productionLoc."/Logs/seq_update.log
log4perl.appender.LOGFILE.mode=append    
log4perl.appender.LOGFILE.layout=PatternLayout
log4perl.appender.LOGFILE.layout.ConversionPattern=[%d{HH:mm:ss EEE MMM dd yyyy}] %6p: %4L: %M: %m%n";
close(L);


Log::Log4perl->init( $config->productionLoc."/Logs/log4perl.conf" );

my $logger = Log::Log4perl->get_logger('fileLogger');
$logger->debug("Using database:".$config->pfamlive->{database});


#-------------------------------------------------------------------------------

my($help, $oldrelease_num, $newrelease_num, $rdb_adminuser, $rdb_adminpass);

GetOptions(
	"help" 	=> \$help,
	"old=i" => \$oldrelease_num,
	"new=i" => \$newrelease_num,
);

&help if($help);
# Check some of the input parameters
if(!$oldrelease_num){
    $logger->warn("*** You need to provide an old release number! *** ");
    &help;
}
if(!$newrelease_num){
    $logger->warn(" *** You need to provide an new release number! *** ");
    &help;
}


if($newrelease_num != $oldrelease_num +1) {
    $logger->warn("*** Your old release number doesn't tally with the new release number! ***");
    &help;
}



# All parameters have been read in.
#-------------------------------------------------------------------------------
# Now start! Basic accountancy

$logger->info("STARTED SEQUENCE UPDATE SCRIPT\n");
my $pwd = getcwd;




my $statusdir     = $config->productionLoc."/Logs/statusdir_$newrelease_num";

#Create directory for files that record status of sequence update process 
if (! -d "$statusdir"){
    mkdir ("$statusdir",0775) or $logger->logdie("Could not make $statusdir:[$!]");
    $logger->info("Created Status directory in $statusdir directory OK\n");
} else {
    $logger->info("Already made Status directory $statusdir\n");
}

# make new dir
my $pfamseqdir = $config->productionLoc."/pfamseq";
if(! -e "$pfamseqdir$newrelease_num") {
 mkdir("$pfamseqdir$newrelease_num") or $logger->logdie("Couldn't make dir $pfamseqdir$newrelease_num:[$!]");
}
else {
  $logger->info("Already made Dir $pfamseqdir$newrelease_num\n");
}

chdir "$pfamseqdir$newrelease_num" or $logger->logdie("Couldn't change directory to '$pfamseqdir$newrelease_num':[$!]");

my $user = $ENV{USER};


# Warn everyone that pfam release has started
unless (-e "$statusdir/mailed.txt") {
    $logger->info("Warning all pfammers that seq update is starting");
    open (FH, "> $statusdir/mailed.txt") or die "Can not write to file $statusdir/mailed.txt";
    close(FH);
    
my $message = <<EOF;
Dear Pfammers,

This is an automatically generated message.

The sequence update has just been started! The database will shortly be locked to prevent
commits while we update the sequence databases.

Thanks

The Pfam Release Committee

EOF

   
my %header = (  To => 'pfam@sanger.ac.uk',
					        From => $user.'@sanger.ac.uk',
					        Subject => 'Release message' );
	 my $mailer = Mail::Mailer->new;
	 $mailer->open(\%header);
	 print $mailer $message; 
   $mailer->close;
} else {
  $logger->info("Already mailed everyone a warning seq update is starting");
}
#-------------------------------------------------------------------------------
# Now lock the database while we update the sequence database

#pflock 


#-------------------------------------------------------------------------------
#Run the ncbi_taxonomy update (takes less than 1 hr)
unless(-e "$statusdir/ncbi_taxonomy"){  
  system("pud-ncbiTaxonomy.pl") and $logger->logdie("Failed to run pud-ncbiTaxonomy.pl:[$!]");
  system("touch $statusdir/ncbi_taxonomy") and die "couldn't touch $statusdir/ncbi_taxonomy:[$!]";
  $logger->info("Run ncbi_taxonomy update");
}else{
  $logger->info("Already updated ncbi_taxonomy");
}


#Build the taxonomy table (takes less than 30 mins)
unless(-e "$statusdir/taxonomy"){ 
  system("pud-taxonomy.pl") and $logger->logdie("Failed to run pud-taxonomy.pl:[$!]");
  system("touch $statusdir/taxonomy") and $logger->logdie( "couldn't touch $statusdir/ncbi_taxonomy:[$!]");
  $logger->info("Run taxonomy table update");
}else{
  $logger->info("Already updated taxonomy");
}
#-------------------------------------------------------------------------------
#All the pfamseq sequence stuff. - ( takes about 12 hours )
unless(-e "$statusdir/updated_pfamseq"){
  #Add Jaina's stuff in here!!!!
  $logger->info("Updating pfamseq");
  system("pud-update_pfamseq.pl -status_dir $statusdir -pfamseq_dir $pfamseqdir$newrelease_num")
    and $logger->logdie("Failed to run pud-update_pfamseq.pl:[$!]");
  $logger->info("Updated pfamseq");
  system("touch $statusdir/updated_pfamseq") and $logger->logdie("Could not touch $statusdir/updated_pfamseq");
}else{
  $logger->info("Already updated pfamseq");   
}

#Now map between uniprot and ncbi - (takes about 1.5 hours)
unless(-e "$statusdir/updated_ncbi_map"){
  #Add Jaina's stuff in here!!!!
  $logger->info("Updating ncbi_map");
  system("pud-uniprotNcbiMapping.pl -release $newrelease_num")
    and $logger->logdie("Failed to run pud-uniprotNcbiMapping.pl:[$!]");
  $logger->info("Updated ncbi_map table");
  system("touch $statusdir/updated_ncbi_map") and 
    $logger->logdie("Could not touch $statusdir/updated_ncbi_map");
}else{
  $logger->info("Already updated ncbi_map");   
}

#-------------------------------------------------------------------------------
#Download proteomes - take about 15 minutes
if(! -e "$statusdir/downloaded_proteomes") {
    my $num = $newrelease_num . ".0";
    
    $logger->info("Downloading proteomes\n");
    # This is a dumping ground for all proteomes.  
    my $dir = "/lustre/pfam/pfam/Production/localdbs/proteome/Release" . $num;
    if (! -d "$dir"){
	mkdir ("$dir",0777) or die "Cannot make dir $dir";
    }
    system("pud-get_proteome.pl $dir") and $logger->logdie("Downloading proteomes failed:[$!]");
    system("touch $statusdir/downloaded_proteomes")   and $logger->logdie(die "couldn't touch $statusdir/downloaded_proteomes:[$!]");
    $logger->info("Downloaded proteome data");
} else {
    $logger->info("Already downloaded proteomes");
}

#Upload proteomes
unless(-e "$statusdir/uploaded_proteomes"){
  my $num = $newrelease_num . ".0";
  $logger->info("Uploading proteomes");
  system("pud-proteome_upload.pl -rel $num") and $logger->logdie("Failed to run |pud-proteome_upload.pl -rel $num|:[$!]");
  $logger->info("Finished uploading proteomes");
}else{
  $logger->info("Already uploaded proteomes");  
}

#-------------------------------------------------------------------------------
## get GenPept from NCBI - takes about 1 hour to download. 12 hours to upload 
unless(-e "$statusdir/ncbi"){  
  system("pud-ncbi.pl -version $newrelease_num") and 
    $logger->logdie("Failed to run pud-ncbi.pl:[$!]");
  system("touch $statusdir/ncbi") and $logger->logdie("couldn't touch $statusdir/ncbi:[$!]");
  $logger->info("Run ncbi seq update");
}else{
  $logger->info("Already updated ncbi sequence database");
}

#-------------------------------------------------------------------------------
#Update metaseq!(takes 0 mins )

#At the moment there is not update
$logger->warn('Until we have a good source of metaseq data, there is nothing to do.'); 

#-------------------------------------------------------------------------------
#SVN commit new sequence databases
#TODO

#-------------------------------------------------------------------------------
# InterPro and Go data (takes less than 15 mins)

if(! -e "$statusdir/done_upload_interpro") {
  $logger->info("Preparing to upload the new interpro database and GO data\n");
  system("pud-update_interpro_and_go.pl") and $logger->logdie( "pud-update_interpro_and_go.pl failed:[$!]" );  
  system("touch $statusdir/done_upload_interpro") and $logger->logdie( "can't touch $statusdir/done_upload_interpro:[$!]");
}else{
  $logger->info("Already done upload on interpro and GO\n");
}

#-------------------------------------------------------------------------------
#Update the interactions table!(takes 0 mins )

#At the moment there is not iPfam, so we are not going to have to do anything
$logger->warn('Until iPfam is resurrected, there is nothing to do.'); 

#-------------------------------------------------------------------------------
#Update the pdb data
# This took 2.5 without the upload (done manaually afterwards) - takes about another 2 hours to upload
unless(-e "$statusdir/done_update_pdb" ){
  $logger->info("Preparing to fetch all the latest pdb data.");
  system("pud-getPdbData.pl") 
    and $logger->logdie("Failed to run pud-getPdbData.pl:[$!]");
  $logger->info("Updated pdb data");
  system("touch $statusdir/done_update_pdb") 
    and $logger->logdie("Failed to touch $statusdir/done_update_pdb");
}else{
  $logger->info("Already done pdb upload\n");
}

#-------------------------------------------------------------------------------
#Calculate the other regions - This takes about 5 hours to calculate....Hmm we may want to fork here (rdf)
unless(-e "$statusdir/done_other_reg_update"){
  $logger->info("Updating other_reg table");
  system("pud-other-reg.pl $pfamseqdir$newrelease_num") 
    and $logger->logdie("Failed to calculate othe regions:[$!]");
  $logger->info("Finished updating other_reg table");
  system("touch $statusdir/done_other_reg_update") 
    and $logger->logdie("Failed to touch $statusdir/done_other_reg_update");
}else{
  $logger->info("Already done other_reg upload\n"); 
}
#-------------------------------------------------------------------------------
#Seed surgery part

#Make all of the directories we need!
my $migrationDir = $config->productionLoc."/SeqDbMigration$newrelease_num";
unless(-d $migrationDir){
  mkdir($migrationDir) or $logger->logdie("Could not make migration dir, $migrationDir:[$!]");  
}else{
  $logger->info("Made directory $migrationDir");  
}

foreach my $d (qw(Families SURGERY)){
  unless(-d "$migrationDir/$d"){
    mkdir("$migrationDir/$d") or $logger->logdie("Could not make migration dir, $migrationDir/$d:[$!]");  
  }else{  
    $logger->info("Made directory $migrationDir/$d");  
  }
}

#Now check out all families. - this failed after about 4 hours with an SVN error.
#Checked the rest of the families out by hand. Will investigate the SVN error......
unless(-e "$statusdir/done_checked_out_families") {
  my $client = Bio::Pfam::SVN::Client->new;
  $logger->info("Proceeding to check out all families into $migrationDir/Families");
  $client->checkoutAllFamilies($migrationDir."/Families");
  open(F, ">$statusdir/done_checked_out_families") 
    or $logger->logdie("Could not open $statusdir/done_checked_out_families");
  close(F);
}else{
  $logger->info("Already check out all families into $migrationDir/Families");
}


#Now check that the families agree with the database.
#TODO.....Although this should never be out of sync.... ;-)


#Find out which families require seed surgery and fix them. This will also check out families that do not require surgery....
if(! -e "$statusdir/done_seed_surgery") {
  $logger->info("Finding out which families require seed surgery\n"); 
  system("pud-seed-surgery.pl -families $migrationDir/Families -surgery $migrationDir/SURGERY -md5file $statusdir/oldpfamseq".$oldrelease_num.".dat")
    and $logger->logdie( "pud-seed-surgery.pl failed:[$!]");
  system("touch $statusdir/done_seed_surgery") and die "can't touch $statusdir/done_seed_surgery:[$!]";
  $logger->info("Determined which families require seed surgery and fixed them\n");
}
else {
  $logger->info("Already determined which families require seed surgery\n");
}

#-------------------------------------------------------------------------------
### There was an issue with some SEED surgery families having zero sized SEEDs
### make sure to check this out and work out where their coming from - Alex
# 10 minutes to run
if (! -e "$statusdir/check_SURGERY_SEEDS_have_size"){
  $logger->info("Check SURGERY SEEDs all have size");
  system ("pud-checkfile.pl -size $migrationDir/SURGERY SEED > $statusdir/check_SURGERY_SEEDS_have_size") 
    and $logger->logdie("Could not run pud-checkfile:[$!]");
  system("touch $statusdir/check_SURGERY_SEEDS_have_size") 
    and $logger->logdie("Could not touch $statusdir/check_SURGERY_SEEDS_have_size:[$!]");
}

if (! -e "$statusdir/check_NON_SURGERY_SEEDS_have_size"){
  $logger->info("Check NON_SURGERY SEEDs all have size");
  system ("pud-checkfile.pl -size $migrationDir/Families SEED > $statusdir/check_NON_SURGERY_SEEDS_have_size")
    and $logger->logdie("Could not run pud-checkfile:[$!]");
  system("touch $statusdir/check_NON_SURGERY_SEEDS_have_size") 
    and $logger->logdie("Could not touch $statusdir/check_NON_SURGERY_SEEDS_have_size:[$!]");
}

if (! -z "$statusdir/check_SURGERY_SEEDS_have_size"){
    die("Some SURGERY SEEDs are zero sized. Check file $statusdir/check_SURGERY_SEEDS_have_size for details");
}  else {
    $logger->info("SURGERY SEEDs all have size");
}

if (! -z "$statusdir/check_NON_SURGERY_SEEDS_have_size"){
    die("Some NON_SURGERY SEEDs are zero sized. Check file $statusdir/check_NON_SURGERY_SEEDS_have_size for details");
}  else {
    $logger->info("NON SURGERY SEEDs all have size");
}

#-------------------------------------------------------------------------------

#Unlock the database
# Unlock the database at this point ready for check ins
if(! -e "$statusdir/database_unlocked") {
    &report("Unlocking database\n");
    system ("pflock -u") and die "Cannot run pflock:[$!]";
    system("touch $statusdir/database_unlocked") and die "Cannot run touch:[$!]";
} else {
    &report("Already unlocked database\n");
}
#-------------------------------------------------------------------------------
if (! -e "$statusdir/searched_all_non_surgery_families"){
  $logger->info("Going to submit all non-surgery families for searching");
  opendir(F, "$migrationDir/Families");
  my @dir = readdir(F);
  closedir(F);
  foreach my $fam (@dir){
    next unless($fam =~ /PF\d{5}/);
    $logger->debug("Going to submit pfbuild for $fam");
    chdir( "$migrationDir/Families/$fam" ) or 
      $logger->logdie("Failed to cd to $migrationDir/Families/$fam:[$!]");
    if(-e "ALIGN"){
      move("ALIGN", "ALIGN.b4mig") or
        $logger->logdie("Failed to move ALIGN sideways because:[$!]");
    } 
    eval{
      system("pfbuild.pl -withpfmake");
    }; 
    if($@){
      $logger->warn("Failed to run pfbuild for $fam: $@");
      open(E, ">>".$statusdir."/pfbuildError.log") or 
        $logger->logdie("Failed to open $statusdir./pfbuildError.log");
      print E "\nFailed to run pfbuild for $fam: $@\n\n";
      close(E);
    }
    chdir($pwd) or $logger->logdie("Failed to cd to $pwd:[$!]");
  }
  system("touch $statusdir/searched_all_non_surgery_families") and 
    $logger->info("Can not touch $statusdir/searched_all_non_surgery_families:[$!]");   
} else {
    &report("Already carried out pfbuilds for non-surgery families");
}

# Tell user to wait until all SEED surgery families have been checked
if(! -e "$statusdir/seed_surgery_alignments_checked") {
  my $ans=0;
  until ($ans eq 'y') {
    $logger->info("Have all the SEED surgery alignments been checked and modified where needed?".
	  " If not go and do this now.  When you have done this come back and press 'y'".
	  " and the program will continue\n");
	 chomp ($ans = <STDIN>);
  }
  system ("touch $statusdir/seed_surgery_alignments_checked")
    and $logger->logdie("Can not touch $statusdir/seed_surgery_alignments_checked:[$!]");
}
$logger->info("Already checked all SEED surgery family alignments - program continuing...\n");


#-------------------------------------------------------------------------------

# BREAKING HERE UNTIL SURGERY HAS BEEN COMPLETED
exit;
#-------------------------------------------------------------------------------
#Assuming all surgery has been performed and families moved back into families dir.

#Check all families have built - list failures. Do not proceed until failure = 0



#Run my overlap script 

#Wait until all overlaps resolved......



# Should put overlap resolution and clan building into an iterative cycle with overlap checks.

#Do overlap checks
if(! -e "$statusdir/resolved_overlaps") {
    &report("Looking for overlaps\n");
    system ("pud-overlap.pl $config->productionLoc/ALL_FAMILIES") and die "pud-overlap.pl failed:[$!]";

    # Check size of overlaps
    if (! -s "$config->productionLoc/ALL_FAMILIES/overlaps"){
	&report("Congratulations you appear to have resolved all overlaps");
	system("touch $statusdir/resolved_overlaps") and die "Cannot run touch:[$!]";
    } else {
	&report("Overlaps still remain to be resolved. Exiting");
    }
} else {
    &report("Already resolved overlaps");
}

#-------------------------------------------------------------------------------
#lock database
#Ensure that the clan dequeuer is offline

# Check in all families
# In release 23 found a lot of families had not been checked out!
# Wrote a little helper program to fix this see ~agb/Scripts/fix_ci.pl
if(! -e "$statusdir/families_checked_in") {
    &report("Check in all families\n");
    chdir "$config->productionLoc/ALL_FAMILIES" or die "Can not change dir to $config->productionLoc/ALL_FAMILIES :[$!]";

    system ("pud-ci.pl \'Release $newrelease_num update\'") and die "pud-ci.pl failed, serious problem!!!:[$!]";
    system("touch $statusdir/families_checked_in") and die "Cannot run touch:[$!]";
} else {
    &report("Already checked in all families\n");
}

#Start up the clan dequeuer



# Now check that everything got checked in correctly and has message
if(! -e "$statusdir/all_checked_in") {
    &report("Test all families checked in OK\n");
    
    my $dir="$config->productionLoc/ALL_FAMILIES";
    opendir(DIR,$dir);
    my @families = readdir(DIR);
    closedir(DIR);
    
    shift @families;
    shift @families;
    
    open (FILE, "> $statusdir/problem_check_ins") or die "Cannot write to file problems";;
    
    # loop through list and run pfinfo.
    my $good=0;
    my $bad=0;
    my $clan="";
    
  FAMILY: foreach my $family ( @families ) {
      my $clan="";
      if( ! -d $family ) {
	  warn "$family is not a directory!";
	  next;
      }
      
      open (PFINFO, "pfinfo $family |") or die "Cannot pfinfo $family";
      while(<PFINFO>){
	  # Find out if family was checked in
	  if (/Release $newrelease_num update/){
	      $good++;
	      next FAMILY;
	  }
	  if (/This family is part of Clan: (\S+)/){
	      $clan=$1;
	  }
      }
      close PFINFO;
      
      $bad++;
      print FILE "$family not updated! $clan\n";
  }
    close FILE;
    print "Checked in: $good\tNot checked in: $bad\n";

    if ($bad ==0){
	system("touch $statusdir/all_checked_in");
	&report("Great! All families are checked in OK!\n");
    } else {
	&report("Shame! You still have $bad families to check in. Look at file $statusdir/problem_check_ins\n");
    }
} else {
    &report("Already checked that all families are checked in\n");
}


# This script does not appear to exist - Alex
#
# run pud-check_view_seqs.pl
#
# Although we do need to address problems of bad ED lines neither of the suggested
# scripts appears to be servicable currently. - Alex
#
# pud-remove_ed.pl <everything_dir>    (need to pfmake in each fam)
# This script does not seem to work correctly. For example it reports that
# ED lines such as Q10WB7.1/120-222 in Ion_trans are not needed when it clearly is!
#
# pud-removebadEDlines.pl (run from everything_dir) # I think this script doesn't work
# Just seems to remove all ED lines!

       
sub help {

print STDERR <<EOF;

This program does release stuff... 

Usage: $0 -new <new release number> -old <old release number> 

Example:  $0 -new 17 -old 16 

-help will print this help message

EOF

exit (1);
}
