#!/usr/local/bin/perl

=head1 NAME

makepfamview - makes view files on the farm.  Although the same in name, this is very different to
the old makepfamview.pl script.

=head1 SYNOPSIS

makepfamview.pl -family <pfam-id> -id <Data::UUID>

=head1 DESCRIPTION

The view files which are made are as follows:
   
  SEED.ann   - SEED with stockholm markup above it
  ALIGN.ann  - ALIGN with stockholm markup above it
  HMM_ls.ann - ls mode HMM with Stockholm markup
  HMM_fs.ann - fs mode HMM with Stockholm markup

=cut

#Core Perl Modules
use strict;
use warnings;
use Getopt::Long;
use Mail::Mailer;
use Log::Log4perl qw(:easy);
use Data::Dumper;
use Data::UUID;
use Digest::MD5 qw(md5_hex);
use Cwd;
use Text::Wrap;
$Text::Wrap::columns = 75;

#Pfam Modules
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::Active_site::as_align;
use Bio::Pfam::FamilyIO;
use HMM::Profile;



#######################################
# Globals that we may want to change! #
#######################################

#% identity used to threshold families for fasta file generation 
my $identity = 90;
#P-values threshold
my $threshold = 0.1;


#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my ($famId, $uid, $help, $tree);

#Get and check the input parameters
GetOptions(
  "family=s" => \$famId,
  "id=s"     => \$uid,  
  "fasta=s"  => \$identity,
  "pvalue=s" => \$threshold,
  "tree=s"   => \$tree,
  "h"        => \$help
);

if($help){
  help(); 
  exit;
}


if(!$uid){
  if($famId){
    mailPfam("Failed to run view process for $famId", "No job id passed to $0"), 
  }else{
    mailPfam("Failed to run view process for a family", "No job id or family id passed to $0"), 
  }
  help();
  $logger->logdie("FATAL:No job id passed to script....."); 
}

unless($famId){
  #If we do not have a family identifier, then we can potentially recover if we have a jobId
  $logger->warn("No Pfam family name supplied, will try and get based on the job ID");        
}

#Lets get a new Pfam config object
my $config = Bio::Pfam::Config->new;

######################################################
# This section deals with getting to job information #
######################################################

#Can we get a PfamJobs Database
#TODO - Use Config::General
my $jobDB = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );


unless($jobDB){
  mailPfam("Failed to run view process", "Could not get connection to the pfam_jobs database");  
}
$logger->debug("Got pfam_job db connection");

#Can we find the record of this job
my $job = $jobDB->getSchema
            ->resultset('JobHistory')
              ->find({'job_id' => $uid});

unless($job){
  mailPfam("Failed to run view process for $famId", "Could not get job information for $uid"); 
}
$logger->debug("Got job databse object");

if($famId){
  if($famId ne $job->entity_id){
    mailPfam("Failed to run view process for $famId", "Miss-match between family id (". $job->entity_id 
                ." and the information contained in the database for $uid");
  }
}

#####################################################
# Test to see if we can get a connection to the RDB #
#####################################################

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

unless($pfamDB){
  mailUserAndFail($job, "View process failed as we could not connect to pfamlive");  
}
$logger->debug("Got pfamlive database connection");

#############################################
# We have enough information to get started #
#############################################
#Change the status of the job from sub to run.
$job->update({status  => 'RUN',
              started => \'NOW()'});

$logger->debug("Updated the status on the job object");

# Get the information about the family
my $pfam = $pfamDB->getSchema
                    ->resultset('Pfama')
                      ->find({pfama_acc => $job->entity_acc });


unless ($pfam and $pfam->pfama_acc){
  mailUserAndFail($job, "Failed to get the pfam entry for family"); 
}
$logger->debug("Got pfam family databse object");


##Now get the alignments and HMMs out of the database

my $align = $pfamDB->getSchema
                      ->resultset('PfamaInternal')
                        ->find({ auto_pfama => $pfam->auto_pfama });

                                 
open(S, ">SEED") or mailUserAndFail($job, "Could not open SEED:[$!]\n");
open(A, ">ALIGN") or mailUserAndFail($job, "Could not open ALIGN:[$!]\n");
  
print S Compress::Zlib::memGunzip($align->seed);
print A Compress::Zlib::memGunzip($align->full);
close A;
close S;


open(H, ">HMM") or mailUserAndFail($job, "Could not open HMM:[$!]\n");
my $hmm = $pfamDB->getSchema
        ->resultset('PfamaHmm')
          ->find({ auto_pfama => $pfam->auto_pfama});

print H $hmm->hmm;
close(H);  

#Check that all of the files are present in the directory, this script assumes that all of the files 
#are in the cwd.
foreach my $f (qw(ALIGN SEED HMM DESC)){
   unless(-e $f and -s $f){
      mailUserAndFail($job, "View process failed as $f was not present\n");  
   }
}
$logger->debug("All family files are present");

#Get database Xrefs for entries
my @xRefs = $pfamDB->getSchema
                    ->resultset('PfamaDatabaseLinks')
                      ->search({auto_pfama => $pfam->auto_pfama});
$logger->debug("Got ". scalar(@xRefs) ." database cross-references");

#Get database literature references
my @litRefs = $pfamDB->getSchema
                    ->resultset('PfamaLiteratureReferences')
                      ->search({auto_pfama => $pfam->auto_pfama},
                               {join     => [qw(literature)],
                                order_by => 'order_added ASC',
                                prefecth => [qw(literature)] });
$logger->debug("Got ". scalar(@litRefs) ." literature references");

#Calculate p value
my $pvalue = exp(-1 * $pfam->lambda * ($pfam->domain_ga - $pfam->mu));
if($pvalue > $threshold) {
   mailUserAndFail($job, $pfam->pfama_id." ls model has failed p value qc check (p value is $pvalue - too big!)");
}


#Run a md5 checksum on the "raw" files and compared to the release versions......!
versionFiles($pfamDB, $pfam, $job);

my $nested_locations = getNestedLocations($pfam->auto_pfama, $pfamDB);
my $clan = getClanData( $pfam->auto_pfama, $pfamDB);

#Active site prediction object
my $asp;

    
#Now start processing the SEED and ALIGN files.  These have to be done before the HMMs.
# The following steps are carried out during this loop:
# 1) Calculation of the consensus strings
# 2) Reformating of the alignments (changing accessions to identifiers).
# 3) Calculation of cigar strings
# 4) Estimation of the phylogenetic tree
# 5) Reordering of the alignments according to the tree
# 6) Making a non-redundant fasta file
# 7) Active site prediction


#Order of the files is important!
foreach my $filename (qw(ALIGN SEED)){
  
 
  #Read the alignment into an object
  open(ALIGN, "$filename") or mailUserAndFail( "Could not open $filename file for reading:[$!]\n");
  my $a = Bio::Pfam::AlignPfam->new();
  $a->read_stockholm(\*ALIGN, 1 );


  #Now query the database to get all of the ids/acc for this family.
  my (%regs, $ali);
  if($filename eq "ALIGN"){
    $logger->debug("Getting sequence identifiers for ALIGN");
    
    #Get all of the region information from the database. This will allows to performs some
    #rudimentary QC and more importantly, exchange accessions for ids in the alignment
    my @regs = $pfamDB->getSchema
	->resultset('PfamaRegFullSignificant')
	->search( { auto_pfama => $pfam->auto_pfama,
		    in_full    => 1 },
		  { join       => [ qw (pfamseq) ],
		    prefetch   => [ qw (pfamseq) ] });                              
    %regs = map {$_->pfamseq_acc.".".$_->seq_version."/".$_->seq_start."-".$_->seq_end => $_} @regs;
    
    
    #Need to remove sequences in alignment which are outcompeted if family is in a clan
    if($a->no_sequences eq @regs) { 
	if($ali->no_sequences != $pfam->num_full) {
	    mailUserAndFail($job, "Missmatch between number of regions in competed PfamA table ($#regs) and competed ALIGN file (".$ali->no_sequences.")");
	}
	$ali = $a;
    }
    else {
	$ali = Bio::Pfam::AlignPfam->new();
	
	foreach my $seq ($a->each_seq) {
	    if(exists($regs{$seq->id.".".$seq->seq_version."/".$seq->start."-".$seq->end})) {
		$ali->add_seq($seq);
	    }
	}
        unless($ali->no_sequences eq @regs) {
	   mailUserAndFail($job, "Missmatch between number of regions in competed PfamA table ($#regs) and competed ALIGN file (".$ali->no_sequences.")");
        }
    } 
  }  

  #Calculate the consensus line at 60% threshold 
  #We may want to make this user defined/overridable?
  $logger->debug("Running consensus.pl on $filename");   
  my $consensus = consensus_line($filename, $ali->length);
  $logger->debug("Finished running consensus.pl on $filename");


  if($filename eq "ALIGN"){
        
     #This hash contains all of the information we require to calculate the domain coverage state.
     #This statistic tries to estimate the average coverage of the seqeunce that the domain in question covers.
     
     my( $no_aa_in_domain, $no_aa_in_sequences);
     my %species;
     
     foreach my $nse (keys %regs){
        $species{ $regs{$nse}->ncbi_taxid } = 1;
        $no_aa_in_sequences += $regs{$nse}->length;
        $no_aa_in_domain += ($regs{$nse}->seq_end - $regs{$nse}->seq_start + 1);
     }
     
     #This part runs alistat on the alignment to get the average length and % ID
     my($averageLength, $percentageId);
     open(ALI,"alistat -f ALIGN |") or mailUserAndFail($job, "Could not open alistat pipe:[$!]");
     #Grab the fields out of alistat
     while(<ALI>) {
      if(/^Average length:\s+(\S+)/){
        $averageLength = $1;
      }elsif(/^Average identity:\s+(\d+)/){ 
         $percentageId = $1; 
      }
     }
     close(ALI);
     
     #Calculate the species and coverage jobs 
     my $averageCoverage = sprintf "%.2f", 100*($no_aa_in_domain/$no_aa_in_sequences);
     my $noSpecies = scalar(keys %species);
     
     $logger->debug("Average sequence coverage by the domain is $averageCoverage");
     $logger->debug("Average length is $averageLength");
     $logger->debug("Number of species is $noSpecies");
     $logger->debug("Percentage identity is $percentageId");
     
     #Enter family stats into the pfamA table
     $pfam->update({  percentage_id    => $percentageId,
                      number_species   => $noSpecies,
                      average_length   => $averageLength,
                      average_coverage => $averageCoverage,
                      full_consensus   => $consensus});

    #Predict active site residues
    $logger->debug("Going to add active site data to FULL");
    $asp = Bio::Pfam::Active_site::as_align->new( -alignment => $ali, -auto => $pfam->auto_pfama, -database => $pfamDB, -nested => $nested_locations );
    $ali = $asp->full;
                      
  }elsif($filename eq "SEED"){
    $logger->debug("Getting sequences identifiers for SEED");
    my @regs = $pfamDB->getSchema
                    ->resultset('PfamaRegSeed')
                      ->search( { auto_pfama => $pfam->auto_pfama },
                                { join       => [ qw (pfamseq) ],
                                  prefetch   => [ qw (pfamseq) ], 
								        key        => 'pfamA_reg_seed_reg_idx'});                              
    %regs = map {$_->pfamseq_acc.".".$_->seq_version."/".$_->seq_start."-".$_->seq_end => $_} @regs;
     
    #QC check
    if(($ali->no_sequences != $pfam->num_seed) or ($ali->no_sequences != scalar(@regs))){
        mailUserAndFail($job, "Missmatch between number of regions in PfamA table (num_seed),".
          " number of regions from PfamA_reg_seed and/or alignment on disk");
    }
    $pfam->update({ seed_consensus   => $consensus});

    #Add predicted active site residues
    $logger->debug("Going to add active site data to SEED");
    $ali = $asp->seed($ali);
  }
  
  $logger->debug("Finished getting sequence accessions");


  #Now exchange the accessions for ids
  foreach my $s ($ali->each_seq) {
   $s->acc( $s->id ); # id from alignment files is actually an accession
	 if ($regs{ $s->acc.".".$s->version."/".$s->start."-".$s->end }) {
	  $s->id( $regs{ $s->acc.".".$s->version."/".$s->start."-".$s->end }->pfamseq_id );
	 }else{
	    &mailUserAndFail($job, "Could not find id for ".$s->acc.".".$s->version."/".$s->start."-".$s->end);
	 }
  }
  $logger->debug("Exchanged identifers for accessions");
  
  my %seq_order;


  #Now run FastTree on the original alignment file
  $logger->debug("Going to run FastTree on $filename"); 
  make_tree($filename, \%regs, "pfamseq");


  #Now change the alignment from accessions to ids.
  #Make a new alignment object
  my $aliIds = Bio::Pfam::AlignPfam->new();
  
  #See of there is a match state string, if so add it to the object.
  if (defined $ali->match_states_string()) {
      $aliIds->match_states_string($ali->match_states_string());
  }
  
  $aliIds->cons_sequence( Bio::Pfam::OtherRegion->new('-seq_id' => 'none',
						        '-from' => 1,
                                			  '-to' => length($consensus),
						        '-type' => "60\%_consenus_sequence",
						     '-display' => $consensus,
					              '-source' => 'Pfam') ); 


   #Now add the sequences back to the object.
   foreach my $seq ($ali->each_seq){
      #Calculate the cigarstring for the object
      my $cString = _generateCigarString($seq->seq, $job);
      $regs{ $seq->acc.".".$seq->version."/".$seq->start."-".$seq->end }->cigar($cString);
	  $logger->debug( $seq->acc.".".$seq->version."/".$seq->start."-".$seq->end );
      $regs{ $seq->acc.".".$seq->version."/".$seq->start."-".$seq->end }->update();
      $aliIds->add_seq($seq, $regs{ $seq->acc.".".$seq->version."/".$seq->start."-".$seq->end }->tree_order); 
   }
   
   
   #Add the secondary structure
   $logger->debug("Adding secondary structure data to $filename");
   #First, get the secondary structure data from the database
   my ($dsspDataRef) = _getDsspData($pfam->auto_pfama, $filename, $pfamDB, $job);
   if($dsspDataRef and scalar( keys(%$dsspDataRef) ) ){
    $logger->debug("Found some SS data");
    #Now add this to the alignment
    my ($noStructures, $map) = markupAlignWithSS($aliIds, $dsspDataRef, $job);
    if($filename eq "ALIGN"){
       if($noStructures){
          $pfam->update({number_structures => $noStructures});
       }
       #Delete all auto_pfamAs
       $pfamDB->getSchema
                  ->resultset('PdbPfamaReg')
                    ->search({ auto_pfamA => $pfam->auto_pfama})->delete;
       #Add the pdbmap data to pdb_pfamA_reg
       my %autoPdbs;
       foreach my $nse (keys %$map){
        foreach my $pdbReg ( @{$map->{$nse}}){
          $logger->debug("Inserting row into PdbPfamaReg ".$regs{$nse}->auto_pfama);
          unless( $autoPdbs{ $pdbReg->{pdb_id}}){
             $autoPdbs{$pdbReg->{pdb_id}} = $pfamDB->getSchema
                                                 ->resultset('Pdb')
                                                  ->find({ pdb_id => $pdbReg->{pdb_id} })
                                                   ->auto_pdb;
          }
          #Now reinsert
          $pfamDB->getSchema
                  ->resultset('PdbPfamaReg')
                    ->create({ auto_pfama_reg_full => $regs{$nse}->auto_pfama_reg_full,
                               auto_pdb            => $autoPdbs{ $pdbReg->{pdb_id }},
                               auto_pfama          => $regs{$nse}->auto_pfama,
                               auto_pfamseq        => $regs{$nse}->auto_pfamseq,
                               chain               => $pdbReg->{chain},
                               pdb_res_start       => $pdbReg->{pdb_start},
                               pdb_res_end         => $pdbReg->{pdb_end},
                               seq_start           => $pdbReg->{seq_start},
                               seq_end             => $pdbReg->{seq_end},
                               });
        }
       }
    }
   }else{
    $logger->debug("Did not find any secondary structure information"); 
   }
   
  write_stockholm_file($filename, $aliIds, $pfam, "pfamseq");   

   
   
  
  #Make family non-redundant for fasta
  if($filename eq "ALIGN"){
    makeNonRedundantFasta($pfam, $identity, $pfamDB, $job);
  }
    
  #Make html versions of the alignment
  my $type;
  if($filename eq 'SEED'){
    $type = 'seed';
  }elsif($filename eq 'ALIGN'){
    $type = 'full'; 
  }
  makeHTMLAlign($filename, $job, 80, $type);
  
  
  #Upload the alignments (stockholm and html) and tree into the database 
  uploadTreesAndAlign($filename, $pfamDB, $pfam, $job, $type);
}

#Now work on the HMMs
$logger->debug("Going to process the HMMs");
processHMMs($pfam, $pfamDB, $job);
$logger->debug("Finished processing the HMMs");

$logger->debug("Going to check the SEED.ann and ALIGN.ann files for errors");
#Okay - Everything should be in the Stockholm file!   
#Now run some QC on the files
foreach my $f (qw(ALIGN.ann SEED.ann)){
  open(QC, "checkflat.pl -v $f|") or mailUserAndFail($job, "Failed to run checkflat.pl on $f:[$!]");
  my $qc = join("", <QC>);
  if($qc =~ /\S+/){
    mailUserAndFail($job, "$f did not pass quality control! Got $qc");
  }
  close(QC);
}
$logger->debug("SEED.ann and ALIGN.ann files passed checks");
  
$logger->debug("Going to check the HMM.ann files for errors");
#Now run QC on the HMMs 
my $dbVersion = $pfamDB->getSchema
                          ->resultset('Version')
                            ->find({});
                            
foreach my $f (qw(HMM.ann)){
    open(QC, "checkhmmflat.pl -v -hmmer ".$dbVersion->hmmer_version." -f $f |") or mailUserAndFail($job, "Failed to run checkhmmflat.pl on $f:[$!]");
    my $qc = join("", <QC>);
    if($qc =~ /\S+/){
      mailUserAndFail($job, "$f did not pass quality control! Got [$qc]");
    }
    close(QC);
}
$logger->debug("HMM.ann pass checks");

#Get cwd
my $cwd = getcwd;


#Remove files, but keep SEED and HMM
unlink glob("SEED.*");
unlink glob("ALIGN*");
unlink glob("HMM.*");


#Start the ncbi searches
$logger->debug("Starting ncbi pfbuild");
system("pfbuild.pl -nobuild -local -withpfmake -db ncbi") and mailUserAndFail($job, "Failed to run pfbuild against ncbi database:[$!]");
my $ncbiFamilyIO = Bio::Pfam::FamilyIO->new;
my $ncbiFamObj = $ncbiFamilyIO->loadPfamAFromLocalFile( "", $cwd );

$logger->debug("Updating ncbi_pfamA_reg");
$pfamDB->updateNcbiPfamA($ncbiFamObj);

my @ncbiRegs = $pfamDB->getSchema->resultset('NcbiPfamaReg')->search( { auto_pfama => $pfam->auto_pfama});
my %ncbiRegs = map {$_->gi->gi."/".$_->seq_start."-".$_->seq_end => $_} @ncbiRegs;

$logger->debug("Running FastTree on ncbi alignment");
#Run FastTree
make_tree("ALIGN", \%ncbiRegs);

my $ncbiAln = Bio::Pfam::AlignPfam->new();

#Add consensus
my $ncbiConsensus = consensus_line("ALIGN", $ncbiFamObj->ALIGN->length);

$ncbiAln->cons_sequence( Bio::Pfam::OtherRegion->new('-seq_id' => 'none',
						        '-from' => 1,
                                			  '-to' => length($ncbiConsensus),
						        '-type' => "60\%_consenus_sequence",
						     '-display' => $ncbiConsensus,
					              '-source' => 'Pfam') ); 


$logger->debug("Generating cigar string for ncbi alignment");
#generate and upload cigar string, and upload tree order
foreach my $seq ( $ncbiFamObj->ALIGN->each_seq ) {
    my $cString = _generateCigarString($seq->seq, $job);
    $ncbiRegs{$seq->id."/".$seq->start."-".$seq->end }->cigar($cString);
    $logger->debug( $seq->id."/".$seq->start."-".$seq->end );
    $ncbiRegs{ $seq->id."/".$seq->start."-".$seq->end }->update();
    $ncbiAln->add_seq($seq, $ncbiRegs{ $seq->id."/".$seq->start."-".$seq->end }->tree_order); 
}



write_stockholm_file("ALIGN", $ncbiAln, $pfam, "ncbi", $ncbiFamObj);

#Make html versions of the alignment
makeHTMLAlign("ALIGN", $job, 80, "ncbi",  $ncbiFamObj);

#Upload the alignments (stockholm and html) and tree into the database 
uploadTreesAndAlign("ALIGN", $pfamDB, $pfam, $job, "ncbi");

#Remove files, but keep SEED and HMM
unlink glob("SEED.*");
unlink glob("ALIGN*");
unlink glob("HMM.*");


#Start the metaseq searches
$logger->debug("Starting metaseq pfbuild");
system("pfbuild.pl -nobuild -local -withpfmake -db metaseq") and  mailUserAndFail($job, "Failed to run pfbuild against metaseq:[$!]");
my $metaFamilyIO = Bio::Pfam::FamilyIO->new;
my $metaFamObj = $metaFamilyIO->loadPfamAFromLocalFile( "", $cwd);


#The version numbers (if present) will be removed from the ids, so add them back
foreach my $seq ( $metaFamObj->ALIGN->each_seq ) {
    if($seq->version) {
	$seq->id($seq->id. "." . $seq->version);
    }
}


$logger->debug("Updating meta_pfamA_reg");
$pfamDB->updateMetaPfamA($metaFamObj);

my @metaRegs = $pfamDB->getSchema->resultset('MetaPfamaReg')
                                  ->search( { auto_pfama => $pfam->auto_pfama},
                                    { join       => [ qw (auto_metaseq) ],
                                      prefetch   => [ qw (auto_metaseq) ] });                              


my %metaRegs = map {$_->auto_metaseq->metaseq_acc."/".$_->seq_start."-".$_->seq_end => $_} @metaRegs;



$logger->debug("Running FastTree on metagenomics alignment");
#Run FastTree
make_tree("ALIGN", \%metaRegs);

my $metaAln = Bio::Pfam::AlignPfam->new();
my $metaConsensus = consensus_line("ALIGN", $metaFamObj->ALIGN->length);

$metaAln->cons_sequence( Bio::Pfam::OtherRegion->new('-seq_id' => 'none',
						        '-from' => 1,
                                			  '-to' => length($metaConsensus),
						        '-type' => "60\%_consenus_sequence",
						     '-display' => $metaConsensus,
					              '-source' => 'Pfam') ); 




$logger->debug("Generating cigar string for metagenomics alignment");
#generate and upload cigar string
foreach my $seq ( $metaFamObj->ALIGN->each_seq ) {
    my $cString = _generateCigarString($seq->seq, $job);
    $metaRegs{$seq->id."/".$seq->start."-".$seq->end }->cigar($cString);
    $logger->debug( $seq->id."/".$seq->start."-".$seq->end );
    $metaRegs{ $seq->id."/".$seq->start."-".$seq->end }->update();
    $metaAln->add_seq($seq, $metaRegs{ $seq->id."/".$seq->start."-".$seq->end }->tree_order); 
}




write_stockholm_file("ALIGN", $metaAln, $pfam, "meta", $metaFamObj ); 

#Make html versions of the alignment
makeHTMLAlign("ALIGN", $job, 80, "meta");

#Upload the alignments (stockholm and html) and tree into the database 
uploadTreesAndAlign("ALIGN", $pfamDB, $pfam, $job, "meta");


#Change the job status to done
finishedJob($job);
$logger->debug("Finished");
exit;


###############
# Subroutines #
###############


sub processHMMs {
  my ($pfam, $db, $job) = @_;
  
  unlink ("HMM.ann") or mailUserAndFail($job, "Failed to remove HMM.ann") if(-e "HMM.ann");
 
  
  #The next two blocks rebuilds and recalibrates the HMM and adds the curated thresholds to the HMM
  
  #process the HMM_ls file first

  my $buildline = cleanBuildLine($pfam->buildmethod);


  $logger->debug("Going to run hmmbuild with the following line: $buildline HMM.ann SEED.ann");
  system($config->hmmer3bin."/$buildline -o  /dev/null HMM.ann SEED.ann")
    and mailUserAndFail($job, "Failed to build HMM.ann, using $buildline HMM.ann SEED.ann");
    
  #Take HMM_ls and add the thresholds into the file 
  open( HMM_OUT, ">HMM.ann.tmp" ) or mailUserAndFail($job, "Could not open HMM.ann.tmp for writing");
  open( HMM, "HMM.ann" ) or mailUserAndFail($job, "Could not open HMM.ann for writing");
  while(<HMM>) {
  	if( /^GA\s+/ ) {
	    print HMM_OUT "GA    ".$pfam->sequence_ga." ".$pfam->domain_ga.";\n";
	    print HMM_OUT "TC    ".$pfam->sequence_tc." ".$pfam->domain_tc.";\n";
	    print HMM_OUT "NC    ".$pfam->sequence_nc." ".$pfam->domain_nc.";\n";
	    print HMM_OUT "BM    ", cleanBuildLine($pfam->buildmethod) , "HMM.ann SEED.ann\n";
	    print HMM_OUT "SM    ", $pfam->searchmethod ,"\n";
	    next;
	 }
	 next if( $_ =~ /^NC\s+/ or $_ =~ /^TC\s+/ );
	 print HMM_OUT $_;
  }
  close HMM;
  close HMM_OUT;
  rename( "HMM.ann.tmp", "HMM.ann" ) or &mailUserAndFail($job, "can't rename HMM.ann.tmp to HMM.ann\n"); 
  
  #Now upload the HMMs into the database
  open(HMM, "HMM.ann") or die;
  my $hmm = join("", <HMM>);
	close(HMM);
	$pfamDB->getSchema
	 ->resultset('PfamaHmm')
	   ->update_or_create({ auto_pfama => $pfam->auto_pfama,
	                        hmm        => $hmm});
  #Now make and generate the HMM logo
  _makeHMMLogo($pfam, "HMM.ann", $pfamDB, $job);
  
}

sub _makeHMMLogo{
  my($pfam, $file, $pfamDB, $job) = @_; 
  
  $logger->debug("Making logo with HMMER2 HMM");
  system($config->hmmer3bin."/hmmconvert -2 HMM.ann > HMM.ann.2")
    and mailUserAndFail($job, "Failed to convert HMM.ann, using hmmconvert");
  
  
  #Read in the HMM_ls file
  $file .= ".2";
  my $logo = HMM::Profile->new(-hmmerfile=>$file) or
    mailUserAndFail($job, "Failed in making HMM logo, couldn't open $file!\n");
  my $outfile = "hmmLogo.png";    
  my $graph_title = $logo->name();
  my $ysize = 500;
  my $xsize = $logo->length() * 34;
  my $greyscale = 0;
  
  #Now go and make the logos
  $logger->debug("Drawing Logo...");
  $logo->draw_logo(
		     -file	  => $outfile,
		     -xsize	  => $xsize,
		     -ysize	  => $ysize,
		     -x_title	  => 'Relative Entropy',
		     -y_title	  => 'Contribution',
		     -graph_title => $graph_title,
		     -greyscale	  => $greyscale
		    ) || mailUSerAndFail("Error writing $file!\n");
    
  $logger->debug("Finished drawing Logo...");
  unless(-s "hmmLogo.png"){
    mailUserAndFail($job, "Failed in making HMM logo, no hmmLogo.png file");
  }
  
  open(LOGO, "hmmLogo.png") or  mailUserAndFail($job, "Failed to open hmmLogo.png file:[$!]");;
  my $hmmLogo = join("", <LOGO>);
  close(LOGO);
  
  #Now upload this logo into the RDB.    
  $pfamDB->getSchema
	   ->resultset('PfamaHmm')
	   ->update_or_create({ auto_pfama => $pfam->auto_pfama,
	                        logo     => $hmmLogo});
}


sub _generateCigarString  {
    my ($str, $job) = @_;
    chomp($str);
    my @chars = split //, $str;

    my $count_for_cigar_string = 0;
    my $state_for_cigar_string = 'M';
    my $cigar_string = '';
    foreach my $char (@chars){
        #print "$char";
        my $new_state;
        if($char ne '.' and $char ne "-"){ # Match
            $new_state = 'M';
        }elsif($char eq '.'){#Gap
            $new_state = 'I';
        }elsif($char eq '-'){
            $new_state = 'D';
        }else{
            mailUserAndFail($job, "Error generating cigar string......unknown string char, $char");
        }

        if($new_state ne $state_for_cigar_string){
            if ($count_for_cigar_string){
                my $sub_cigar_string;
                $sub_cigar_string = $count_for_cigar_string unless $count_for_cigar_string == 1;
                $sub_cigar_string .= $state_for_cigar_string;
                $cigar_string .= $sub_cigar_string;
            }
            $count_for_cigar_string = 0;
        }
        $state_for_cigar_string = $new_state;
        $count_for_cigar_string++;
    }

    if ($count_for_cigar_string){
        my $sub_cigar_string;
        $sub_cigar_string = $count_for_cigar_string unless $count_for_cigar_string == 1;
        $sub_cigar_string .= $state_for_cigar_string;
        $cigar_string .= $sub_cigar_string;
    }

    return $cigar_string;
}

# 

sub _getDsspData {
  my ($autoPfamA, $filename, $pfamDB, $job) = @_;
  $logger->debug("Going to fetch Secondary structure information for auto_pfamA $autoPfamA");
 
  my (%famDSSP, @dssp);
  if($filename eq 'ALIGN'){
      @dssp = $pfamDB->getSchema
                          ->resultset("PdbResidueData")
                            ->search({auto_pfama => $autoPfamA,
                                      in_full    => 1 },
                                     {join => [qw(auto_pfamseq auto_pdb pfamA_reg_full_significant)],
                                      select => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)],
                                      as     => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)]});

  }elsif($filename eq 'SEED'){
          @dssp = $pfamDB->getSchema
                          ->resultset("PdbResidueData")
                            ->search({auto_pfama => $autoPfamA},
                                      {join => [qw(auto_pfamseq auto_pdb pfamA_reg_seed)],
                                       select => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)],
                                       as     => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)]});
  }else{
    mailUserAndFail($job, "Unknown file name passed in ($filename) to _getDsspData, expected ALIGN or SEED"); 
  }

  #Now stuff it into a data structure for working on
  my $acc2map;
  foreach (@dssp){
    $acc2map->{$_->get_column('pfamseq_acc')}
        ->{$_->get_column('pfamseq_seq_number')}
          ->{$_->get_column('pdb_id')."_".$_->get_column('chain')}
            ->{$_->get_column('pdb_seq_number')} = $_->get_column('dssp_code');
  }
  return ($acc2map);
}
  
sub markupAlignWithSS{
  my ($aln, $dsspDataRef) = @_;
  my $noStructures;
  my $map;
  my @allSsStrings;

  foreach my $seq ($aln->each_seq()) {
    my $start = $seq->start;
    my $end = $seq->end;
    my @ali = split(//, $seq->seq);
    my %ss;
    my $p = $start;
    my %uniqueMaps;
    
    foreach my $pos (keys %{$$dsspDataRef{$seq->acc}}){
      foreach my $pdb_chain (keys %{$$dsspDataRef{$seq->acc}{$pos}}){
	       $uniqueMaps{$pdb_chain} = 1;
      }
    }

    foreach my $res (@ali){
      if($res eq "." or $res eq "-"){
	       #Add $res to the ssString
	       foreach my $pdb_chain (keys %uniqueMaps){
	         $ss{$pdb_chain}{ssString} .= "$res";
	       }
      }else{
      	#Okay, we have a residue
	       foreach my $pdb_chain (keys %uniqueMaps){
	         #Test to see if there is positional information for this residue?
	         if( values %{$$dsspDataRef{$seq->acc}{$p}{$pdb_chain}}){
	           while (my ($pdbResNum, $dsspCode) = each  %{$$dsspDataRef{$seq->acc}{$p}{$pdb_chain}}){
	             if($dsspCode =~ /\S+/){
		              $ss{$pdb_chain}{ssString} .= $dsspCode;
	             }else{
		              $ss{$pdb_chain}{ssString} .= "-";
	             }
	             if(! $ss{$pdb_chain}{start} || !$ss{$pdb_chain}{end}){
		            $ss{$pdb_chain}{start} = $pdbResNum;
            		$ss{$pdb_chain}{end} = $pdbResNum;
            		$ss{$pdb_chain}{seq_start} = $p;
            		$ss{$pdb_chain}{seq_end} = $p;
            	 }else{
            		$ss{$pdb_chain}{start} = $pdbResNum if($pdbResNum < $ss{$pdb_chain}{start});
            		$ss{$pdb_chain}{end} = $pdbResNum if($pdbResNum > $ss{$pdb_chain}{end});
            		$ss{$pdb_chain}{seq_start} = $p if($p < $ss{$pdb_chain}{seq_start});
            		$ss{$pdb_chain}{seq_end} = $p if($p > $ss{$pdb_chain}{seq_end});
            	 }
              }
	        }else{
	         #If not then put an X for undef
	         $ss{$pdb_chain}{ssString} .= "X";
	       }
	     }
	   $p++;
     }
   }

    my @ssForMerging;

    #Remove any strings that lack SS
    foreach my $pdb_chain (keys %ss){
      # T,S,B,E,H,G,I
      if($ss{$pdb_chain}{ssString}){
	delete $ss{$pdb_chain} unless($ss{$pdb_chain}{ssString} =~ /[TSBEHGI]/);
      }else{
	delete $ss{$pdb_chain};
      }
      #If we do not delete the hash add it to the 
      if($ss{$pdb_chain}){
	push(@ssForMerging, $ss{$pdb_chain}{ssString});
	my ($pdb, $chain) = split(/_/, $pdb_chain);
	$chain = "" if(!$chain);
	
	#Put the mapping in to the alignment
	my $link = Bio::Annotation::DBLink->new();
	$link->database( "PDB" );
	$link->primary_id( $pdb." ".$chain);
	$link->optional_id( $ss{$pdb_chain}{start}."-".$ss{$pdb_chain}{end}.";"  );
	$seq->annotation(Bio::Annotation::Collection->new()) unless ($seq->annotation);
	$seq->annotation->add_Annotation('dblink', $link);
	$noStructures++;
	#Use this to populate the pdb_pfamA_reg table......!
	my $nse = $seq->acc.".".$seq->version."/".$seq->start."-".$seq->end;
	push(@{$map->{$nse}}, { pdb_id => $pdb,
	                        chain => $chain,
	                        pdb_start => $ss{$pdb_chain}{start},
	                        pdb_end   => $ss{$pdb_chain}{end},
	                        seq_start => $ss{$pdb_chain}{seq_start},
	                        seq_end => $ss{$pdb_chain}{seq_end} } );
	   }
    }

    #Add anything pdbs we have here as an Xref.
    #Compress multiple SS to consenus
    #print Dumper(@ssForMerging);
    if(scalar(@ssForMerging)){
      my $consensus;
      if(scalar(@ssForMerging) > 1){
	       $consensus = secStrucConsensus(\@ssForMerging);
      }else{
	       $consensus = $ssForMerging[0];
      }
      push(@allSsStrings,$consensus);
      $seq->sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => $seq->acc,
						    '-from' => $start,
						    '-to' => $end,
						    '-type' => "sec_struct",
						    '-display' => $consensus,
						    '-source' => 'Pfam'));
    }
  }

  #Calculate consensus string for the whole alignment.
  if(scalar(@allSsStrings)){
    my $allConsensus;
    if(scalar(@allSsStrings) > 1){
      $allConsensus = secStrucConsensus(\@allSsStrings);
    }else{
      $allConsensus = $allSsStrings[0];
    }
    #print STDERR "**** CON SEC $allConsensus *****\n";
    $aln->cons_sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => "seq_cons",
						       '-from' => 1,
						       '-to' => length($allConsensus),
						       '-type' => "secondary_structure",
						       '-display' => $allConsensus,
						       '-source' => 'Pfam')); 
  }
  return ($noStructures, $map);
}

sub secStrucConsensus {
  my $ssStringsRef=shift;
  my $num = scalar(@$ssStringsRef);
  my $length=length $$ssStringsRef[0];
  my $gapcount=0;
  my $consensusstring="";
  my (@count,@consensuschar);
  my $numchar=0;
  my ($m,$n,$x,$z,$charindex,$ass,$ambigous,$pos,$character,$prevchar,$gapcharacter,$maxcount);
  $prevchar = "";
  if ($num>1) {
    for ($m=0;$m<$length;$m++) {
      for ($n=0;$n<$num;$n++) {
	$character=substr($$ssStringsRef[$n],$m,1);
	if ($character ne "-" && $character ne "." && $character ne "X") {
	  if (!($prevchar=~/$character/)) {
	    $consensuschar[$numchar]=$character;
	    $count[$numchar]++;
	    $numchar++;
	    $prevchar.=$character;
	  }
	  else {
	    $pos=index $prevchar,$character;
	    $count[$pos]++;
	  }
	}
	else {
	  $gapcount++;
	  $gapcharacter=$character;
	}
      }
      if ($gapcount eq $num) {
	$consensusstring.=$gapcharacter;
      }
      else {
	$maxcount=0;  
	my $end_count=@count;
	for ($x=0;$x<$end_count;$x++){
	  if ($count[$x]>$maxcount) {
	    $charindex=$x;
	    $maxcount=$count[$x];
	  }
	}
	my $ambigious=grep /$maxcount/,@count;
	if ($ambigious>1) {
	  for ($z=0;$z<$end_count;$z++) { # Changed from @count to $end_count for speed
	    if ($count[$z] eq $maxcount) {
	      $ass.=$consensuschar[$z];
	    }
	  }
	  $ass=~s/G/H/g;
	  $ass=~s/I/H/g;
	  $ass=~s/B/E/g;
	  $ass=~s/S/T/g;
	  if ($ass=~/^(.)\1+$/) {
	    $consensusstring.=$1;
	  }
	  else {
	    $consensusstring.="C";
	  }
	  $ass="";
	}
	else {
	  $consensusstring.=$consensuschar[$charindex];
	}
      }
      $prevchar="";
      $gapcount=$numchar=0;
      undef @count;
      undef @consensuschar;
    }
  }
  else {
    $consensusstring=$$ssStringsRef[0];
  }
  return $consensusstring;
}



sub mailPfam {
  my($title, $message) = @_;
	my %header = ( 	To => 'rdf@sanger.ac.uk',
					From => 'rdf@sanger.ac.uk',
					Subject => $title );
					
	my $mailer = Mail::Mailer->new;
	$mailer->open(\%header);
  print $mailer $message;
  $mailer->close;
  exit(1);
}

sub mailUserAndFail {
  my($job, $message) = @_;

  if($job->user_id){
    my %header = (  To => $job->user_id.'@sanger.ac.uk',
					          From => 'rdf@sanger.ac.uk',
					          Subject => 'Error in view process for '.$job->entity_id );
	 my $mailer = Mail::Mailer->new;
	 $mailer->open(\%header);
	 print $mailer $job->entity_id."\n".$message;
   $mailer->close;
  }else{
    mailPfam("View process for ".$job->entity_acc." failed", "No user found for the job"); 
  }
  
  $job->update({status  => 'FAIL',
                closed => \'NOW()'}); 
  exit(1);  
}


sub submitJob{
  my ($jobDB, $pfam, $job) = @_; 
  #Submit a new job to the job database
  $logger->debug("Submitting job:$job");
  my $uid = Data::UUID->new()->create_str();
  $jobDB->getSchema
            ->resultset('JobHistory')
              ->create({ status     => 'PEND',
                                        job_id     => $uid,
                                        family_acc => $pfam->pfamA_acc,
                                        family_id  => $pfam->pfamA_id,
                                        job_type   => $job,
                                        options    => '',
                                        opened     => \'NOW()',
                                        user_id    => 'rdf' });
}


sub finishedJob {
  my $job = shift;
  
  #Set the job status to be done!
  $job->update({status => 'DONE',
                closed => \'NOW()'}); 
}

sub makeNonRedundantFasta{
  my($pfam, $identity, $pfamDB, $job) = @_;
   #Use belvu to make the full alignment 90% non-redundant.
   $identity = $identity/100;
   system("weight -f $identity -o ALIGN.90 ALIGN.ann 2> /dev/null") and mailUserAndFail($job, "Could not run command \"weight -n $identity -o ALIGN.90 ALIGN.ann\":[$!]\n");

    open (BEL, "sreformat fasta ALIGN.90 2> /dev/null |")
     or mailUserAndFail($job, "Could not open command \"belvu -n $identity -o fasta ALIGN.ann\":[$!]\n");
    
    open(FAMFA, ">family.fa") or mailUserAndFail($job, "Failed to open family.fa:[$!]");

    #Parse the output, remove gap charcaters and put the family accessions and name as part of the 
    #header line for each sequence.
    while(<BEL>) {
      if(/\>(\S+\/\d+\-\d+)/ ){
        chomp;
        print FAMFA "$_ ". $pfam->pfama_acc.".". $pfam->version .";". $pfam->pfama_id .";\n";
      }else{
        chomp;
        s/[\.-]//g;
        print FAMFA uc($_)."\n";
      }
    }
    close(BEL);
    close(FAMFA); 
    
    #gzip the file and add it to the database!
    open(GZFA, "gzip -c family.fa |") or mailUserAndFail($job, "Failed to gzip family.fa:[$!]");
    my $familyFA = join("", <GZFA>);
    $pfamDB->getSchema
            ->resultset('PfamaFasta')
              ->update_or_create({ auto_pfama => $pfam->auto_pfama,
                                   fasta      => $familyFA,
                                   nr_threshold  => $identity},
                                   { key => 'UQ_pfamA_fasta_1' });
}

sub makeHTMLAlign{
  my ($filename, $job, $block, $type) = @_;
  $logger->debug("Making HTML aligment for $type $filename");
  system("consensus.pl -method clustal -file $filename > $filename.con") 
    and mailUserAndFail( $job, "Failed to run consensus.pl:[$!]");
  system("clustalX.pl -a $filename.ann -c $filename.con -b $block > $filename.html") 
    and mailUserAndFail( $job, "Failed to run clustalX.pl:[$!]" );

  open(ALI, "gzip -c $filename.html |") or mailUserAndFail($job, "Failed to gzip file $filename.html" );
  my $align = join("", <ALI>);
  

  if($type eq 'seed') {

  }
  elsif( ($type eq 'full') or ($type eq 'ncbi') or ($type eq 'meta') ){
     
    #Make the posterior probablility alignment.
    system("heatMap.pl -a $filename.ann -b $block > $filename.pp") 
        and mailUserAndFail( $job, "Failed to run heatMap.pl ($type):[$!}" );
    
    open(GZPP, "gzip -c $filename.pp |") or mailUserAndFail($job, "Failed to gzip $filename.pp:[$!]");
    my $pp = join("", <GZPP>);
    
    $pfamDB->getSchema
                     ->resultset('AlignmentsAndTrees')
                      ->update_or_create( {auto_pfama => $pfam->auto_pfama,
                                           type       => $type,
                                           post       => $pp},
                                          { key => 'UQ_alignments_and_trees_1' });
  
  }else{
    mailUserAndFail($job,"Incorrect type ($type) passed to uploadTreesAndAlign. Expected 'align', 'seed', 'meta' or 'ncbi'");   
  }
  
    $pfamDB->getSchema
                     ->resultset('AlignmentsAndTrees')
                      ->update_or_create( {auto_pfama => $pfam->auto_pfama,
                                           type       => $type,
                                           jtml       => $align},
                                          { key => 'UQ_alignments_and_trees_1' });
  
  $logger->debug("Finished making $type HTML alignment");
}

sub uploadTreesAndAlign {
  my($filename, $pfamDB, $pfam, $job, $type) = @_;
 
  unless( ($type eq 'seed') or ($type eq 'full') or ($type eq 'ncbi') or ($type eq 'meta') ){
    mailUserAndDie($job,"Incorrect type ($type) passed to uploadTreesAndAlign. Expected 'full', 'seed', 'meta' or 'ncbi'");  
  }
 
  $logger->debug("Uploading $type trees and alignments");

  #Do this is steps.  Two reasons, better error tracking and more memory efficient
  my $row = $pfamDB->getSchema
                     ->resultset('AlignmentsAndTrees')
                      ->update_or_create( {auto_pfama => $pfam->auto_pfama,
                                           type       => $type},
                                          { key => 'UQ_alignments_and_trees_1' });
  
  my $file; 
  open(ANN, "gzip -c $filename.ann|") or mailUserAndDie($job, "Failed to run gzip -c $filename.ann:[$!]"); 
  while(<ANN>){
    $file .= $_;
  } 
  close(ANN);
  $row->update({ alignment => $file});
  
  $file = '';
  open(TREE, "gzip -c $filename.tree|") or mailUserAndDie($job, "Failed to run gzip -c $filename.tree:[$!]"); 
  while(<TREE>){
    $file .= $_;
  }
  close(TREE);
  $row->update({ tree => $file});
 
  $file = '';
  open(HTML, "gzip -c $filename.html|") or mailUserAndDie($job, "Failed to run gzip -c $filename.tree:[$!]"); 
  while(<HTML>){
    $file .= $_;
  }
  close(HTML);
  $row->update({ jtml => $file});

}

sub cleanBuildLine{
  my $buildline = shift;
  
  my @Opts = split(/\s+/, $buildline);
  $buildline = join(' ', @Opts[0..($#Opts - 2)]); #Removes the exisitng HMM_ls SEED from the end of the line
  $buildline =~ s/-fF/-F -f/g;
	$buildline =~ s/-Ff/-F -f/g;
	$buildline =~ s/--cpu 1//;
  $buildline =~ s/-o \/dev\/null//;

  return($buildline);
}

sub versionFiles{
  my($pfamDB, $pfam, $job) = @_;
  
  my %fileCheckSums;
  foreach my $f (qw(SEED ALIGN)){
    open(F, $f) or mailUserAndFail($job, "Could not version $f:[$!]");
    $fileCheckSums{$f} = md5_hex(join("", <F>));
  }
  
  #Add the thresholds into the HMMs!  This is what really determines the version of the family
  foreach my $f (qw(HMM)){
      open(F, $f) or mailUserAndFail($job, "Could not version $f:[$!]");
      my $hmm;
      while(<F>){
         $hmm .= $_;
         unless( /^CKSUM\s+/ ) {
	         $hmm .= "GA    ".$pfam->sequence_ga." ".$pfam->domain_ga.";\n";
	         $hmm .= "TC    ".$pfam->sequence_tc." ".$pfam->domain_tc.";\n";
	         $hmm .= "NC    ".$pfam->sequence_nc." ".$pfam->domain_nc.";\n";
	       }
      }
      $fileCheckSums{$f} = md5_hex($hmm);
  }
  #Update the current versions table with these versions
  my $currentVersions = $pfamDB->getSchema
                             ->resultset('CurrentPfamVersion')
                              ->update_or_create({ auto_pfama     => $pfam->auto_pfama,
                               seed      => $fileCheckSums{SEED},
                               align     => $fileCheckSums{ALIGN},
                               #desc_file => $fileCheckSums{DESC},
                               hmm       => $fileCheckSums{HMM},
                               });
  #Get the release versions
  my $releasedVersions = $pfamDB->getSchema
                             ->resultset('ReleasedPfamVersion')
                              ->find({ auto_pfama     => $pfam->auto_pfama});
  
  my ($thisVersion, $changeStatus);
  if($releasedVersions and $releasedVersions->auto_pfama){
        $changeStatus = 'NOCHANGE';
        #If the release version are different, then we need to add them to the 
        if($releasedVersions->hmm ne $currentVersions->current_hmm){
            $thisVersion = $releasedVersions->version + 1 ;
            $changeStatus = 'CHANGED';
        }else{
          if( ($releasedVersions->seed ne $currentVersions->current_seed) or
              ($releasedVersions->align ne $currentVersions->current_align) or
              ($releasedVersions->desc ne $currentVersions->current_desc)){
                $changeStatus = 'CHANGED';
              }
        }
   }else{
      #looks like it is a new family
       $changeStatus = 'NEW';
       $thisVersion = 1;
  }
  $pfam->update({version       => $thisVersion,
                change_status => $changeStatus });
}





sub getNestedLocations {
  my ($auto_pfamA, $pfamDB) = @_;
  
  my @rows = $pfamDB->getSchema
                     ->resultset('NestedLocations')
                      ->search({auto_pfamA => $auto_pfamA});
  return (\@rows);
}


sub getClanData {
  my ($auto_pfamA, $pfamDB) = @_;
  
  my $row = $pfamDB->getSchema
                     ->resultset('Clans')
                      ->find({ 'clan_memberships.auto_pfamA' => $auto_pfamA},
                             { join      => [qw( clan_memberships )] });
                               
  if($row and $row->clan_acc){
    return($row->clan_acc);
  }
}


sub write_stockholm_file {

   my ($filename, $aln, $pfam, $type, $famObj) = @_;

   open(ANNFILE,">$filename.ann") or &exit_with_mail("Could not open $filename.ann for writing [$!]");
   print ANNFILE "# STOCKHOLM 1.0\n";
   #Mimic this with what is loaded in the database
   #$en->write_stockholm_ann(\*ANNFILE);
   print ANNFILE "#=GF ID   ", $pfam->pfama_id , "\n";
   print ANNFILE "#=GF AC   ", $pfam->pfama_acc,".", $pfam->version, "\n";
   print ANNFILE "#=GF DE   ", $pfam->description, "\n";
   if($pfam->previous_id and $pfam->previous_id =~ /\S+/){
       print ANNFILE "#=GF PI   ", $pfam->previous_id, "\n";
   }
   print ANNFILE "#=GF AU   ", $pfam->author, "\n";
   print ANNFILE "#=GF SE   ", $pfam->seed_source, "\n";


   #Put in the ga, nc, tc and also add the build and search method lines.
   if($type eq "pfamseq") {
       print ANNFILE "#=GF GA   ". sprintf "%.2f %.2f;\n", $pfam->sequence_ga, $pfam->domain_ga;
       print ANNFILE "#=GF TC   ". sprintf "%.2f %.2f;\n", $pfam->sequence_tc, $pfam->domain_tc;
       print ANNFILE "#=GF NC   ". sprintf "%.2f %.2f;\n", $pfam->sequence_nc, $pfam->domain_nc;
       print ANNFILE "#=GF BM   ", cleanBuildLine($pfam->buildmethod) , "HMM.ann SEED.ann\n";
       print ANNFILE "#=GF SM   ", $pfam->searchmethod ,"\n";    
   }
   else {
       print ANNFILE "#=GF GA   ". sprintf "%.2f %.2f;\n", $famObj->DESC->CUTGA->{seq}, $famObj->DESC->CUTGA->{dom};
       print ANNFILE "#=GF TC   ". sprintf "%.2f %.2f;\n", $famObj->DESC->CUTTC->{seq}, $famObj->DESC->CUTTC->{dom};
       print ANNFILE "#=GF NC   ". sprintf "%.2f %.2f;\n", $famObj->DESC->CUTNC->{seq}, $famObj->DESC->CUTNC->{dom};
       print ANNFILE "#=GF BM   ". $famObj->DESC->BM ."\n";;
       print ANNFILE "#=GF SM   ", $famObj->DESC->SM ."\n";  
   }
      
   print ANNFILE "#=GF TP   ", $pfam->type , "\n";
              
   #Add Nested domains if they are present
   if($type eq 'pfamseq' and $nested_locations and scalar(@$nested_locations)){
    foreach my $n (  @$nested_locations){
      print ANNFILE "#=GF NE   ", $n->nested_pfama_acc ,";\n";
      print ANNFILE "#=GF NL   ", $n->pfamseq_acc;
      if($n->seq_version and $n->seq_version =~ /\d+/){
        print ANNFILE ".".$n->seq_version;
      }
      print ANNFILE "/".$n->seq_start."-".$n->seq_end."\n";
      }
   }
   #Add the reference
   foreach my $ref (@litRefs){
      if($ref->pmid){
        if (($ref->comment)&& ($ref->comment ne "NULL")){ 
           print ANNFILE wrap("#=GF RC   ","#=GF RC   ",$ref->comment);
           print ANNFILE "\n";
        }
       print ANNFILE "#=GF RN   [".$ref->order_added."]\n";
       print ANNFILE "#=GF RM   ".$ref->pmid."\n";
       print ANNFILE wrap("#=GF RT   ","#=GF RT   ", $ref->title);
       print ANNFILE "\n";
       print ANNFILE wrap("#=GF RA   ","#=GF RA   ", $ref->author);
       print ANNFILE "\n";
       print ANNFILE "#=GF RL   ".$ref->journal."\n";
      } 
   } 
   
   #DB Xrefs
   #Add this special case of database cross reference
   my @interpro = $pfamDB->getSchema->resultset('Interpro')->search( {auto_pfama => $pfam->auto_pfama} );
   
   print ANNFILE "#=GF DR   INTERPRO; ",$interpro[0]->interpro_id, ";\n" if($interpro[0] and $interpro[0]->interpro_id =~ /\S+/);
   
   foreach my $xref ( @xRefs){
     #Construct the DR lines.  Most do not have additional paramters. In the database
     #the other_params has a trailing ";" that should ideally not be there.  Otherwise
     #one could simply use join! 

     if($xref->other_params and $xref->other_params =~ /\S+/){
      print ANNFILE "#=GF DR   ".$xref->db_id."; ". $xref->db_link ."; " .$xref->other_params."\n";
     }else{
      print ANNFILE "#=GF DR   ".$xref->db_id."; ".$xref->db_link.";\n";
     }
     #Print out any comment
     if($xref->comment and $xref->comment =~ /\S+/){
         print ANNFILE "#=GF DC   ".$xref->comment."\n";
     }
   }
   
   #Annotation comments
   #TODO - Fix the fact that all comments are stored with a single leading whitespace
   #Currently, the text wrap is handling this!
   if($pfam->comment and $pfam->comment =~ /\S+/){
     print ANNFILE wrap("#=GF CC   ","#=GF CC   ", $pfam->comment);
     print ANNFILE "\n";
   }
   
   print ANNFILE "#=GF SQ   ", scalar($aln->no_sequences()), "\n";
   my $stock = $aln->write_stockholm;
   if($$stock[0] =~ /^\# STOCKHOLM/){
      shift(@$stock); #This removes the STOCKHOLM 1.0 tag, but nasty, but hey!
  }
   foreach my $line (@{$stock}){
       print ANNFILE $line;
   }
   close(ANNFILE);
}

sub consensus_line {
    my ($filename, $ali_length) = @_;

    my $consensus;
    open (CON, "consensus.pl -file $filename -method pfam -thr 60|") or &mailUserAndFail($job, "Failed to run consensus.pl on $filename");
    while (<CON>) {
	if(/^(consensus\/60%)(\s+)(\S+)/){
	    $consensus = $3;
	    last;
	}
    }
    close(CON);

    #Check that the consensus line is the correct length.  
    if($ali_length != length($consensus)){
	mailUserAndFail($job, "Error with consensus line. Got ".(length($consensus))."but expected".$ali_length); 
    }
    
    return $consensus;
}


sub make_tree { 

    my ($filename, $regs, $pfamseq) = @_;

  open(TREE, "sreformat a2m $filename | FastTree -nj -boot 100 |") or &mailUserAndFail($job, "Could not open pipe on sreformat and FastTree -nj -boot 100 $filename\n");

  
  open(TREEFILE, ">$filename.tree") or mailUserAndFail($job, "Failed to open $filename.tree:[$!]");

  my $line = <TREE>;
  close TREE;
  my @tree = split(/,/, $line);


  #Exchange the treefile accessions for ids (not for ncbi or metaseq), and set the tree order on the database object
  my $order = 1;
  foreach my $acc (@tree) {
      my ($before1, $before2, $nm, $st, $en, $after);
      if($acc =~ m/(.+)?([\,\(])(\S+)\/(\d+)-(\d+)(.+)/g) {
	  ($before1, $before2, $nm, $st, $en, $after) = ($1, $2, $3, $4, $5, $6);
      }
      elsif($acc =~ m/([\,\(])?(\S+)\/(\d+)-(\d+)(.+)/g) {
	  ($before1, $nm, $st, $en, $after) = ($1, $2, $3, $4, $5);
      }

      $before1 = "" unless($before1);
      $before2 = "" unless($before2);
      $after = "" unless($after);
      
      if($pfamseq) {
          if($acc =~ /\;/) {
	      print TREEFILE $before1.$before2.$regs->{"$nm/$st-$en"}->pfamseq_id."/".$st."-".$en.$after;
	  }
	  else {
	      print TREEFILE $before1.$before2.$regs->{"$nm/$st-$en"}->pfamseq_id."/".$st."-".$en.$after.",";
	  }

      }
      else {
	  if($acc =~  /\;/) {
	      print TREEFILE "$acc";
	  }
	  else {
	      print TREEFILE "$acc".",";
	  }
      }
      
      
      if($regs->{"$nm/$st-$en"}) {     
	  $regs->{"$nm/$st-$en"}->tree_order($order++);
      } 
      else{
	  $logger->warn("key [$nm/$st-$en] is not in the hash"); 
      }
  }      
  close TREEFILE;	      
}


sub help {
  
print<<EOF

usage: $0 -id <UUID> -family <pfamA_id>

Complete list of options:

family : name of the pfam family to be processed
id     : the unique identifier assigned to the job
fasta  : threshold for making the Pfam-A.fasta file. Default is set to 90% identity
pvalue : P-value threshold used for testing that the Pfams thresholds are sensible. Default is set to 0.1
h      : prints this help statement

This script will take the input job and convert the unformated Pfam files and convert them to
formated versions ready for the release. This script tries to take all information from the database,
thereby detaching Pfam from the RCS system. If everything works, this should produce the following files:

SEED.ann    - SEED with stockholm markup above it 
ALIGN.ann   - ALIGN (FULL) with stockholm markup above it
HMM_ls.ann  - ls mode HMM with Stockholm markup
HMM_fs.ann  - fs mode HMM with Stockholm markup
SEED.html   - SEED alignment coloured according to clustalx colouring scheme
ALIGN.html  - FULL alignment coloured according to clustalx colouring scheme
SEED.tree   - NJ tree based on the SEED alignment
ALIGN.tree  - NJ tree based on the FULL alignment
hmmLogo.png - The HMM logo
family.fa   - Non-redundant version the ALIGN file

All of these files are stored in CURRENT and in the database.

If errors are detected the user will normally be mailed with a short message.

EOF
  
  
}
