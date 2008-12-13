#!/software/bin/perl
#
# This code is desgined to act as a layer on top os LSF, taking jobs out of the  
# pfam_jobs database and submit them to the farm.  Ideal, this would only encapsulate 
# the job submission, but due to the complexity of some jobs, there is some 'logic'
# about dependencies included.  When run out of debug mode, the process will be forked
# and the parent process terminated so that the script runs in the background.
# 
# Author        : rdf
# Maintainer    : $Author: rdf $
# Created       : 2008-05-05
# Last Modified : $Date: 2008-12-13 01:16:13 $
# Version       : $Revision: 1.1 $;
# Id            : $Id: pfamJobDequeue.pl,v 1.1 2008-12-13 01:16:13 rdf Exp $

use strict;
use warnings;
use Data::Dumper;
use IO::File;
use Getopt::Long;
use File::Rsync; 

# Our Module Found in Pfam-Core
use Bio::Pfam::Queues::IntQueue;

our $DEBUG = 1; 

#Set up an object that can perform rsync copying
my $rsyncObj = File::Rsync->new( { compress     => 1,
                                   recursive    => 1, 
                                   rsh          => '/usr/bin/ssh', 
                                   'rsync-path' => '/usr/bin/rsync' } );

# Get a new queue stub
my $qsout = Bio::Pfam::Queues::IntQueue->new();

# Deamonise the process if we are not trying to debug the code
$qsout->daemonise unless($DEBUG);


#Now set off the infinite loop!
while(1) {
  #See if there are any pending jobs
  my $ref = $qsout->satisfy_pending_job();
  
  $DEBUG && print Dumper($ref, $qsout->{'jobTypes'});
  
  #If there are not any jobs, see if there are any pending jobs
  unless($ref->{'id'}) {
     sleep 15; #Poll the database every 15 seconds....1 per LSF Poll
  }else{  
    #THere is a job to run!
    my $error = 0;    
    my (@cmds, $host, $tmpDir, $queue, $resource);
  
    if($ref->{'job_type'} eq "view"){
      #Build up the command here to run the view process!
      #Depending on the size of the family, dictates where the job should be scheduled to!
      if($ref->{'family_size'}){
        #if( $ref->{'family_size'} >= 20000 ){
		if( $ref->{'family_size'} >= 2000 ){
          #Use turing
          $resource = "-R\'select[mypfamlive<300 && mem>20000] rusage[mypfamlive=10:mem=20000]\'";
          $host   = $qsout->hugeMemNode;
          $tmpDir = $qsout->hugeMemTmpDir;
          $queue  = "hugemem";
          $host = 'turing';
          $tmpDir = '/tmp';
        }elsif($ref->{'family_size'} < 3000){
          #Use small
          $resource = "-R\'select[mypfamlive<300 && type==X86_64] rusage[mypfamlive=10]\'";
          $host =  $qsout->farmNode;
          $tmpDir = $qsout->tmpDir;
          $queue  =  "small";
        }elsif($ref->{'family_size'} >= 3000 and $ref->{'family_size'} < 8000){
          #Use normal
          $resource = "-R\'select[mem>1500 && mypfamlive<300 && type==X86_64] rusage[mypfamlive=10:mem=1500]\'";
          $host =  $qsout->farmNode;
          $tmpDir = $qsout->tmpDir;
          $queue  =  "normal";
        }else{
          #Higher memory requirements and run on long
          $resource = "-M13000000 -R\'select[type==X86_64 && mem>13000 && mypfamlive<300] rusage[mypfamlive=10:mem=13000]\'";
          $host     =  $qsout->farmNode;
          $tmpDir   = $qsout->tmpDir;
          $queue    = "long";
        }
      }else{
        #If we do not have a size of family, this will work in most cases.
        $resource = "-R\'select[type==X86_64 && mem>7000 && mypfamlive<300] rusage[mypfamlive=10:mem=7000]\'";        
        $host =  $qsout->farmNode;
        $tmpDir = $qsout->tmpDir;
        $queue = "long";
      }
        
      #Perform the Rsync
      $rsyncObj->exec( { src => $qsout->currentDir."/".$ref->{'family_id'}, dest => $host.":".$tmpDir."/".$ref->{'job_id'} } );
      if($rsyncObj->err){
        $error =  "View process failed: Error copying family files for [".$ref->{'family_id'}.
                  "] because [".join("", @{$rsyncObj->err})."]\n";
                  print STDERR "$error\n";
                  print STDERR $qsout->currentDir."/".$ref->{'family_id'}."\n";
                  print STDERR $host.":".$tmpDir."/".$ref->{'job_id'};
      }
      #Step 3 - Build up the command that we want to run
      unless($error){
        my $cmd = $ref->{'command'};
        $cmd .= " -id ".$ref->{'job_id'}; 
        $cmd .= " -family ".$ref->{'family_id'};
		if($ref->{'family_size'} >= 20000){
          $cmd .= " -tree upgma";
		} 
        $cmd .= " && rm -fr ".$tmpDir."/".$ref->{'job_id'};
        push(@cmds, $cmd);
      }
    }elsif( $ref->{'job_type'} eq "genPept") {
      #This job searches the HMM against the part of ncbi genPept that is not mapped to uniProt
      
      #The runtime and memory usage is relatively constant
      $host =  $qsout->farmNode;
      $tmpDir = $qsout->tmpDir;
      
      #Copy the HMMs, DESC, SEED, ALIGN across to the farm using Rsync
      if(-d  $qsout->currentDir."/".$ref->{'family_id'}){
      $rsyncObj->exec( { src => $qsout->currentDir."/".$ref->{'family_id'}, dest => $host.":".$tmpDir."/".$ref->{'job_id'} } );
      if($rsyncObj->err){
        $error =  "View process failed: Error copying family files for [".$ref->{'family_id'}.
                      "] because [".join("", @{$rsyncObj->err})."]\n";
                      print STDERR "$error\n";
                      print STDERR $qsout->currentDir."/".$ref->{'family_id'}."\n";
                      print STDERR $host.":".$tmpDir."/".$ref->{'job_id'};
      }
  	}else{
		$error .=  "Directory ".$qsout->currentDir."/".$ref->{'family_id'}." does not exist";
	 }
      unless($error){
        #TODO - Fix this hard coding
        #These should all go into a config!  This can be updated at release time. 
        my $ncbiUniqDbSize = 7606502;
        my $ncbiDbSize = 12554697;
        my $noJobs = 8;
        my $farm_blast_db = "/data/blastdb/Pfam/ncbi";
        my $options = "--cpu 1 -E 1000 -A 0 -Z $ncbiUniqDbSize";
      
        #For each mode 
        foreach my $mode (qw(ls fs)){
          #Run hmmsearch!
          $queue  =  "normal";
          $resource = "-R\'select[mem>1500 && type==X86_64]\'";
          my $cmd = "hmmsearch $options HMM_$mode ".$farm_blast_db."/ncbi_bigger\$\{LSB_JOBINDEX\}\.fa > NCBIOUT_$mode.\$\{LSB_JOBINDEX\} \n ";
          
          
          my $fh = IO::File->new();
          $DEBUG && print " bsub -q $queue  $resource \n";
          $fh->open( "| bsub -q $queue  $resource -J".$ref->{'job_id'}."search\"[1-$noJobs]\" -o ".$tmpDir."/".$ref->{'job_id'}.".log");
          $fh->print("cd ".$tmpDir."/".$ref->{'job_id'}."/".$ref->{'family_id'}."\n");
          $fh->print( "$cmd\n");
          $fh->close;
        
        
          #Once all of these jobs have run, add them together and with the regions mapped from UniProt        
          my $opts = "-n $noJobs -m $mode -hmm_o NCBIOUT_$mode. -cwd ".$tmpDir."/".$ref->{'job_id'}."/".$ref->{'family_id'}.
                     " -acc ".$ref->{'family_acc'}." -dbsize ".$ncbiUniqDbSize." -tot_seqs $ncbiDbSize";
          
          
          my $cmd2 = "chmod +rw * && /nfs/team71/pfam/cara/src/pfam/cara/ncbi/ncbi_hmm_postexec_and_merge.pl $opts -dbsize $ncbiUniqDbSize";
          #Quick jobs, so run on small
          $queue  =  "small";
          $resource = "-R\'select[mem>1500 && mypfamlive<300 && type==X86_64] rusage[mypfamlive=10:mem=1500]\'";
          my $fh2 = IO::File->new();
          $fh2->open( "| bsub -q $queue  $resource -J".$ref->{'job_id'}."merge -w\'done(".$ref->{'job_id'}."search)\' -o ".$tmpDir."/".$ref->{'job_id'}.".log");
          $fh2->print("cd ".$tmpDir."/".$ref->{'job_id'}."/".$ref->{'family_id'}."\n");
          $fh2->print( "$cmd2\n");
          $fh2->close;
        }
      
        #make the alignment at the end!
        $queue = "small";
        my $fh3 = IO::File->new();
        my $cmd = "/nfs/team71/pfam/rdf/scripts/pfmake_farm.pl".
                  " -id ".$ref->{'job_id'}." -type ".$ref->{'job_type'}." -acc ".$ref->{'family_acc'} ;
                  
        $fh3->open( "| bsub -q $queue  $resource -J".$ref->{'job_id'}." -w\'done(".$ref->{'job_id'}."merge)\' -o ".$tmpDir."/".$ref->{'job_id'}.".log");
        $fh3->print("cd ".$tmpDir."/".$ref->{'job_id'}."/".$ref->{'family_id'}."\n");
        $fh3->print( "$cmd\n");
        $fh3->close;
        
      }
      #This is not as elegant as the view process
      $qsout->update_job_status($ref->{id}, 'RUN');
    }elsif( $ref->{'job_type'} eq "genPeptView" or $ref->{'job_type'} eq "metaView") {
      #Build up the command here to run the view process!
      #Depending on the size of the family, dictates where the job should be scheduled to!
      if($ref->{'family_size'} >= 0){
        if( $ref->{'family_size'} >= 12000 ){
          #Use turing
		  #Perform the Rsync
		  
			
          $resource = "-R\'select[mypfamlive<300 && mem>20000] rusage[mypfamlive=10:mem=20000]\'";
          $host   = $qsout->hugeMemNode;
          $tmpDir = $qsout->hugeMemTmpDir;
          $queue  = "hugemem";
          $host = 'turing';
          $tmpDir = '/tmp';	
		  my $src = "farm-login:/lustre/scratch1/sanger/pfam/".$ref->{'job_id'};
	      my $dest = $host.":".$tmpDir;
		print STDERR "|$host, $src, $dest|\n";
		$rsyncObj->exec( { src => $src, dest => "/tmp" } );
		 $rsyncObj->exec( { src => "/tmp/".$ref->{'job_id'}, dest => $dest } );
		  if($rsyncObj->err){
		      $error =  "View process failed: Error copying family files for [".$ref->{'family_id'}.
		                   "] because [".join("", @{$rsyncObj->err})."]\n";
		                   print STDERR "$error\n";
		              print STDERR $host.":".$tmpDir."/".$ref->{'job_id'};
			}
        }elsif($ref->{'family_size'} < 3000){ #Use small
          $resource = "-R\'select[mypfamlive<300 && type==X86_64] rusage[mypfamlive=10]\'";
          $host =  $qsout->farmNode;
          $tmpDir = $qsout->tmpDir;
          $queue  =  "small";
        }elsif($ref->{'family_size'} >= 3000 and $ref->{'family_size'} < 5000){
          #Use normal
          $resource = "-M4000000 -R\'select[mem>4000 && mypfamlive<300 && type==X86_64] rusage[mypfamlive=10:mem=4000]\'";
          $host =  $qsout->farmNode;
          $tmpDir = $qsout->tmpDir;
          $queue  =  "normal";
        }else{
          #Higher memory requirements and run on long
          $resource = "-M13000000 -R\'select[type==X86_64 && mem>13000 && mypfamlive<300] rusage[mypfamlive=10:mem=13000]\'";
          $host     =  $qsout->farmNode;
          $tmpDir   = $qsout->tmpDir;
          $queue    = "long";
        }
      }else{
        #If we do not have a size of family, this will work in most cases.
        $resource = "-M13000000 -R\'select[type==X86_64 && mem>13000 && mypfamlive<300] rusage[mypfamlive=10:mem=13000]\'";        
        $host =  $qsout->farmNode;
        $tmpDir = $qsout->tmpDir;
        $queue = "long";
      }
        
      
      #Step 3 - Build up the command that we want to run
      unless($error){
        my $cmd = $ref->{'command'};
        $cmd .= " -id ".$ref->{'job_id'}; 
        $cmd .= " -family ".$ref->{'family_id'};
		    if($ref->{'family_size'} >= 20000){
          $cmd .= " -tree upgma";
		    } 
		    $cmd .= " -type ".$ref->{'job_type'};
        #$cmd .= " && rm -fr ".$tmpDir."/".$ref->{'job_id'};
        push(@cmds, $cmd);
      }
      
      
    }elsif($ref->{'job_type'} eq "metagenomics"){
      #The runtime and memory usage is relatively constant
      $host =  $qsout->farmNode;
      $tmpDir = $qsout->tmpDir;
      
      #Copy the HMMs, DESC, SEED, ALIGN across to the farm using Rsync
      if(-d  $qsout->currentDir."/".$ref->{'family_id'}){
        $rsyncObj->exec( { src => $qsout->currentDir."/".$ref->{'family_id'}, dest => $host.":".$tmpDir."/".$ref->{'job_id'} } );
        if($rsyncObj->err){
          $error =  "View process failed: Error copying family files for [".$ref->{'family_id'}.
                      "] because [".join("", @{$rsyncObj->err})."]\n";
                      print STDERR "$error\n";
                      print STDERR $qsout->currentDir."/".$ref->{'family_id'}."\n";
                      print STDERR $host.":".$tmpDir."/".$ref->{'job_id'};
        }
        
      }else{
		      $error .=  "Directory ".$qsout->currentDir."/".$ref->{'family_id'}." does not exist";
	     }
	      
	       
      unless($error){
        #TODO - Fix this hard coding
        #These should all go into a config!  This can be updated at release time. 
        my $metaUniqDbSize = 489225;
        my $metaDbSize = 6612632;
        my $noJobs = 1;
        my $farm_blast_db = "/data/blastdb/Pfam/metaseq";
        my $options = "--cpu 1 -E 1000 -A 0 -Z $metaDbSize";
      
        #For each mode 
        foreach my $mode (qw(ls fs)){
          #Run hmmsearch!
          $queue  =  "normal";
          $resource = "-R\'select[mem>1500 && type==X86_64]\'";
          my $cmd = "hmmsearch $options HMM_$mode ".$farm_blast_db."/metaseq\.\$\{LSB_JOBINDEX\} > METAOUT.$mode.\$\{LSB_JOBINDEX\} \n ";
          
          
          my $fh = IO::File->new();
          $DEBUG && print " bsub -q $queue  $resource \n";
          $fh->open( "| bsub -q $queue  $resource -J".$ref->{'job_id'}."search\"[1]\" -o ".$tmpDir."/".$ref->{'job_id'}.".log");
          $fh->print("cd ".$tmpDir."/".$ref->{'job_id'}."/".$ref->{'family_id'}."\n");
          $fh->print( "$cmd\n");
          $fh->close;
        
        }
          #Once all of these jobs have run, add them together and with the regions mapped from UniProt  
          #-dbsize 489225 -tot_seqs 6612632 -n 1 -hmm_o metaout -acc PF09278 -type metagenomics      
          my $opts = "-n $noJobs -hmm_o METAOUT -acc ".$ref->{'family_acc'}." -dbsize ".$metaUniqDbSize." -tot_seqs $metaDbSize -type ".$ref->{'job_type'};
          
          
      my $cmd2 = "chmod +rw * &&".
      "/nfs/team71/pfam/rdf/scripts/sequenceSearchMerge.pl $opts &&".
      "/nfs/team71/pfam/rdf/scripts/pfmake_farm.pl -id ".$ref->{'job_id'}." -acc ".$ref->{'family_acc'}." -type ".$ref->{'job_type'};
       
       #Quick jobs, so run on small
       $queue  =  "small";
       $resource = "-R\'select[mem>1500 && mypfamlive<300 && type==X86_64] rusage[mypfamlive=10:mem=1500]\'";
        my $fh2 = IO::File->new();
          $fh2->open( "| bsub -q $queue  $resource -J".$ref->{'job_id'}."merge -w\'done(".$ref->{'job_id'}."search)\' -o ".$tmpDir."/".$ref->{'job_id'}.".log");
          $fh2->print("cd ".$tmpDir."/".$ref->{'job_id'}."/".$ref->{'family_id'}."\n");
          $fh2->print( "$cmd2\n");
          $fh2->close;
        
      
        
      }
      
      #This is not as elegant as the view process
      unless($error){
        $qsout->update_job_status($ref->{id}, 'RUN');
      }
      
      
    }elsif( defined $ref->{'job_type'} ){
      carp( "Unknown job type ".$ref->{'job_type'});
    }else{
      carp("Job type is undefined"); 
    }
    
    
    #Now submit the generic jobs using this method!
    foreach my $cmd (@cmds){
      $DEBUG && print STDERR "Submitting id=$ref->{'id'}, command=$cmd, $error\n";
      #Now for the lsf bit
      if($cmd) {
        #Now set up the lsf requirements
        $DEBUG && print STDERR "$cmd";
        my $fh = IO::File->new();
        $DEBUG && print " bsub -q $queue  $resource \n";
        $fh->open( "| bsub -q $queue  $resource -o ".$tmpDir."/".$ref->{'job_id'}.".log");
        $fh->print("cd ".$tmpDir."/".$ref->{'job_id'}."/".$ref->{'family_id'}."\n");
        $fh->print( "$cmd\n");
        $fh->close;
      }else{
        $error .= "unrecognised command line, commoand=|".$cmd."|\n";
      }
    }
    
    unless($error){
      $qsout->update_job_status($ref->{id}, 'SUB');
    }
    
    if($error){    
      $qsout->update_job_status($ref->{id}, 'FAIL');
      $qsout->update_job_stream($ref->{id}, 'stderr', $error);
      next;
    }
  }
}

exit(0);
