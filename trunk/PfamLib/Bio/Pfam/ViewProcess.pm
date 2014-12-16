package Bio::Pfam::ViewProcess;

use strict;
use warnings;
use Data::UUID;
use Mail::Mailer;
use Getopt::Long qw(GetOptionsFromArray);
use Mail::Mailer;
use Log::Log4perl qw(:easy);
use Data::Dumper;
use Data::Dump;
use Data::UUID;
use Digest::MD5 qw(md5_hex);
use Cwd;
use Text::Wrap;
use JSON;
use Data::Printer;
use File::Touch;
use DDP;

#Need the version from github https://github.com/DaGaMs/Logomat
use HMM::Profile;

#Pfam Modules
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::Active_site::as_align;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Bio::Pfam::ViewProcess::Consensus;

$Text::Wrap::unexpand = 0;
$Text::Wrap::columns = 75;


sub new {
  my( $class, @args ) = @_;
  
  my $self = {};
  bless ($self, $class);  
  
  $self->{config} = Bio::Pfam::Config->new;
  $self->{jobdb}  = Bio::Pfam::PfamJobsDBManager->new( %{ $self->config->pfamjobs } );
  unless($self->{jobdb}){
    $self->mailPfam( "Failed to run view process", "Could not get connection to the pfam_jobs database" );
  }
  
  Log::Log4perl->easy_init($DEBUG);
  my $logger = get_logger();
  
  $self->{logger} = $logger;

  $self->logger->debug("Got pfam_job db connection");
  $self->{pfamdb} = Bio::Pfam::PfamLiveDBManager->new( %{ $self->config->pfamliveAdmin } );
  
  unless ($self->{pfamdb}) {
    $self->mailPfam( "Failed to run view process", "View process failed as we could not connect to pfamlive" );
  }
  $self->logger->debug("Got pfamlive database connection");
  return($self);
}

sub config {
  my($self) = @_;
  return($self->{config});  
}

sub logger {
  my($self) = @_;
  return($self->{logger});  
}

sub jobdb {
  my ($self) = @_;
  return($self->{jobdb});  
}

sub pfamdb {
  my($self) = @_;
  return($self->{pfamdb});  
}

sub options {
  my ( $self, $opts) = @_;
  
  if( $opts and ref($opts) eq 'HASH'){
    $self->{options} = $opts;  
  }
  
  return( $self->{options});
}

sub job {
  my ( $self, $job) = @_;
  
  if( $job ){
    $self->{job} = $job;  
  }
  
  return( $self->{job}); 
}

sub pfam {
  my ( $self, $pfam) = @_;
  
  if( $pfam ){
    $self->{pfam} = $pfam;  
  }
  
  return( $self->{pfam}); 
}

sub initiateActiveSiteObj {
  my ($self, $GFAnn) = @_;
  my $asp;
    $asp = Bio::Pfam::Active_site::as_align->new(
        -pfama_acc      => $self->pfam->pfama_acc,
      -database  => $self->pfamdb,
      -nested    => $GFAnn->{nestA}
    );
  $self->activeSite($asp); 
}

sub activeSite{
  my ( $self, $asp) = @_;
  
  if( $asp ){
    $self->{asp} = $asp;  
  }
 
  return( $self->{asp}); 
}

sub initiateViewProcess {
  my ( $self, $famObj, $name ) = @_;

  unless ( $famObj->isa("Bio::Pfam::Family::PfamA") ) {
    die("Expected a Bio::Pfam::Family::PfamA object");
  }

  unless ( $famObj->DESC->AC and $famObj->DESC->ID ) {
    die(
"Entry accession or ID is not defined, therefore cannot add to the jobs database!"
    );
  }

  unless ($name) {
    die "Needed a user id\n";
  }

  if ( $famObj->DESC->CL ) {
    $self->initiateClanViewProcess( $famObj->DESC->CL, $name);
  }
  else {
#TODO - put the line below back again, only removed for debugging
    $self->initiateFamilyViewProcess( $famObj, $name ); 
  }
}

sub initiateClanViewProcess {
  my ( $self, $clanAcc, $name ) = @_;

  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();
  
#TODO - the section below is removed for debugging and needs to be put back  
#Look for any existing clan jobs!
#-------------------------------------------------------------------------------
# Now clear out any existing jobs for that family.
  my @jobs = $self->jobdb->getSchema->resultset('JobHistory')->search(
    {
      entity_acc => $clanAcc,
      -and       => [
        status => { '!=' => 'DONE' },
        status => { '!=' => 'KILL' }
      ],
    }
  );

  if ( scalar(@jobs) ) {
    foreach my $job (@jobs) {
      if ( $job->lsf_id and $job->lsf_id =~ /\d+/ ) {

        #$self->killJob( $job->lsf_id );
      }
      $job->update(
        {
         status => 'KILL',
          closed => \'NOW()'
        }
      );
    }
  }

  my $clan      = $self->pfamdb->getClanData($clanAcc);
  my $clanMembs = $self->pfamdb->getClanMembership($clanAcc);

  foreach my $memb (@$clanMembs) {

    #Now look for any families belonging to that jobs.
    #print "*** $memb ***\n";
    $self->killFamilyJob( $memb->pfama_acc->pfama_acc);
  }

  #Add the clan job to the pfam_jobs database.
   my $guard = $self->jobdb->getSchema->txn_scope_guard;
    eval{
    my $r = $self->jobdb->getSchema->resultset('JobHistory')->create(
      {
        status     => 'PEND',
        job_id     => $uid,
        entity_acc => $clan->clan_acc,
        entity_id  => $clan->clan_id,
        job_type   => 'clan',
        options    => '',
        opened     => \'NOW()',
        user_id    => $name
      }
    );

    my $s = $self->jobdb->getSchema->resultset('JobStream')->create(
      {
        id     => $r->id,
        stdin  => '',
        stdout => '',
        stderr => ''
      }
    );
    $guard->commit;
  };

  if ($@) {
    die "Failed during 'clan view' job submission transaction!: [$@]\n";
  }

  print STDERR "Submitted job to make clan view files - job-id: $uid\n";

}

sub initiateFamilyViewProcess {
  my ( $self, $famObj, $name ) = @_;

  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();
  
#-------------------------------------------------------------------------------
# Now clear out any existing jobs for that family.
  $self->killFamilyJob( $famObj->DESC->AC );

  my $size = $famObj->scores->numRegions;

  #Add the job to the pfam_jobs database.
   my $guard = $self->jobdb->getSchema->txn_scope_guard;
   eval{
    my $r = $self->jobdb->getSchema->resultset('JobHistory')->create(
      {
        status      => 'PEND',
        job_id      => $uid,
        entity_acc  => $famObj->DESC->AC,
        entity_id   => $famObj->DESC->ID,
        entity_size => $size,
        job_type    => 'family',
        options     => '',
        opened      => \'NOW()',
        user_id     => $name
      }
    );

    my $s = $self->jobdb->getSchema->resultset('JobStream')->create(
      {
        id     => $r->id,
        stdin  => '',
        stdout => '',
        stderr => ''
      }
    );
    $guard->commit;
  };

  #Lets tran
  if ($@) {
    die "Failed during job submission transaction!: [$@]\n";
  }

  print STDERR "Submitted job to make view files - job-id: $uid\n";

  #Now release the lock on the family!
}

sub killFamilyJob {
  my ($self, $acc ) = @_;
  my @jobs = $self->jobdb->getSchema->resultset('JobHistory')->search(
    {
      entity_acc => $acc,
      -and       => [
        status => { '!=' => 'DONE' },
        status => { '!=' => 'KILL' }
      ],
    }
  );

  if ( scalar(@jobs) ) {
    foreach my $job (@jobs) {
      if ( $job->lsf_id and $job->lsf_id =~ /\d+/ ) {

        #$self->killJob( $job->lsf_id );
      }
      $job->update(
        {
          status => 'KILL',
          closed => \'NOW()'
        }
      );
    }
  }

}

sub killJob {
  my ($self, $job_id) = @_;

  require LSF::Job;

  #Build the job oblect for the job_id
  my $lsfJob = LSF::Job->new($job_id);

  #Now kill is! Muuuuhahah!
  eval { $lsfJob->kill if ($lsfJob); };
  if ($@) {
    print STDERR
      "Got the following error trying to kill off an old view process:\n$@\n";
    print STDERR
"This may cause downstream problems, so you should tell someone who knows what it going on.\n";
  }
}


sub submitJob {
  my ( $self, $type ) = @_;

  #Submit a new job to the job database
  $self->logger->debug("Submitting job:$type");
  my $uid = Data::UUID->new()->create_str();
  $self->jobdb->getSchema->resultset('JobHistory')->create(
    {
      status     => 'PEND',
      job_id     => $uid,
      family_acc => $self->pfam->pfama_acc,
      family_id  => $self->pfam->pfama_id,
      job_type   => $type,
      options    => '',
      opened     => \'NOW()',
      user_id    => 'rdf'
    }
  );
}

sub initiateAncillaryViewProcess {
  my ( $self ) = @_;
  
  my $name = getpwuid($>); 
  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();
  
  my $rs = $self->jobdb->getSchema->resultset('JobHistory')->search(
    {
      job_type => 'ancillary',
      -and     => [
        status => { '!=' => 'DONE' },
        status => { '!=' => 'KILL' }
      ],
    }
  );
  #There is a running job.
  unless ( $rs->count ) {
    eval{
    #Add the ancillary job to the pfam_jobs database.
      my $guard = $self->jobdb->getSchema->txn_scope_guard;
      my $r = $self->jobdb->getSchema->resultset('JobHistory')->create(
        {
          status     => 'PEND',
          job_id     => $uid,
          entity_acc => 'Ancillary',
          entity_id  => 'Ancillary',
          job_type   => 'Ancillary',
          options    => '',
          opened     => \'NOW()',
          user_id    => $name
        }
      );

      my $s = $self->jobdb->getSchema->resultset('JobStream')->create(
        {
          id     => $r->id,
          stdin  => '',
          stdout => '',
          stderr => ''
        }
      );
    $guard->commit;  
    };
    if ($@) {
      die "Failed during job submission transaction!: [$@]\n";
    }
  }
}

sub mailPfam {
  my ( $self, $title, $message, $nodie ) = @_;

 
  my %header = (
    To      => $self->config->view_process_admin,
    From    => $self->config->view_process_admin,
    Subject => $title
  );

  my $mailer = Mail::Mailer->new;
  $mailer->open( \%header );
  print $mailer $message;
  $mailer->close;
  exit(1) unless ($nodie);
}

sub mailUserAndFail {
  my ( $self, $message ) = @_;

  if ( $self->job->user_id ) {

    my %header = (
      To      => $self->job->user_id . '@ebi.ac.uk',
      Cc      => $self->config->view_process_admin,
      From    => $self->config->view_process_admin,
      Subject => 'Error in view process for ' . $self->job->entity_id
    );

    my $mailer = Mail::Mailer->new;
    $mailer->open( \%header );
    print $mailer $self->job->entity_id . "\n" . $message;
    $mailer->close;
  }
  else {
    $self->mailPfam( "View process for " . $self->job->entity_acc . " failed",
      "No user found for the job" );
  }

  $self->job->update(
    {
      status => 'FAIL',
      closed => \'NOW()'
    }
  );
  exit(1);
}

#Now start processing the SEED and ALIGN files.  These have to be done before the HMMs.
# The following steps are carried out during this loop:
# 1) Calculation of the consensus strings
# 2) Reformating of the alignments (changing accessions to identifiers).
# 3) Calculation of cigar strings
# 4) Estimation of the phylogenetic tree
# 5) Reordering of the alignments according to the tree
# 6) Making a non-redundant fasta file
# 7) Active site prediction
# 8) Update the proteome information
# 9) Determine taxonomic range

sub processALIGN {
  my ( $self, $GFAnn ) = @_;

  #Some labels that are fixed.
  my $filename = 'ALIGN';
  my $type     = 'full';

  my $a = $self->readAlignment($filename);

#Get all of the region information from the database. This will allows to performs some
#rudimentary QC and more importantly, exchange accessions for ids in the alignment

  my $regs = $self->_getFullRegions();
  my $ali =  $self->_verifyFullRegions( $regs, $a );

#-------------------------------------------------------------------------------
#Adding annotations.
#TODO remove print below (debugging)
#print "Ali: " . p($ali);

  #Predict active site residues
  $self->logger->debug("Going to add active site data to FULL");
  $ali = $self->activeSite->full($ali);

  #By default, the ids are actually accesions. This swaps them over.
  my $aliIds = $self->_alignAcc2id( $ali, $regs );

  #Add secondary structure strings
  my ($ssStrings) = $self->addSecondaryStructure( $type, $aliIds, $regs);

  #This will caclulcate the consenus at 60% and add the secondary structure consensus
  $self->addConsensuses( $aliIds, $type, $ssStrings, $filename );

  #Write out the annotated file.
  $self->logger->debug("Making stockholm file for $filename");
  $self->write_stockholm_file( $filename, $aliIds, $GFAnn);

  if ( $aliIds->num_sequences <= 5000 ) {
    $self->makeHTMLAlign( $filename, 80, $type );
  }

#-------------------------------------------------------------------------------
#Calculate stats on the alignment
  $self->_fullAlignmentStats( $aliIds, $regs );
  $self->_align2cigar( $aliIds, $regs );

#-------------------------------------------------------------------------------
#Addition files derived from the full alignment.

  #Make family non-redundant for fasta
  $self->logger->debug("Making fasta file for $filename");
  $self->makeNonRedundantFasta;

  $self->makeRPAligns( $aliIds, $regs, $GFAnn );

  #Upload the alignments (stockholm and html) and tree into the database
  $self->uploadTreesAndAlign( $filename, $type );
}

sub _align2cigar {
  my ( $self, $ali, $regs ) = @_;

  #Now add the sequences back to the object.
  foreach my $seq ( $ali->each_seq ) {

    #Calculate the cigarstring for the object
    my $cString = $self->_generateCigarString( $seq->seq);

    #Note this may be $seq->acc
    $regs->{ $seq->acc . "."
        . $seq->version . "/"
        . $seq->start . "-"
        . $seq->end }->{dom}->cigar($cString);
    $regs->{ $seq->acc . "."
        . $seq->version . "/"
        . $seq->start . "-"
        . $seq->end }->{dom}->update();
  }
}

sub makeRPAligns {
  my ( $self, $aliIds, $regs, $GFAnn ) = @_;

  #p($aliIds);
  my @rplevels = qw(rp15 rp35 rp55 rp75);
  my $counts;
  foreach my $l (@rplevels) {
    $counts->{'number_'.$l} =0;
    my $rpali = Bio::Pfam::AlignPfam->new();
    my @ssStrings;
    $self->logger->debug("Working on level $l");
    foreach my $seq ( $aliIds->each_seq ) {
      my $key =
          $seq->acc . "."
        . $seq->seq_version . "/"
        . $seq->start . "-"
        . $seq->end;
      if ( exists( $regs->{$key} ) and $regs->{$key}->{$l} ) {
        $rpali->add_seq($seq);
        if ( $seq->sec_struct ) {
          push( @ssStrings, $seq->sec_struct->display );
        }
      }
    }
    $counts->{'number_'.$l} = $rpali->num_sequences;
    #IT is feasible that no sequences in the family are in RP.
    next if($rpali->num_sequences == 0);
    my $filename = "ALIGN.$l";
  
#Secondary Structure from RPs
#This will caclulcate the consenus at 60% and add the secondary structure consensus
    $self->addConsensuses( $rpali, $l, \@ssStrings, $filename );

    #Write out the annotated file.
    $self->logger->debug("Making stockholm file for $filename");
    #TODO - remove all gaps. Memory footprint too high to use bioperl as it
    #with duplicate the alignment. Looked into eseal, but that it has a bug
    #rdf 26/09/2012
    $self->write_stockholm_file( $filename, $rpali, $GFAnn );
    # system($self->config->hmmer3binDev."/esl-reformat --informat stockholm --mingap pfam $filename.ann > $filename.ann.nogap")
    system("esl-reformat --informat stockholm --mingap pfam $filename.ann > $filename.ann.nogap")
      and $self->mailUserAndFail("Problem running esl-reformat to remove gaps.");
    rename("$filename.ann.nogap", "$filename.ann") or $self->mailUserAndFail("Failed to rename RP nogap alignment"); 
     
    if ( $aliIds->num_sequences <= 5000 ) {
      $self->makeHTMLAlign( $filename, 80, $l );
    }
    $self->uploadTreesAndAlign($filename, $l );
  }

 #Now update number in rp full alignments.
 $self->pfam->update($counts); 
}

sub _getFullRegions {
  my ( $self ) = @_;

  $self->logger->debug("Getting sequence identifiers for ALIGN");
   my $regs = $self->pfamdb
                    ->getSchema
                      ->resultset('Pfamseq')
                        ->search({'pfam_a_reg_full_significants.pfama_acc' => $self->pfam->pfama_acc,
                                  'pfam_a_reg_full_significants.in_full' => 1,                },
                                 { prefetch => 'pfam_a_reg_full_significants' });

  my %regs;
  while(my $row = $regs->next){
    my @doms = $row->pfam_a_reg_full_significants;
    foreach my $dom (@doms){
      my $data = $row->get_column_data;
      $data->{dom} = $dom;
      $regs{ $row->pfamseq_acc . "."
             .$row->seq_version . "/"
             .$dom->seq_start . "-"
                    . $dom->seq_end 
                      } = $data;
     }
  }

	return ( \%regs );
}

sub _getSeedRegions {
  my ( $self ) = @_;
  $self->logger->debug("Getting sequences identifiers for SEED");
  my $regs = $self->pfamdb
                    ->getSchema
                      ->resultset('Pfamseq')
                        ->search({'pfam_a_reg_seeds.pfama_acc' => $self->pfam->pfama_acc,
                                                  },
                                                            { prefetch => 'pfam_a_reg_seeds' });

  my %regs;
  while(my $row = $regs->next){
    my @doms = $row->pfam_a_reg_seeds;
    foreach my $dom (@doms){
      my $data = $row->get_column_data;
      $data->{dom} = $dom;
      $regs{ $row->pfamseq_acc . "."
             .$row->seq_version . "/"
             .$dom->seq_start . "-"
                    . $dom->seq_end 
                      } = $data;
     }
  }
  
  return ( \%regs );
}

sub _verifyFullRegions {
  my ( $self, $regs, $a ) = @_;
  my ($ali);

#Need to remove sequences in alignment which are outcompeted if family is in a clan
  my @regs = keys(%$regs);
  if ( $a->num_sequences eq @regs ) {
    $ali = $a;
    if ( $ali->num_sequences != $self->pfam->num_full ) {
      $self->mailUserAndFail( 
"Missmatch between number of regions in competed PfamA table ($#regs) and competed ALIGN file ("
          . $ali->num_sequences
          . ")" );
    }
  }
  else {
    $ali = Bio::Pfam::AlignPfam->new();

    foreach my $seq ( $a->each_seq ) {
      if (
        exists(
          $regs->{
                $seq->id . "."
              . $seq->seq_version . "/"
              . $seq->start . "-"
              . $seq->end
            }
        )
        )
      {
        $ali->add_seq($seq);
      }
    }
    unless ( $ali->num_sequences eq @regs ) {
      $self->mailUserAndFail(
"Missmatch between number of regions in competed PfamA table ($#regs) and competed ALIGN file ("
          . $ali->num_sequences
          . ")" );
    }
    if ( defined $a->match_states_string() ) {
      $ali->match_states_string( $a->match_states_string() );
    }
  }

  return ($ali);
}

sub _verifySeedRegions {
  my ( $self, $regs, $ali ) = @_;

  my @regs = keys(%$regs);

  #QC check
  if ( ( $ali->num_sequences != $self->pfam->num_seed )
    or ( $ali->num_sequences != scalar(@regs) ) )
  {
    $self->mailUserAndFail(
          "Missmatch between number of regions in PfamA table (num_seed),"
        . " number of regions from PfamA_reg_seed and/or alignment on disk" );
  }
}

sub addConsensuses {
  my ($self, $aliObj, $type, $ssStrings, $filename )= @_;
  if ( defined($ssStrings) and ref($ssStrings) eq 'ARRAY'  and scalar(@$ssStrings) ) {
    my $consensus;
    if ( scalar(@$ssStrings) > 1 ) {
      $consensus = $self->secStrucConsensus($ssStrings);
    }
    else {
      $consensus = $ssStrings->[0];
    }
    $aliObj->cons_sec_struct(
      Bio::Pfam::OtherRegion->new(
        '-seq_id'  => "seq_cons",
        '-from'    => 1,
        '-to'      => length($consensus),
        '-type'    => "secondary_structure",
        '-display' => $consensus,
        '-source'  => 'Pfam'
      )
    );
  }

  #Consensus information....
  open( ALIGN, ">", $filename )
    or $self->mailUserAndFail(
    "Could not open $filename  file for writing:[$!]\n");
  $aliObj->write_Pfam( \*ALIGN );
  close(ALIGN);

  my $consensus = $self->consensus_line( $filename, $aliObj->length );
  $aliObj->cons_sequence(
    Bio::Pfam::OtherRegion->new(
      '-seq_id'  => 'none',
      '-from'    => 1,
      '-to'      => length($consensus),
      '-type'    => "60\%_consenus_sequence",
      '-display' => $consensus,
      '-source'  => 'Pfam'
    )
  );

}

sub _alignAcc2id {
  my ( $self, $ali, $regs, $type ) = @_;

  #Now change the alignment from accessions to ids.

  #Make a new alignment object
  my $aliIds = Bio::Pfam::AlignPfam->new();

  #Now exchange the accessions for ids
  foreach my $s ( $ali->each_seq ) {
    $s->acc( $s->id );    # id from alignment files is actually an accession
    my $i = $s->acc . "." . $s->version . "/" . $s->start . "-" . $s->end;
    if ( $regs->{$i} ) {
      $s->id( $regs->{$i}->{pfamseq_id} );
    }
    else {
      $self->mailUserAndFail( 
        "Could not find id for $i" );
    }
    if ( defined($type) and  $type eq 'seed' ) {
      $aliIds->add_seq( $s, $regs->{$i}->{dom}->tree_order );
    }
    else {
      $aliIds->add_seq($s);
    }
  }

  #See of there is a match state string, if so add it to the object.
  if ( defined $ali->match_states_string() ) {
    $aliIds->match_states_string( $ali->match_states_string() );
  }
  return ($aliIds);

}

sub _fullAlignmentStats {
  my ( $self, $aliIds, $regs ) = @_;

#-------------------------------------------------------------------------------
#Full alignment stats.
  my ( $no_aa_in_domain, $no_aa_in_sequences );
  my %species;

  my %seen;
  foreach my $nse ( keys %$regs ) {
    $species{ $regs->{$nse}->{ncbi_taxid} } = 1;
    if ( !exists( $seen{ $regs->{$nse}->{pfamseq_id} }) ) {
      $no_aa_in_sequences += $regs->{$nse}->{length};
      $seen{ $regs->{$nse}->{pfamseq_id} }++;
    }
    $no_aa_in_domain +=
      ( $regs->{$nse}->{dom}->seq_end - $regs->{$nse}->{dom}->seq_start + 1 );
  }

  #This part runs alistat on the alignment to get the average length and % ID
  my ( $averageLength, $percentageId );

  #to run into issues making FASTA....
  open( ALI, $self->config->binLocation . '/esl-alistat --amino --informat SELEX ALIGN |' )
    or $self->mailUserAndFail( 
    "Could not open alistat pipe:[$!]" );

  #Grab the fields out of alistat
  while (<ALI>) {
    if (/^Average length:\s+(\S+)/) {
      $averageLength = $1;
    }
    elsif (/^Average identity:\s+(\d+)/) {
      $percentageId = $1;
    }
  }
  close(ALI);

  #Calculate the species and coverage jobs
  my $averageCoverage = sprintf "%.2f",
    100 * ( $no_aa_in_domain / $no_aa_in_sequences );
  my $noSpecies = scalar( keys %species );

  $self->logger->debug("Average sequence coverage by the domain is $averageCoverage");
  $self->logger->debug("Average length is $averageLength");
  $self->logger->debug("Number of species is $noSpecies");
  $self->logger->debug("Percentage identity is $percentageId");

  $self->pfam->update(
    {
      percentage_id    => $percentageId,
      number_species   => $noSpecies,
      average_length   => $averageLength,
      average_coverage => $averageCoverage,
      full_consensus   => $aliIds->cons_sequence->display,
      num_full         => $aliIds->num_sequences,
      number_archs     => 0
    }
  );

#-------------------------------------------------------------------------------

}

sub readAlignment {
  my ($self, $filename) = @_;

  #Read the alignment into an object
  open( ALIGN, "$filename" )
    or $self->mailUserAndFail(
    "Could not open $filename file for reading:[$!]\n");
  my $a = Bio::Pfam::AlignPfam->new();
  $a->read_stockholm( \*ALIGN, 1 );
  close(ALIGN);
  return $a;
}

sub processSEED {
  my ( $self, $GFAnn ) = @_;
  my $filename = 'SEED';
  my $type     = 'seed';
  my $ali      = $self->readAlignment($filename);

  my $regs = $self->_getSeedRegions;
  $self->_verifySeedRegions( $regs, $ali );

  #Add predicted active site residues
  $self->logger->debug("Going to add active site data to SEED");
  $ali = $self->activeSite->seed($ali);

  #Make a tree for the alignment.
  $self->make_tree( $filename, $regs, "pfamseq" );

#This mot only sets the id, but will reorder as regs now contains the tree order.
  my $aliIds = $self->_alignAcc2id( $ali, $regs, $type );

  my $ssStrings =
    $self->addSecondaryStructure( $type, $aliIds, $regs );

#This will caclulcate the consenus at 60% and add the secondary structure consensus
  $self->addConsensuses( $aliIds, $type, $ssStrings, $filename );

  #Now run FastTree on the original alignment file
  $self->logger->debug("Going to run FastTree on $filename");

  $self->write_stockholm_file( $filename, $aliIds, $GFAnn );

  $self->makeHTMLAlign( $filename, 80, $type );

  #Note this updates the regions. It may be better to pull this out and
  #explicitly update the regs.
  $self->_align2cigar( $aliIds, $regs);

  $self->pfam->update(
    {
      seed_consensus => $aliIds->cons_sequence->display,
      num_seed       => $aliIds->num_sequences,
    }
  );

  #Upload the alignments (stockholm and html) and tree into the database
  $self->uploadTreesAndAlign( $filename,$type );
}

sub addSecondaryStructure {
  my ( $self, $type, $aliIds, $regs ) = @_;

  my $ssStrings;
  #Add the secondary structure
  $self->logger->debug("Adding secondary structure data to $type");
  
  #First, get the secondary structure data from the database
  my ($dsspDataRef) =
    $self->_getDsspData( $type );
  if ( $dsspDataRef and scalar( keys(%$dsspDataRef) ) ) {
    $self->logger->debug("Found some SS data");

    #Now add this to the alignment
    my($noStructures, $map);
    ( $noStructures, $map, $ssStrings ) =
      $self->markupAlignWithSS( $aliIds, $dsspDataRef );

   
    if ( $type eq "full" ) {
      if ($noStructures) {
        $self->pfam->update( { number_structures => $noStructures } );
      }else{
        $self->pfam->update( { number_structures => 0 } );
      }
      #$self->logger->debug(p($map));
      #Delete all auto_pfamAs
      $self->pfamdb->getSchema->resultset('PdbPfamAReg')
        ->search( { pfama_acc => $self->pfam->pfama_acc } )->delete;

      #Add the pdbmap data to pdb_pfamA_reg
      my %autoPdbs;
      foreach my $nse ( keys %$map ) {
        foreach my $pdbReg ( @{ $map->{$nse} } ) {
          
            #"Inserting row into PdbPfamaReg " . $regs->{$nse}->auto_pfama );

          #Now reinsert
          $self->pfamdb->getSchema->resultset('PdbPfamAReg')->create(
            {
              auto_pfama_reg_full => $regs->{$nse}->{dom}->auto_pfama_reg_full,
              pdb_id              => $pdbReg->{pdb_id},
              pfama_acc          => $regs->{$nse}->{dom}->pfama_acc,
              pfamseq_acc        => $regs->{$nse}->{dom}->pfamseq_acc,
              chain               => $pdbReg->{chain},
              pdb_res_start       => $pdbReg->{pdb_start},
              pdb_start_icode     => $pdbReg->{pdb_start_icode},
              pdb_res_end         => $pdbReg->{pdb_end},
              pdb_end_icode       => $pdbReg->{pdb_end_icode},
              seq_start           => $pdbReg->{seq_start},
              seq_end             => $pdbReg->{seq_end},
            }
          );
        }
      }
    }
  }
  return($ssStrings);
}

sub checkflat {
  my ( $self, $filename ) = @_;
  $self->logger->debug("Going to check the $filename for errors");
  #Okay - Everything should be in the Stockholm file!
  #Now run some QC on the files

  open( QC, "checkflat.pl -v $filename|" )
    or $self->mailUserAndFail(
    "Failed to run checkflat.pl on $filename:[$!]" );
  my $qc = join( "", <QC> );
  if ( $qc =~ /\S+/ ) {
    $self->mailUserAndFail(
      "$filename did not pass quality control! Got $qc" );
  }
  close(QC);
  $self->logger->debug("$filename passed checks");
}

#-------------------------------------------------------------------------------
#update the proteome information
sub pfamProteomes {
  my ( $self ) = @_;
  $self->logger->debug("Deleting proteome information");
  $self->pfamdb->getSchema->resultset('ProteomeRegion')
    ->search( { pfama_acc => $self->pfam->pfama_acc } )->delete;

  $self->logger->debug("Updating the proteome information");

  my $dbh = $self->pfamdb->getSchema->storage->dbh;
  $dbh->do(
"INSERT INTO proteome_regions (auto_proteome, pfamseq_acc, pfamA_acc, count) "
      . "SELECT c.auto_proteome, p.pfamseq_acc, r.pfamA_acc, count(*) FROM "
      . "pfamA_reg_full_significant r,  proteome_pfamseq p, complete_proteomes c "
      . "WHERE r.pfamseq_acc=p.pfamseq_acc AND in_full=1 AND c.auto_proteome=p.auto_proteome AND pfamA_acc='"
      . $self->pfam->pfama_acc
      . "' GROUP BY r.pfamseq_acc" )
    or $self->mailUserAndFail(
    "Failed to update the proteome data for "
      . $self->pfam->pfama_acc
      . " because: "
      . $dbh->errstr
    );
}

sub pfamTaxDepth {
  my ( $self ) = @_;

#-------------------------------------------------------------------------------
#Now update the taxomonic depth/range information
  $self->logger->debug("Deleting tax depth information");
  $self->pfamdb->getSchema->resultset('PfamATaxDepth')
    ->search( { pfama_acc => $self->pfam->pfama_acc } )->delete;

  $self->logger->debug("Fetching taxonomic roots");

  my @ranges =
    $self->pfamdb->getSchema->resultset('Taxonomy')->search( { parent => 'root' } );

  #Generate a temporary table in the same fashion as before.
  my $dbh = $self->pfamdb->getSchema->storage->dbh;
  my $table     = "_taxDist$$";    #Keep the table name for later use.
  my $statement =
      "create temporary table $table ( `pfamseq_acc` varchar(10) NOT NULL,"
    . "PRIMARY KEY (pfamseq_acc) )";
  $dbh->do($statement) or die $dbh->errstr;

  #Now populate it;
  $dbh->do(
"INSERT INTO $table SELECT distinct s.pfamseq_acc from pfamseq s, pfamA_reg_full_significant r
            WHERE in_full=1 AND r.pfamseq_acc=s.pfamseq_acc and r.pfamA_acc='"
      . $self->pfam->pfama_acc . "'"
    )
    or $dbh->errstr;

#-------------------------------------------------------------------------------
#Now grab the different taxonomic ranges;
  my $ranges =
    $dbh->selectall_arrayref(
    'select lft, rgt, level from taxonomy where parent="root"')
    or die $dbh->errstr;

#Now loop over the tax ranges and perform a look-up on each if there is an overlap
  my $sthTaxDepth = $dbh->prepare(
    "SELECT level, lft, rgt from taxonomy 
                                    WHERE lft<= ? and rgt>= ?
                                    AND level!='NULL' 
                                    ORDER BY rgt-lft DESC"
  ) or die $dbh->errstr();

  my $sthMinMax = $dbh->prepare(
    "SELECT min(lft), max(rgt), count(pfamseq_acc) 
                                            FROM  $table r, pfamseq s, taxonomy t 
                                            WHERE s.pfamseq_acc=r.pfamseq_acc 
                                            AND t.ncbi_taxid=s.ncbi_taxid 
                                            AND t.ncbi_taxid!=0 
                                            AND lft >= ?
                                            AND rgt <= ?"
  ) or die $dbh->errstr();
  my ( %depth, %count );
  foreach my $r (@$ranges) {

    #Test to see if it falls in range
    #Now get the max/min range for the family based on the temp table.
    $sthMinMax->execute( $r->[0], $r->[1] );
    my ( $min, $max, $count ) = $sthMinMax->fetchrow_array;

    next unless ( $min and $max );
    $sthTaxDepth->execute( $min, $max );
    foreach my $tax ( @{ $sthTaxDepth->fetchall_arrayref } ) {
      $depth{ $r->[2] } .= $tax->[0] . ";";
    }
    $count{ $r->[2] } = $count;
  }
  $sthTaxDepth->finish;
  $sthMinMax->finish;
  
  use DDP;
  p(%depth);
  p(%count);
  
}

sub processHMMs {
  my ( $self ) = @_;

  unlink("HMM.ann")
    or
   $self->mailUserAndFail( "Failed to remove HMM.ann" )
    if ( -e "HMM.ann" );

#The next two blocks rebuilds and recalibrates the HMM and adds the curated thresholds to the HMM

  #process the HMM file first
  #We are going to use this a liogm
  my $pfam = $self->pfam; 
  my $buildline = $self->_cleanBuildLine( $pfam->buildmethod );

  $self->logger->debug(
    "Going to run hmmbuild with the following line: $buildline HMM.ann SEED.ann"
  );
  system( $self->config->hmmer3bin . "/$buildline -o  /dev/null HMM.ann SEED.ann" )
    and $self->mailUserAndFail(
    "Failed to build HMM.ann, using $buildline HMM.ann SEED.ann" );

  #Take HMM_ls and add the thresholds into the file
  open( HMM_OUT, ">HMM.ann.tmp" )
    or $self->mailUserAndFail( "Could not open HMM.ann.tmp for writing" );
  open( HMM, "HMM.ann" )
    or $self->mailUserAndFail(
    "Could not open HMM.ann for writing" );
  while (<HMM>) {
    if (/^GA\s+/) {
      print HMM_OUT "GA    "
        . $pfam->sequence_ga . " "
        . $pfam->domain_ga . ";\n";
      print HMM_OUT "TC    "
        . $pfam->sequence_tc . " "
        . $pfam->domain_tc . ";\n";
      print HMM_OUT "NC    "
        . $pfam->sequence_nc . " "
        . $pfam->domain_nc . ";\n";
      print HMM_OUT "BM    ", $self->_cleanBuildLine( $pfam->buildmethod ),
        "HMM.ann SEED.ann\n";
      print HMM_OUT "SM    ", $pfam->searchmethod, "\n";
      next;
    }
    next if ( $_ =~ /^NC\s+/ or $_ =~ /^TC\s+/ );
    print HMM_OUT $_;
  }
  close HMM;
  close HMM_OUT;
  rename( "HMM.ann.tmp", "HMM.ann" )
    or $self->mailUserAndFail( 
    "can't rename HMM.ann.tmp to HMM.ann\n" );
  $self->logger->debug("Going to check the HMM.ann files for errors");

  #Now run QC on the HMMs
  my $dbVersion = $self->pfamdb->getSchema->resultset('Version')->find( {} );

  foreach my $f (qw(HMM.ann)) {
    open( QC,
      "checkhmmflat.pl -v -hmmer " . $dbVersion->hmmer_version . " -f $f |" )
      or $self->mailUserAndFail(
      "Failed to run checkhmmflat.pl on $f:[$!]" );
    my $qc = join( "", <QC> );
    if ( $qc =~ /\S+/ ) {
      $self->mailUserAndFail(
        "$f did not pass quality control! Got [$qc]" );
    }
    close(QC);
  }
  $self->logger->debug("HMM.ann pass checks");

  #Now upload the HMMs into the database
  open( HMM, "HMM.ann" ) or die;
  my $hmm = join( "", <HMM> );
  close(HMM);
  $self->pfamdb->getSchema->resultset('PfamAHmm')->update_or_create(
    {
      pfama_acc => $pfam->pfama_acc,
      hmm        => $hmm
    }
  );

  #Now make and generate the HMM logo
  #TODO - uncomment this section
  $self->_makeHMMLogo( "HMM.ann" );

}

sub _makeHMMLogo {
  my ( $self, $file) = @_;

  $self->logger->debug("Making logo HMM");
  return;
  my $logo = HMM::Profile->new( -hmmerfile => $file )
    or $self->mailUserAndFail( "Failed in making HMM logo, couldn't open $file!\n" );
  my $outfile     = "hmmLogo.png";
  my $graph_title = $logo->name();
  my $ysize       = 400;
  my $xsize       = $logo->length() * 34;
  my $greyscale   = 0;

  #Now go and make the logos
  $self->logger->debug("Drawing Logo...");
  $logo->draw_logo(
    -file           => $outfile,
    -xsize          => $xsize,
    -ysize          => $ysize,
    -x_title        => 'Position',
    -y_title        => 'Contribution',
    -graph_title    => $graph_title,
    -greyscale      => $greyscale,
    -height_logodds => 1,
    -regular_font   => $self->config->fontDir . '/arial.ttf',
    -bold_font      => $self->config->fontDir . '/arialbd.ttf'
    )
    || $self->mailUserAndFail("Error writing logo $file!\n");

  $self->logger->debug("Finished drawing Logo...");
  unless ( -s "hmmLogo.png" ) {
    $self->mailUserAndFail( "Failed in making HMM logo, no hmmLogo.png file" );
  }

  open( LOGO, "hmmLogo.png" )
    or $self->mailUserAndFail( "Failed to open hmmLogo.png file:[$!]" );
  my $hmmLogo = join( "", <LOGO> );
  close(LOGO);

  #Now upload this logo into the RDB.
  $self->pfamdb->getSchema->resultset('PfamAHmm')->update_or_create(
    {
      pfama_acc => $self->pfam->pfama_acc,
      logo       => $hmmLogo
    }
  );
  return;
}

sub _generateCigarString {
  my ( $self, $str ) = @_;
  chomp($str);
  my @chars = split( //, $str );

  my $count_for_cigar_string = 0;
  my $state_for_cigar_string = 'M';
  my $cigar_string           = '';
  foreach my $char (@chars) {

    #print "$char";
    my $new_state;
    if ( $char ne '.' and $char ne "-" ) {    # Match
      $new_state = 'M';
    }
    elsif ( $char eq '.' ) {                  #Gap
      $new_state = 'I';
    }
    elsif ( $char eq '-' ) {
      $new_state = 'D';
    }
    else {
      $self->mailUserAndFail( 
        "Error generating cigar string......unknown string char, $char" );
    }

    if ( $new_state ne $state_for_cigar_string ) {
      if ($count_for_cigar_string) {
        my $sub_cigar_string;
        $sub_cigar_string = $count_for_cigar_string
          unless $count_for_cigar_string == 1;
        $sub_cigar_string .= $state_for_cigar_string;
        $cigar_string     .= $sub_cigar_string;
      }
      $count_for_cigar_string = 0;
    }
    $state_for_cigar_string = $new_state;
    $count_for_cigar_string++;
  }

  if ($count_for_cigar_string) {
    my $sub_cigar_string;
    $sub_cigar_string = $count_for_cigar_string
      unless $count_for_cigar_string == 1;
    $sub_cigar_string .= $state_for_cigar_string;
    $cigar_string     .= $sub_cigar_string;
  }

  return $cigar_string;
}

sub _getDsspData {
  my ( $self, $type ) = @_;
  
  my $pfamA = $self->pfam->pfama_acc;
  $self->logger->debug(
    "Going to fetch Secondary structure information for pfamA $pfamA"
  );


  my ( %famDSSP, @dssp );
  if ( $type eq 'full' ) {
    @dssp = $self->pfamdb->getSchema->resultset("PdbResidueData")->search(
      {
        "pfam_a_reg_full_significants.pfama_acc" => $pfamA,
        "pfam_a_reg_full_significants.in_full"    => 1,
        observed                          => 1
      },
      {
        join   => [qw( pfam_a_reg_full_significants )],
        select => [
          qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number pdb_insert_code dssp_code)
        ],
        as => [
          qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number pdb_insert_code dssp_code)
        ]
      }
    );

  }
  elsif ( $type eq 'seed' ) {
    @dssp = $self->pfamdb->getSchema->resultset("PdbResidueData")->search(
      {
        "pfam_a_reg_seeds.pfama_acc" => $pfamA,
        observed              => 1
      },
      {
        join   => [qw( pfam_a_reg_seeds )],
        select => [
          qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number pdb_insert_code dssp_code)
        ],
        as => [
          qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number pdb_insert_code dssp_code)
        ]
      }
    );
  }
  else {
    $self->mailUserAndFail(
"Unknown file name passed in ($type) to _getDsspData, expected full or seed"
    );
  }

  #Now stuff it into a data structure for working on
  my $acc2map;
  foreach (@dssp) {
    unless ( $acc2map->{ $_->get_column('pfamseq_acc') }
      ->{ $_->get_column('pfamseq_seq_number') }
      ->{ $_->get_column('pdb_id') . "_" . $_->get_column('chain') } )
    {

      $acc2map->{ $_->get_column('pfamseq_acc') }
        ->{ $_->get_column('pfamseq_seq_number') }
        ->{ $_->get_column('pdb_id') . "_" . $_->get_column('chain') }->{
        $_->get_column('pdb_seq_number')
          . (
          defined( $_->get_column('pdb_insert_code') )
          ? $_->get_column('pdb_insert_code')
          : ''
          )
        }
        = $_->get_column('dssp_code');
    }
  }
  return ($acc2map);
}

#-------------------------------------------------------------------------------

=head2 markupAlignWithSS 

  Title    :
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub markupAlignWithSS {
  my ( $self, $aln, $dsspDataRef ) = @_;
  my $noStructures;
  my $map;
  my @allSsStrings;

  foreach my $seq ( $aln->each_seq() ) {
    my $start = $seq->start;
    my $end   = $seq->end;
    my @ali   = split( //, $seq->seq );
    my %ss;
    my $p = $start;
    my %uniqueMaps;

    foreach my $pos ( keys %{ $$dsspDataRef{ $seq->acc } } ) {
      foreach my $pdb_chain ( keys %{ $$dsspDataRef{ $seq->acc }{$pos} } ) {
        $uniqueMaps{$pdb_chain} = 1;
      }
    }

    foreach my $res (@ali) {

      #We are at insert/delete position
      if ( $res eq "." or $res eq "-" ) {

        #Add $res to the ssString
        foreach my $pdb_chain ( keys %uniqueMaps ) {
          $ss{$pdb_chain}{ssString} .= "$res";
        }
      }
      else {

        #Okay, we have a residue
        foreach my $pdb_chain ( keys %uniqueMaps ) {

          #Test to see if there is positional information for this residue?
          if ( values %{ $$dsspDataRef{ $seq->acc }{$p}{$pdb_chain} } ) {
            while ( my ( $pdbResNum, $dsspCode ) =
              each %{ $$dsspDataRef{ $seq->acc }{$p}{$pdb_chain} } )
            {
              $dsspCode = defined($dsspCode) ? $dsspCode : '';
              if ( $dsspCode =~ /\S+/ ) {
                $ss{$pdb_chain}{ssString} .= $dsspCode;
              }
              else {
                $ss{$pdb_chain}{ssString} .= "-";
              }

              my $pdbInsCode = '';
              ( $pdbResNum, $pdbInsCode ) = $pdbResNum =~ /(\d+)(\S?)/;

              if ( !$ss{$pdb_chain}{start} || !$ss{$pdb_chain}{end} ) {
                $ss{$pdb_chain}{start}       = $pdbResNum;
                $ss{$pdb_chain}{end}         = $pdbResNum;
                $ss{$pdb_chain}{icode_start} = $pdbInsCode;
                $ss{$pdb_chain}{icode_end}   = $pdbInsCode;
                $ss{$pdb_chain}{seq_start}   = $p;
                $ss{$pdb_chain}{seq_end}     = $p;
              }
              else {

                if ( $p <= $ss{$pdb_chain}{seq_start} ) {
                  if ( $ss{$pdb_chain}{start} == $pdbResNum ) {
                    if ( ( $ss{$pdb_chain}{icode_start} cmp $pdbInsCode ) == 1 )
                    {
                      $ss{$pdb_chain}{icode_start} = $pdbInsCode;
                    }
                  }
                  else {
                    $ss{$pdb_chain}{icode_start} = $pdbInsCode;
                  }
                  $ss{$pdb_chain}{start}     = $pdbResNum;
                  $ss{$pdb_chain}{seq_start} = $p;
                }

                if ( $p >= $ss{$pdb_chain}{seq_end} ) {

                  #NEed to check out insert codes
                  if ( $ss{$pdb_chain}{end} == $pdbResNum ) {
                    if ( ( $pdbResNum cmp $ss{$pdb_chain}{icode_start} ) == 1 )
                    {
                      $ss{$pdb_chain}{icode_start} = $pdbInsCode;
                    }
                  }
                  else {
                    $ss{$pdb_chain}{icode_start} = $pdbInsCode;
                  }
                  $ss{$pdb_chain}{end}     = $pdbResNum;
                  $ss{$pdb_chain}{seq_end} = $p;

                }
              }
            }
          }
          else {

            #If not then put an X for undef
            $ss{$pdb_chain}{ssString} .= "X";
          }
        }
        $p++;
      }
    }

    my @ssForMerging;

    #Remove any strings that lack SS
    foreach my $pdb_chain ( keys %ss ) {

      # T,S,B,E,H,G,I
      if ( $ss{$pdb_chain}{ssString} ) {
        delete $ss{$pdb_chain}
          unless ( $ss{$pdb_chain}{ssString} =~ /[TSBEHGI]/ );
      }
      else {
        delete $ss{$pdb_chain};
      }

      #If we do not delete the hash add it to the
      if ( $ss{$pdb_chain} ) {
        push( @ssForMerging, $ss{$pdb_chain}{ssString} );
        my ( $pdb, $chain ) = split( /_/, $pdb_chain );
        $chain = "" if ( !$chain );

        #Put the mapping in to the alignment
        my $link = Bio::Annotation::DBLink->new();
        $link->database("PDB");
        $link->primary_id( $pdb . " " . $chain );
        $link->optional_id( $ss{$pdb_chain}{start}
            . $ss{$pdb_chain}{icode_start} . "-"
            . $ss{$pdb_chain}{end}
            . $ss{$pdb_chain}{icode_end}
            . ";" );
        $seq->annotation( Bio::Annotation::Collection->new() )
          unless ( $seq->annotation );
        $seq->annotation->add_Annotation( 'dblink', $link );
        $noStructures++;

        #Use this to populate the pdb_pfamA_reg table......!
        my $nse =
          $seq->acc . "." . $seq->version . "/" . $seq->start . "-" . $seq->end;
        push(
          @{ $map->{$nse} },
          {
            pdb_id          => $pdb,
            chain           => $chain,
            pdb_start       => $ss{$pdb_chain}{start},
            pdb_start_icode => $ss{$pdb_chain}{icode_start},
            pdb_end         => $ss{$pdb_chain}{end},
            pdb_end_icode   => $ss{$pdb_chain}{icode_end},
            seq_start       => $ss{$pdb_chain}{seq_start},
            seq_end         => $ss{$pdb_chain}{seq_end}
          }
        );
      }
    }

    #Add anything pdbs we have here as an Xref.
    #Compress multiple SS to consenus
    #print Dumper(@ssForMerging);
    if ( scalar(@ssForMerging) ) {
      my $consensus;
      if ( scalar(@ssForMerging) > 1 ) {
        $consensus = $self->secStrucConsensus( \@ssForMerging );
      }
      else {
        $consensus = $ssForMerging[0];
      }
      push( @allSsStrings, $consensus );
      $seq->sec_struct(
        Bio::Pfam::OtherRegion->new(
          '-seq_id'  => $seq->acc,
          '-from'    => $start,
          '-to'      => $end,
          '-type'    => "sec_struct",
          '-display' => $consensus,
          '-source'  => 'Pfam'
        )
      );
    }
  }

#  #Calculate consensus string for the whole alignment.
#  if ( scalar(@allSsStrings) ) {
#    my $allConsensus;
#    if ( scalar(@allSsStrings) > 1 ) {
#      $allConsensus = secStrucConsensus( \@allSsStrings );
#    }
#    else {
#      $allConsensus = $allSsStrings[0];
#    }
#
#    #print STDERR "**** CON SEC $allConsensus *****\n";
#    $aln->cons_sec_struct(
#      Bio::Pfam::OtherRegion->new(
#        '-seq_id'  => "seq_cons",
#        '-from'    => 1,
#        '-to'      => length($allConsensus),
#        '-type'    => "secondary_structure",
#        '-display' => $allConsensus,
#        '-source'  => 'Pfam'
#      )
#    );
#  }
  return ( $noStructures, $map, \@allSsStrings );
}

sub secStrucConsensus {
  my ($self, $ssStringsRef) = @_;
  my $num             = scalar(@$ssStringsRef);
  my $length          = length $$ssStringsRef[0];
  my $gapcount        = 0;
  my $consensusstring = "";
  my ( @count, @consensuschar );
  my $numchar = 0;
  my (
    $m,         $n,        $x,            $z,
    $charindex, $ass,      $ambigous,     $pos,
    $character, $prevchar, $gapcharacter, $maxcount
  );
  $prevchar = "";

  if ( $num > 1 ) {
    for ( $m = 0 ; $m < $length ; $m++ ) {
      for ( $n = 0 ; $n < $num ; $n++ ) {
        $character = substr( $$ssStringsRef[$n], $m, 1 );
        if ( $character ne "-" && $character ne "." && $character ne "X" ) {
          if ( !( $prevchar =~ /$character/ ) ) {
            $consensuschar[$numchar] = $character;
            $count[$numchar]++;
            $numchar++;
            $prevchar .= $character;
          }
          else {
            $pos = index $prevchar, $character;
            $count[$pos]++;
          }
        }
        else {
          $gapcount++;
          $gapcharacter = $character;
        }
      }
      if ( $gapcount eq $num ) {
        $consensusstring .= $gapcharacter;
      }
      else {
        $maxcount = 0;
        my $end_count = @count;
        for ( $x = 0 ; $x < $end_count ; $x++ ) {
          if ( $count[$x] > $maxcount ) {
            $charindex = $x;
            $maxcount  = $count[$x];
          }
        }
        my $ambigious = grep /$maxcount/, @count;
        if ( $ambigious > 1 ) {
          for ( $z = 0 ; $z < $end_count ; $z++ )
          {    # Changed from @count to $end_count for speed
            if ( $count[$z] eq $maxcount ) {
              $ass .= $consensuschar[$z];
            }
          }
          $ass =~ s/G/H/g;
          $ass =~ s/I/H/g;
          $ass =~ s/B/E/g;
          $ass =~ s/S/T/g;
          if ( $ass =~ /^(.)\1+$/ ) {
            $consensusstring .= $1;
          }
          else {
            $consensusstring .= "C";
          }
          $ass = "";
        }
        else {
          $consensusstring .= $consensuschar[$charindex];
        }
      }
      $prevchar = "";
      $gapcount = $numchar = 0;
      undef @count;
      undef @consensuschar;
    }
  }
  else {
    $consensusstring = $$ssStringsRef[0];
  }
  return $consensusstring;
}


sub makeNonRedundantFasta {
  my ( $self ) = @_;

  $self->logger->debug("The average identity of the alignment is ".$self->pfam->percentage_id);
  
  #Some families such as viruses are highly similar, raise the threshold a bit
  #above the average id.
  my $identity = $self->options->{identity};
  if($self->pfam->percentage_id > $identity){
    $identity =   $self->pfam->percentage_id + 1;
    $identity = 100 if($identity > 100);
  }
  
  #Use belvu to make the full alignment 90% non-redundant.
  $identity = $identity / 100;
  my $system_command = $self->config->binLocation . "/esl-weight --informat stockholm --amino -f --idf $identity -o ALIGN.90 ALIGN.ann 2> /dev/null";
  system( $system_command ) == 0
    or $self->mailUserAndFail( "System command failed ($system_command): [$!]\n");

  $system_command = $self->config->binLocation . '/esl-reformat fasta ALIGN.90 2> /dev/null |';
  open( BEL, $system_command )
    or $self->mailUserAndFail( "Could not open command ($system_command): [$!]\n" );

  open( FAMFA, '>family.fa' )
    or $self->mailUserAndFail( "Failed to open family.fa:[$!]" );

#Parse the output, remove gap charcaters and put the family accessions and name as part of the
#header line for each sequence.
  while (<BEL>) {
    if (/\>(\S+\/\d+\-\d+)/) {
      chomp;
      print FAMFA "$_ "
        . $self->pfam->pfama_acc . "."
        . $self->pfam->version . ";"
        . $self->pfam->pfama_id . ";\n";
    }
    else {
      chomp;
      s/[\.-]//g;
      print FAMFA uc($_) . "\n";
    }
  }
  close(BEL);
  close(FAMFA);

  #gzip the file and add it to the database!
  open( GZFA, "gzip -c family.fa |" )
    or $self->mailUserAndFail( 
    "Failed to gzip family.fa:[$!]" );
  my $familyFA = join( "", <GZFA> );
  $self->pfamdb->getSchema->resultset('PfamAFasta')->update_or_create(
    {
      pfama_acc   => $self->pfam->pfama_acc,
      fasta        => $familyFA,
      nr_threshold => $identity
    },
    { key => 'UQ_pfamA_fasta_1' }
  );
}

sub makeHTMLAlign {
  my ( $self, $filename, $block, $type ) = @_;
  $self->logger->debug("Making HTML aligment for $type $filename");
  system("consensus.pl -method clustal -file $filename > $filename.con")
    and $self->mailUserAndFail( 
    "Failed to run consensus.pl:[$!]" );
  system(
    "clustalX.pl -a $filename.ann -c $filename.con -b $block > $filename.html" )
    and $self->mailUserAndFail( 
    "Failed to run clustalX.pl:[$!]" );

  open( ALI, "gzip -c $filename.html |" )
    or $self->mailUserAndFail( 
    "Failed to gzip file $filename.html" );
  my $align = join( "", <ALI> );

  if ( ( $type ne 'seed' ) ) {

    #Make the posterior probablility alignment.
    system("heatMap.pl -a $filename -b $block > $filename.pp")
      and $self->mailUserAndFail( 
      "Failed to run heatMap.pl ($type):[$!}" );

    open( GZPP, "gzip -c $filename.pp |" )
      or $self->mailUserAndFail( 
      "Failed to gzip $filename.pp:[$!]" );
    my $pp = join( "", <GZPP> );

    $self->pfamdb->getSchema->resultset('AlignmentAndTree')->update_or_create(
      {
        pfama_acc => $self->pfam->pfama_acc,
        type       => $type,
        post       => $pp
      },
      { key => 'UQ_alignments_and_trees_1' }
    );

  }

  $self->pfamdb->getSchema->resultset('AlignmentAndTree')->update_or_create(
    {
      pfama_acc => $self->pfam->pfama_acc,
      type       => $type,
      jtml       => $align
    },
    { key => 'UQ_alignments_and_trees_1' }
  );
  $self->logger->debug("Finished making $type HTML alignment");
}

sub makePPAlign {
  my ( $self, $filename, $block, $type ) = @_;
  $self->logger->debug("Making PP aligment for $type $filename");

  if ( ( $type ne 'seed' ) ) {

    #Make the posterior probablility alignment.
    system("heatMap.pl -a $filename -b $block > $filename.pp")
      and $self->mailUserAndFail( 
      "Failed to run heatMap.pl ($type):[$!}" );

    open( GZPP, "gzip -c $filename.pp |" )
      or $self->mailUserAndFail( 
      "Failed to gzip $filename.pp:[$!]" );
    my $pp = join( "", <GZPP> );

    $self->pfamdb->getSchema->resultset('AlignmentAndTree')->update_or_create(
      {
        pfama_acc => $self->pfam->pfama_acc,
        type       => $type,
        post       => $pp
      },
      { key => 'UQ_alignments_and_trees_1' }
    );

  }
  $self->logger->debug("Finished making $type PP alignment");
}

sub uploadTreesAndAlign {
  my ( $self, $filename, $type ) = @_;

  $self->logger->debug("Uploading $filename, $type");


  unless ( ( $type eq 'seed' )
    or ( $type eq 'full' )
    or ( $type eq 'rp15' )
    or ( $type eq 'rp35' )
    or ( $type eq 'rp55' )
    or ( $type eq 'rp75' )
    or ( $type eq 'ncbi' )
    or ( $type eq 'meta' ) )
  {
    $self->mailUserAndFail( 
"Incorrect type ($type) passed to uploadTreesAndAlign. Expected 'full', 'seed', 'meta', 'ncbi' or 'rp15-75'"
    );
  }

  $self->logger->debug("Uploading $type trees and alignments");

  #Do this is steps.  Two reasons, better error tracking and more memory efficient
  my $row = $self->pfamdb->getSchema->resultset('AlignmentAndTree')->find_or_create(
    {
      pfama_acc => $self->pfam->pfama_acc,
      type       => $type
    },
    { key => 'UQ_alignments_and_trees_1' }
  );

  my $file;
  open( ANN, "gzip -c $filename.ann|" )
    or $self->mailUserAndFail("Failed to run gzip -c $filename.ann:[$!]" );
  while (<ANN>) {
    $file .= $_;
  }
  close(ANN);
   $self->logger->debug("Length of gzip $filename.ann is:".length($file));
  $row->update( { alignment => $file } );

  if($type eq 'seed'){
    $file = '';
    open( TREE, "gzip -c $filename.tree|" )
      or $self->mailUserAndFail( 
      "Failed to run gzip -c $filename.tree:[$!]" );
    while (<TREE>) {
      $file .= $_;
    }
    close(TREE);
    $row->update( { tree => $file } );
  }
  
  
  if(-e "$filename.html"){
  $file = '';
  open( HTML, "gzip -c $filename.html|" )
    or $self->mailUserAndFail( "Failed to run gzip -c $filename.tree:[$!]" );
  while (<HTML>) {
    $file .= $_;
  }
  close(HTML);
  $row->update( { jtml => $file } );
  }
}

sub _cleanBuildLine {
  my ($self, $buildline) = @_;

  my @Opts = split( /\s+/, $buildline );
  $buildline =
    join( ' ', @Opts[ 0 .. ( $#Opts - 2 ) ] )
    ;    #Removes the exisitng HMM_ls SEED from the end of the line
  $buildline =~ s/-fF/-F -f/g;
  $buildline =~ s/-Ff/-F -f/g;
  $buildline =~ s/--cpu 1//;
  $buildline =~ s/-o \/dev\/null//;

  return ($buildline);
}

sub versionFiles {
  my ( $self ) = @_;

  my %fileCheckSums;
  foreach my $f (qw(SEED ALIGN)) {
    open( F, $f )
      or $self->mailUserAndFail( $self->job,
      "Could not version $f:[$!]" );
    $fileCheckSums{$f} = md5_hex( join( "", <F> ) );
  }

#Add the thresholds into the HMMs!  This is what really determines the version of the family
  foreach my $f (qw(HMM)) {
    open( F, $f )
      or $self->mailUserAndFail( $self->job,
      "Could not version $f:[$!]" );
    my $hmm;
    while (<F>) {
      $hmm .= $_;
      unless (/^CKSUM\s+/) {
        $hmm .= "GA    " . $self->pfam->sequence_ga . " " . $self->pfam->domain_ga . ";\n";
        $hmm .= "TC    " . $self->pfam->sequence_tc . " " . $self->pfam->domain_tc . ";\n";
        $hmm .= "NC    " . $self->pfam->sequence_nc . " " . $self->pfam->domain_nc . ";\n";
      }
    }
    $fileCheckSums{$f} = md5_hex($hmm);
  }

  #Update the current versions table with these versions
  my $currentVersions =
    $self->pfamdb->getSchema->resultset('CurrentPfamVersion')->update_or_create(
    {
      pfama_acc => $self->pfam->pfama_acc,
      seed       => $fileCheckSums{SEED},
      align      => $fileCheckSums{ALIGN},
      desc_file  => $fileCheckSums{DESC} ? $fileCheckSums{DESC} : '',
      hmm        => $fileCheckSums{HMM},
    }
    );

  #Get the release versions
  my $releasedVersions =
    $self->pfamdb->getSchema->resultset('ReleasedPfamVersion')
    ->find( { pfama_acc => $self->pfam->pfama_acc } );

  my ( $thisVersion, $changeStatus );
  if ( $releasedVersions and $releasedVersions->pfama_acc ) {
    $changeStatus = 'NOCHANGE';

    #If the release version are different, then we need to add them to the
    if ( $releasedVersions->hmm ne $currentVersions->hmm ) {
      $thisVersion  = $releasedVersions->version + 1;
      $changeStatus = 'CHANGED';
    }
    else {
      if ( ( $releasedVersions->seed ne $currentVersions->seed )
        or ( $releasedVersions->align     ne $currentVersions->align )
        or ( $releasedVersions->desc_file ne $currentVersions->desc_file ) )
      {
        $changeStatus = 'CHANGED';
      }
    }
  }
  else {

    #looks like it is a new family
    $changeStatus = 'NEW';
    $thisVersion  = 1;
  }
  $self->pfam->update(
    {
      version       => $thisVersion,
      change_status => $changeStatus
    }
  );
}

sub getNestedLocations {
  my ( $self ) = @_;

  my @rows =
    $self->pfamdb->getSchema->resultset('NestedLocation')
    ->search( { pfama_acc => $self->pfam->pfama_acc } );
  my @nests;
  foreach my $r (@rows) {
    push( @nests, $r->pfamseq_acc . "/" . $r->seq_start . "-" . $r->seq_end );
  }
  return ( \@rows, \@nests );
}

sub getClanData {
  my ( $self ) = @_;

  my $row =
    $self->pfamdb->getSchema->resultset('Clan')
    ->find( { 'clan_memberships.pfama_acc' => $self->pfam->pfama_acc },
    { join => [qw( clan_memberships )] } );

  if ( $row and $row->clan_acc ) {
    return ( $row->clan_acc );
  }
}

sub write_stockholm_file {
  my ( $self, $filename, $aln, $GFAnn) = @_;

  my $pfam = $self->pfam;
  open( ANNFILE, ">$filename.ann" )
    or $self->mailUserAndExit("Could not open $filename.ann for writing [$!]");
  $self->writeGFAnnotationBlock(\*ANNFILE, $pfam, $GFAnn);
  
  if(ref($aln) eq 'HASH'){
    if(exists($aln->{GS})){
      print ANNFILE "#=GF SQ   ", scalar( @{$aln->{GS}} ), "\n";
      foreach my $line (@{ $aln->{GS} }){
        print ANNFILE $line;
      }
    }
    if(exists($aln->{GR})){
      foreach my $line ( @{ $aln->{GR} } ) {
        print ANNFILE $line;
      }
    }
    if(exists($aln->{GC})){
      foreach my $line ( @{$aln->{GC}} ) {
        print ANNFILE $line;
      }
    }
    print ANNFILE "//\n";
    close(ANNFILE);
  }else{
    print ANNFILE "#=GF SQ   ", scalar( $aln->num_sequences() ), "\n";
    my $stock = $aln->write_stockholm;
    if ( $$stock[0] =~ /^\# STOCKHOLM/ ) {
      shift(@$stock);    #This removes the STOCKHOLM 1.0 tag, but nasty, but hey!
    }
    foreach my $line ( @{$stock} ) {
      print ANNFILE $line;
    }
    close(ANNFILE);
    $self->checkflat( $filename . ".ann"); 
  }
  
}

#Designed to read in a HMMER3 stockholm alignment and write out a Pfam annotated
#alignment.
sub writeAnnotateAlignment {
  my ($self, $filename, $GFAnn) = @_;
  
  my $alignData = {};
  
  open(F, '<', $filename) or $self->logger->logdie("Could not open $filename:[$!]");
  while(<F>){
    if(/^STOCKHOLM 1.0/){
      next;
    }elsif(/^\/\/$/){
      next;
    }elsif(/^#=GS/){
      push(@{ $alignData->{GS} }, $_);
    }elsif(/^#=GR.*PP\s+\S+$/){
      next;
    }elsif(/^$/){
      next;
    }elsif(/^#=GC/){
      push(@{ $alignData->{GC} }, $_);
    }else{
      #This contains all alignment lines and GR stockholm lines.
      push(@{ $alignData->{GR} }, $_);
    }
  }

  $self->write_stockholm_file($filename, $alignData, $GFAnn);
  #$filename .= ".ann";
  return($filename, scalar( @{$alignData->{GS}} ));
}


sub writeGFAnnotationBlock {
  my ($self, $annfile, $pfam, $GFAnn) = @_;
  
  print $annfile "# STOCKHOLM 1.0\n";

  #Mimic this with what is loaded in the database
  #$en->write_stockholm_ann(\*$annfile);
  print $annfile "#=GF ID   ", $pfam->pfama_id,    "\n";
  print $annfile "#=GF AC   ", $pfam->pfama_acc,   ".", $pfam->version, "\n";
  print $annfile "#=GF DE   ", $pfam->description, "\n";
  if ( $pfam->previous_id and $pfam->previous_id =~ /\S+/ ) {
    print $annfile "#=GF PI   ", $pfam->previous_id, "\n";
  }
  print $annfile "#=GF AU   ", $pfam->author,      "\n";
  print $annfile "#=GF SE   ", $pfam->seed_source, "\n";

  #Put in the ga, nc, tc and also add the build and search method lines.
  print $annfile "#=GF GA   " . sprintf "%.2f %.2f;\n", $pfam->sequence_ga,
    $pfam->domain_ga;
  print $annfile "#=GF TC   " . sprintf "%.2f %.2f;\n", $pfam->sequence_tc,
    $pfam->domain_tc;
  print $annfile "#=GF NC   " . sprintf "%.2f %.2f;\n", $pfam->sequence_nc,
    $pfam->domain_nc;
  print $annfile "#=GF BM   ", $self->_cleanBuildLine( $pfam->buildmethod ),
    "HMM.ann SEED.ann\n";
  print $annfile "#=GF SM   ", $pfam->searchmethod, "\n";

  print $annfile "#=GF TP   ", $pfam->type, "\n";

  #If we find some wikipedia lines, print them out.
  if ( exists( $GFAnn->{wiki} ) and scalar( @{ $GFAnn->{wiki} } ) ) {
    my $wikiString;
    foreach my $w ( @{ $GFAnn->{wiki} } ) {
      print $annfile wrap( "#=GF WK   ", "#=GF WK   ", $w->auto_wiki->title );
      print $annfile "\n";
    }
  }

  #Add Nested domains if they are present
  if ( $GFAnn->{nestL}
    and scalar( @{ $GFAnn->{nestL} } ) )
  {
    foreach my $n ( @{ $GFAnn->{nestL} } ) {
      print $annfile "#=GF NE   ", $n->nested_pfama_acc, ";\n";
      print $annfile "#=GF NL   ", $n->pfamseq_acc;
      if ( $n->seq_version and $n->seq_version =~ /\d+/ ) {
        print $annfile "." . $n->seq_version;
      }
      print $annfile "/" . $n->seq_start . "-" . $n->seq_end . "\n";
    }
  }

  #Add the reference
  foreach my $ref ( @{ $GFAnn->{lrefs} } ) {
    if ( $ref->auto_lit->pmid ) {
      if ( ( $ref->comment ) && ( $ref->comment ne "NULL" ) ) {
        print $annfile wrap( "#=GF RC   ", "#=GF RC   ", $ref->comment );
        print $annfile "\n";
      }
      print $annfile "#=GF RN   [" . $ref->order_added . "]\n";
      print $annfile "#=GF RM   " . $ref->auto_lit->pmid . "\n";
      print $annfile wrap( "#=GF RT   ", "#=GF RT   ", $ref->auto_lit->title );
      print $annfile "\n";
      print $annfile wrap( "#=GF RA   ", "#=GF RA   ", $ref->auto_lit->author );
      print $annfile "\n";
      print $annfile "#=GF RL   " . $ref->auto_lit->journal . "\n";
    }
  }

  print $annfile "#=GF DR   INTERPRO; ", $GFAnn->{interpro}->interpro_id, ";\n"
    if ( $GFAnn->{interpro} and $GFAnn->{interpro}->interpro_id =~ /\S+/ );

  foreach my $xref ( @{ $GFAnn->{xrefs} } ) {

#Construct the DR lines.  Most do not have additional paramters. In the database
#the other_params has a trailing ";" that should ideally not be there.  Otherwise
#one could simply use join!

    if ( $xref->other_params and $xref->other_params =~ /\S+/ ) {
      print $annfile "#=GF DR   "
        . $xref->db_id . "; "
        . $xref->db_link . "; "
        . $xref->other_params . ";\n";
    }
    else {
      print $annfile "#=GF DR   " . $xref->db_id . "; " . $xref->db_link . ";\n";
    }

    #Print out any comment
    if ( $xref->comment and $xref->comment =~ /\S+/ ) {
      print $annfile "#=GF DC   " . $xref->comment . "\n";
    }
  }

  #Annotation comments
  #Currently, the text wrap is handling this!
  if ( $pfam->comment and $pfam->comment =~ /\S+/ ) {
    print $annfile wrap( "#=GF CC   ", "#=GF CC   ", $pfam->comment );
    print $annfile "\n";
  }
  
}
sub consensus_line {
  my ( $self, $filename, $ali_length ) = @_;

  my $c = Bio::Pfam::ViewProcess::Consensus->new( { alignfile => $filename });
  $c->pfamStyle;
  #Not fetch the pfam Consensus
  my $consensus = $c->pfamStyleConsensus; 
  
  #Check that the consensus line is the correct length.
  if ( $ali_length != length($consensus) ) {
    $self->mailUserAndFail( 
          "Error with consensus line. Got "
        . ( length($consensus) )
        . "but expected"
        . $ali_length );
  }

  return $consensus;
}

sub make_tree {

  my ( $self, $filename, $regs, $pfamseq ) = @_;

  #Double check this still works
  open( TREE, "esl-reformat --informat selex afa $filename | FastTree -fastest -nj -boot 100 |" )
    or $self->mailUserAndFail( 
    "Could not open pipe on sreformat and FastTree -nj -boot 100 $filename\n" );

  open( TREEFILE, ">$filename.tree" )
    or $self->mailUserAndFail(
    "Failed to open $filename.tree:[$!]" );

  my $line = <TREE>;
  close TREE;
  my @tree = split( /,/, $line );

#Exchange the treefile accessions for ids (not for ncbi or metaseq), and set the tree order on the database object
  my $order = 1;
  foreach my $acc (@tree) {
    my ( $before1, $before2, $nm, $st, $en, $after );
    if ( $acc =~ m/(.+)?([\,\(])(\S+)\/(\d+)-(\d+)(.+)/g ) {
      ( $before1, $before2, $nm, $st, $en, $after ) =
        ( $1, $2, $3, $4, $5, $6 );
    }
    elsif ( $acc =~ m/([\,\(])?(\S+)\/(\d+)-(\d+)(.+)/g ) {
      ( $before1, $nm, $st, $en, $after ) = ( $1, $2, $3, $4, $5 );
    }

    $before1 = "" unless ($before1);
    $before2 = "" unless ($before2);
    $after   = "" unless ($after);

    if ($pfamseq) {
      if ( $acc =~ /\;/ ) {
        print TREEFILE $before1 . $before2
          . $regs->{"$nm/$st-$en"}->{pfamseq_id} . "/"
          . $st . "-"
          . $en
          . $after;
      }
      else {
        print TREEFILE $before1 . $before2
          . $regs->{"$nm/$st-$en"}->{pfamseq_id} . "/"
          . $st . "-"
          . $en
          . $after . ",";
      }

    }
    else {
      if ( $acc =~ /\;/ ) {
        print TREEFILE "$acc";
      }
      else {
        print TREEFILE "$acc" . ",";
      }
    }

    if ( $regs->{"$nm/$st-$en"} ) {
      $regs->{"$nm/$st-$en"}->{dom}->tree_order( $order++ );
    }
    else {
      $self->logger->warn("key [$nm/$st-$en] is not in the hash");
    }
  }
  close TREEFILE;
}

sub makeSpeciesJsonString {
  my ( $self ) = @_;

  my $json = JSON->new;
  my $dbh  = $self->pfamdb->getSchema->storage->dbh;

  #List of seed sequences
  my $seedSth =
    $dbh->prepare(
"select s.pfamseq_acc from pfamA_reg_seed r, pfamseq s where s.pfamseq_acc=r.pfamseq_acc and pfamA_acc= ?"
    );
  my $fullSth =
    $dbh->prepare(
"select t.species, parent, minimal, rank, s.pfamseq_acc, t.ncbi_taxid, count(s.pfamseq_acc) from pfamA_reg_full_significant r, pfamseq s left join taxonomy t on t.ncbi_taxid=s.ncbi_taxid where s.pfamseq_acc=r.pfamseq_acc and pfamA_acc= ? and in_full =1 and t.species!=\'NULL\' group by s.pfamseq_acc"
    );
  my $taxFSth =
    $dbh->prepare(
    "select parent, minimal, level, rank from taxonomy where ncbi_taxid=?");
  my $unclassSth =
    $dbh->prepare(
q[ SELECT s.species, s.taxonomy, s.pfamseq_acc, COUNT(s.pfamseq_acc), s.ncbi_taxid FROM pfamA_reg_full_significant r, pfamseq s LEFT JOIN taxonomy t ON t.ncbi_taxid = s.ncbi_taxid WHERE s.pfamseq_acc = r.pfamseq_acc AND pfamA_acc = ? AND in_full=1 AND t.ncbi_taxid IS null GROUP BY s.pfamseq_acc ]
    );

#-------------------------------------------------------------------------------

  my @expectedLevels =
    qw(superkingdom kingdom phylum class order family genus species);
  my %superkingdom_members = (
    'Bacteria'              => 'bacterium',
    'Eukaryota'             => 'eukaryote',
    'Archea'                => 'archea',
    'Viruses'               => 'virus',
    'Viroids'               => 'viroid',
    'Other sequences'       => 'sequence',
    'Unclassified'          => 'sequence',
    'Unclassified sequence' => 'sequence',
  );

  $seedSth->execute($self->pfam->pfama_acc);
  my %seedSeqs;
  foreach my $rRef ( @{ $seedSth->fetchall_arrayref } ) {

    #print $rRef->[0]."\n";
    $seedSeqs{ $rRef->[0] }++;
  }
  my @tree;
  my %seenTaxIds;
  my $unique  = 1;
  my $counter = 1;

  #----------------------------------------

  # build the list of unclassified levels

  $unclassSth->execute($self->pfam->pfama_acc);
  foreach my $rRef ( @{ $unclassSth->fetchall_arrayref } ) {
    my $thisBranch;
    $thisBranch->{sequence} = {
      seqAcc     => $rRef->[2],
      seedSeq    => ( defined( $seedSeqs{ $rRef->[2] } ) ? 1 : 0 ),
      numDomains => $rRef->[3]
    };

    $thisBranch->{species} = {
      node  => $rRef->[0],
      taxid => 0
    };

    my $speciesCounter = 0;

    #We do not have a useful taxid, use species name
    unless ( $seenTaxIds{ $rRef->[0] } ) {
      $speciesCounter++;
    }
    $seenTaxIds{ $rRef->[0] }++;

    my ($skName) = $rRef->[1] =~ /^(.*?)\; /;
    my $sk_member = $superkingdom_members{$skName} || 'entity';
    for ( my $i = 0 ; $i < $#expectedLevels ; $i++ ) {

# $thisBranch->{$expectedLevels[$i]}->{node} = $i > 0 ? 'Unclassified '.$expectedLevels[$i] : $skName;
      $thisBranch->{ $expectedLevels[$i] }->{node} =
        $i > 0 ? "Uncategorised $sk_member" : $skName;
    }

    my $previousNode;
    for ( my $i = 0 ; $i <= $#expectedLevels ; $i++ ) {
      my $node;
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if (  $c->{node}
            and $c->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $expectedLevels[$i] }->{node} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {

        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if (  $skNode->{node}
            and $skNode->{node} eq
            $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $skNode;
          }
        }

        unless ($node) {
          $node = {
            node   => $thisBranch->{ $expectedLevels[$i] }->{node},
            parent => 'root'
          };
          push( @tree, $node );
        }
      }
      $node->{id} = $unique++;
      $node->{numSequences}++;
      $node->{numSpecies} += $speciesCounter;
      $node->{numDomains} += $rRef->[3];

      $previousNode = $node;
    }
    push( @{ $previousNode->{sequences} }, $thisBranch->{sequence} );
  }

  #----------------------------------------

  # build the full tree

  $fullSth->execute($self->pfam->pfama_acc);
  foreach my $rRef ( @{ $fullSth->fetchall_arrayref } ) {

    my $thisBranch;
    $thisBranch->{sequence} = {
      seqAcc     => $rRef->[4],
      seedSeq    => ( defined( $seedSeqs{ $rRef->[4] } ) ? 1 : 0 ),
      numDomains => $rRef->[6]
    };

    $thisBranch->{ $rRef->[3] } = {
      node  => $rRef->[0],
      taxid => $rRef->[5]
    };

    my $speciesCounter = 0;
    unless ( $seenTaxIds{ $rRef->[5] } ) {
      $speciesCounter = 1;
    }
    $seenTaxIds{ $rRef->[5] }++;

    my $atRoot = 0;

    #print "Looking up ".$rRef->[1]."\n";

    $taxFSth->execute( $rRef->[1] );
    my $rHashRef = $taxFSth->fetchrow_hashref;

    until ($atRoot) {
      $atRoot = 1 if ( $rHashRef->{parent} and $rHashRef->{parent} == 1 );
      if ( $rHashRef->{minimal} and $rHashRef->{minimal} == 1 ) {

        #Harvest the information that we want!
        $thisBranch->{ $rHashRef->{rank} } = { node => $rHashRef->{level} };
      }
      $taxFSth->execute( $rHashRef->{parent} );
      $rHashRef = $taxFSth->fetchrow_hashref;

    }

    my $previousNode;
    for ( my $i = 0 ; $i <= $#expectedLevels ; $i++ ) {
      my $node;

#Become the same name as the parent if this taxonomic level is unknown. Everything should have a superkingdom.
      unless ( $thisBranch->{ $expectedLevels[$i] } ) {
        die "Trying to assign a name to a superkingdom" unless ( $i > 0 );
        die "Trying to assign "
          . $expectedLevels[$i]
          . " name based on "
          . $expectedLevels[ $i - 1 ]
          . " level\n"
          unless ( $thisBranch->{ $expectedLevels[ $i - 1 ] } );
        $thisBranch->{ $expectedLevels[$i] }->{node} =
          '(No ' . $expectedLevels[$i] . ')';

        # $thisBranch->{ $expectedLevels[ $i - 1 ] }->{node};
      }
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if (  $c->{node}
            and $c->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $expectedLevels[$i] }->{node} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {

        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if (  $skNode->{node}
            and $skNode->{node} eq
            $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $skNode;
          }
        }

        unless ($node) {
          $node = {
            node   => $thisBranch->{ $expectedLevels[$i] }->{node},
            parent => 'root'
          };
          push( @tree, $node );
        }
      }
      $node->{id} = $unique++;
      $node->{numSequences}++;
      $node->{numSpecies} += $speciesCounter;
      $node->{numDomains} += $rRef->[6];

      $previousNode = $node;
    }
    push( @{ $previousNode->{sequences} }, $thisBranch->{sequence} );
  }

  # stats...
  my ( $totalSequence, $totalSpecies, $totalDomains );
  foreach my $skNode (@tree) {
    $totalSequence += $skNode->{numSequences};
    $totalSpecies  += $skNode->{numSpecies};
    $totalDomains  += $skNode->{numDomains};
  }

  # make a root for the tree
  my $rootedTree = {
    id           => 0,
    node         => 'root',
    numSequences => $totalSequence,
    numSpecies   => $totalSpecies,
    numDomains   => $totalDomains,
    children     => \@tree
  };

  my $json_string = $json->encode($rootedTree);

  $self->pfamdb->getSchema->resultset('PfamASpeciesTree')->update_or_create(
    {
      pfama_acc  => $self->pfam->pfama_acc,
      json_string => $json_string
    }
  );
}

sub processOptions {
  my ( $self ) = @_;
  my ( $famId, $uid, $help, $tree, $noclean );
  my $options = {};
  #% identity used to threshold families for fasta file generation
  my $identity = 90;
  my @opts = @ARGV;
  #Get and check the input parameters
  GetOptionsFromArray(
    \@opts,
    "family=s" => \$famId,
    "id=s"     => \$uid,
    "fasta=s"  => \$identity,
    "noclean"  => \$noclean,
    "h"        => \$help
  );

  if ($help) {
    $self->options($options);
    return;
  }

  if ( !$uid ) {
    if ($famId) {
      $self->mailPfam(
        "Failed to run view process for $famId",
        "No job id passed to $0"
        ),
        ;
    }
    else {
      $self->mailPfam(
        "Failed to run view process for a family",
        "No job id or family id passed to $0"
        ),
        ;
    }
    help();
    $self->logger->logdie("FATAL:No job id passed to script.....");
  }

  $options->{uid}      = $uid;
  $options->{identity} = $identity;
  $options->{famId}    = $famId;
  $options->{noclean}  = $noclean;
  $self->options($options);
}

sub getJob {
  my ( $self, $uuid, $entityId ) = @_;


  if(!$uuid and exists($self->options->{uid})){
    $uuid = $self->options->{uid}  
  }

  unless ( $self->options->{famId} ) {

#If we do not have a family identifier, then we can potentially recover if we have a jobId
    $self->logger->warn(
      "No Pfam family name supplied, will try and get based on the job ID");
  }

######################################################
  # This section deals with getting to job information #
######################################################

  #Can we get a PfamJobs Database
  #my $jobDB = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );

  #Can we find the record of this job
  my $job =
    $self->jobdb->getSchema->resultset('JobHistory')
    ->find( { 'job_id' => $self->options->{uid} } );

  unless ($job) {
      $self->mailPfam(
      "Failed to run view process for " . $self->options->{famId},
      "Could not get job information for" . $self->options->{uid}
    );
  }
  $self->logger->debug("Got job databse object");

  if ( $self->options->{famId} ) {
    
      if ( $self->options->{famId} ne $job->entity_id ) {
	  $self->mailPfam( "Failed to run view process for "
	  . $self->options->{famId}
          . "Miss-match between family id ("
          . $job->entity_id
     	  . " and the information contained in the database for "
     	  . $self->options->{uid} );
      } 
  }
  
  #Now add it to the object
  $self->job($job);
}

sub getPfamObj {
  my ( $self ) = @_;

  # Get the information about the family
  my $pfam =
    $self->pfamdb->getSchema->resultset('PfamA')
    ->find( { pfama_acc => $self->job->entity_acc } );

  unless ( $pfam and $pfam->pfama_acc ) {
    $self->mailUserAndFail("Failed to get the pfam entry for family" );
  }
  
  $self->logger->debug("Got pfam family databse object");
  
  $self->pfam($pfam);
 
}

sub resetStats {
  my ($self) = @_;
    $self->logger->debug("Resetting stats.");
  
  $self->pfam->update( { number_archs => '0',
                         number_species => '0',
                         number_structures => '0',
                         number_ncbi => '0',
                         number_meta => '0',
                         average_length => '0',
                         percentage_id => '0',
                         average_coverage => '0',
                         seed_consensus => '',
                         full_consensus => '',
                         number_shuffled_hits => '0',
                         number_rp15 => '0',
                         number_rp35 => '0',
                         number_rp55 => '0',
                         number_rp75 => '0',
			 number_ref_proteome => '0',
  } );  
  
}

##Now get the alignments and HMMs out of the database
sub getAllFiles {
  my ( $self ) = @_;

  my $row =
    $self->pfamdb->getSchema->resultset('PfamAInternal')
    ->find( { pfama_acc => $self->pfam->pfama_acc } );

  eval{
    $row->msas_uncompressed;
  };
  if ($@) {
    $self->mailUserAndFail( "Failed to uncompress all files: $@" );
  }
  my $hmm =
    $self->pfamdb->getSchema->resultset('PfamAHmm')
    ->find( { pfama_acc => $self->pfam->pfama_acc } );

  open( H, ">HMM" )
    or $self->mailUserAndFail( "Could not open HMM:[$!]\n" );
  print H $hmm->hmm;
  close(H);

#Check that all of the files are present in the directory, this script assumes that all of the files
#are in the cwd.
  foreach my $f (qw(ALIGN SEED HMM)) {
    unless ( -e $f and -s $f ) {
      $self->mailUserAndFail(
        "View process failed as $f was not present\n" );
    }
  }
  $self->logger->debug("All family files are present");
}


sub cleanUp {
  my ($self) = @_;
  
  if($self->options->{noclean}){
    $self->logger->info("Skipping clean up.");
    return;  
  }
  $self->logger->debug("cleaning up");
  my @files = glob("ALIGN* SEED* HMM* family.fa*  hmmLogo.png*");

  foreach my $f (@files){
    unlink($f) or $self->mailUserAndFail("Could not remove file, $f");  
  }
    
}

sub getGFAnnotations {
  my ( $self ) = @_;

  my %annotations;

  #Get database Xrefs for entries
  my @xRefs =
    $self->pfamdb->getSchema->resultset('PfamADatabaseLink')
    ->search( { pfama_acc => $self->pfam->pfama_acc } );
  $self->logger->debug( "Got " . scalar(@xRefs) . " database cross-references" );

  #Get database literature references
  my @litRefs =
    $self->pfamdb->getSchema->resultset('PfamALiteratureReference')->search(
    { pfama_acc => $self->pfam->pfama_acc },
    {
      join     => [qw(auto_lit)],
      order_by => 'order_added ASC',
      prefetch => [qw(auto_lit)]
    }
    );
  $self->logger->debug( "Got " . scalar(@litRefs) . " literature references" );

  #Nestest location information
  my ( $nested_locations, $nested_anchors ) =
    $self->getNestedLocations;

  #Get the clan information
  my $clan = $self->getClanData;

  #Need to put WK lines in here!
  my @wiki = $self->pfamdb->getSchema->resultset('PfamAWiki')->search(
    { pfama_acc => $self->pfam->pfama_acc },
    {
      join     => qw(auto_wiki),
      prefetch => qw(auto_wiki)
    }
  );

  #DB Xrefs
  #Add this special case of database cross reference
  my @interpro =
    $self->pfamdb->getSchema->resultset('Interpro')
    ->search( { pfama_acc => $self->pfam->pfama_acc } );

  $annotations{xrefs}    = \@xRefs;
  $annotations{lrefs}    = \@litRefs;
  $annotations{nestL}    = $nested_locations;
  $annotations{nestA}    = $nested_anchors;
  $annotations{clan}     = $clan;
  $annotations{wiki}     = \@wiki;
  $annotations{interpro} = $interpro[0];

  return( \%annotations);
}

sub searchShuffled {
  my ( $self) = @_;
  $self->logger->debug("Running against shuffled database");
  my $shuffled = $self->config->shuffledLoc . '/shuffled';
  system(
  $self->config->hmmer3bin."/hmmsearch --cpu 0 --noali --domtblout shuffled.hits --cut_ga HMM.ann $shuffled > /dev/null"
    )
    and $self->mailUserAndFail( "Failed to run hmmsearch:[$!]\n");
    
  open( H, "shuffled.hits" ) or $self->mailUserAndFail( "Could not open shuffled.hits:[$!]");

  my $noHits = 0;
  while (<H>) {
    next if (/^#/);
    $noHits++;
  }
  close(H);
  unlink('shuffled.hits');
  $self->pfam->update( { number_shuffled_hits => $noHits } );
}

sub statusCheck {
  my($self, $file, $count) = @_;
  
  if(!defined($count)){
    if(-e $self->options->{statusdir}."/".$file){
      return 1;
    }else{
      return 0;
    }
  }else{
    $self->logger->debug("Globing for ".$self->options->{statusdir}.'/'.$file);
    my @files = glob($self->options->{statusdir}.'/'.$file.'.*');
    my $fCount = 0;
    my $pat = $self->options->{statusdir}.'/'.$file.'.';
    foreach my $f (@files){
      
      $fCount++ if($f =~ /$pat\d+\.done/); 
    }
    if($fCount == $count){
      return 1;
    }else{
      return 0;
    }
  }
}

sub touchStatus {
  my ($self, $file) = @_;
  my @files;
  push(@files, $self->options->{statusdir}."/".$file);
  touch(@files);
}

=head1 AUTHOR

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
