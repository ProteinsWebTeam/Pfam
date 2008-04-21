package Bio::Pfam::ViewProcess;

use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::PfamRCS;

use Data::UUID;

use strict;
use warnings;

sub intiateViewProcess {
  my($en, $name) = @_;
  
  unless( $en->isa("Bio::Pfam::EntryA_RCS") ){
    die( "Expected a Bio::Pfam::EntryA_RCS object" );
  }
  
  unless($en->acc and $en->id){
    die( "Entry accession or ID is not defined, therefore cannot add to the jobs database!");   
  }
  
  unless( $name ){
    die( "User not defined!"); 
  }
  
  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();
  my $db = Bio::Pfam::PfamJobsDBManager->new('host', 'pfamdb2a', 'user', 'pfam', 'port', '3303', 'password', 'mafp1');  
  
  unless ( $db ){
    die ("Could not get a connection to the PfamJob database!");
  }
  

  #Add the job to the pfam_jobs database.
  #Need to steal the transactions part from John.
  my $transaction = sub {
    my $r = $db->getSchema
              ->resultset('JobHistory')
                ->create({ status     => 'PEND',
                                          job_id     => $uid,
                                          family_acc => $en->acc,
                                          family_id  => $en->id,
                                          job_type   => 'view',
                                          options    => '',
                                          opened     => \'NOW()',
                                          user_id    => $name });
                                          
    my $s = $db->getSchema
              ->resultset('JobStream')
                ->create( { id => $r->id });  
  };  
  
  #Lets tran 
  eval {
    $db->getSchema
        ->schema
          ->txn_do( $transaction );
  };
  if ( $@ ) {
    die "Failed during job submission transaction!: [$@]\n"; 
  }        
  
  print STDERR "Submitted job to make view files - job-id: $uid\n";
  #Now release the lock on the family!
  Bio::Pfam::PfamRCS::abort_lock_on_family( $en->id );  
}

1;