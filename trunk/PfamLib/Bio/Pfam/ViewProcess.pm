package Bio::Pfam::ViewProcess;

use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::PfamRCS;

use Data::UUID;

use strict;
use warnings;

sub initiateViewProcess {
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
  
  my $size = '';
  
  unless($en->num_full){
     $size = $en->num_full;
  }
  #Add the job to the pfam_jobs database.
  #Need to steal the transactions part from John.
  my $transaction = sub {
    my $r = $db->getSchema
              ->resultset('JobHistory')
                ->create({ status     => 'PEND',
                                          job_id      => $uid,
                                          family_acc  => $en->acc,
                                          family_id   => $en->id,
                                          family_size => $size,
                                          job_type    => 'view',
                                          options     => '',
                                          opened      => \'NOW()',
                                          user_id     => $name });
                                          
    my $s = $db->getSchema
              ->resultset('JobStream')
                ->create( { id => $r->id });  
  };  
  
  #Lets tran 
  eval {
       $db->getSchema
           ->txn_do( $transaction );
  };
  if ( $@ ) {
    die "Failed during job submission transaction!: [$@]\n"; 
  }        
  
  print STDERR "Submitted job to make view files - job-id: $uid\n";
  #Now release the lock on the family!
  Bio::Pfam::PfamRCS::abort_lock_on_family( $en->id );  
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
