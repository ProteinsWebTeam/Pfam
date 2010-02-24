package Bio::Pfam::ViewProcess;

use strict;
use warnings;
use Data::UUID;
use Mail::Mailer;

use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::PfamLiveDBManager;

sub initiateViewProcess {
  my ( $famObj, $name, $config ) = @_;

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
    Bio::Pfam::ViewProcess::initiateClanViewProcess( $famObj->DESC->CL, $name,
      $config );
  }
  else {
    Bio::Pfam::ViewProcess::initiateFamilyViewProcess( $famObj, $name,
      $config );
  }
}

sub initiateClanViewProcess {
  my ( $clanAcc, $name, $config ) = @_;

  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();
  my $db  = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );
  unless ($db) {
    die("Could not get a connection to the PfamJob database!");
  }

  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

#Look for any existing clan jobs!
#-------------------------------------------------------------------------------
# Now clear out any existing jobs for that family.
  my @jobs = $db->getSchema->resultset('JobHistory')->search(
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
        Bio::Pfam::ViewProcess::killJob( $job->lsf_id );
      }
      $job->update(
        {
          status => 'KILL',
          closed => \'NOW()'
        }
      );
    }
  }

  my $clan      = $pfamDB->getClanData($clanAcc);
  my $clanMembs = $pfamDB->getClanMembership($clanAcc);

  foreach my $memb (@$clanMembs) {

    #Now look for any families belonging to that jobs.
    print "*** $memb ***\n";
    Bio::Pfam::ViewProcess::killFamilyJob( $memb->auto_pfama->pfama_acc, $db );
  }

  #Add the clan job to the pfam_jobs database.
  my $transaction = sub {
    my $r = $db->getSchema->resultset('JobHistory')->create(
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


    my $s = $db->getSchema->resultset('JobStream')->create( { id => $r->id,
                                                              stdin => '',
                                                              stdout => '',
                                                              stderr => ''} );


  };

  #Lets tran
  eval { $db->getSchema->txn_do($transaction); };
  if ($@) {
    die "Failed during 'clan view' job submission transaction!: [$@]\n";
  }

  print STDERR "Submitted job to make clan view files - job-id: $uid\n";

}

sub initiateFamilyViewProcess {
  my ( $famObj, $name, $config ) = @_;

  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();
  my $db  = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );

  unless ($db) {
    die("Could not get a connection to the PfamJob database!");
  }

#-------------------------------------------------------------------------------
# Now clear out any existing jobs for that family.
  Bio::Pfam::ViewProcess::killFamilyJob( $famObj->DESC->AC, $db );

  my $size = $famObj->ALIGN->no_sequences;

  #Add the job to the pfam_jobs database.
  my $transaction = sub {
    my $r = $db->getSchema->resultset('JobHistory')->create(
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

    my $s = $db->getSchema->resultset('JobStream')->create( { id => $r->id,
                                                              stdin => '',
                                                              stdout => '',
                                                              stderr => ''} );

  };

  #Lets tran
  eval { $db->getSchema->txn_do($transaction); };
  if ($@) {
    die "Failed during job submission transaction!: [$@]\n";
  }

  print STDERR "Submitted job to make view files - job-id: $uid\n";

  #Now release the lock on the family!
}

sub initiateAncillaryViewProcess {
  my ( $name, $config ) = @_;

  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();
  my $db  = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );

  unless ($db) {
    die("Could not get a connection to the PfamJob database!");
  }
  my $rs = $db->getSchema->resultset('JobHistory')->search(
    {
      job_type => 'ancillary',
      -and     => [
        status => { '!=' => 'DONE' },
        status => { '!=' => 'KILL' }
      ],
    }
  );
  unless ( $rs->count ) {
    #Add the ancillary job to the pfam_jobs database.
    my $transaction = sub {
      my $r = $db->getSchema->resultset('JobHistory')->create(
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


      my $s = $db->getSchema->resultset('JobStream')->create( { id => $r->id,
                                                              stdin => '',
                                                              stdout => '',
                                                              stderr => ''} );

    };

    #Lets tran
    eval { $db->getSchema->txn_do($transaction); };
    if ($@) {
      die "Failed during job submission transaction!: [$@]\n";
    }
  }
}

sub killFamilyJob {
  my ( $acc, $db ) = @_;
  my @jobs = $db->getSchema->resultset('JobHistory')->search(
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
        Bio::Pfam::ViewProcess::killJob( $job->lsf_id );
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

  my ($job_id) = @_;

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

sub mailPfam {
  my ( $title, $message, $nodie ) = @_;
  my %header = (
    To      => 'rdf@sanger.ac.uk',
    From    => 'rdf@sanger.ac.uk',
    Subject => $title
  );

  my $mailer = Mail::Mailer->new;
  $mailer->open( \%header );
  print $mailer $message;
  $mailer->close;
  exit(1) unless ($nodie);
}

sub mailUserAndFail {
  my ( $job, $message ) = @_;

  if ( $job->user_id ) {
    my %header = (
      To      => $job->user_id . '@sanger.ac.uk',
      Cc      => 'rdf@sanger.ac.uk',
      From    => 'rdf@sanger.ac.uk',
      Subject => 'Error in view process for ' . $job->entity_id
    );
    my $mailer = Mail::Mailer->new;
    $mailer->open( \%header );
    print $mailer $job->entity_id . "\n" . $message;
    $mailer->close;
  }
  else {
    mailPfam( "View process for " . $job->entity_acc . " failed",
      "No user found for the job" );
  }

  $job->update(
    {
      status => 'FAIL',
      closed => \'NOW()'
    }
  );
  exit(1);
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
