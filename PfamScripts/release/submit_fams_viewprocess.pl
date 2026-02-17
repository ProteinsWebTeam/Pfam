#!/usr/bin/env perl


use strict;
use warnings;
use Data::UUID;
use Cwd;
use Getopt::Long;
use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;




#Get clans
my ($single, $list, $auto);
GetOptions(
  'single=s' => \$single,
  'list=s' => \$list,
  'auto' => \$auto
);


my $config = Bio::Pfam::Config->new;
my $pfamJobsDB = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );


my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $release_dbh    = $pfamDB->getSchema->storage->dbh;

my $db = $pfamDB->{database};
print "Working on database $db\n";

my $pwd = getcwd;
print "Working on directory $pwd\n";






my @fam_list;

if ($single) {
    print "Got single family $single to process...\n";
    push @fam_list, $single;
} elsif ($list) {
    print "Got list file $list to process...\n";
    open (my $fam_list_fh, '<', $list);
    foreach my $fam_acc (<$fam_list_fh>) {
      chomp $fam_acc;
      push @fam_list, $fam_acc;
    }
} elsif ($auto) {
    print "Finding families to process...\n";
    my $release_sth = $release_dbh->prepare(
      "select pfamA_acc from pfamA where pfamA_acc not in (select pfamA_acc from alignment_and_tree where type = 'seed') order by pfamA_acc;" );
    $release_sth->execute;
    while (my @row = $release_sth->fetchrow_array) {
      push @fam_list, @row;
    }
} else {
    die "Usage: perl submit_fams_viewprocess.pl [-single FAM_ACC] [-list FAM_LIST_FILE] [-auto]\n";
}




my $cur_q = 0;
my $MAX_Q = 50;

foreach my $fam_acc (@fam_list) {

  my $fam_id = $pfamDB->acc2id($fam_acc);

  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();

  $cur_q++;
  $cur_q = 1 if ($cur_q > $MAX_Q);

  # Now clear out any existing jobs for that family.
  my @jobs = $pfamJobsDB->getSchema->resultset('JobHistory')->search({
        entity_acc => $fam_acc,
        -and       => [
          status => { '!=' => 'DONE' },
          status => { '!=' => 'KILL' }
        ],
  });

  if ( scalar(@jobs) ) {
      foreach my $job (@jobs) {
        if ( $job->lsf_id and $job->lsf_id =~ /\d+/ ) {

          #$self->killJob( $job->lsf_id );
        }
        $job->update({
            status => 'KILL',
            closed => \'NOW()'
        });
      }
  }


  # Get number of full sequences to assign job details
  my $release_sth = $release_dbh->prepare(
    "select num_full from pfamA where pfamA_acc = '$fam_acc' " );
  $release_sth->execute;
  my $num_seqs = $release_sth->fetchrow_array();

  my $mem = "2G";
  my $time = "1:00:00";

  if ($num_seqs > 800000) {
    $mem = "64G";
    $time = "1-00:00:00";
  } elsif ($num_seqs > 400000) {
    $mem = "32G";
    $time = "1-00:00:00";
  } elsif ($num_seqs > 250000) {
    $mem = "16G";
    $time = "12:00:00";
  } elsif ($num_seqs > 10000) {
    $mem = "10G";
    $time = "8:00:00";
  } elsif ($num_seqs > 5000) {
    $mem = "8G";
    $time = "4:00:00";
  } elsif ($num_seqs > 500) {
    $mem = "4G";
    $time = "2:00:00";
  }

  #Add the job to the pfam_jobs database.
  my $r = $pfamJobsDB->getSchema->resultset('JobHistory')->create(
    {
      status      => 'PEND',
      job_id      => $uid,
      entity_acc  => $fam_acc,
      entity_id   => $fam_id,
      job_type    => 'family',
      options     => '',
      opened      => \'NOW()',
      user_id     => $ENV{'USER'}
    }
  );

  my $s = $pfamJobsDB->getSchema->resultset('JobStream')->create(
    {
      id     => $r->id,
      stdin  => '',
      stdout => '',
      stderr => ''
    }
  );

  system "sbatch --time=${time} --mem=${mem} -o \"${uid}log\" --job-name=famQ${cur_q} --dependency=singleton --wrap='mkdir -p ${pwd}/${uid}/${fam_id} && cd ${pwd}/${uid}/${fam_id} && makepfamview_pointrel.pl -id ${uid} -family ${fam_id} && rm -rf ${pwd}/${uid}'";

  print "Submitted job to make view files for family - job-id: ${uid} ${fam_acc} ${fam_id}\n";

}


