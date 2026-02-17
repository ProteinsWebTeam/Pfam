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






my @clan_list;

if ($single) {
    print "Got single clan $single to process...\n";
    push @clan_list, $single;
} elsif ($list) {
    print "Got list file $list to process...\n";
    open (my $clan_list_fh, '<', $list);
    foreach my $clan_acc (<$clan_list_fh>) {
      chomp $clan_acc;
      push @clan_list, $clan_acc;
    }
} elsif ($auto) {
    print "Finding clans to process...\n";
    my $release_sth = $release_dbh->prepare(
      'select c.clan_acc from clan c left join clan_alignment_and_relationship a on c.clan_acc = a.clan_acc where c.competed != 1 or a.stockholm is null' );
    $release_sth->execute;
    while (my @row = $release_sth->fetchrow_array) {
      push @clan_list, @row;
    }
} else {
    die "Usage: perl submit_clans_viewprocess.pl [-single CLAN_ACC] [-list CLAN_LIST_FILE] [-auto]\n";
}


my $cur_q = 0;
my $MAX_Q = 25;

foreach my $clan_acc (@clan_list) {

  my $clan_id = $pfamDB->clanAcc2Id($clan_acc);

  #Make a unique idea for the job!
  my $uid = Data::UUID->new()->create_str();

  $cur_q++;
  $cur_q = 1 if ($cur_q > $MAX_Q);

  # Now clear out any existing jobs for that family.
  my @jobs = $pfamJobsDB->getSchema->resultset('JobHistory')->search({
        entity_acc => $clan_acc,
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
    "select sum(p.num_full) from clan_membership cm join pfamA p on cm.pfamA_acc = p.pfamA_acc where cm.clan_acc = '$clan_acc' group by cm.clan_acc " );
  $release_sth->execute;
  my $num_seqs = $release_sth->fetchrow_array();

  my $mem = "1G";
  my $time = "3:00:00";

  if ($num_seqs > 3000000) {
    $mem = "32G";
    $time = "5-00:00:00";
  } elsif ($num_seqs > 1000000) {
    $mem = "16G";
    $time = "2-00:00:00";
  } elsif ($num_seqs > 100000) {
    $mem = "8G";
    $time = "1-00:00:00";
  } elsif ($num_seqs > 10000) {
    $mem = "4G";
    $time = "12:00:00";
  } elsif ($num_seqs > 1000) {
    $mem = "2G";
    $time = "6:00:00";
  }


  #Add the job to the pfam_jobs database.
  my $r = $pfamJobsDB->getSchema->resultset('JobHistory')->create({
      status      => 'PEND',
      job_id      => $uid,
      entity_acc  => $clan_acc,
      entity_id   => $clan_id,
      job_type    => 'clan',
      options     => '',
      opened      => \'NOW()',
      user_id     => $ENV{'USER'}
  });

  my $s = $pfamJobsDB->getSchema->resultset('JobStream')->create({
      id     => $r->id,
      stdin  => '',
      stdout => '',
      stderr => ''
  });

  system "sbatch --time=${time} --mem=${mem} -o \"${uid}log\" --job-name=clanQ${cur_q} --dependency=singleton --wrap='mkdir -p ${pwd}/${uid}/${clan_id} && cd ${pwd}/${uid}/${clan_id} && makeclanview_pointrel.pl -id ${uid} -clan ${clan_acc} && rm -rf ${pwd}/${uid}'";
  print "Submitted job to make view files for clan - job-id: ${uid} ${clan_acc} ${clan_id}\n";

}


