#!/usr/bin/env perl

#Script to compare the table definitions of a release db against pfamlive
#(although it can be used to compare tables in any 2 databases).
#It compares the md5 checksums of 'show create table' and reports
#how many tables have the same definition and gives a list of tables
#that have different definitions (if any)
#Need to pass in the release db config and the pfamlive config 
#on the command line

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

my ($release_config, $live_config);
GetOptions('release_config=s'  => \$release_config,
            'live_config=s'    => \$live_config);

unless(-s $release_config and -s $live_config) {
  die "Need to specify release config and pfamlive config on the command line\nEg $0 -release_config /nfs/production/xfam/xfam/rdf/pfam_29_0/config/pfam_svn.conf -live_config /nfs/production/xfam/pfam/software/Conf/pfam_svn.conf\n";
}


#Store existing config
my $existingConfig=$ENV{'PFAM_CONFIG'};


#Change to release db config
$ENV{'PFAM_CONFIG'}=$release_config;


#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Get list of tables in release db
my $sth=$dbh->prepare("show tables");
$sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
my ($t);
$sth->bind_columns(\$t);
my @tables;
while ($sth->fetch()) {
  push(@tables,$t);
}


#Get the md5 checksums for 'show create table' for all tables in the release db
my %releaseMD5;
my $releaseDB=tableMD5($release_config, \@tables, \%releaseMD5);


#Do the same for the live db
my %liveMD5;
my $liveDB=tableMD5($live_config, \@tables, \%liveMD5);


#Change back to original config
$ENV{'PFAM_CONFIG'}=$existingConfig;


#Compare md5 checksums for all tables
my $same=0;
my $different="";
my $total=@tables;
foreach my $table (@tables) {
  if($releaseMD5{$table} eq $liveMD5{$table}) {
    $same++;
  }
  else {
    $different .= "$table\n";
  }
}


#Print the results
print "$same/$total of the table defintions in $releaseDB are the same as those in $liveDB\n";
if($different) {
  print "\nThese tables have differences in the table definitions:\n$different\n";
}




sub tableMD5 {
  my ($config_location, $tables, $hash) = @_;
  $ENV{'PFAM_CONFIG'}=$config_location;
  my $config = Bio::Pfam::Config->new;
  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

  my $db_connect="mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database};

  foreach my $table (@$tables) { 
    #Get 'show create table' text, remove auto_increment, get md5 checksum
    open(MD5, "$db_connect -e \"SHOW CREATE TABLE $table\" | sed -r 's/AUTO_INCREMENT=[0-9]+/AUTO_INCREMENT=XXX/g' | md5sum |") or die "$!";
    while(<MD5>) {
      if(/^(\S+)/) {
        $hash->{$table}=$1;
        last;
      }
    }
    close MD5;
  }
  return($pfamDB->{database});
}
