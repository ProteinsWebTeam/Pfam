#!/usr/bin/env perl

#This script merges two clans together
#It takes the members of one clan (clanA), and moves them to 
#another clan (clanB), then kills clanA.
#The script:
#   * update the CL line in clanA family DESC files to point to clanB
#   * removes all MB lines in the CLANDESC for clanA
#   * adds MB lines for families in clanA to the CLANDESC for clanB
#   * changes the clan_acc for families in clanA to clanB in the clan_membership table in the database
#   * checkout the CLANDESC file for clanA
#   * kills clanA via clkill


use strict;
use warnings;
use Cwd;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my ($clan_acc_kill, $clan_acc_merge, $help);
GetOptions(     'kill=s'  => \$clan_acc_kill,
		   'merge_into=s' => \$clan_acc_merge,
                   'help' => \$help);

#Check user options are given
if(!$clan_acc_kill or !$clan_acc_merge or $help) {
	help();
    exit;
} 


#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Check clan acccessions are in the database
foreach my $clan ($clan_acc_kill, $clan_acc_merge) {
	my $result =$pfamDB->getSchema->resultset('Clan')->find( {clan_acc => $clan} );
	unless($result) {
		die "The clan [$clan] is not a valid clan accession\n";
	}
}

#Make a new dir
my $dir_name = "$clan_acc_kill" . "_" . "$clan_acc_merge" . "_merge";
print STDERR "Making new directory $dir_name\n\n";
mkdir($dir_name, 0775) or die "Couldn't mkdir $dir_name, $!"; 
chdir($dir_name) or die "Couldn't chdir into $dir_name, $!";


#Get clan membership for clan to be killed
my @clan_to_kill=$pfamDB->getSchema->resultset('ClanMembership')->search(
    { clan_acc => $clan_acc_kill },
    { 
      join     => 'pfama_acc',
      order_by => 'pfama_acc.pfama_acc ASC',
      prefetch => 'pfama_acc'
    }
);
print STDERR "Going to move these families from $clan_acc_kill to $clan_acc_merge, and then kill $clan_acc_kill:\n";
my @clan_members;
foreach my $c (@clan_to_kill) {
    my $pfamA_acc = $c->pfama_acc->pfama_acc;
    my $pfamA_id = $c->pfama_acc->pfama_id;
    print STDERR "$pfamA_acc $pfamA_id\n";
    push(@clan_members, $pfamA_acc);
}


my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };   # don't allow control C for a bit!


#Checkout the families in clan $clan_acc_kill and change the CL line in these families DESC file to point to $clan_acc_merge
print STDERR "\nUpdating the CL line in the DESC file for families in $clan_acc_kill to point to $clan_acc_merge\n";
foreach my $pfamA_acc (@clan_members) {
    print STDERR "Updating CL line in $pfamA_acc/DESC\n";
    system("pfco $pfamA_acc") and die "Couldn't 'pfco $pfamA_acc', $!";
    system("sed -i -r 's/CL   \\w+/CL   $clan_acc_merge/' $pfamA_acc/DESC") and die "Couldn't 'sed -i -r 's/CL   \\w+/CL   $clan_acc_merge/' $pfamA_acc/DESC' replace CL in $pfamA_acc/DESC, $!";
    system("svn commit -m 'ADMINBYPASS: clmerge, changing CL line from $clan_acc_kill to $clan_acc_merge' $pfamA_acc/DESC")
}


#Checkout clan to be killed and remove MB lines
print STDERR "\nRemoving all MB lines from $clan_acc_kill CLANDESC\n";
system("clco $clan_acc_kill") and die "Couldn't run 'clco $clan_acc_kill', $!";
system("sed -i -r '/^MB/d' $clan_acc_kill/CLANDESC") and die "Couldn't run 'sed -i -r '/^MB/d' $clan_acc_kill/CLANDESC', $!";
system("svn commit -m 'ADMINBYPASS: clmerge, removing all MB lines from CLANDESC prior to killing the clan' $clan_acc_kill/CLANDESC");


#Checkout the other clan, and add MB lines
print STDERR "\nAdding MB lines for families that were in $clan_acc_kill to the CLANDESC for $clan_acc_merge\n";
system("clco $clan_acc_merge") and die "Couldn't run 'clco $clan_acc_merge', $!";
open(CLANDESC, ">>$clan_acc_merge/CLANDESC") or die "Couldn't open fh to $clan_acc_merge/CLANDESC, $!";
foreach my $pfamA_acc (@clan_members) {
    print CLANDESC "MB   $pfamA_acc;\n";
    print STDERR "$pfamA_acc\n";
}
close CLANDESC;
system("svn commit -m \"ADMINBYPASS: clmerge - added MB lines to CLANDESC for the following families that used to be in $clan_acc_kill: @clan_members\" $clan_acc_merge/CLANDESC");


#Update database
print STDERR "\nChanging clan_acc from $clan_acc_kill to $clan_acc_merge in clan_membership table in the database for families that used to be in $clan_acc_kill\n";
my $st = $dbh->prepare("update clan_membership set clan_acc='$clan_acc_merge' where clan_acc = '$clan_acc_kill'");
$st->execute() or die "Couldn't update clan_membership table in database, ". $st->errstr."\n";


#Kill the clan
print STDERR "\nKilling $clan_acc_kill\n";
system("clkill -m \"Merging with $clan_acc_merge\" -f $clan_acc_merge $clan_acc_kill");


if ($caught_cntrl_c) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** You should check that the script has done what you wanted (use pfinfo/clinfo to see if the CL/MB lines are correct, and check the clan_membership table in the database is correct" ;
}

chdir("../") or die "Couldn't chdir up from $dir_name, $!";
print STDERR "\n\n$clan_acc_kill has been killed. If the annotation for $clan_acc_merge needs updating, please update it now.\nThere is a copy of the CLANDESC file for the clan that has been killed here [$dir_name/$clan_acc_merge/CLANDESC] should you need it.\n";


sub help {

print<<EOF;


  This script is used to merge two clans together. You need to provide the
  clan to kill (clanA), and the clan that it will be merged into (clanB) 
  on the command line:

  Usage: $0 -kill CLXXXX -merge_into CLXXXX


  The script will do the following things:
   * update CL line in DESC files for families in clanA to point to clanB
   * remove all MB lines in CLANDESC for clanA
   * add MB lines for families that were in clanA to the CLANDESC for clanB
   * update clan_membership table so all families that were in clanA are 
     now in clanB
   * checkout the CLANDESC file for clanA
   * kill clanA

  If required, you should update the annotation in the CLANDESC file for clanB 
  after this script is finished.
  
  *Note*: Merging two clans together is irreversible, so please be sure that you 
  want to merge the two clans together before running this script.
EOF

exit(1);

}
