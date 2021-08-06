#!/usr/bin/env perl

#This script adds families to a clan
#It will do the following for each family being added:
# - add an MB line to the CLANDESC
# - add the CL line to the DESC file
# - add the family to  the clan membership table in the database


use strict;
use warnings;
use Cwd;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;

my ($clan_acc, @families_to_add, $help);
my $message = "";
GetOptions( 'clan=s' => \$clan_acc,
             'add=s' => \@families_to_add,
             'm=s'   => \$message,
             'help' => \$help);

#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#Check input options
@families_to_add = split(/,/,join(',',@families_to_add));
check_options($help, $clan_acc, \@families_to_add, $pfamDB);

#Check database is not locked
my @lock_data = $pfamDB->getSchema->resultset('Lock')->search({ 'locked' => 1});
if(@lock_data) {
    my $lock_data = shift @lock_data; #Should only ever be one row
    print STDERR "\nThe database is currently locked by ". $lock_data->locker . " which means clan changes cannot be made at this time\n\n";
    exit 1;
}

#Make the dir to work in
mkdir($clan_acc, 0755) or die "Couldn't mkdir $clan_acc, $!";
chdir($clan_acc) or die "Couldn't chdir into $clan_acc, $!";

my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };   # don't allow control C for a bit!

#Add MB line to clan
#Add CL line to DESC
#Add to clan membership table
add_to_clan($clan_acc, \@families_to_add, $dbh, $message);

print STDERR "\n@families_to_add have been added to $clan_acc\n";

if($caught_cntrl_c) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** You should check that the script has done what you wanted (use pfinfo/clinfo to see if the CL/MB lines are correct, and check the clan_membership table in the database is correct" ;
}

sub check_options {

    my ($help, $clan_acc, $families_to_add, $pfamDB) = @_;

    if($help) {
        help();
    }

    #Check we have at least one family to add
    unless(@$families_to_add) {
        help();
    }

    #Check clan_acc is in the db
    if($clan_acc) {
        my @clan = $pfamDB->getSchema->resultset("Clan")->search({ "clan_acc" => $clan_acc });
        unless(@clan) {
            die "$clan_acc is not a valid clan accession\n";
        }
    }
    else {
        help();
    }

    #Check all the families to add are in the database, but not already in a clan
    my $error="";
	foreach my $pfamA_acc (@$families_to_add) {
        my $pfamA = $pfamDB->getSchema->resultset("PfamA")->find({ "pfama_acc" => $pfamA_acc });
        unless($pfamA) {
            $error .= "$pfamA_acc is not a valid Pfam accession\n";
        }

		my $clan = $pfamDB->getSchema->resultset("ClanMembership")->find({ "pfama_acc" => $pfamA_acc });
		if($clan) {
			$error .= "$pfamA_acc is already in clan ".$clan->clan_acc->clan_acc ."\n";
		}
    }

    if(-d $clan_acc) {
        $error .= "A directory called $clan_acc already exists in the current working directory, please remove this and then run the script again.\n";
    }


    if($error) {
        die "$error";
    }

    return 0;
}

sub add_to_clan {
    my ($clan_acc, $families_to_add, $dbh, $message) = @_;

    
    #Add CL line to DESC of famlies
    print STDERR "Adding CL lines to the DESC file of families\n";
    foreach my $pfamA_acc (@$families_to_add) {
        system("pfco $pfamA_acc") and die "Couldn't 'pfco $pfamA_acc', $!";
        my $familyIO = Bio::Pfam::FamilyIO->new;
        my $newFamObj = $familyIO->loadPfamAFromLocalFile( $pfamA_acc, ".");

        my $descObj = $familyIO->parseDESC("$pfamA_acc/DESC");
        $descObj->CL($clan_acc);
        $familyIO->writeDESC($descObj, $pfamA_acc);
        my $commit_message = "CLADD:Added CL line to $clan_acc in the DESC files of the following families: @$families_to_add";
        if($message) {
            $commit_message .= "\n$message";
        }
        system("svn commit -m '$commit_message' $pfamA_acc/DESC")
    }

    #Check out the clan and add MB lines to CLANDESC file for the families we are adding
    print STDERR "\n\nAdding MB lines to CLANDESC file of $clan_acc\n";
    system("clco $clan_acc") and die "Couldn't run 'clco $clan_acc', $!";
    open(CLANDESC, ">>$clan_acc/CLANDESC") or die "Couldn't open fh to $clan_acc/CLANDESC, $!";
    foreach my $pfamA_acc (@$families_to_add) {
        print CLANDESC "MB   $pfamA_acc;\n";
        print STDERR "$pfamA_acc\n";
    }
    close CLANDESC;
    my $commit_message = "CLADD:Added MB lines in CLANDESC for the following families: @$families_to_add";
    if($message) {
        $commit_message .= "\n$message";
    }
    system("svn commit -m '$commit_message' $clan_acc/CLANDESC");

    #Update clan_membership table
    print STDERR "\n\nAdding families to the clan_membership table in the database\n";
    my $st = $dbh->prepare("INSERT INTO clan_membership (clan_acc, pfamA_acc) VALUES ('$clan_acc', ?)");

    foreach my $pfamA_acc (@$families_to_add) {
        $st->execute($pfamA_acc) or die "Couldn't update clan_membership table in database, ". $st->errstr."\n";
    }
}

sub help {

print<<EOF;


  This script adds families to a clan. You need to give the
  clan accession and the families to add on the command line.

  The script will do the following things for each family being
  added from to the clan:
  * add an MB line to the CLANDESC
  * add the CL line to the DESC file
  * add the family to the clan membership table in the database

  Usage:

  You need to specify the clan accession, and the families
  that you wish to add to the clan as a comma separated
  list:

  $0 -clan CLXXXX -add PFXXXXX,PFXXXXX,PFXXXXX

  Eg. $0 -clan CL0001 -add PF00001,PF00002

  Options:
    -m               :message that describes the changes you have made
                      to this family (optional)
EOF

exit(1);

}
