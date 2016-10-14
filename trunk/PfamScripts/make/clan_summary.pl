#! /usr/bin/perl -w

# Script to collate file of information about Pfam clans

use strict;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

# mysql query to get list of clans.
my $mysql_command="mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database}." -e 'select clan_acc, clan_id from clan'";
my %clans;
print STDERR "Using ".$pfamDB->{database}." data\n";
open (FH, "$mysql_command |");
while(<FH>){
    if (/^(CL\d{4})\s+(\S+)/){
        $clans{$1}=$2;
    }
}

open (CSV, "> clans.csv") or die "Cannot write to file clans.csv"; # This is used to create a google doc to track clan editing
print CSV "Accession, Id, \n";


# Print summary information
my $num_clans=keys %clans;
print "Pfam contains $num_clans Clans\n\n";
my $num_families_in_clans=0;
my %counts;

# Code for running queries against Pfam database
my $sth=$dbh->prepare("select pfamA.pfamA_acc,pfamA_id,type,model_length from clan join clan_membership join pfamA where clan.clan_acc = clan_membership.clan_acc and clan_membership.pfamA_acc = pfamA.pfamA_acc and clan.clan_acc=?");
foreach my $clan (sort keys %clans){
    print CSV "$clan,$clans{$clan}\n";
    print "$clan $clans{$clan}\n";
    print "   Pfam ac\tType\tHMM len\tPfam id\n";

    $sth->execute($clan) or die "Couldn't execute statement ".$sth->errstr."\n";
    my ($pfamA_acc, $pfamA_id, $type, $model_length);
    $sth->bind_columns(\$pfamA_acc, \$pfamA_id, \$type, \$model_length);

    my $n=0;
    while ($sth->fetch()) {
      print "   $pfamA_acc\t$type\t$model_length\t$pfamA_id\n";
      $num_families_in_clans++;
      $n++;
    }

    print "\n";

    $counts{$n}++;

}

print "Pfam clans contain $num_families_in_clans families\n";

open (OUTPUT, "> clan_summary.csv") or die "cannot write to clan_summary.csv file";
print OUTPUT "Size,Frequency\n";
foreach my $n (sort numerically keys %counts){
    print OUTPUT "$n,$counts{$n}\n";
}
close OUTPUT;

sub numerically {$a <=> $b}
