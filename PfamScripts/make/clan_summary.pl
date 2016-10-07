#! /usr/bin/perl -w

# Script to collate file of information about Pfam clans
# Can use option -release to use release 27 data. Otherwise uses pfamlive

use strict;
use Getopt::Long;

# mysql query to get list of clans.
my %clans;

my ($release);
&GetOptions('release!'  => \$release);

# Connect to databases with mysql
if ($release){
    print STDERR "Using release 27 data\n";
    open (FH, "mysql -hmysql-xfam-dev -P4423 -upfam -pmafp1 pfam_27_0 -e 'select clan_acc, clan_id from clan;' |");
} else {
    print STDERR "Using pfam_live data\n";
    open (FH, "mysql -h mysql-pfam-live -P4430 -uadmin -pc5yj9WHC pfam_live -e 'select clan_acc, clan_id from clan;' |");
}
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
foreach my $clan (sort keys %clans){
    print CSV "$clan,$clans{$clan}\n";
    print "$clan $clans{$clan}\n";
    print "   Pfam ac\tType\tHMM len\tPfam id\n";

    if ($release){
	open(FH, "mysql -hmysql-xfam-dev -P4423 -upfam -pmafp1 pfam_27_0 -e 'select pfamA.pfamA_acc,pfamA_id,type,model_length from clan join clan_membership join pfamA where clan.clan_acc = clan_membership.clan_acc and clan_membership.pfamA_acc = pfamA.pfamA_acc and clan.clan_acc=\"$clan\";' |");
    } else {
	open(FH, "mysql -h mysql-pfam-live -P4430 -uadmin -pc5yj9WHC pfam_live -e 'select pfamA.pfamA_acc,pfamA_id,type,model_length from clan join clan_membership join pfamA where clan.clan_acc = clan_membership.clan_acc and clan_membership.pfamA_acc = pfamA.pfamA_acc and clan.clan_acc=\"$clan\";' |");
    }

    my $n=0;
    while(<FH>){
        if (/^(PF\d{5})\s+(\S+)\s+(\S+)\s+(\S+)/){
            my $pfamA_acc=$1;
            my $pfamA_id=$2;
            my $type=$3;
            my $model_length=$4;
	    print "   $pfamA_acc\t$type\t$model_length\t$pfamA_id\n";
	    $num_families_in_clans++;
	    $n++;
	}
    }
    close FH;
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
