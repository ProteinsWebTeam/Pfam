#! /usr/bin/env perl

#To analyse Pfam overlap files - counts number of overlapping sequences, finds which families/clans these are in and gives details of any which include a sequence in the seed of either family.
use strict;
use warnings;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my $help;
&GetOptions ("help" => \$help);

help() if($help);

my $overlap_file = "overlap";
unless(-s $overlap_file) {
  print STDERR "Can't find '$overlap_file' file in current working directory\n";
  help();
}


#Set up database
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;


open my $fh_read, '<', $overlap_file or die "Couldn't open $overlap_file. $!\n"; 

#create hashes for uniprot accessions and PfamIDs in overlap file 
my %uniprot;
my %pfamA;
my @seed;

while ( <$fh_read> ){
    my @data = split /\s+/m, $_;

    my $uniprot_ac = $data[1];
    $uniprot{$uniprot_ac}=1;

    my $pfamA_id = $data[7];
    $pfamA{$pfamA_id}++; #to count occurances of each family

    if (/SEED/) {
        push @seed, $_;
    }
}

close $fh_read or die "unable to close '$overlap_file'. $!\n";

#count keys in uniprot accession hash
my $count = keys (%uniprot);
print "There are $count overlapping sequences.\n\n";

print "There are overlaps with the following families:\n";

#Find out which families are in a clan; key=pfamA_id, value=clan
my %clan; 
clan(\%clan, $dbh);

#Print all families, and the clan accession if the family is in a clan
my %overlapping_clans;
foreach my $id (keys %pfamA) {
  if($clan{$id}) {
    print "$id\t($pfamA{$id} domains)\t$clan{$id}\n";
    $overlapping_clans{$clan{$id}}=1; 
  }
  else {
    print "$id\t($pfamA{$id} domains)\n";
  }
}

#Print a list of all the clans that the families in the overlap file belong to
print "\nThese clans overlap: ";
foreach my $c (keys %overlapping_clans) {
  print "$c \n";
}

#Print a list of overlaps including the seeds of one or other family
print "\nThe following overlaps include sequences occuring in seed alignments:\n";
print @seed;

#subroutine for clans
sub clan {
  my ($ref, $dbh) = @_;

  my $st = $dbh->prepare("select pfamA_id, c.clan_acc from pfamA p left join clan_membership m on p.pfamA_acc=m.pfamA_acc left join clan c on m.clan_acc=c.clan_acc") or die "Failed to prepare statement:".$dbh->errstr."\n";

  $st->execute() or die "Couldn't execute statement ".$st->errstr."\n";;

  my $array_ref = $st->fetchall_arrayref();

  foreach my $row (@$array_ref) {
    my ($pfamA_id, $clan_acc) = ($row->[0], $row->[1]);

    if($clan_acc) {
      $$ref{$pfamA_id}=$clan_acc;
    }
  }
}

sub help {
print STDERR << "EOF";

This script analyses Pfam overlap files:
1 - Counts the number of UniProt entries where there are overlaps
2 - Reports the families which overlap, and how many domains overlap with each
3 - Reports what clans the overlapping families are in
4 - Gives details of any overlaps involving the SEED of one or other family

To use this script, run it in the same working directory as your 'overlap' file.

The output is to the screen
 
EOF
 
exit (0);
}




