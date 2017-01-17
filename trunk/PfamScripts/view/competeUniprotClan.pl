#!/usr/bin/env perl

#Script to compete clans using uniprot sequences
#Can be run on all clans (-all) or a single clan (-clan <clan_acc>)
#The script competes the clan, and for each clan member it:
# -removes outcompeted regions from the uniprot alignments stored
#  in the alignment_and_tree table
# -populates number_uniprot in the pfamA table


use strict;
use warnings;
use Bio::Pfam::Clan::Compete;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Compress::Zlib;
use Getopt::Long;


my ($all, $clan);
GetOptions('all' => \$all,
  'clan=s'  => \$clan);

unless($all or $clan) {
  die "Need to specify whether to do all clans (-all) or a single clan (-clan <clan_acc>)\n";
}
if($all and $clan) {
  die "Can't use -all and -clan option together\n";
}


#Database stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#Create array of all clans to work on
my @clans;
if($all) {
  @clans=$pfamDB->getSchema->resultset('Clan')->search();
}
else {
  @clans=$pfamDB->getSchema->resultset('Clan')->find({clan_acc => $clan});
}

#Loop through the clans
foreach my $clan (@clans) {
  print STDERR "Doing ".$clan->clan_acc."\n";

  #Compete clan
  Bio::Pfam::Clan::Compete::competeClanUniprot( $clan->clan_acc, $pfamDB );
  
  #Update clan member alignments, and number_uniprot in pfamA
  my @clanMembers=$pfamDB->getSchema->resultset('ClanMembership')->search( { clan_acc => $clan->clan_acc });  
  my $st_update_pfamA = $dbh->prepare("update pfamA set number_uniprot=? where pfamA_acc=?");
  foreach my $member (@clanMembers) {

    my $pfamA_acc=$member->pfama_acc->pfama_acc;
    print STDERR "Updating alignment for $pfamA_acc\n";

    #Store all regions in full
    my %in_full;
    my $st_in_full = $dbh->prepare("select uniprot_acc, ali_start, ali_end from uniprot_reg_full where pfamA_acc=? and in_full=1");
    $st_in_full->execute($pfamA_acc) or die "Couldn't execute statement ".$st_in_full->errstr."\n";
    my ($acc, $st, $en);
    $st_in_full->bind_columns(\$acc, \$st, \$en);
    while ($st_in_full->fetch()) {
      my $acc_se="$acc/$st-$en";
      $in_full{$acc_se}=1;
    }

    #Get uncompeted alignment from db
    my $rs = $pfamDB->getSchema->resultset('AlignmentAndTree')->find( { type => 'uniprot', pfama_acc => $pfamA_acc } );
    my $alignment = Compress::Zlib::memGunzip($rs->alignment) ;

    #Create competed alignment
    my $num_full=0;
    my $competed_aln;

    while($alignment =~ /(.+)\n/g) {
      my $line = $1; 
      if($line =~ /^(\S+)\/(\d+)-(\d+)/) {
        my ($acc, $st, $en) = ($1, $2, $3);
        if($acc =~ /(\S+)\.\d+/) {
          $acc=$1;
        }
        my $acc_se="$acc/$st-$en";
        if($in_full{$acc_se}) {
          $competed_aln.= "$line\n";
          $num_full++;
        }
      }
      elsif($line =~ /^\/\//) {
        $competed_aln.="$line\n";
      }
      else {
        die "Unrecognised line in alignment: $_";
      }
    }

    #Upload competed alignment to db
    $pfamDB->getSchema->resultset('AlignmentAndTree')->update_or_create({
        pfama_acc => $pfamA_acc,
        alignment  => Compress::Zlib::memGzip($competed_aln),
        type       => 'uniprot'
      }
    );  

    #Update num_uniprot in pfamA
    $st_update_pfamA->execute($num_full, $pfamA_acc);

  }
}
