#!/usr/local/bin/perl
use lib '/nfs/WWWdev/SANGER_docs/cgi-bin/Rfam';

use RfamWWWConfig;
use EntryWWW;

use lib '/nfs/team71/pfam/mm1/rfam/scripts/Modules';

require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/Bio/Rfam/DB_RDB.pm";
require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/Bio/Rfam.pm";

use strict;

my $outdir = shift;

die "need output dir dopey! \n" if(!$outdir);

my  $rdb = Bio::Rfam->switchover_rdb();
print "RDB: $rdb \n";

my (@blee) = $rdb->query("select auto_rfam, rfam_id, rfam_acc from rfam");
#$rdb->add_rfamseq(  "/nfs/team71/pfam/mm1/scripts/Rfam/embl.dat");

foreach my $res (@blee) {
  my @boo = @{$res};
  my $auto_rfam = $boo[0];
  my $rfam_id = $boo[1];
  my $rfam_acc = $boo[2];
  open(_OUT, ">$outdir/$rfam_acc.species") or die "Canna write to: $outdir/$rfam_acc.species as $! \n";
  print _OUT "ID $rfam_id\n";
  my (@rfamseq) = $rdb->query("select distinct rfamseq_acc, species, taxonomy from rfamseq, rfam_reg_full where auto_rfam = '$auto_rfam' and rfam_reg_full.auto_rfamseq = rfamseq.auto_rfamseq");

  foreach my $seq (@rfamseq) {
    my (@res) = @{$seq};

    my ($rfamseq_acc, $species, $taxonomy);
    $rfamseq_acc = $res[0];
    $species = $res[1];
    $taxonomy = $res[2];
 #   print "ACC: $rfamseq_acc : species: $species : tax: $taxonomy \n";
    print _OUT "$rfamseq_acc~$species~$taxonomy\n";
    #foreach (@res) {
    #  print "$_ ";
    #}
#    print "\n";
  }

  close(_OUT);
}
#print "DONE : @blee \n";
