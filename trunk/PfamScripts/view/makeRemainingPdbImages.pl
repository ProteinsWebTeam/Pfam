#!/usr/bin/env perl

#identify PDB entries without images and attempt to rectify this

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess::PdbImage;
use Log::Log4perl qw(:easy);
use DDP;
use Getopt::Long;

my ($statusdir);
&GetOptions(
    'statusDir=s' => \$statusdir,
);

#set up db stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#get the logger

Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

#identify PDB enties where either image = NULL and delete these rows from the database

my %null;
my $st1 = $dbh->prepare("select pdb_id from pdb_image where pdb_image is NULL") or $logger->logdie("Failed to prepare statement" . $dbh->errstr);
my $st2 = $dbh->prepare("select pdb_id from pdb_image where pdb_image_sml is NULL") or $logger->logdie("Failed to prepare statement" . $dbh->errstr);

$st1->execute();
$st2->execute();
my $arrayref1 = $st1->fetchall_arrayref();
my $arrayref2 = $st2->fetchall_arrayref();

foreach my $row1 (@$arrayref1){
    $null{$row1->[0]}=1;
}
foreach my $row2 (@$arrayref2){
    $null{$row2->[0]}=1;
}

my $st3 = $dbh->prepare("delete from pdb_image where pdb_id = ?") or $logger->logdie("Failed to prepare statement" . $dbh->errstr);

foreach my $nullid (keys %null){
    $st3->execute($nullid);
}


#identify rows in PDB which do not have an image in pdb_image

my %pdb;
my %image;
my %noimage;

my $st4 = $dbh->prepare("select pdb_id from pdb") or $logger->logdie("Failed to prepare statement" . $dbh->errstr);
my $st5 = $dbh->prepare("select pdb_id from pdb_image") or $logger->logdie("Failed to prepare statement" . $dbh->errstr);
$st4->execute();
$st5->execute();
my $arrayref4 = $st4->fetchall_arrayref();
my $arrayref5 = $st5->fetchall_arrayref();

foreach my $row4 (@$arrayref4){
    $pdb{$row4->[0]}=1;
}

foreach my $row5 (@$arrayref5){
    $image{$row5->[0]}=1;
}

foreach my $id (keys %pdb){
    unless ($image{$id}){
        $noimage{$id}=1;
    }
}

#for each of these rows, run makePdbImages.pl with a single accession (bsub to farm - use a job group). need to make a separate dir for each as pdbImage file will have same name for each when run from acc

foreach my $id (keys %noimage){
    $logger->debug("Working on $id");
    my $dir = $statusdir . "/" . $id;
    system("mkdir $dir") and $logger->logdie("Couldn't make directory $dir");
    my $mem = '24000';
    my $grp = "/PfamViewGroup";
    my $queue = $config->{farm}->{lsf}->{queue};
    my $out = $dir . "/$id.out";
    my $cmd = "bsub -q $queue -M $mem -g $grp -o $out \"makePdbImages.pl -acc $id -statusdir $dir\"";
    system($cmd) and $logger->logdie("Could not submit farm job for $id");
}
