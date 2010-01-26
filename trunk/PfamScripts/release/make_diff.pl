#! /software/bin/perl -w

#Script to make the diff file from the current_pfam_version and released_pfam_version tables in rdb
#The final step in the script updates the released_pfam_version table using the data in current_pfam_version
#Need to pass the diff file from the last release on the command line, along with the old release and new release number.

use strict;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Getopt::Long;


my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

my ($file , $old_rel, $new_rel);

GetOptions( 'file=s' => \$file,
 	 'new_rel=i' => \$new_rel,
	 'old_rel=i' => \$old_rel);

unless($file and -s $file and $old_rel and $new_rel) {
    die "Usage: $0 -file  /lustre/pfam/pfam/Production/Pfam/RELEASES/23.0/diff -old_rel 23 -new_rel 24";
}


print STDERR "Parsing $file...";
my $dead;
my %old;
open(FH, $file) or die "Couldn't open $file $!";
while(<FH>) {
    if(/^FI\s+(PF\d\d\d\d\d)\S+\s+(\S+)\s+(\S+)/) {
	if($3 eq "DEAD") {              
            $dead .= sprintf("%2s  %-11s  %-17s  %-30s \n", "FI", $1, $2, "DEAD");
	}
	else {
	    $old{$1}=$2;
	}
    }
    elsif(/^NR/) {
	print "NR  Rel$old_rel" . "_0\n";
	print "OR  Rel$new_rel" . "_0\n";
	my $today = gmtime();

	print "DA  $today\n";
    }
    elsif(/^CC\s+(.+)/) {
	print "CC  $1\n";
    }   
}
close FH;
print STDERR "done\n";



print STDERR "Retrieving data from current_pfam_version...";
my @current = $pfamDB->getSchema
                      ->resultset('CurrentPfamVersion')
                      ->search({}, join => 'auto_pfama');
my (%current, %pfamA);;
foreach my $row (@current) {
    $current{$row->get_column('auto_pfama')}{'seed'}=$row->seed;
    $current{$row->get_column('auto_pfama')}{'align'}=$row->align;
    $current{$row->get_column('auto_pfama')}{'desc'}=$row->desc_file;
    $current{$row->get_column('auto_pfama')}{'hmm'}=$row->hmm;
    $current{$row->get_column('auto_pfama')}{'pfamA_id'}=$row->pfama_id;
    $current{$row->get_column('auto_pfama')}{'pfamA_acc'}=$row->pfama_acc .".". $row->version;
    $pfamA{$row->pfama_acc}= $row->pfama_id;
}
print STDERR "done\n";


print STDERR "Retrieving data from released_pfam_version...\n";
my @released = $pfamDB->getSchema
                      ->resultset('ReleasedPfamVersion')
                      ->search({},);
my %released;
foreach my $row (@released) {
    $released{$row->get_column('auto_pfama')}{'seed'}=$row->seed;
    $released{$row->get_column('auto_pfama')}{'align'}=$row->align;
    $released{$row->get_column('auto_pfama')}{'desc'}=$row->desc_file;
    $released{$row->get_column('auto_pfama')}{'hmm'}=$row->hmm;
}
foreach my $auto (keys %current) {

    if(exists($released{$auto})) {

	my ($seed, $align, $desc, $hmm);

	my $change="";

        $change .= "SEED " unless($released{$auto}{'seed'} eq $current{$auto}{'seed'});
        $change .= "HMM "  unless($released{$auto}{'hmm'} eq $current{$auto}{'hmm'});
        $change .= "FULL " unless($released{$auto}{'align'} eq $current{$auto}{'align'});
        $change .= "DESC " unless($released{$auto}{'desc'} eq $current{$auto}{'desc'});

	my $c;
	if($change) {
	    $c = "CHANGE   $change";
	}
	else {
	    $c = "NOCHANGE";
	}
	print sprintf ("%2s  %-11s  %-17s  %-30s \n", "FI", $current{$auto}{'pfamA_acc'}, $current{$auto}{'pfamA_id'}, $c);

    }
}



foreach my $auto (keys %current) {
    unless(exists($released{$auto})) {
	my $c = "NEW      SEED HMM FULL DESC";
	print sprintf ("%2s  %-11s  %-17s  %-30s \n", "FI", $current{$auto}{'pfamA_acc'}, $current{$auto}{'pfamA_id'}, $c);
    }
}


print $dead;
foreach my $pfamA (keys %old) {

    unless(exists($pfamA{$pfamA})) {
	my $c = "DEAD";
	print sprintf ("%2s  %-11s  %-17s  %-30s \n", "FI", $pfamA, $old{$pfamA}, $c);
    }
}
print STDERR "done\n";



print STDERR "Deleting old data from released_pfam_version...";
my $dbh = $pfamDB->getSchema->storage->dbh;
my $delete = $dbh->prepare("delete from released_pfam_version");
$delete->execute() or die "Failed to delete old data from released_pfam_version ".$delete->errstr."\n";
print STDERR "done\n";


print STDERR "Downloading data from current_pfam_version...";
my $download = $dbh->prepare("select c.auto_pfamA, seed, align, desc_file, hmm, version into outfile '/tmp/current.dat' from pfamA as a, current_pfam_version as c where a.auto_pfamA = c.auto_pfamA");
$download->execute() or die "Failed download from current_pfam_data ".$download->errstr."\n";
print STDERR "done\n";


print STERR "Uploading data to released_pfam_data\n";
my $upload = $dbh->prepare("load data infile '/tmp/current.dat' into table released_pfam_versions");
$upload->execute() or die "Failed to upload data into released_pfam_data ".$upload->errstr."\n";
print STERR "done\n";
