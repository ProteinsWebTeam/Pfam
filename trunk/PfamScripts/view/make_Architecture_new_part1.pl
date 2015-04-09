#!/usr/bin/env perl

#make architectures - part 1.
#retrieves pfamseq_acc and is_frag from pfamseq
#splits into chunks and submits make_Architecture_new_part2.pl to the farm as a job array
#options - -fileonly only creates and sorts files
#          -uploadonly only uploads data (files already created)
#Default - both creates files and uploads.

use strict;
use warnings;
use DDP;
use Log::Log4perl qw(:easy);
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use File::Slurp;
use String::CRC32;
use Cwd;
use Getopt::Long;

my ($file, $upload);
&GetOptions(
    'fileonly' => \$file,
    'uploadonly' => \$upload,
);
if ($file && $upload){
    die "-fileonly and -uploadonly options are incompatible\n";
}

#set up db stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#get the logger

Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $dir = getcwd;
my $count=0;
#file only section
unless ($upload){#options - -fileonly only creates and sorts files
#          -uploadonly only uploads data (files already created)
#Default - both creates files and uploads.
#Dump data from pfamseq to file - pfamseq_acc, is_frag
    unless (-s "pfamseq_dump"){
        $logger->debug("Getting pfamseq accessions from database");
        my $host = $pfamDB->{host};
        my $user = $pfamDB->{user};
        my $pass = $pfamDB->{password};
        my $port = $pfamDB->{port};
        my $db = $pfamDB->{database};
        my $cmd = "mysql -h $host -u $user -p$pass -P $port $db --quick -e \"select pfamseq_acc, is_fragment from pfamseq\" > pfamseq_dump";
        system($cmd) and die "Could not obtain sequence accessions from database";
    }

#Split file into chunks of 1 million sequences
    unless (-e "split_files"){
        system("split -d -l 1000000 pfamseq_dump pfamseq_dump_") and die "Could not split pfamseq_dump\n";
        system ("touch split_files") and die "Could not touch file\n";
    }
}#end of file only section

#set off part 2 of the script as a job array
#get number of files for job array
my @files = read_dir($dir);
foreach my $file (@files){
    if ( $file =~ /pfamseq_dump_\d+/ ){
        $count++;
    }
}


#farm parameters
$logger->debug("Submitting $count farm jobs");
my $queue = 'production-rh6';
my $resource = "rusage[mem=25000]";
my $memory = 25000;  
my $fh = IO::File->new();
my $options = '';
my $n = 100;
if ($upload){
    $options = "-uploadonly";
    #if uploading, only allow one job at a time
    $n = 1;
}
if ($file){
    $options = "-fileonly";
}

$fh->open( "| bsub -q $queue -M $memory -R $resource -o $dir/arch.\%J.\%I.log -Jarch\"[1-$count]%$n\"");
$fh->print( "make_Architecture_new_part2.pl -chunk \$\{LSB_JOBINDEX\} $options" );

$fh->close;

