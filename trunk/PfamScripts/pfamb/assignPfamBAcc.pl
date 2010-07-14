#!/software/bin/perl

use strict;
use warnings;

use Data::Dumper;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;
use Compress::Zlib;

## To Use log4per......
use Log::Log4perl qw(get_logger :levels);
Log::Log4perl->init( \<<EOF
log4perl.rootLogger=WARN, SCREEN,FILE

# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n
#log4perl.appender.SCREEN.Threshold=WARN

# An example of a file appender - change file accordingly!
log4perl.appender.FILE=Log::Log4perl::Appender::File
log4perl.appender.FILE.filename=/tmp/log4perl.log
log4perl.appender.FILE.mode=write
log4perl.appender.FILE.layout=PatternLayout
log4perl.appender.FILE.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);  

my $logger = get_logger();

## end logger stuff

my($help, $rdb_name, $rdb_host, $rdb_user, $rdb_port, $relDir, $acc, $accfile, $rdb_pass, $infomode, $graphmode, $verbosemode, %adda_map, $block, $identity);

GetOptions(
  "help"      => \$help,
  "rdb=s"     => \$rdb_name,
  "u=s"       => \$rdb_user,
  "h=s"       => \$rdb_host,
  "port=i"    => \$rdb_port,
  "p=s"       => \$rdb_pass,
  "relDir=s"    => \$relDir
);


exec('perldoc', $0) if($help);

if(!$relDir){
  $relDir = ".";
  $logger->warn("Setting the relDir to be $relDir"); 
}

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}

#Check that we can connect to the database.
$logger->debug("Got pfamlive database connection");
my $dbh = $pfamDB->getSchema->storage->dbh;

my @prePfamB = $pfamDB->getSchema
                      ->resultset('Pfamb')
                        ->search({},
                                 { order_by => 'number_regions DESC' });

open(PFAMB, ">$relDir/Pfam-B") or $logger->logdie("Could not open $relDir/Pfam-B:[$!]");
open(PFAMBFA, ">$relDir/Pfam-B.fasta") or $logger->logdie("Could not open $relDir/Pfam-B.fasta:[$!]");



my $count = 1;
foreach my $pfb (@prePfamB){
  if ($pfb->number_regions > 0){
    my $pfamB_id  = "Pfam-B_$count";
    my $pfamB_acc = sprintf "PB%06d", $count; 
    $logger->debug("Working on $pfamB_acc");
    $pfb->update({  pfamb_acc         => $pfamB_acc,
                         pfamb_id          => $pfamB_id});
    
                         
    
    my $fa = $pfamDB->getSchema
                      ->resultset('PfambFasta')
                        ->find({ auto_pfamb => $pfb->auto_pfamb });
    
    my $fasta = Compress::Zlib::memGunzip( $fa->fasta );
    if($fasta =~ /(\>\S+)\s+PB\d+;\s+Pfam-B_\d+;/){
    	$fasta =~ s/(\>\S+)\s+PB\d+;\s+Pfam-B_\d+;/$1 $pfamB_acc; $pfamB_id;/g;   
	}else{
		$fasta =~ s/(\>\S+)/$1 $pfamB_acc; $pfamB_id;/g;
	}
    $fa->update({fasta => Compress::Zlib::memGzip($fasta)});
   
    my $ali = $pfamDB->getSchema
                      ->resultset('PfambStockholm')
                        ->find({ auto_pfamb => $pfb->auto_pfamb });

    my $alignment = Compress::Zlib::memGunzip( $ali->stockholm_data );
    $alignment =~ s/#=GF ID   Not decided yet/#=GF ID   $pfamB_id/;
    $alignment =~ s/#=GF ID   Pfam-B_\d+/#=GF ID   $pfamB_id/;
    $alignment =~ s/#=GF AC   Not decided yet/#=GF AC   $pfamB_acc/;
    $alignment =~ s/#=GF AC   PB\d+/#=GF AC   $pfamB_acc/;
    $ali->update({stockholm_data => Compress::Zlib::memGzip($alignment)});
    if(length($alignment) <20){
      $logger->warn("$pfamB_acc has a rather small aligmment file");
    }
    if(length($fasta) <20){
      $logger->warn("$pfamB_acc has a rather small fasta file");
    }
    print PFAMB   $alignment;  
    print PFAMBFA $fasta;
                
    $count++;
  }
}
close(PFAMB);
close(PFAMBFA);
