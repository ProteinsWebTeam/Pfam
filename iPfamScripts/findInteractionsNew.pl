#!/usr/local/bin/perl

use lib '/Users/rdf//Work/iPfamWebsite/iPfamSchemata';
use lib '/Users/rdf//Work/iPfamWebsite/iPfamLib';
use strict;
use warnings;
use iPfam;
use Bio::iPfam::iPfamDBManager;
use Data::Dumper;
use Getopt::Long;
use Time::HiRes qw(time);


use Log::Log4perl qw(get_logger :levels);
BEGIN {
    # Explicit initialisation if we can't find the conf file
    Log::Log4perl->init( \<<EOF
log4perl.rootLogger=ERROR, SCREEN
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d{yyyy-MM-dd HH:mm:ss}: line %4L, %M: %m%n
EOF
    );
} 
get_logger( 'Bio::iPfam::Structure::PDBFactory'    )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Chain'    )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Entity'  )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Atom'  )->level( $INFO );
get_logger( 'Bio::iPfam::iPfamDBManager'  )->level( $INFO );
get_logger( 'iPfam'  )->level( $INFO );


my ($pdbId, $help, $outputDir, $slopFactor, $rdbName, $debug);



GetOptions (
	    'pdb=s'          => \$pdbId,
	    'help'           => \$help,
	    'output=s'       => \$outputDir,
	    'sf=s'           => \$slopFactor,
	    'databaseName=s' => \$rdbName,
	    'debug'          => \$debug
	   );

#Check input parameters
unless($pdbId){
  die "Please provide a pdb identifier to work on\n";
}

my $db = Bio::iPfam::iPfamDBManager->new('port', '3306', 'host', '127.0.0.1', "password", "mafp1");

unless($db){
  die "Failed to get a connection to the database......\n"; 
}

#State that we have started this pdb file
$db->startJob($pdbId, "findInts");

#Get the pdb entry, build the objects and locate the domains
my $pdbObj = &iPfam::getPdb($pdbId); # Use this at sanger

#Now calculate all interactions for the PDB file;
if($pdbObj){  
  iPfam::calInts($pdbObj, $db);
  #Now we make the Biomolecule
  if($pdbObj->biomolecule and ref($pdbObj->biomolecule) eq "HASH"){
    foreach my $bioNo (keys %{ $pdbObj->biomolecule }){
      my $transPdbObj = $pdbObj->get_biomolecule($bioNo);
      iPfam::calInts($transPdbObj, $db);
    }
  }
}else{
  print STDERR "Failed for $pdbId\n";
  exit;
}

$db->endJob($pdbId);
