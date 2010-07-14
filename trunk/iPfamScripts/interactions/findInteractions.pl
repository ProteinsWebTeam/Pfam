#!/software/bin/perl

use strict;
use warnings;
use iPfam;
use Bio::iPfam::iPfamDBManager;
use Data::Dumper;
use Getopt::Long;
use Time::HiRes qw(time);
my ($pdbId, $help, $outputDir, $slopFactor, $debug, $numberPdbs, $page,
    $rdb_name, $rdb_user, $rdb_host, $rdb_port, $rdb_pass);

use Log::Log4perl qw(get_logger :levels);
BEGIN {
    # Explicit initialisation if we can't find the conf file
    Log::Log4perl->init( \<<EOF
log4perl.rootLogger=INFO, SCREEN
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d{yyyy-MM-dd HH:mm:ss}: line %4L, %M: %m%n
EOF
    );
} 

my $logger = Log::Log4perl->get_logger('iPfamFindInteractions');

GetOptions (
  
	   'numberPdbs=i'   => \$numberPdbs,
	   'page=i'           => \$page,
	   'pdb=s'          => \$pdbId,
	   'help'           => \$help,
	   'output=s'       => \$outputDir,
	   'sf=s'           => \$slopFactor,
	   'debug'          => \$debug,
	   	#Database connection parameters
   	"rdb=s"    => \$rdb_name,
    "u=s"      => \$rdb_user,
  "h=s"      => \$rdb_host,
  "port=i"   => \$rdb_port,
  "p=s"      => \$rdb_pass,
	   
);

#Check input parameters
#Check the database connection parameters. By default we are expecting
#to connect to a database residing in the staging instance.

if(!$rdb_name){
  $logger->logdie("*** No database name provided ***");
}

if(!$rdb_host){
  $rdb_host = "pfamdb2a";
  $logger->warn("*** No host name provide, guessing at $rdb_host ***");
}

if(!$rdb_port){
  $rdb_port = "3302";
  $logger->warn("*** No rdb port  provide, guessing at $rdb_port ***");
}

if(!$rdb_user){
  $rdb_user = "pfam";
  $logger->warn("*** No rdb user provided, guessing at $rdb_user ***");
}

if(!$rdb_pass){
  $logger->logdie("*** No rdb password provided ***");
}

#Now check that these actuall work
my $db = Bio::iPfam::iPfamDBManager->new('port', $rdb_port, 'host', $rdb_host, "password", $rdb_pass, 'database', $rdb_name);
unless($db){
  $logger->logdie("Failed to get a connection to the database......"); 
}

my @pdbList;
if($pdbId){
  my $pdbRow = $db->getSchema
                  ->resultset("Pdb")
			              ->find({pdb_id => $pdbId});
  push(@pdbList, $pdbRow) if($pdbRow and $pdbRow->pdb_id);
}elsif($numberPdbs and $page){
  @pdbList = $db->getSchema
                  ->resultset("Pdb")
			              ->search(undef,
			                       { page => $page,
			                         rows => $numberPdbs });
}else{
  $logger->logdie("Do not know what pdb entry or entries to work on, see $0 -help"); 
}
foreach my $pdbRow ( @pdbList ){
  $logger->info('Starting to search for interactions within entry '.$pdbRow->pdb_id); 
  
  #State that we have started this pdb file
  $db->startJob($pdbRow->pdb_id, "findInts");

  #Get the pdb entry, build the objects and locate the domains
  my $pdbObj = &iPfam::getPdb($pdbRow->pdb_id); # Use this at sanger

  #Now calculate all interactions for the PDB file;
  if($pdbObj){  
    eval{
        iPfam::calInts($pdbObj, $db);
    };
    
    my $error;
    if ($@){
      $error = $@;
    }else{
    #Now we make the Biomolecule
      if($pdbObj->biomolecule and ref($pdbObj->biomolecule) eq "HASH"){
        $logger->info('Going to make biomolecules');
        foreach my $bioNo (keys %{ $pdbObj->biomolecule }){
          my $transPdbObj = $pdbObj->get_biomolecule($bioNo);
          eval{
            iPfam::calInts($transPdbObj, $db);
          };
        }
      }
    } 
    if($@){
      $error .= $@;
      $logger->warn("ERROR, $@");
    }
    unless($error){
      $logger->info('Finished to search for interactions within entry '.$pdbRow->pdb_id); 
      $db->endJob($pdbRow->pdb_id);
    }
  }else{
     $logger->warn( "Failed get the pdb file entry for". $pdbRow->pdb_id );
  }

}

