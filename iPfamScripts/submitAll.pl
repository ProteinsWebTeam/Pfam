#!/software/bin/perl
#
# Author     : rdf
# Maintainer : $Id: submitAll.pl,v 1.3 2008-11-23 19:54:15 rdf Exp $
# Created    : 
# Modified   : $Date: 2008-11-23 19:54:15 $
# 

=head1 DESCRIPTION

  Script to submit blocks of interaction searches to the WTSI farm.    

=cut

use strict;
use warnings;
use LSF::Job RaiseError => 1, PrintError => 1, PrintOutput => 1;
use Bio::iPfam::iPfamDBManager;
use Getopt::Long;

#------------------------------------------------------------------------------------------------------
#Logger setup
use Log::Log4perl qw(get_logger :levels);
BEGIN {
    # Explicit initialisation if we can't find the conf file
    Log::Log4perl->init( \<<EOF
log4perl.rootLogger=DEBUG, SCREEN
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d{yyyy-MM-dd HH:mm:ss}: line %4L, %M: %m%n
EOF
    );
} 
my $logger = Log::Log4perl->get_logger('iPfamSubmitAll');


#---------------------------------------------------------------------------------------------------
my ($help, $numberPdbsPerJob, $page, $slopFactor, $debug, $sqlResource, $lsfQueue, 
     $rdb_name, $rdb_user, $rdb_host, $rdb_port, $rdb_pass, $maxLoad);

GetOptions (
  #Important parameters for the submission.
	'numberPdbs=i'   => \$numberPdbsPerJob,
	'sf=s'           => \$slopFactor,
	'maxLoad=i'      => \$maxLoad,
	'sqlResource=s'  => \$sqlResource,
	'lsfQueue=s'     => \$lsfQueue, 
	#Database connection parameters
	"rdb=s"    => \$rdb_name,
  "u=s"      => \$rdb_user,
  "h=s"      => \$rdb_host,
  "port=i"   => \$rdb_port,
  "p=s"      => \$rdb_pass,
  #Script controllers
	'debug'          => \$debug,
  'help'           => \$help,
);


#Job parameters
unless($numberPdbsPerJob){
  $numberPdbsPerJob = 50;
  $logger->warn("*** Number of PDBs not supplied. Setting to $numberPdbsPerJob ***");
}

unless($maxLoad){
  $maxLoad = 100;
  $logger->warn("*** The max load on SQL was not specified. Setting maxLoad to be $maxLoad ***");
}

unless($sqlResource){
  $sqlResource = "mypfamstg";
  $logger->warn("*** No LSF sql resource specified. Setting to $sqlResource ***");
}

unless($lsfQueue){
  $lsfQueue = 100;
  $logger->warn("*** The max load on SQL was not specified. Setting maxLoad to be $maxLoad ***");
}


#Now check that a valid resource has been 
unless($sqlResource =~ /mypfam[dev|stg|live|arc]/){
  $logger->logdie("*** Invaild LSF pfam sql resources: $sqlResource ***" );
}

#Now check that the load levels are sensible
if($sqlResource eq 'mypfamlive' or $sqlResource eq 'mypfamstg'){
  if($maxLoad > 300){
    $logger->warn("*** $maxLoad is too high for this resource, setting to 300 ***");
    $maxLoad = 300;  
  }
}elsif($maxLoad > 100){
  $logger->warn("*** $maxLoad is too high for this resource, setting to 100 ***");
  $maxLoad = 100;  
}

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
  die "Failed to get a connection to the database......\n"; 
}

my $totalNumberStructures = $db->getSchema
                                ->resultset("Pdb")
			                           ->search();


my $numJobs = 	int($totalNumberStructures/$numberPdbsPerJob)+1;



#Now Build up the execution statement.
my $exec =  'cd /lustre/scratch1/sanger/pfam/ipfam; findInteractions.pl ';
$exec .= " -numberPdbs $numberPdbsPerJob";
$exec .= " -page \$\{LSB_JOBINDEX\}";
$exec .= " -h $rdb_host -rdb $rdb_name -port $rdb_port -p $rdb_pass";
if($slopFactor){
  $exec .= "-sf $slopFactor"; 
}
$logger->debug("LSF:  -J ipfam[1-$numJobs], -q $lsfQueue -R select[mem>1500 && $sqlResource<$maxLoad && type==X86_64] rusage[$sqlResource=10:mem=1500]");
$logger->debug("Would execute the following cmd:$exec");

#$resource = "-R\'select[mem>1500 && mypfamlive<100 && type==X86_64] rusage[mypfamlive=10:mem=1500]\'";		                           
#use LSF::Job RaiseError => 0;
my $job = LSF::Job->submit(-q => $lsfQueue,
                           #-J => "ipfam[1-$numJobs]",
                           -J => "ipfam[3-$numJobs]",
                           -o => '/lustre/scratch1/sanger/pfam/ipfam/%J.%I.test',
                           -M => '1500000',
                           -R => "select[mem>1500 && $sqlResource<$maxLoad && type==X86_64] rusage[$sqlResource=10:mem=1500]",
                           $exec);

