#!/software/bin/perl

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

use Log::Log4perl qw(get_logger :levels);
Log::Log4perl->init( \<<EOF
log4perl.rootLogger=DEBUG, SCREEN
# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);

my $logger = get_logger();
$logger->level($DEBUG);

my($help, $verbosemode, $addafile, $noJobs, $tmpDir, $queue, $resource, $memory);
 
GetOptions(
  "help"         => \$help,
  'verbose'      => \$verbosemode,
  'addafile=s'   => \$addafile,
  'jobs=i'       => \$noJobs,
  'farmTmpDir=s' => \$tmpDir,
  'resource=s'   => \$resource,
  'queue=s'      => \$queue  
);

unless($verbosemode){
   $logger->level($WARN);
}

if(!$addafile){
  help();
  $logger->logdie("No adda file supplied, can not proceed!");  
}

#Now check the database connection paramters are present. If not guess, check the string before
#we proceed.

$logger->level($DEBUG);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}

#Check that we can connect to the database.
$logger->debug("Got pfamlive database connection");
my $dbh = $pfamDB->getSchema->storage->dbh;

if(!$noJobs){
  $noJobs = 1000; 
  $logger->debug("Setting number of jobs to default $noJobs");
}


if(!$tmpDir){
  $tmpDir = "/lustre/scratch103/sanger/".$ENV{USER}."/pfamB";
  $logger->debug("Setting farm tmp directory to be $tmpDir");
}

if(-e $tmpDir){
  system("rm -fr $tmpDir");
}
mkdir($tmpDir);

if(!$resource){
  $resource = "-R'select[type==X86_64 && mem>1500 && mypfamlive<500] rusage[mypfamlive=10:mem=1500]'";
  $logger->debug("Setting farm resource request to be $resource");
}

if(!$memory){
  $memory = 1500000;  
}


if(!$queue){
  $queue = "normal"; 
  $logger->debug("Setting farm queue to be $queue");
}

$logger->debug("Removing old adda table if present");
my $sqlDelOld = "DROP TABLE IF EXISTS adda";
$dbh->do($sqlDelOld) or $logger->logdie("Error deleting:".$dbh->errstr);

$logger->debug("Creating data table");
#Generate the adda table.
my $sql = "CREATE TABLE adda (
  adda_acc int(10) NOT NULL,
  seqacc varchar(6) NOT NULL,
  version tinyint(4) NOT NULL,
  start int(6) unsigned NOT NULL,
  end int(6) unsigned NOT NULL,
  KEY seqacc_idx (seqacc, version),
  KEY adda_idx (adda_acc)
)";
$dbh->do($sql) or $logger->logdie($dbh->errstr);

$logger->debug("Creating preliminary pfamB data table");

$logger->debug("Removing old adda table if present");
my $sqlDelOldB = "DROP TABLE IF EXISTS _preliminary_pfamB";
$dbh->do($sqlDelOldB) or $logger->logdie("Error deleting:".$dbh->errstr);

$logger->debug("Creating pre Pfam-B data table");

my $sqlB =  "CREATE TABLE `_preliminary_pfamB` (
  `auto_pfamB` int(10) unsigned NOT NULL auto_increment,
  `number_archs` int(8) unsigned default NULL,
  `number_species` int(8) unsigned default NULL,
  `number_structures` int(8) unsigned default NULL,
  `number_regions` int(5) unsigned default '0',
  PRIMARY KEY  (`auto_pfamB`)
)";

$dbh->do($sqlB) or $logger->logdie($dbh->errstr);




$logger->debug("Purging Pfam-B associated tables");
foreach my $table (qw( pfamB_stockholm pfamB_fasta pfamB_reg pfamB_database_links pdb_pfamB_reg pfamB)){
  $logger->debug("Deleting from $table");
  my $deleteSth = $dbh->prepare("DELETE FROM $table");
  $deleteSth->execute or $logger->logdie("Error executing delete statement on $table:".DBI->errstr);
  $deleteSth->finish;
}

$logger->debug("Preparing adda table insert statement");
my $addaSth = $dbh->prepare("INSERT INTO adda (adda_acc, seqacc, version, start, end) VALUES (?,?,?,?,?)");

$logger->debug("Going to parse the adda data");
my($addaData, $totSeq) = parseAddaData($addaSth, $addafile);
$logger->debug("Going to split adda alignments into roughly $noJobs jobs");
my $actualJobs = makeAccList($addaData, $totSeq, $noJobs, $tmpDir);

$addaData = undef;
$totSeq = undef;

#$logger->debug("Rsyncing acclist to farm");
##Rsync the acc files on t¤o the farm
##Set up an objec that can perform rsync copying
#my $rsyncObj = File::Rsync->new( { archive      => 1, 
#                                   compress     => 1,
#                                   recursive    => 1, 
#                                   rsh          => '/usr/bin/ssh', 
#                                   'rsync-path' => '/usr/bin/rsync' } );
# 
#for(my $i = 1; $i <= $actualJobs; $i++){                                   
#	$rsyncObj->exec( { src => "ADDA_acclist.$i", dest => $farmNode.":".$tmpDir } );
#	if($rsyncObj->err){
#		$logger->warn("Failed to rsync ADDA_acclist.$i acc files to the farm,".
#	                 "because [".join("", @{$rsyncObj->err})."]");
#	}
#}
#Submit the jobs to the farm
my $fh = IO::File->new();
#print "bsub -q $queue  ".$resource." -o ".$tmpDir."/adda2pfam.\%J.log  -Jadda2pfam\"[1-$actualJobs]\"";

$fh->open( "| bsub -q $queue  ".$resource." -o ".$tmpDir."/adda2pfam.\%J.\%I.log  -Jadda2pfam\"[1-$actualJobs]\"");
#$fh->open( "| bsub -q $queue  ".$resource." -o ".$tmpDir."/adda2pfam.\%J.\%I.log  -Jadda2pfam\"[1]\"");
$fh->print( "cd ".$tmpDir."/\$\{LSB_JOBINDEX\}\n");
$fh->print( "make-pfamB-fromAdda.pl -accfile ADDA_acclist.\$\{LSB_JOBINDEX\}\n");
$fh->print( "rm -fr ".$tmpDir."/\$\{LSB_JOBINDEX\}\n");
$fh->close;

exit;
#TODO - put correct executable strings into bsub!
my $assginAccfh = IO::File->new();
$assginAccfh->open( "| bsub -q $queue  ".$resource." -o ".$tmpDir."/assignAcc.log  -w\'done(adda2pfam)\'");
$assginAccfh->print( "assignPfamBAcc.pl rdb connect params etc \n" );
$assginAccfh->close;
exit;

###############
# Subroutines #
###############

sub parseAddaData {
	my($addaSth, $addafile) = @_;
	
	#This will act as our store of adda regions
	my $adda;
	
	open(ADDA, "$addafile") || $logger->logdie("Could not open file $addafile:[$!]\n");
	while(<ADDA>){
		#A0A000|A0A000_9ACTO     35      371     2820 - try and match a line like this
		if(/(\S{6})\.(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
			my $seqacc = $1;
			my $version = $2;
			my $start  = $3;
			my $end = $4;
			my $a = $5;
			$start++;
			$end--;
			my $length = $end - $start +1;
			$adda->{$a}->{seqs}++;
			$adda->{$a}->{residues} += $length;
			push (@{$adda->{$a}->{regions}}, {start => $start, end => $end, version => $version, seqacc => $seqacc});
		}else{
			chomp;
			$logger->warn("Did not parse line|$_|");
		}
	}

  #Remove alignments according to size
	my ($addaPrune, $totSeq);
	foreach my $a (keys %$adda){
		#First step - must have more than 2 sequence, but less than 1000
		next if($adda->{$a}->{seqs} > 1000 or $adda->{$a}->{seqs} < 2);
		
		#Make sure that the alignment is useful!
		if($adda->{$a}->{residues} > 500000){
			$logger->debug("$a has less than 1000 sequences, but more than 500000 resdidues to align!");
			next;
		}
		if($adda->{$a}->{residues} < 40){
			$logger->debug("$a has 2-1000  sequences, but less than 40 resdidues to align!");
			next;
		}
	
		foreach my $r (@{$adda->{$a}->{regions}}) {	
			#Okay - if we get here we want to store this formation;
			$addaSth->execute($a, $r->{seqacc}, $r->{version}, $r->{start}, $r->{end}) or
			$logger->logdie($dbh->errstr);
		}

		$addaPrune->{$a} = $adda->{$a}->{seqs};
		$totSeq += $adda->{$a}->{seqs};
	}
	return ($addaPrune, $totSeq);
}


sub makeAccList {
	my ($addaPrune, $totSeq, $noJobs, $tmpDir) = @_; 

	my $chunk = (int( scalar($totSeq) / $noJobs ) ) + 1;

	my @addaList = sort{$a <=> $b} keys %$addaPrune;
	$logger->debug("There will be approx. $chunk sequenecs per job");

	my $i =1;
	my ($cS, @list);
	$cS = 0;
	foreach my $a (@addaList){
		if($cS > $chunk){
		  mkdir($tmpDir."/".$i) or $logger->logdie("Could not make $tmpDir/".$i."[$!]");
			open(L, ">$tmpDir/$i/ADDA_acclist.$i") || die "Could not open ADDA acc list:[$!]\n";
		
			foreach (@list){
				print L "$_\n";
			}
			close(L);
			undef @list;
			$cS = 0;
			$i++;
		}
		$cS += $addaPrune->{$a};
		push(@list, $a);
	}
	return $i;
}

sub help {
  print STDERR <<HELP

Wibble

HELP
  
}


 
