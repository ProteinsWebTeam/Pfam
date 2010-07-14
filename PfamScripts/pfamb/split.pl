#!/software/bin/perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;



## To Use log4per......
use Log::Log4perl qw(get_logger :levels);
Log::Log4perl->init( \<<EOF
log4perl.rootLogger=WARN, SCREEN
# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);

my $logger = get_logger();
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

my $sqlDelOld = "DROP TABLE IF EXISTS adda";
$dbh->do($sqlDelOld) or $logger->logdie("Error deleting:".$dbh->errstr);

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

#Prepare the insert statement.
my $sth = $dbh->prepare("insert into adda (adda_acc, seqacc, version, start, end) values(?,?,?,?,?)");


my $filename = 	"adda_release2009.domains";
my $noJobs   =  "3000";
my($addaData, $totSeq) = parseAddaData($sth, $filename);
my $actualJobs = makeAccList($addaData, $totSeq, $noJobs);

sub parseAddaData {
	my($sth, $file) = @_;
	my $adda;

	open(ADDA, $file) || die "Could not open $file:[$!]\n";
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

	my ($addaPrune, $totSeq);

	foreach my $a (keys %$adda){
		next if($adda->{$a}->{seqs} > 500 or $adda->{$a}->{seqs} < 2);
		if($adda->{$a}->{residues} > 500000){
			print $logger->debug("$a has less than 500 sequences, but more than 500000 resdidues to align!");
		}
		if($adda->{$a}->{residues} > 500000){
			$logger->debug("$a has less than 500 sequences, but more than 500000 resdidues to align!");
			next;
		}
		if($adda->{$a}->{residues} < 40){
			$logger->debug("$a has 2-500  sequences, but less than 40 resdidues to align!");
			next;
		}
	
		foreach my $r (@{$adda->{$a}->{regions}}) {	
			#Okay - if we get here we want to store this formation;
			$sth->execute($a, $r->{seqacc}, $r->{version}, $r->{start}, $r->{end}) or
			$logger->logdie($dbh->errstr);
		}
		
		$addaPrune->{$a} = $adda->{$a}->{seqs};
		$totSeq += $adda->{$a}->{seqs};
	}
	return ($addaPrune, $totSeq);
}


sub makeAccList {
	my ($addaPrune, $totSeq, $noJobs) = @_; 

	my $chunk = (int( scalar($totSeq) / $noJobs ) ) + 1;

	my @addaList = sort{$a <=> $b} keys %$addaPrune;
	$logger->debug("There will be $chunk sequenecs");

	my $i =1;
	my ($cS, @list);
	$cS = 0;
	foreach my $a (@addaList){
		if($cS > $chunk){
			open(L, ">ADDA_acclist.$i") || die "Could not open ADDA acc list:[$!]\n";
		
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

