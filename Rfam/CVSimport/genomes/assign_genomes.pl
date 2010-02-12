#!/software/bin/perl -w

=head1 NAME

assign_genomes.pl

=head1 DESCRIPTION

Obtains the list of completed genomes for all domains (Euk, Prok etc) from the EBI
 http://www.ebi.ac.uk/genomes/.   It queries the RDB to see if these accessions are
in our rfamseq database or if they are in one of the CON and ANN files. Note this list
from the EBI is not related to the EMBL release so there is often several hundred 
genomes available that we cant annotate as not in our current version of EMBL.

Outputs a lookup file 'Genomes_assigned' which is needed for later code and a log file for info.
 
=head1 PARAMETERS

 ./assign_genomes.pl -rel 10.0 -db rfam_10_0

=head1 AUTHOR jd7@sanger.ac.uk

=cut

use strict;
use LWP;
use IO::File;
use DBI;         # DB access
use Rfam;
use Compress::Zlib;
use IO::File;
use IO::Socket;
use Getopt::Long;

my ($dir, $help, $release, $comparaVer, $db);

&GetOptions( "release=s" => \$release,
	     "db=s"     => \$db,
             "h|help" => \$help );


if( $help) {
    &help();
    exit(1);
}

my $rfamseq=$Rfam::rfamseq_current_dir;


if (! defined $release){
    warn "\nRelease must be specified e.g 9.0\n";
    &help();
    exit;
}

if (! defined $db){
    warn "\nThe relevant RDB  database must be specified e.g. rfam_9_0_new\n";
    &help();
    exit;
}

if ($db!~/rfam_\d+\_\d+/){
    warn "\nThe database name is in the wrong format?? should be like this: rfam_9_0_new\n";
    &help();
    exit;
}

if (!  -d "$rfamseq/CON_ANN"){
    warn "\nThere doesnt appear to be a $rfamseq/CON_ANN dir\n?";
    &help();
    exit;
}

print STDERR "Reading files from $rfamseq/CON_ANN\n";

my $IDfile="$rfamseq/CON_ANN/IDlist";

if (!  -e "$IDfile"){
    warn "\nThe IDlist file does not exist yet in teh CON_ANN dir. Run the command: grep -H ^ID rel*dat > IDlist \n ";
    &help();
    exit;
}

#location for ouput files- RELEASE DIR
my $outdir;
if( not -d "$rfamseq/Genomes\_$release/" ) {
    mkdir( "$rfamseq/Genomes\_$release/", 0755 ) or die "Cant make release directory $rfamseq/Genomes\_$release/ $!";
    $outdir="$rfamseq/Genomes\_$release/";
}else {
    $outdir="$rfamseq/Genomes\_$release/";
}
print STDERR "Writing files to $outdir\n";

my $report='Genomes_assigned';
if (-e "$outdir/$report" || -e "$outdir/log" ){
    warn "WARNING: The files 'Genomes_assigned' and 'log' already exists in $outdir. Are you sure you want to overwrite?\n";
	exit(1);
}


open(REPORT, ">$outdir/$report") || die "cant open outfile for report $!";
print REPORT join("\t", "#Source", "EBI_acc", "Rfam_acc", "EBI_NCBI_id", "Rfam_ncbi_id", "CONfile"), "\n";
open(LOG, ">$outdir/log") || die "cant open outfile for report $!";


#global variables used
my @acc; #list of genome accessions
my %mapping; #for keeping the auto_rfamseq for later?
my @unasssigned; # list of all genome accessions not in rfamseq or in CON files 
my %tax; #store the ebi tax id for each acc

#--------------------------------------------

#read in all the IDlist and sort into file:
print STDERR "Reading in the con file accessions ID list\n";
my %file;

open (ID, "<$IDfile") || die "problem reading in the $IDfile $!";
while (my $l = <ID>){
    #rel_ann_env_01_r100.dat:ID   DP000238; SV 1; circular; genomic DNA; ANN; ENV; 2045086 BP.
    if ($l=~/^rel_(\w{3}_\w{3}_\d+)_r\d+.dat\:ID\s+(\S+)\;\s+SV\s+(\d+)\;/){
	my $acc=$2;
	my $v=$3;
	my $f=$l;
	$f=~s/\:.*\n//g;
	$file{$acc}=$v."_".$f;
    }else{
	die "Problem with format of $l\n";
    }
}

print STDERR "Finished reading in the con file accessions\n";


#--------------------------------------------


#RDB stuff
#get the rdb info from Rfam.pm-open own conenction
# set up the DB connection and statement handles

my $dbName=$db;
my $dbHost=$Rfam::rdbHostDev;
my $dbPort=$Rfam::rdbPortDev;
my $dbUser=$Rfam::rdbUserDev;
my $dbPass=$Rfam::rdbPassDev;

my $dsn    = "dbi:mysql:$dbName:$dbHost:$dbPort";
my $dbAttr = { RaiseError => 1,
                           PrintError => 1 };

# connect
my $dbh = DBI->connect( $dsn, $dbUser, $dbPass, $dbAttr )
  or die "(EE) ERROR: couldn't connect to database: $!";

# prepare all the queries that we'll need

# query
my $asth = $dbh->prepare( 'SELECT version, ncbi_id from rfamseq where rfamseq_acc=? ' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

#---------------------------------------------------

print STDERR "GETTING EBI list of genomes\n";

my $ua = new LWP::UserAgent;
$ua->agent("AVAce Indexer/1.1");
$ua->proxy( [ 'ftp', 'http' ], 'http://wwwcache.sanger.ac.uk:3128' );

my @accRejected;
my @url = qw( http://www.ebi.ac.uk/genomes/archaea.details.txt 
              http://www.ebi.ac.uk/genomes/bacteria.details.txt
	      http://www.ebi.ac.uk/genomes/eukaryota.details.txt
	      http://www.ebi.ac.uk/genomes/phage.details.txt
	      http://www.ebi.ac.uk/genomes/virus.details.txt 
	      );
              


foreach my $url ( @url ) {
    my $req = new HTTP::Request GET => $url;
    my $res = $ua->request( $req );
    if( $res->is_success ) {
	&writeEBI($res->content, $url); 
	foreach ( split( /\n/, $res->content ) ) {
            unless (/^\#/){
		my @data=split ("\t", $_);
		$tax{$data[0]}=$data[3];
		push( @acc, $data[0] );
	    }
	}
    }
}
print LOG "Number of genome seq accessions ", scalar(@acc), "\n";

#-------------------------------------------------------------
#main##
print STDERR "Starting to sort the accessions from the EBI list\n";

my $rfcount=0;
my $concount=0;
my %conlist;

#ebi list of accessions
ACC:foreach my $acc (@acc){

    # split into accession and version
    $acc =~/^(\S+)\.(\d+)/; 
    my $accession=$1; #ebi
    my $version= $2;  #ebi
   
    #check if is in rfamseq - get version and tax id
    my $rfv;
    my $cf;
    my $rf_ncbi;

    my $row=&checkRfamseq($accession);

    if (defined $row->[0]){
	$rfv=$row->[0];
	$rf_ncbi=$row->[1];;
	++$rfcount;
	#print source/EBI accession and version/Rfam_acc&version/ebi taxid/rfam taxid
	print REPORT "Rfam\t$acc\t$accession\.$rfv\t$tax{$acc}\t$rf_ncbi";
	#report problems
	my $err;
	if ($acc ne "$accession\.$rfv"){
	    $err.="\tVER_ISSUE";
	}elsif($tax{$acc} != $rf_ncbi){
	    $err.="\tTAX_DIFF"; 
	}
	print REPORT "$err" if $err;
        print REPORT "\n";
    	next ACC;
    }
     
    #if not in Rfam check which con file to use
    if (defined $file{$accession}){
	$cf=$file{$accession};
	my $type;
	my $cv;
	if ($cf=~/^(\d+)_rel_(\S+)_r\d+.dat/){
	    $type=$2;
	    $cv=$1;
	}else{
	    die "Problem with the file format $file{$accession}\n";
	}

	++$concount;
	#note this is technically bad as the tax id is assigned on the ebi acc.v
	print REPORT "CON\t$acc\t$accession.$cv\t$tax{$acc}\tNA\t$type";
	my $err;
	if ($acc ne "$accession\.$cv"){
	    $err.="\tVER_ISSUE";
	}
	print REPORT "$err\n" if $err;
        print REPORT "\n";
	next ACC;

    }
       
    if (! defined $rfv && ! defined $cf) { push( @unasssigned, $acc);}
  
}# each acc


my $nocon=scalar(@unasssigned);

$dbh->disconnect;

print LOG "rfam=$rfcount, confiles=$concount unassigned=$nocon\n";
print LOG join("||",@unasssigned), "\n";

close(REPORT);
close(LOG);

 
###SUBROUTINES#######################


sub writeEBI {
    my ($data, $url) =@_;
    my $out;
    if ( $url=~/.*\/(.*.details.txt)$/){ $out=$1;}
    open(OUT,  ">$outdir/$out ") || die "Cant open outfile for writing compara data";
    print OUT $data;
    close(OUT);

} 

sub checkRfamseq {

  my $acc = shift;
      $asth->execute($acc);
      my @row = $asth->fetchrow(); 
      die '(EE) ERROR: error whilst retrieving rfamseq info: ' . $dbh->errstr . "\n"
	  if $DBI::err;
  
      $asth->finish;
      
      return \@row;
}


##################################################################################################
sub help {
    print STDERR <<EOF;

This gets the current list of genomes from ebi (eukaryote, prokaryote etc)  we are planning to annotate:
We use:

-ebi genomes list for each phylo catagory (downloaded using http request)

-generates a Genomes_$release directory in the current rfamseq dir on nfs
-we save a copy of ebi files -as a record of what was available at the time 
-these accs are then checked against the rfam rdb (ie do we have the complete genome seq in rfamseq?
-else is it in the con files:
-writes out a mapping for each genome-acc -> location where we will get the ag data from
-also adds the ebi_ncbi_tax id assigned with the acc and the ncbi_id we have for it in rfamseq.

-this two stages is good for the moment but essentially this should be merged with the make_genomes_agp.pl
-writes mapping out to file called 'Genomes_assigned' in release dir/Genomes
-writes out to 'log' file in release dir/Genomes

Usage:  assign_genomes.pl -rel 9.1 -db rfam_9_0_new

Options:       -h                  show this help
	       -rel                release identifier
	       -db                 database to look up rfamseqs
EOF

}
