#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use LWP::Simple;
use Log::Log4perl qw( :easy );
use Data::Dump qw( dump );
use Digest::MD5 qw( md5_hex );
use Config::General qw( SaveConfig );
use Getopt::Long;
use File::Copy;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
# get the version of pfam as a command line argument;

my ($version, $skip);

GetOptions (
  'version=i' =>  \$version,
  'skip'      =>  \$skip
); 


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

unless($version){
  $logger->logdie("You need to provide the version number as a parameter");  
}

# get the config file for the pfam.
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin });
my $dbh = $pfamDB->getSchema->storage->dbh;

my $NCBI_PATH = 'ftp://ftp.ncbi.nih.gov/ncbi-asn1';
my $store_dir = $config->localDbsLoc."/ncbi" || $logger->logdie( "cant get the localDbsLoc from the config file;");
my $production_location = $config->{ productionLocation } || $logger->logdie( "cant get productionLocation from config file");
my $nfs_location = $config->{ ncbi }->{ location } || $logger->logdie( "cant get the location( nfs) from the config file");
my $pfamseqDir = $production_location."/pfamseq".$version;
my $pfamseqNcbiFile = $pfamseqDir.'/ncbi';
my $pfamseqNcbiFileIdx = $pfamseqNcbiFile.'.ssi';

$logger->debug( "Production-Location :$production_location |nfs-location: $nfs_location ");

$logger->info( "Downloading file release number.....[ $store_dir]");
getstore( $NCBI_PATH."/GB_Release_Number","$store_dir/GB_Release_Number" ) 
  || $logger->logdie( "cant store the file GB_Release_Number in $store_dir/GB_Release_Number");    

#Download the fasta file from NCBI
my $file = "$store_dir/nr.gz";
unless($skip){
  $logger->info( "Downloading file nr.gz.....");
  $NCBI_PATH = 'ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/';
  getstore($NCBI_PATH."/nr.gz", $file) 
    || $logger->logdie("Can not store nr.gz in $file");
  $logger->info("downloaded nr.");
}

# now get all the files from the directory and parse it to append the fasta sequences to make a single file.
#This separates out the NR header lines into one per sequence.....
if(!-e $pfamseqNcbiFile){
  parseAndWriteFasta( $file, $logger, $pfamseqDir );
}

if(! -e $pfamseqNcbiFileIdx){
  # now index the file using esl-sfetch;
  my $cwd = getcwd;
  chdir($pfamseqDir) or 
    $logger->logdie("Could not chdir to $pfamseqDir:[$!]");
  
  system( "esl-sfetch --index ncbi ") 
    and $logger->logdie( "cant index the file using esl-sfetch");
}

my $numberSeqs = 0;
open(STATS, "esl-seqstat $pfamseqNcbiFile |") or 
  $logger->logdie("Could not run esl-seqstat on $pfamseqNcbiFile:[$!]");
while(<STATS>){
  chomp;
  if(/Number of sequences\:\s+(\d+)/){
    $numberSeqs = $1;
    last;
  }
}
close(STATS);

# copy the sequence file, index file and dat file to /nfs/pfam_nfs/pfam/ncbi/
copy( $pfamseqNcbiFile,"$nfs_location/ncbi") || 
  $logger->logdie( "cant copy the file $pfamseqNcbiFile");
copy( $pfamseqNcbiFileIdx,"$nfs_location/ncbi.ssi") || 
  $logger->logdie( "cant copy the file $pfamseqNcbiFile");

# now change the config file;
$logger->info("Changing pfam config file\n");

  my $configNew = new Config::General($ENV{PFAM_CONFIG}); 
  my %ac = $configNew->getall; 
  
  my $pfam_config = $ENV{PFAM_CONFIG};
  copy( $pfam_config, "$pfam_config.$$.old") || $logger->logdie("Could not move config file");;
  $ac{ ncbi }->{ dbsize } = $numberSeqs; 

  SaveConfig( $pfam_config, \%ac );
  
  # checking whether we could get a proper config object
  my $newConfig;
  eval { 
    $newConfig = Bio::Pfam::Config->new; 
  };
  
  if($@) { 
    $logger->logdie("Problem modifying the pfam_config ($pfam_config) file");
  }
  $config = $newConfig;

exit;


#---------------------------------------------------------------------------------------------------------------------------------
# subroutine uncompress the fasta file and parses the contents to create two kinds of output,
# as tab delimited file and other as fasta file with all sequences in single file.

sub parseAndWriteFasta {
  my ( $file, $logger, $dir ) = @_;
  
  open( OUT1,">$dir/ncbi") || 
    $logger->logdie( "cant open the file ncbi for writing");
  my($head, $sequence, $gi_lines, $seq_lines, $line_count);
  open( IN, "gunzip -c $file | ") || $logger->logdie( "cant open the file $file for reading ");
  while( <IN> ){
    $line_count++;
    chomp;
    # some of the lines also contain dbj or emb instead of gb so i have explicitly added those as well
    if( $_ =~ /^\>gi[|].*$/){
      
      my $ter_acc = '\N';
      if(defined($head) and defined($sequence)){ 
        parseAndWrite($head, $sequence, \*OUT1);
        $gi_lines++;
        $seq_lines++;
      }
      $sequence = undef;
      $head = $_;
    }elsif( $_ =~ /^([A-Z]+)$/){
      $sequence .= $1;
    }
    else{
      $logger->logdie( "|$file| contains invalid character at line $line_count\n - $_ \n");
    }
    
  } # end of while
  close IN;
  parseAndWrite($head, $sequence, \*OUT1)
      if(defined($head) and defined($sequence));
  close OUT1;
  return ( $gi_lines,$seq_lines ) ;
}

sub parseAndWrite {
  my($head, $sequence, $fh1) = @_; 
  
  my $md5 = md5_hex( $sequence ) if( defined $sequence );
  my $l = length( $sequence );
  my @heads = split( /\001/, $head );
  my @pheads;
  foreach my $h (@heads) {
    if ( my ( $gi, $gb_prefix, $gb, $ter_acc, $desc, $os ) =
      $h =~ /^\S?gi\|(\d+)\|(\S+)\|(\S+)?\|(\S+)? (.*?)(\[.*\])?$/ )
    {
      $ter_acc = ($ter_acc ? $ter_acc : '\N');
      $gb = ($gb ? $gb : '\N');
      $desc .= " $os" if($os); 
      #print out redundant fasta file:
      print $fh1 ">$gi $gb_prefix|$gb $desc\n$sequence\n";
    }
    else {
      die "NR header $h did not match\n";
    }
    last;
  }
}
