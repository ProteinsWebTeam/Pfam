#!/software/bin/perl

use strict;
use warnings;
use LWP::Simple;
use Log::Log4perl qw( :easy );
use Data::Dump qw( dump );
use Digest::MD5 qw( md5_hex );
use Config::General qw( SaveConfig );
use Getopt::Long;
use File::Copy;
use Bio::Pfam::Config;
# get the version of pfam as a command line argument;

my $VERSION;

GetOptions (
  'version=i' =>  \$VERSION 
); 


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

unless($VERSION){
  $logger->logdie("You need to provide the version number as a parameter");  
}

# get the config file for the pfam.
my $config = Bio::Pfam::Config->new;

my $NCBI_PATH = 'ftp://ftp.ncbi.nih.gov/ncbi-asn1/protein_fasta';
my $store_dir = $config->localDbsLoc."/ncbi" || $logger->logdie( "cant get the localDbsLoc from the config file;");
my $production_location = $config->{ productionLocation } || $logger->logdie( "cant get productionLocation from config file");
my $nfs_location = $config->{ ncbi }->{ location } || $logger->logdie( "cant get the location( nfs) from the config file");

$logger->debug( "Production-Location :$production_location |nfs-location: $nfs_location ");

# downloading the ncbi_index file using get method;
my $content = get( $NCBI_PATH ) || $logger->logdie( "cant download the Ncbi_index file from the path: $NCBI_PATH");
  
$logger->info( "Downloading file release number.....");
getstore( $NCBI_PATH."/GB_Release_Number","$store_dir/GB_Release_Number" ) || $logger->logdie( "cant store the file GB_Release_Number in $store_dir/GB_Release_Number");    

$logger->info( "Downloading file nr.gz.....");
$NCBI_PATH = 'ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/';
mirror($NCBI_PATH."/nr.gz", "$store_dir/nr.gz") || $logger->logdie("Can not store nr.gz in $store_dir/nr.gz");
$logger->info("downloaded nr.");
# when it reaches here all the files are present in the ncbi directory and now parse each file to get the fasta sequence;

my $counter = 1;
my $total_seqs = {};

# now get all the files from the directory and parse it to append the fasta sequences to make a single file.
my $file = "$store_dir/nr.gz";
$logger->info( "now working on file number $counter: $file");

#This separates out the NR header lines into one per sequence..... 
my $num_seq = gz2string( $file, $logger );
  

# now index the file using esl-sfetch;
chdir("$production_location/pfamseq$VERSION") or 
  $logger->logdie("Could not chdir to $production_location/pfamseq$VERSION:[$!]"); 
if(-e "ncbi.ssi"){
  unlink("ncbi.ssi");
}
system( "esl-sfetch --index ncbi ") and $logger->logdie( "cant index the file using esl-sfetch");

# copy the sequence file, index file and dat file to /nfs/pfam_nfs/pfam/ncbi/
copy( "$production_location/pfamseq".$VERSION."/ncbi","$nfs_location/ncbi") || 
  $logger->logdie( "cant copy the file $production_location/pfamseq.$VERSION/ncbi");
copy( "$production_location/pfamseq".$VERSION."/ncbi.ssi","$nfs_location/ncbi.ssi") || 
  $logger->logdie( "cant copy the file $production_location/pfamseq.$VERSION/ncbi.ssi");

# now change the config file;
$logger->info("Changing pfam config file\n");

  my $configNew = new Config::General($ENV{PFAM_CONFIG}); 
  my %ac = $configNew->getall; 
  
  my $pfam_config = $ENV{PFAM_CONFIG};
  move( $pfam_config, "$pfam_config.$$.old") || $logger->logdie("Could not move config file");;
  $ac{ ncbi }->{ dbsize } = $num_seq; 

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
# subroutine gz2string uncompress the fasta file and parses the contents to create two kinds of output,
# as tab delimited file and other as fasta file with all sequences in single file.

sub gz2string {
  my ( $file, $logger ) = @_;

  open( OUT1,">$production_location/pfamseq".$VERSION."/ncbi") || 
    $logger->logdie( "cant open the file ncbi for writing");
  my($head, $sequence, $gis, $line_count);
  open( IN, "gunzip -c $file | ") || $logger->logdie( "cant open the file $file for reading ");
  while( <IN> ){
    $line_count++;
    chomp;
    # some of the lines also contain dbj or emb instead of gb so i have explicitly added those as well
    if( $_ =~ /^\>gi[|].*$/){
      
      my $ter_acc = '\N';
      if(defined($head) and defined($sequence)){ 
        $gis += parseAndWrite($head, $sequence, \*OUT1);
        
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
  $gis += parseAndWrite($head, $sequence, \*OUT1)
      if(defined($head) and defined($sequence));
  close OUT1;
  return $gis;
}

sub parseAndWrite {
  my($head, $sequence, $fh) = @_; 
  
  my $md5 = md5_hex( $sequence ) if( defined $sequence );
  my $l = length( $sequence );
  my @heads = split( /\001/, $head );
  my @pheads;
  my $count =0;
  foreach my $h (@heads) {
    if ( my ( $gi, $gb_prefix, $gb, $ter_acc, $desc, $os ) =
      $h =~ /^\S?gi\|(\d+)\|(\S+)\|(\S+)?\|(\S+)? (.*?)(\[.*\])?$/ )
    {
      $gb = ($gb ? $gb : '');
      $desc .= " $os" if($os); 
      #print out redundant fasta file:
      print $fh ">$gi $gb_prefix|$gb $desc\n$sequence\n";
      $count++;
    }
    else {
      die "NR header $h did not match\n";
    }
    last;
  }
  return $count;
}
