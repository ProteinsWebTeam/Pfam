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
  'skip'      =>  \$skip
); 

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


# get the config file for the pfam.
my $config = Bio::Pfam::Config->new;

my $NCBI_PATH = 'ftp://ftp.ncbi.nih.gov/ncbi-asn1';
my $store_dir = $config->localDbsLoc."/ncbi" || $logger->logdie( "cant get the localDbsLoc from the config file;");
my $nfs_ncbi_dir = $config->{ ncbi }->{ location } || $logger->logdie( "cant get the location( nfs) from the config file");
$logger->debug( "Ncbi file will be copied to $nfs_ncbi_dir");


#Download the fasta file from NCBI
my $file = "$store_dir/nr.gz";
unless($skip){
  $logger->info("Removing old files in $store_dir");
  unlink(glob "$store_dir/*");
  $logger->info( "Downloading file release number.....[ $store_dir]");
  getstore( $NCBI_PATH."/GB_Release_Number","$store_dir/GB_Release_Number" ) 
    || $logger->logdie( "cant store the file GB_Release_Number in $store_dir/GB_Release_Number");   

  $logger->info( "Downloading file nr.gz.....");
  $NCBI_PATH = 'ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/';
  getstore($NCBI_PATH."/nr.gz", $file) 
    || $logger->logdie("Can not store nr.gz in $file");
  $logger->info("downloaded nr.");

  system("gunzip $file") and $logger->logdie("Failed to gunzip $file, $!");
}

#Index file
my $cwd = getcwd;
$logger->info("Indexing nr");
chdir($store_dir) or $logger->logdie("Couldn't chdir into $store_dir, $!");
system( "esl-sfetch --index nr") and $logger->logdie( "Can't index $store_dir/nr using esl-sfetch, $!");
chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");

#Copy ncbi file to $nfs_ncbi
$logger->info("Copying to $nfs_ncbi_dir");
my $ncbi_fasta = $store_dir."/nr";
copy( $ncbi_fasta, "$nfs_ncbi_dir/ncbi") ||  
  $logger->logdie( "Failed to copy $ncbi_fasta to $nfs_ncbi_dir/ncbi");
  copy( $ncbi_fasta.".ssi","$nfs_ncbi_dir/ncbi.ssi") ||  
    $logger->logdie( "Failed to copy $ncbi_fasta.ssi to $nfs_ncbi_dir/ncbi.ssi");

my $numberSeqs=0;
open(STATS, "esl-seqstat $nfs_ncbi_dir/ncbi |") or 
  $logger->logdie("Could not run esl-seqstat on $nfs_ncbi_dir/ncbi:[$!]");
while(<STATS>){
  chomp;
  if(/Number of sequences\:\s+(\d+)/){
    $numberSeqs = $1;
    last;
  }
}
close(STATS);


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
#This is now obsolete
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
