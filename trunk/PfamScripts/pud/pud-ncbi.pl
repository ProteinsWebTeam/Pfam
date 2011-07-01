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
use Bio::Pfam::PfamLiveDBManager;
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

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin });
my $dbh = $pfamDB->getSchema->storage->dbh;

$logger->debug( "Production-Location :$production_location |nfs-location: $nfs_location ");

# downloading the ncbi_index file using get method;
my $content = get( $NCBI_PATH ) || $logger->logdie( "cant download the Ncbi_index file from the path: $NCBI_PATH");

# now split the content using '\n' and look for a ncbi file pattern (gbXXXX.fsa_aa.gz )
# and store those files into the corresponding ncbi directory mentioned in the config file.


  
$logger->info( "Downloading file release number.....");
getstore( $NCBI_PATH."/GB_Release_Number","$store_dir/GB_Release_Number" ) || $logger->logdie( "cant store the file GB_Release_Number in $store_dir/GB_Release_Number");    

$logger->info( "Downloading file nr.gz.....");
$NCBI_PATH = 'ftp://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/';

unless (-e "$store_dir/nr.gz"){
  getstore($NCBI_PATH."/nr.gz", "$store_dir/nr.gz") || $logger->logdie("Can not store nr.gz in $store_dir/nr.gz");
}
$logger->info("downloaded nr.");
# when it reaches here all the files are present in the ncbi directory and now parse each file to get the fasta sequence;

my $counter = 1;
my $total_seqs = {};

# now get all the files from the directory and parse it to append the fasta sequences to make a single file.
my $file = "$store_dir/nr.gz";
$logger->info( "now working on file number $counter: $file");
 
( $total_seqs->{ $file }->{ gi_lines }, $total_seqs->{ $file }->{ seq_lines } ) = gz2string( $file, $logger );
  



## copy the file from local to pfamdb2a machine using scp;
## currently the name is hard-coded, should discuss with rob and decide about it.
system("scp $production_location/pfamseq".$VERSION."/ncbi.dat ". $config->pfamliveAdmin->{host} .":/tmp/ncbi.dat") 
  and $logger->logdie("Could not scp ncbi.dat to ".$config->pfamliveAdmin->{host}.":[$!]");

##get the dbh handle for the database specifed in the config file;


# now delete the contents of the ncbi_seq table;
eval{
  $dbh->do( "delete from ncbi_seq "); 
};
if( $@ ){
  $logger->logdie( "Could not delete the contents of the table: ncbi_seq".$dbh->errstr );
}

my $sth = $dbh->prepare( "LOAD DATA INFILE '/tmp/ncbi.dat' INTO TABLE ncbi_seq" );
#
## catch the errors, if encountered;
eval{
  $sth->execute();  
};

if( $@ ){
  $logger->logdie( "cant load the data from file to the ncbi_seq table ".$dbh->errstr );
}

# copy the file to /lustre/pfam/pfam/Production/pfamseq24

# now index the file using esl-sfetch;
chdir("$production_location/pfamseq$VERSION") or 
  $logger->logdie("Could not chdir to $production_location/pfamseq$VERSION:[$!]"); 
system( "esl-sfetch --index ncbi ") and $logger->logdie( "cant index the file using esl-sfetch");

# copy the sequence file, index file and dat file to /nfs/pfam_nfs/pfam/ncbi/

copy( "$production_location/pfamseq".$VERSION."/ncbi","$nfs_location/ncbi") || 
  $logger->logdie( "cant copy the file $production_location/pfamseq.$VERSION/ncbi");
copy( "$production_location/pfamseq".$VERSION."/ncbi.ssi","$nfs_location/ncbi.ssi") || 
  $logger->logdie( "cant copy the file $production_location/pfamseq.$VERSION/ncbi.ssi");

# now change the config file;

#get the total number of sequences from the ncbi_seq table;

my $ncbi_seq = $dbh->prepare("select count(*) from ncbi_seq");
$ncbi_seq->execute || 
  $logger->logdie("Query fail: couldn't calculate the total ncbi sequences present in database ".$ncbi_seq->errstr."\n");
my $total_db_sequences = $ncbi_seq->fetchrow;

  
# DOUBLE CHECK WITH ROB AS THIS PIECE OF SNIPPET WILL CHANGE THE PFAM CONFIG EACH TIME.
$logger->info("Changing pfam config file\n");

  my $configNew = new Config::General($ENV{PFAM_CONFIG}); 
  my %ac = $configNew->getall; 
  
  my $pfam_config = $ENV{PFAM_CONFIG};
  move( $pfam_config, "$pfam_config.$$.old") || $logger->logdie("Could not move config file");;
  $ac{ ncbi }->{ dbsize } = $total_db_sequences; 

  SaveConfig( $pfam_config, \%ac ) || $logger->logdie( "cant change the Pfam_config file");
  
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
  
  open( OUT, ">$production_location/pfamseq".$VERSION."/ncbi.dat") || 
    $logger->logdie( "cant open the file ncbi.dat for writing ");
  open( OUT1,">$production_location/pfamseq".$VERSION."/ncbi") || 
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
        parseAndWrite($head, $sequence, \*OUT, \*OUT1);
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
  parseAndWrite($head, $sequence, \*OUT, \*OUT1)
      if(defined($head) and defined($sequence));
  close OUT;
  close OUT1;
  return ( $gi_lines,$seq_lines ) ;
}

sub parseAndWrite {
  my($head, $sequence, $fh1, $fh2) = @_; 
  
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
      print $fh1 print OUT "$gi\t$gb\t$ter_acc\t$md5\t'$desc'\t$l\t$sequence\n";
      #print out redundant fasta file:
      print $fh2 ">$gi $gb_prefix|$gb $desc\n$sequence\n";
    }
    else {
      die "NR header $h did not match\n";
    }
    last;
  }
}
