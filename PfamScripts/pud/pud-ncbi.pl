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
use Net::SCP;

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


foreach my $line (split(/\n/, $content)){
  
  if( $line =~ /(gb\w+.fsa_aa.gz)/){
    $logger->info( " Downloading file $1.....");
    getstore( $NCBI_PATH."/$1","$store_dir$1" ) || $logger->logdie( "cant store the file $1 in $store_dir/$1");    
  
  } # end of $line =~ pattern   

} # end of foreach ( split )

# when it reaches here all the files are present in the ncbi directory and now parse each file to get the fasta sequence;

my $counter = 1;
my $total_seqs = {};

# now get all the files from the directory and parse it to append the fasta sequences to make a single file.

foreach my $file ( glob( "$store_dir/*.gz" ) ) {
  
  $logger->info( "now working on file number $counter: $file");
 
  ( $total_seqs->{ $file }->{ gi_lines }, $total_seqs->{ $file }->{ seq_lines } ) = gz2string( $file, $logger );
  
  $counter++;
}# end of foreach( files )

# now check whether the total lines are equal to file or not;
my $total_gi = 0 ;
my $total_seq_in_file= 0 ;

foreach( keys %{ $total_seqs }){
  $total_gi += $total_seqs->{ $_ }->{ gi_lines } if( defined $total_seqs->{ $_ }->{ gi_lines } );
  $total_seq_in_file+= $total_seqs->{ $_ }->{ seq_lines } if( defined $total_seqs->{ $_ }->{ seq_lines } );
}

$logger->info("total gi:$total_gi|$total_seq_in_file|");

# copy the file from local to pfamdb2a machine using scp;
# currently the name is hard-coded, should discuss with rob and decide about it.

my $scp = Net::SCP->new( { "host"=> $pfamDB->{host} } );
my $f1 = "$production_location/pfamseq".$VERSION."/ncbi.dat";
$scp->put("$f1", "/tmp/ncbi.dat") or $logger->logdie("Could not scp ncbi.dat to ".$pfamDB->{host}. " " . $scp->{errstr});

#get the dbh handle for the database specifed in the config file;


# now delete the contents of the ncbi_seq table;
eval{
  $dbh->do( "delete from ncbi_seq "); 
};
if( $@ ){
  $logger->logdie( "Could not delete the contents of the table: ncbi_seq".$dbh->errstr );
}

my $sth = $dbh->prepare( "LOAD DATA INFILE '/tmp/ncbi.dat' INTO TABLE ncbi_seq" );

# catch the errors, if encountered;
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

if( $total_db_sequences !=  $total_seq_in_file){
  $logger->logdie( "Diffence in total ncbi sequence count: sequences in fasta file: $total_seq_in_file, sequences in database: $total_db_sequences ");
}else {
  
  # DOUBLE CHECK WITH ROB AS THIS PIECE OF SNIPPET WILL CHANGE THE PFAM CONFIG EACH TIME.
  $logger->info("Changing pfam config file\n");

  my $config = new Config::General($ENV{PFAM_CONFIG}); 
  my %ac = $config->getall; 
  
  my $pfam_config = $ENV{PFAM_CONFIG};
  move( $pfam_config, "$pfam_config.$$.old") || $logger->logdie("Could not move config file");;
  $ac{ ncbi }->{ dbsize } = $total_seq_in_file; 

  SaveConfig( $pfam_config, \%ac );
  
  # checking whether we could get a proper config object
  my $newConfig;
  eval { 
    $newConfig = Bio::Pfam::Config->new; 
  };
  
  if($@) { 
    $logger->logdie("Problem modifying the pfam_config ($pfam_config) file");
  }

}

exit;


#---------------------------------------------------------------------------------------------------------------------------------
# subroutine gz2string uncompress the fasta file and parses the contents to create two kinds of output,
# as tab delimited file and other as fasta file with all sequences in single file.

sub gz2string {
  my ( $file, $logger ) = @_;
  
  open( OUT, ">>$production_location/pfamseq".$VERSION."/ncbi.dat") || 
    $logger->logdie( "cant open the file ncbi.dat for writing ");
  open( OUT1,">>$production_location/pfamseq".$VERSION."/ncbi") || 
    $logger->logdie( "cant open the file ncbi for writing");
  
  open( IN, "gunzip -c $file | ") || $logger->logdie( "cant open the file $file for reading ");
  my ( $gi, $gb, $gb_prefix, $desc, $sequence, $line_count, $gi_lines, $seq_lines );
  while( <IN> ){
    $line_count++;
    chomp;
    # some of the lines also contain dbj or emb instead of gb so i have explicitly added those as well
    if( $_ =~ /^\>gi[|](\d+)[|](gb|dbj|emb)[|]([A-Za-z0-9\.]+)[|](.*)$/){
      
      my $md5 = md5_hex( $sequence ) if( defined $sequence );
      my $ter_acc = '\N';
      
      #The tab delimited file
      print OUT "$gi\t$gb\t$ter_acc\t$md5\t'$desc'\t" if( defined $gi and  defined $gb and defined $md5 and defined $desc );      
      print OUT length( $sequence )."\t$sequence\n" if( defined $sequence );  
      
      
      #The fasta file
      if( defined $gi and  defined $gb and defined $md5 and defined $desc ){
        $gi_lines++;
        print OUT1 ">gi|$gi|$gb_prefix|$gb|$desc\n";
      }
        
      if( defined $sequence ){
        $seq_lines++;
        print OUT1 "$sequence\n";
      }
      # now i uninitialise the value of sequence and other variables, for the next sequence;
      $sequence = '';
      ( $gi,$gb_prefix, $gb, $desc ) = ( $1, $2, $3 ,$4);
      
    }elsif( $_ =~ /^([A-Z]+)$/){
      $sequence .= $1;
    }
    else{
      $logger->logdie( "|$file| contains invalid character at line $line_count\n");
    }
    
  } # end of while
  close IN;
  $logger->info( "the total lines present in $file is $line_count" ); 
  close OUT;
  close OUT1;
  return ( $gi_lines,$seq_lines ) ;
}
