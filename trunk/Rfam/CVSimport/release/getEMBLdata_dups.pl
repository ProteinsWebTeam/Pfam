#!/software/bin/perl -w


=head1 NAME 

getEMBLdata_dups.pl


=head1 DESCRIPTION

a modified version of getEMBLdata_all.pl to deal with sequences that have > 1 ncbi_identifiers.
It requires that fixdups.pl has already been run and generated the file of resolved ncbi_mappings.

I then works pretty much as the getEMBLdata_all.pl

Read in the list of accessions and ncbi_ids- small set of ~ 4000

Pull out all the Mole data we need for each EMBL seq to load into the rdb. 


All data for each sequence is stored in has %seq based on accession_version.
A single query to mole to collate all the acc, seq_name, de lines  
Another query to geth all the previous acc data;
The taxonomy_id has already been assigned so write to file


=head1 PARAMETERS

Requires a release version and input file of resolved duplicate accessions

=head1 AUTHOR jd7@sanger.ac.uk

=cut


use strict;
use DBI;
use Rfam;
use Getopt::Long;
use File::Path;

my $tax;
my $class;
my $rel;
my $overwrite;

&GetOptions(
  'rel:s'               => \$rel,
  'ovr'                 => \$overwrite,
);

if (!$rel){
    die "Need to have a release dir\n";
}

my %seq;
my %preAcchash;  #hash for all previous accessions for each seq 

#for RDB data
my $rootdir=$Rfam::rfamseq_root_dir; 
my $dir="$rootdir/$rel/DATA";

umask(002);
if ( ! -d $dir){
     eval { mkpath($dir) };
     if ($@) {
         print "Couldn't create $dir: $@";
     }
   
 }

#outfile name
my $outfile="$dir/Data_STD_Duplicates";

if (-e "$outfile" && !$overwrite){
       die "$outfile already exists-check you want to overwrite it and use the overwrite option\n"; 
} 

my $infile="$dir/Duplicate_ncbi_list_resolved";
if (! -e "$infile" ){
       die "Cant find the input file $infile\n"; 
} 


######Mole DB connection parameters--------

my $dbName= 'embl_100';
my $dbHost='cbi3';
my $dbUser='genero';

#Mole connection and statement handles

my $dbAttr = { RaiseError => 1,
                           PrintError => 1 };
#Mole connect
my $dbh = DBI->connect("dbi:mysql:$dbName;$dbHost", $dbUser,"", $dbAttr )  
    or die "(EE) ERROR: couldn't connect to database: $!";

#NB left join important 
#left join as I found some accs with no entries in description
my $asth = $dbh->prepare("SELECT e.entry_id, e.molecule_type, e.sequence_length, 
                          e.data_class, e.tax_division, d.description 
                          FROM entry as e, description as d 
                          where e.entry_id=d.entry_id and e.accession_version =?" )
  or die '(EE) ERROR: couldn\'t prepare query to retreive descritpion data: ' . $dbh->errstr;

#secondary accessions
#not all seqs have secondary accessions
my $bsth = $dbh->prepare("select a.accession 
                          from accession as a, entry as e where e.entry_id=a.entry_id 
                          AND a.qualifier='secondary' and e.entry_id=?" )
  or die '(EE) ERROR: couldn\'t prepare query to retrieve secondary accessions: ' . $dbh->errstr;

##MAIN----------------------------------------------------------------------------------------


#read in the resolved accessions list:

open(IN, "<$infile") || die "cant open the input file\n";
my @rows=<IN>;
chomp @rows;
close(IN);

#parse each accession and ncbi-id

open(OUT,">$outfile") || die "Cant open the outfile\n";
foreach my $r (@rows){
    my ($accV, $ncbi)=split("\t", $r);
    print STDERR "$accV:$ncbi\n";
    $accV=~ /(\S+)\.(\d+)$/ or die "(EE) Dont get a match on the accession version so problems\n";
    my $ac=$1;
    my $av=$2;

    #get all the entry_id info
    print STDERR "(ii) Doing query to get description and entry_data;\n";
    my $delines;
    unless(  $delines= getDElines($accV) ) {
        print STDERR "(EE) ERROR: couldn't get the description data for $accV\n";        
    }
    
    
    #parse the DE lines and store data for each seq :
    my ($entry_id, $mol, $length, $class, $tax, $desc);
    foreach my $entry (@$delines){
                 ($entry_id, $mol, $length, $class, $tax, $desc)=@$entry;
                 print STDERR join(",", $entry_id, $mol, $length, $class, $tax, $desc), "\n";
                 #some de lines are very long and have wierd tabs at end
                 my $de=substr($desc, 0, 240);
                 $de=~s/\t.*//g;
                 $de=~s/\n.*//g;  
                 
                 if ( !$entry_id || !$mol || !$length || ! $class || !$tax || !$de) {
                     print STDERR "(EE) Missing data in", join("|",$accV, $entry_id, $mol, $length, $class, $tax, $de ), "\n";
                 }
                 
                 my $source="EMBL;".$class.";".$tax;
                 
                 #store the info
                 $seq{$accV}={ 'acc' => $ac, 
                               'mol'  => $mol,
                               'ncbi' => $ncbi,
                               'len' => $length,
                               'desc' => $de,
                               'version' => $av,
                               'source' => $source,
                           };
             }#end of storing de line info
                 
                 
                 #get all the previous accession and store for each accession
                 print STDERR "(ii) Doing query to get all the previous accessions for $accV\n";
    my $preAccs;
    unless(  $preAccs= getAccs($entry_id)) {
        print STDERR "(EE) ERROR: no previous acc data obtained for $accV\n";
    }
    
    #parse the previous accs
    
    foreach my $prev (@$preAccs){
         push (@{$preAcchash{$accV}}, $prev->[0]);
    }
  
    if (defined @{$preAcchash{$accV}} ){
        $seq{$accV}->{'previous_acc'}= join(";", @{$preAcchash{$accV}});
    }else {
        $seq{$accV}->{'previous_acc'}= '';
    }
    
    
    
    print OUT join("\t", $seq{$accV}->{'acc'},
                   $seq{$accV}->{'version'},
                   $seq{$accV}->{'ncbi'},
                            $seq{$accV}->{'mol'},
                            $seq{$accV}->{'len'},
                            $seq{$accV}->{'desc'},
                            $seq{$accV}->{'previous_acc'},
                            $seq{$accV}->{'source'} ), "\n";
    
         } # end of all accesions
    close (OUT);
    

     $dbh->disconnect; #mole


#----------------------------------------------
#methods----------------------------------------


sub getDElines{
    my $acc=shift;
    
    $asth->execute($acc);

    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get embl data: "
	    . $dbh->errstr . "\n";
        return;
    }
    
    my $row = $asth->fetchall_arrayref();
    if( $asth->err ) {
        print STDERR "(WW) WARNING: error whilst retrieving query asth"
            . $dbh->errstr . "\n";
        return;
    }
    
    return $row;
}




sub getAccs{

    my $entry=shift;
    
    $bsth->execute($entry);

    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get embl data: "
	    . $dbh->errstr . "\n";
        return;
    }
    
    my $row = $bsth->fetchall_arrayref();
    if( $bsth->err ) {
        print STDERR "(WW) WARNING: error whilst retrieving query bsth"
            . $dbh->errstr . "\n";
        return;
    }
    
    return $row;
}

