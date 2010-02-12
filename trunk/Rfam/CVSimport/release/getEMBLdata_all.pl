#!/software/bin/perl -w


=head1 NAME 

getEMBLdata_all.pl

=head1 DESCRIPTION

Pull out all the Mole data we need for each EMBL seq to load into the rdb. 

In order to split into manageable chunks this process is run once for each tax_division in each dataclass.
This means for EMBL100 STD and WGS= 55 million seqs this is is run ~23 times. 
The data outputs to flat files in the nfs/ directory.
Single file for each class and tax
All the data for a single seq is in a single line of output file
order:
accession   version ncbi_id mol_type  length  description  previous-accs  source
AB000098        1       10116   mRNA    1468    Rattus norvegicus mRNA for MIPP65, complete cds.                EMBL;STD;ROD


All data for each sequence is stored in has %seq based on accession_version.
A single query to mole based on class and tax div is used to collate all the acc, seq_name, de lines  
A single query to mole based on class and tax div is used to get the ncbi tax ids for each seq

**sequences with more than one tax_id are currently bumped and parsed separately**as currently no way to get 
the correct one from mole.. working on it..
**outputs these accessions and the multiple tax ids to a file in /DATA/

The current version has two accession hacks for human sequenences that had no de lines in mole-problem in mole. I guess this will change from release to release so look into it.

=head1 PARAMETERS

Requires a tax division, data class and release version

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
  'tax:s'               => \$tax,
  'class:s'             => \$class,
  'rel:s'               => \$rel,
  'ovr'                 => \$overwrite,
);

if (!$tax || !$class || !$rel){
    die "Need to have a tax and $class and release dir\n";
}

my %seq;
my %idhash;  #hash for all accessions and list of ncbi taxid 
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
my $outfile="Data_".$class.".".$tax;

my $dups="Dups.".$class.".".$tax;

if (-e "$dir/$outfile" && !$overwrite){
       die "$outfile already exists-check you want to overwrite it and use the overwrite option\n"; 
} 

if (-e "$dir/$dups" && !$overwrite){
       die "$dups already exists-check you want to overwrite it and use the overwrite option\n"; 
} 

#seq source
my $string="EMBL;".$class.";".$tax;

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


#Mole prepare all the queries

#get the min and max entries which will be needed for chunking big datasets;
my $dsth = $dbh->prepare("select min(entry_id), max(entry_id) from entry where tax_division=? AND data_class=?" )
  or die '(EE) ERROR: couldn\'t prepare query to retreive descritpion data: ' . $dbh->errstr;


#NB left join important 
#left join as I found some accs with no entries in description
my $asth = $dbh->prepare("SELECT e.entry_id, e.accession_version, e.name, 
                          e.molecule_type, e.sequence_length, d.description  
                          FROM entry as e LEFT JOIN description as d ON e.entry_id=d.entry_id 
                          WHERE tax_division=? AND data_class=? 
                          AND e.entry_id >= ? and e.entry_id <= ? order by entry_id" )
  or die '(EE) ERROR: couldn\'t prepare query to retreive descritpion data: ' . $dbh->errstr;

#secondary accessions
#not all seqs have secondary accessions
my $bsth = $dbh->prepare("select  e.accession_version, a.accession from accession as a, entry as e 
                          where e.entry_id=a.entry_id 
                          AND a.qualifier='secondary' 
                          AND e.tax_division=? and e.data_class=?
                          AND a.entry_id >=? and a.entry_id <=?" )
  or die '(EE) ERROR: couldn\'t prepare query to retrieve secondary accessions: ' . $dbh->errstr;

#taxids
#all have at least one taxid, some have multiple
#selecting on taxonomy_id means the first(lowest numbered) is the correct one.
my $csth = $dbh->prepare("select e.accession_version, t.taxonomy_id, 
                          t.ncbi_tax_id from taxonomy as t, entry as e 
                          where t.entry_id=e.entry_id 
                          AND tax_division=? and data_class=?
                          AND e.entry_id >=? and e.entry_id <=?
                          order by t.taxonomy_id")
    or die '(EE) ERROR: couldn\'t prepare query to retrieve ncbi tax ids: ' . $dbh->errstr;


 

##MAIN----------------------------------------------------------------------------------------


#see if it is a huge dataset; we need to chunk up the ids
print STDERR "(ii) Doing query to get min and max entry_ids for $tax and $class;\n";
my $minmax;
unless(  $minmax= getMaxMin($tax, $class) ) {
    print STDERR "(EE) ERROR: couldn't get the min and max for this tax and class\n";
    exit();
}

my ($start, $max)=(@{$minmax->[0]});

my $incr= 50000; #chunk size

print STDERR "entry_id range= start = $start, end= $max\n";
CHUNK:while ( $start <= $max){

    my $end= $start + $incr;
    
    my %seq;
    my %idhash;  #hash for all accessions and list of ncbi taxid 
    my %preAcchash;  #hash for all previous accessions for each seq 
    my @accList; # single ncbi_accessions
    my @multi;  # multiple ncbi_accessions

    print STDERR "dealing with $start -> $end\n";

    ##get a list of ALL entires for this tax_division and data class
    print STDERR "(ii) Doing query to get all the accessions and de lines for $tax and $class in this set;\n";
    my $delines;
    unless(  $delines= getDElines($tax, $class, $start, $end) ) {
        print STDERR "(EE) ERROR: no data for this tax and class\n";
        next;
    }
    
    #parse the DE lines and store data for each seq :
    foreach my $entry (@$delines){
        my ($entry_id, $accession_version, $name, $mol, $length, $desc)=@$entry;
        $accession_version=~ /(\S+)\.(\d+)$/ or die "(EE) Dont get a match on the accession version so problems\n";
        my $ac=$1;
        my $av=$2;
             
        #hacks for problem HUM seqs
        if ($tax eq "HUM" && $class eq "STD"){
            if ($ac eq "AL118499") {$desc="Human DNA sequence from clone RP5-1168M15 on chromosome 20 Contains the C20orf109 gene similar to ubiquitin, the 5' end of the CTNNBL1 gene for catenin beta like 1 and two CpG islands, complete sequence";}
            if ($ac eq "Z93041") {$desc="Human DNA sequence from clone LA16c-366D3 on chromosome 16,complete sequence.";}
        }

        #some de lines are very long and have wierd tabs at end
        my $de=substr($desc, 0, 240);
        $de=~s/\t.*//g;
        $de=~s/\n.*//g;  

        if ( !$entry_id || !$accession_version || !$name || !$mol || !$length || !$de || !$ac | !$av){
            print STDERR "(EE) Missing data in", join("|", $entry_id, $accession_version, $name, $mol, $length, $de), "\n";
        }
        $seq{$accession_version}={ 'acc' => $ac, 
                                   'name' => $name,
                                   'mol'  => $mol,
                                   'len' => $length,
                                   'desc' => $de,
                                   'version' => $av,
                                   'source' => $string,
			   };
    }
    
    #if there are no seq in this set move onto next chunk.
    my $DEEntries=@$delines;
    print STDERR "'$DEEntries' DE entries are in mole for $tax and $class division\n";
    if ($DEEntries==0) {
        $start=$end + 1;
        $end='';
        next CHUNK; 
    } 
 
######ACCESSIONS###-------------------------------------------------------------

##get all the previous accession and store for each accession
    print STDERR "(ii) Doing query to get all the previous accessions for $tax and $class in this set;\n";
    my $preAccs;
    unless(  $preAccs= getAccs($tax, $class, $start, $end )) {
        print STDERR "(EE) ERROR: no data for this tax and class\n";
        next;
    }
    
##Parse the previous accession for each acc.
    foreach my $prev (@$preAccs){
        my ($acc_v, $pa)=@$prev;
        #store all the the prvious accessions
         push (@{$preAcchash{$acc_v}}, $pa);
    }
    
    my $preAccEntries= keys(%preAcchash);
    print STDERR "'$preAccEntries' sequences with previous accessions for $tax and $class division\n";
    
####  NCBI TAX IDS  #####---------------------------------------------------------
    
##get a list of ALL ncbi_tax_id entires for this tax_division and data class
    print STDERR "(ii) Doing query to get all the tax ids;\n";
    my $data;
    unless(  $data= getNCBI($tax, $class, $start, $end )) {
        print STDERR "(EE) ERROR: no data for this tax and class\n";
        next;
    }

##Parse the mole data for nbi_tax_ids.
##store IDS for each accession:
    foreach my $a (@$data){
        my ($acc_v, $junk, $ncbiId)=@$a;
         push (@{$idhash{$acc_v}}, $ncbiId);
    }
    
##Identify which seqs have greater than one ncbi_id
#this section can go-more for interest than anything else
#this is still essentially a hack to get around the problems of duplicate ncbi_ids.
    foreach my $GBacc (keys(%idhash)){
        if (@{$idhash{$GBacc}} > 1){
            #add to array to process later
            push(@multi, $GBacc);
        }
        elsif ( @{$idhash{$GBacc}} == 1) {
            push(@accList, $GBacc);
        }else{
            die "no ncbi data for $GBacc so problems!";
        }    
    }
    
### STORE THE ACCESSIONS WITH MULTIPLE TAXIDS
open(DUPS, ">>$dir/$dups") || die "Can't open the $dups file\n";
foreach my $a (@multi){
    print DUPS join("\t", $a, @{$idhash{$a}}), "\n";
}    
close(DUPS)  ;  

  print STDERR "Entries obtained with multiple_taxids= " . @multi, "\n";
  print STDERR "Entries with single taxids= ". @accList, "\n";


### COLLATE data for each accession #####------------------------------------
   
    open(OUT, ">>$dir/$outfile") || die "Cant open the outfile for writing to$!";
    @accList=sort(@accList);
    foreach my $acc (@accList){
        #print $acc;    
        ##deal WITH PREVIOUS ACCS
        if (defined @{$preAcchash{$acc}} ){
            $seq{$acc}->{'previous_acc'}= join(";", @{$preAcchash{$acc}});
        }else {


            $seq{$acc}->{'previous_acc'}= '';
        }
    
       #assign the first tax_id
        unless ($seq{$acc}->{'ncbi'}=$idhash{$acc}->[0]){
            die "No ncbi id found for $acc\n";
        }
            print OUT join("\t", $seq{$acc}->{'acc'},
                           $seq{$acc}->{'version'},
                           $seq{$acc}->{'ncbi'},
                           $seq{$acc}->{'mol'},
                           $seq{$acc}->{'len'},
                           $seq{$acc}->{'desc'},
                           $seq{$acc}->{'previous_acc'},
                           $seq{$acc}->{'source'} ), "\n";

    
        
    } # end of all accesions
    close (OUT);
    
    $start=$end + 1;
    $end='';
    
}#end of while 
##tidy up
    $dbh->disconnect; #mole


#----------------------------------------------
#methods----------------------------------------

sub getMaxMin {
    my ($tax, $class)=@_;
    
    $dsth->execute($tax, $class);
    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get min/max data: "
	    . $dbh->errstr . "\n";
        return;
    }
    my $row = $dsth->fetchall_arrayref();
     if( $dsth->err ) {
        print STDERR "(WW) WARNING: error whilst retrieving query dsth"
            . $dbh->errstr . "\n";
        return;
    }
    
    return $row;
}


sub getDElines{
    my ($tax, $class, $start, $end)=@_;
    
    $asth->execute($tax, $class, $start, $end);

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

    my ($tax, $class, $start, $end)=@_;
    
    $bsth->execute($tax, $class, $start, $end);

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



sub getNCBI {

    my ($tax, $class, $start, $end)=@_;
 
    $csth->execute($tax, $class, $start, $end);

    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get ncbi tax ids: "
	    . $dbh->errstr . "\n";
        return;
    }
    
    my $row = $csth->fetchall_arrayref();
    if( $csth->err ) {
        print STDERR "(WW) WARNING: error whilst retrieving query csth"
            . $dbh->errstr . "\n";
        return;
    }
    
    return $row;
}







#############################################################################
