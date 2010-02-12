#!/software/bin/perl -w

=head1 NAME

getEMBLseq.pl

=head1 DESCRIPTION

dowloads all of the sequences we need from mole.
We need the embl_sv.txt files (list of all rfamseq accessions in order along with lengths)
This is obtained by a simple command line perl from the DATA files

Reads in the list of accessions line at a time
stores up data until have set with total bp <=500,000,000
When reached appropriate dataset;
Parses through the accessions, gets the seq from mole and gets the de line.
Writes the set out to a file

Starts counter at zero again and does next set of seqs.

Note currently has hacks in subroutine for the two human seqs with bad DE lines (see subroutines)

**for next sequence update (11) need to get it to generate the seIndex file at same time-last time didnt realise this file was needed until after this script was run so got data with hacky makeIndex.pl. Simply need a flat text file with dbfilename\tfirstseqname\tlastseqname



=head1 PARAMETERS

Requires a SORTED embl_sv.txt file and release version

=head1 AUTHOR jd7@sanger.ac.uk

=cut



use strict;

use DBI;
use Rfam;
use Getopt::Long;
use File::Path;

my $rel;
&GetOptions(
   'rel:i' => \$rel
);

if (!$rel){
    die "Need to have a release dir\n";
}

#$infile directory
my $rootdir=$Rfam::rfamseq_root_dir;
my $infile="$rootdir/$rel/DATA/embl_sv.txt";

#my outfile
my $filecount=1;
my $pad_len=3;
my $fileId=sprintf ("%0${pad_len}d", $filecount);
my $outfile="$rootdir/$rel/rfamseq".$rel."_";
my $counter=0;
my $seqs=[];


# RDB stuff-----------------------------

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

#get the sequence for a single acc from mole
my $asth = $dbh->prepare(" select sequence from sequence as s, entry as e where e.entry_id=s.entry_id and e.accession_version=?" )
  or die '(EE) ERROR: couldn\'t prepare query to retreive descritpion data: ' . $dbh->errstr;

#get the description for acc from mole.
my $bsth = $dbh->prepare(" select description from description as d, entry as e where e.entry_id=d.entry_id and e.accession_version=?" )
  or die '(EE) ERROR: couldn\'t prepare query to retreive descritpion data: ' . $dbh->errstr;



#-----------------------------------
#main

#open the list of accessions and lengths
open(IN, "<$infile") || die "cant open the $infile\n";
while (my $line=<IN>){
    my ($accV, $len)=split("\t",$line);
    #open a file
    if (($counter+$len) <= 500000000){    
        $counter+=$len;
        push(@$seqs, $accV);
    }else{
        #print the set to file with indexed id
        my $out=$outfile."$fileId.fa";
        open(OUT, ">$out") || die "cant open the $out\n";
        foreach my $s (@$seqs){
            #get seq from mole
            my $seqstring;
                unless(  $seqstring= getSeq($s) ) {
                    print STDERR "(EE) ERROR: no seq for this acc\n";
                    next;
                }
            my $desc;
                unless(  $desc= getDe($s) ) {
                    print STDERR "(EE) ERROR: no desc for this acc\n";
                    next;
                }
            
           
            print OUT ">$s $desc\n$seqstring\n";
            
        }
        close (OUT);
        #increment file name
        ++$filecount;
        $fileId=sprintf ("%0${pad_len}d", $filecount);
        $seqs=[$accV];
        $counter=$len;
        
    }
    
} #end of infile

#print the last one

my $out=$outfile."$fileId.fa";
open(OUT, ">$out") || die "cant open the $out\n";
foreach my $s (@$seqs){
    #get seq from mole
    #print to file
    my $seqstring;
    unless(  $seqstring= getSeq($s) ) {
        print STDERR "(EE) ERROR: no seq for this acc\n";
        next;
    }
    my $desc;
    unless(  $desc= getDe($s) ) {
        print STDERR "(EE) ERROR: no desc for this acc\n";
        next;
    }
    
    
    print OUT ">$s $desc\n$seqstring\n";
    
}

close (OUT);

#tidy up
close (IN);
$dbh->disconnect();



#subroutins----------------------
sub getDe {
     my $acc=shift;

     $bsth->execute($acc);
     if( $DBI::err ) {
         print STDERR "(WW) WARNING: error executing  query to get de lines "
             .  $dbh->errstr . "\n";
         return;
     }

     my $row =  $bsth->fetchrow();
     if( $bsth->err ) {

        print STDERR "(WW) WARNING: error whilst retrieving query bsth"
             . $dbh->errstr . "\n";
         return;
     }
     $bsth->finish();
     if ($acc eq "AL118499.39") {$row="Human DNA sequence from clone RP5-1168M15 on chromosome 20
Contains the C20orf109 gene similar to ubiquitin, the 5' end of the CTNNBL1 gene for catenin beta like 1 and two CpG islands, complete sequence";}
     if ($acc eq "Z93041.2") {$row="Human DNA sequence from clone LA16c-366D3 on chromosome 16,complete sequence.";}
     my $de=substr($row, 0, 240);
     $de=~s/\t.*//g;
     $de=~s/\n.*//g;
     return $de;
 

 }
sub getSeq {
     my $acc=shift;

     $asth->execute($acc);
     if( $DBI::err ) {
         print STDERR "(WW) WARNING: error executing  query to get seq data"
             .  $dbh->errstr . "\n";
         return;
     }

     my $rows =  $asth->fetchall_arrayref();
     if( $asth->err ) {

        print STDERR "(WW) WARNING: error whilst retrieving query asth"
             . $dbh->errstr . "\n";
         return;
     }
    
     $asth->finish()
     #join data
     my $seq;
     foreach my $r (@$rows){
         $seq.=$r->[0];
     }
     return $seq;
 }
  
