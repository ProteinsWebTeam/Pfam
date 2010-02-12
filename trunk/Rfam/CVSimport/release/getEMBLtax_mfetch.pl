#!/software/bin/perl -w

=head1 NAME 

getEMBLtax_mfetch.pl

=head1 DESCRIPTION

Used when doing the rfamseq updte after the sequence data has been loaded into rfamseq.
This code is used to get the taxonomy information for all of the sequences in rfamseq database.
Selects all the unique ncbi_ids, and a relevant accession for it- stores this output in /DATA/ as an archive

Uses batches of mfetch to get he species and tax string for an accession.
creates the data for loading into taxonomy table.

NB I didnt use the mole-tree (ls and rs...stuff) as the results obtained with it were slightly different than
when I used mftetch. Long discussions with pg5 - we decided to stick with mfetch OS and OC strings for now.
 

=head1 PARAMETERS

release version needed.
need to specify/change the rdb_name for the new seq database.

=head1 AUTHOR jd7@sanger.ac.uk

=cut


use strict;
use DBI;
use Rfam;
use Getopt::Long;

my ($rel, $overwrite);
&GetOptions(
  'rel:s'               => \$rel,
  'ovr'                 => \$overwrite,
);


if (! $rel){
    die "Must specify a release version so we know where to put the data.\n";
}

my $rootdir=$Rfam::rfamseq_root_dir;
if ( ! -d "$rootdir/$rel/DATA"){
     die "The directory $rootdir/$rel/DATA for the output doesnt yet exists-this cant be right**check it!!\n";
 }

my $outfile="$rootdir/$rel/DATA/TaxData";
if (-e "$outfile" && !$overwrite){
       die "$outfile already exists-check you want to overwrite it and use the overwrite option\n";
}

#RDB stuff--------
my( $dbHost, $dbPort, $dbName, $dbUser, $dbPass );

$dbHost=$Rfam::rdb_host; 
$dbUser='pfamro';
$dbPort=$Rfam::rdb_port;
$dbName='rfamlive_seq10';
$dbPass='';

# set up the DB connection and statement handles
my $dsn    = "dbi:mysql:database=$dbName;host=$dbHost;port=$dbPort";
my $dbAttr = { RaiseError => 1,
                             PrintError => 1 };
# connect
my $dbh = DBI->connect( $dsn, $dbUser, $dbPass, $dbAttr )
  or die "(EE) ERROR: couldn't connect to database: $!";

#
my $asth = $dbh->prepare( "select ncbi_id, rfamseq_acc, version from rfamseq  group by ncbi_id;" )
  or die '(EE) ERROR: couldn\'t prepare query to select data from ' . $dbh->errstr;


####  NCBI TAX IDS  #####---------------------------------------------------------

print STDERR "Getting the data from the rfamlive RDB\n";
my $data;
 unless( $data = getData() ) {
        die "(WW) WARNING: couldn't retrieve the data from rfamlive";
        next;
    }

my %tax;  
my %acc2ncbi; #keys=accession value=ncbi_id
my @accs;

#this is a big query so write the data to file incase fails..
print STDERR "writing the sql data to file\n";
open(STORE, ">SQl_tax_data") || die "Cant open the file for sql data\n";
foreach my $row (@$data){
    my ($ncbi_id, $acc, $v)= @$row;
    
    $acc2ncbi{$acc.".".$v}=$ncbi_id;
    push(@accs, $acc.".".$v);
    print STORE "$ncbi_id\t$acc"."."."$v\n";
       
}

close(STORE);

####Do mfetch to get all the taxonomy info
##mfetch in sets of 200 and write to file

open (OUT, ">$outfile") || die "Cant open $outfile\n";

 while (@accs ){
     my @subset;
     @subset=splice(@accs  , 0, 200);
     my %tax;
     my ($ocstring, $acc_v, $species);
     print STDERR "preparing to mfetch a set of 200\n";

     my $fh = IO::File->new;
     $fh  -> open( "mfetch  -d emblrelease -f 'tax id org' @subset |" );
     while (<$fh>) {
         if(  /^ID\s+(.*)\.$/ ) {
             my ($acc, $v, $junk)=split(";", $1);
             $v=~ s/\s+SV\s+//g ;
             $acc_v=$acc.".".$v;
         }
         
         if(  /^OS\s+(.*)/ ) {
             $species=$1; 
         }	
         if( /^OC\s+(.*\;)$/ ) {    #previous rows
             $ocstring .= "$1 ";
         }
         if(  /^OC\s+(.*\.)$/ ) {  #last value needed so store and reset
             $ocstring .= "$1";
             if ($ocstring eq "\."){
                 $ocstring="Unclassified";
             }
         
             if ($acc_v && $species && $ocstring){
                 
                 #look up the ncbi id for this 
                 my $ncbi=$acc2ncbi{$acc_v};
                 
                 if (defined $tax{$ncbi}) { die "Already seen this ncbi id $ncbi\n"; }
                 
                 $tax{$ncbi}={'species'=> $species,
                              'tax' => $ocstring
                              };
                 
                 $ocstring=undef;
                 $species=undef;
                 $acc_v=undef;
             } 
         } #end of parsing OC string, ready for next
     }#end of 200
         
     $fh -> close;
     foreach my $av (keys(%tax) ){
         print OUT join("\t",$av,$tax{$av}->{'species'}, $tax{$av}->{'tax'} ), "\n"; 
     }
     
     sleep 2;
 } #end of all accs

close(OUT);




#----------------------------------------------
#SUB Routines----------------------------------------
sub getData{
    $asth->execute();
    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get rfamlive dataa:
"
            . $dbh->errstr . "\n";
        return;
    }
    my $row = $asth->fetchall_arrayref();
     if( $asth->err ) {
        print STDERR "(WW) WARNING: error whilst retrieving query asth"
            . $dbh->errstr . "\n";
        return;
    }
    $asth->finish();
    return $row;
}


#############################################################################
