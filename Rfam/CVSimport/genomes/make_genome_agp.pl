#!/software/bin/perl -w


=head1 NAME

make_genomes_agp.pl

=head1 DESCRIPTION

This generates the Golden Path mapping for all of the complete genomes we are going to map to . Needs the Genomes-assigned file and the CON and ANN files. Should really be run on the farm for the big euk datsets.

=head1 AUTHOR

jd7@sanger.ac.uk

=cut

use strict;
use LWP;
use Getopt::Long;
use Cwd;
use Rfam;
use DBI;  
use DateTime;
use RfamUtils;

my ( $dataset, $release, $db, $help);

&GetOptions( "dataset=s" => \$dataset,
	     "release=s" => \$release,
	     "db=s"=> \$db,
	     "h|help" => \$help
	     );

if( $help) {
    &help();
    exit(1);
}

if ( !$dataset || ($dataset!~/^con|rfam$/) ){
    print STDERR "\n You must specify dataset to parse e.g. 'rfam' or 'con'\n";
    &help();
    exit(1);

}

if ( !$release || $release!~/^\d+\.\d+/){
    print STDERR "\n You must specify -release version e.g 10.0 or 10.1\n";
    &help();
    exit(1);

}

if (! $db || $db!~/^rfam_\d+_\d+$/){
    print STDERR "\n You must specify which RDB to use e.g rfam_9_0\n";
    &help();
    exit(1);

}


#globals
#confiles dir
my $rfamseq=$Rfam::rfamseq_current_dir;
my $con_ann="$rfamseq/CON_ANN";
print STDERR "Using the CON_ANN files from $con_ann\n";

#input and output directory
my $outdir="$rfamseq/Genomes\_$release";
print STDERR "Using the Genomes_assigned file from $outdir\n";

#check for input files
my $infile="$outdir/Genomes_assigned";
if (! -e "$infile"){
    print STDERR "No genomes_assigned file in the $infile\n";
    &help();
    exit(1);
}

#-------------------------------------------
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
my $qsth = $dbh->prepare( 'SELECT r.version, r.description,  r.length, t.species, t.tax_string FROM rfamseq as r, taxonomy as t where r.ncbi_id=t.ncbi_id and r.rfamseq_acc=? and r.version=?' )
  or die '(EE) ERROR: couldn\'t prepare query to retrieve Rfam IDs: ' . $dbh->errstr;

#--------------------------------------------
# get the list of confiles 
my @confiles = glob( "$con_ann/rel_*dat.mini" );

if (scalar(@confiles) == 0){
    die "Problem: no mini confiles in dir $con_ann. Maybe you havent preparsed the con and ann files? Do this with parse_ann_con.pl. It essntially just strips out the lines we need and makes the files a lot smaller\n";
}

#map  species id to confile
my %file;
foreach my $con (@confiles){
    if ( $con=~/rel_\w{3}_(\w{3})_\d+_r\d+.dat/){
        my $species=$1;
        push (@{$file{$species}}, $con);
    }else{
        warn "Formatting of confiles doesnt fit current reg ex $!";
    }
}# hash of con files

print STDERR "Finished parsing the list of CON files\n";
#----------------------------------------------

#read in the accessions mapping data
my @data;
open( L, "< $infile" ) || die "cant open infile ($infile) in $infile $!";
@data=<L>;
chomp @data;
close (L);

#split up the Rfam and CON data
my @Rfam;
my @CON;

if ($dataset eq 'rfam'){
    @Rfam = grep {/Rfam/} @data;
    shift @Rfam; #remove header 
}elsif ($dataset eq 'con'){
    @CON= grep {/CON/} @data;
    shift @CON; #remove header
}else {
    die "Cant determine which dataset to parse should be 'rfam' or 'con'\n";
}

#split the starting CON accessions into type
my %contype; # split accs into files 
my %tax;
foreach my $conrow (@CON){
    my @coninfo=split("\t", $conrow);
    my ($accession, $ebi_ncbi, $rfam_ncbi, $type)=@coninfo[2..5];
    my $sp;
    $tax{$accession}=$ebi_ncbi;
    if ($type=~/^\w{3}_(\w{3})_\d+/){
	$sp=$1;
        push( @{$contype{$sp}}, $accession);
    }else{
	print $type;
	die "Problem with the reg exp on the CON ANN files $type  $!";
    }
}

foreach my $sp (keys (%contype)){
    my $c=scalar(@{$contype{$sp}});
    print STDERR "Require $c Accesssions for species: $sp\n"; 
}

# MAIN ------------------------------------
my $cons; 

#write agp data
if ($dataset eq 'con'){ goto CON; }
my $agpout="$outdir/genome_agp_embl100\.$dataset";
if( -e $agpout ){
print STDERR "Overwriting the existing ap file $agpout\n"; 
}

open (OUT, ">$agpout") || die "cant open outfile $!";

#subroutine this:
#parse each Rfam genome acc
my $skip=0;

foreach my $row ( @Rfam ) {
     $cons={}; 
     my @info=split("\t", $row);
     my $acc=$info[2]; #rfam acc & version
     my $ncbi_id=$info[4]; #rfam/mole assigned ncbi_id not ebi one
     my $trim_acc=$acc;
  
     $trim_acc=~ s/\.(\d+)//g;
     my $v=$1;
     
     #print STDERR "Gathering data for $trim_acc and version '.$v'\n";
     $qsth->execute($trim_acc, $v);
     if ($DBI::err){
	 print STDERR "(WW) Error executing query to get rfamseq data from RDB\n" .$dbh->errstr. "\n";
	 exit; 
     }

     my @data=$qsth->fetchrow();
     if ($dbh->err){
	 print STDERR "(WW) Error retrieving data from RDB\n" .$dbh->errstr. "\n";
	 exit; 
     }
    
     my $datastring=join("", @data);
     if(! @data){
	 print STDERR "(WW)Accession version not in rfamseq!!\n";
	 next;
     }elsif ($data[1]=~/mitochondr|chloroplast/) {++$skip; next;}
     
     #data is :version, description, length,species, tax_string
     $cons->{$acc}->{'AC'} = "AC   $acc"; #has accession.version
     $cons->{$acc}->{'DE'} = "DE   $data[1]";
     $cons->{$acc}->{'TX'} = "TX   $ncbi_id";
     $cons->{$acc}->{'OC'} = "OC   $data[4]";
     $cons->{$acc}->{'OS'} = "OS   $data[3]";
     $cons->{$acc}->{'LE'} = "LE   $data[2]";
     $cons->{$acc}->{'RfamGP'} ="GP   1\t$data[2]\t$acc\t1\t$data[2]\t+";
 } # end of each Rfam acc

my @list=keys(%$cons);
&write(\@list, $cons);
print STDERR "$skip accessions skipped as mitoch/chloroplast\n"; 

#*****************************

#parse each CON file
CON:foreach my $ctype (keys(%contype)){
    

    $cons={}; 
    my $start_time=time();
   # $ctype='hum';
    #outfile
    my $agpout="$outdir/genome_agp_embl100\.$dataset\.$ctype";
    if( -e $agpout ){
	print STDERR "Overwriting the existing ap file $agpout\n"; 
    }
    open (OUT, ">$agpout") || die "cant open outfile $!";

    my $confile=$file{$ctype};
    print STDERR "Reading $ctype con and ann files\n";
    my $consdata= &read_cons($confile);
    print STDERR "Finished parsing con and ann files\n";

    #each accession mapped to this con file 
    print STDERR "Starting to parse each CON accession we need to for $ctype \n";

    STARTACC:foreach my $conacc ( @{$contype{$ctype}}){
        #print STDERR  "Starting acc: $conacc\n";     	
	if ($conacc=~/^DG000/){next STARTACC;}; #skip these for the moment as are con_con_con
        my $trimacc=$conacc;
	$trimacc=~ s/\.\d+//g;
	my $last_GP;
	if ( $consdata->{$trimacc}){
	    
 	    print OUT "AC   $conacc\n";
 	    print OUT "DE   ", $consdata->{$trimacc}->{'DE'}, "\n";
	    print OUT "TX   ", $tax{$conacc},"\n";
 	    print OUT "OC   ", $consdata->{$trimacc}->{'OC'}, "\n";
	    print OUT "OS   ", $consdata->{$trimacc}->{'OS'}, "\n";
	    print OUT "LE   ", $consdata->{$trimacc}->{'length'}, "\n";

	    foreach my $ctg ( @{ $consdata->{$trimacc}->{'ctg'} } ) {
		#check if id is a contig itself (contig of contig mappings);	
		my $nestedacc=$ctg->{'id'};
		
		if (defined  $consdata->{$nestedacc}){
		    #print STDERR  "\tNESTDED acc: $nestedacc\n";
		    my $superstart=$ctg->{'chrst'};
		    my $superend=$ctg->{'chren'};
		    my $checker;
		    my $last_end;
		    
		  NESTEDACC: foreach my $nested_ctg (@{ $consdata->{$nestedacc}->{'ctg'} }){
		      
		      my $nested_ctg_acc=$nested_ctg->{'id'};
		      
		      if ( $consdata->{$nested_ctg_acc}){
			  print STDERR  "DOUBLE NESTED acc:$nested_ctg_acc";    
			  warn "WARNING Nested nested contig $nested_ctg_acc and $trimacc\n";
			  next STARTACC;
			  
		      }else{ 
			  my $newstart=($nested_ctg->{'chrst'}+$superstart)-1;
			  my $newend=($nested_ctg->{'chren'}+$superstart)-1;
			  print OUT ( "GP   ", 
				      $newstart, "\t", $newend, "\t",
				      $nested_ctg->{'id'}, ".", $nested_ctg->{'ver'}, "\t", 
				      $nested_ctg->{'clst'}, "\t", $nested_ctg->{'clen'}, "\t", $nested_ctg->{'strand'},
				      "\n" );
			  
			  $last_end=$newend;
			  
		      }
		      
		  }#each seq in nested con
		      if ($last_end && $superend !=$last_end){
			  die "NESTED:$superend and $last_end should be the same...$nestedacc, $trimacc\n"; 	
		      }
		}else{ #this is a simple contig
		    print OUT ( "GP   ", 
				$ctg->{'chrst'}, "\t", $ctg->{'chren'}, "\t",
				$ctg->{'id'}, ".", $ctg->{'ver'}, "\t", 
				$ctg->{'clst'}, "\t", $ctg->{'clen'}, "\t", $ctg->{'strand'},
				"\n" );
		}
		
		$last_GP=$ctg->{'chren'};
	    }#end of ctg lines for this conacc
		
		#little check on mapping is finished;
		if ( ( $consdata->{$trimacc}->{'length'} - $last_GP ) > 100 ) {
		    print STDERR $last_GP, " ", $consdata->{$trimacc}->{'length'}, "\n";
		    my $missing= ($consdata->{$trimacc}->{'length'}) - $last_GP;
		    print STDERR  "Problem with GP lines-for $trimacc final mapping does not match with CON length $missing bp missing\n";
		}
	    print OUT "\/\/\n";
    
	} #parsing relevant acc
    	
    }#each conacc
    my $end_time=time();
    my $con= $end_time - $start_time;
    my $con_time  = RfamUtils::secs2human($con);
    print STDERR "Time to collect acc from $ctype = ", $con_time,"\n"; 	
    close(OUT);
   # exit;	
}#each ctype

exit(1);
############SUBROUTINES##############################


 sub write{
     #data is :version, description, length,species, tax_string
     my ($acclist, $hash)=@_;
     foreach my $a (@$acclist){
	 
	 print OUT join("\n",  $hash->{$a}->{'AC'},
			       $hash->{$a}->{'DE'},
			       $hash->{$a}->{'TX'},
			       $hash->{$a}->{'OC'},
			       $hash->{$a}->{'OS'},
			       $hash->{$a}->{'LE'},
			       $hash->{$a}->{'RfamGP'},
			       ), "\n\/\/\n";
	 	
    }
} 

sub read_cons {
    my $fileset = shift;
    my %seqs;
    foreach my $file (@$fileset){

        print STDERR "Parsing $file\n";
        open( E, $file ) or die "can't find your cons file [$file]\n";
        my(  $acc, $offset );
        my( $circular, $length );
	while(<E>) {

        if( /^ID\s+.*\s+(\d+)\s+BP\./ ) {
            $length = $1;
            $circular = 0;
            $circular = 1 if( /circular/i );
        }
        if( /^AC\s+(\S+)\;/ ) {
            $acc = $1;
            $offset = 0;
            $seqs{$acc}->{'length'} = $length;
            $seqs{$acc}->{'circular'} = $circular;
        }
        if( /^(DE)\s+(.*)/ or /^(OC)\s+(.*)/ or /^(OS)\s+(.*)/ ) {
            $seqs{$acc}->{$1} .= "$2 ";
        }
        if( my( $stuff ) = /^CO\s+(.*)/ ) {
            foreach my $el ( split( ',', $stuff ) ) {

                my $strand = "+";
                if( $el =~ /gap\((\d+)\)/ ) {
                    $offset += $1;
                    next;
                }

                if( $el =~ /gap\(unk100\)/ ) {
                    $offset += 100;
                    next;
                }
                if( $el =~ /complement\((\S+)\)/ ) {
                    $el = $1;
                    $strand = "-";
                }


                if( my( $id, $ver, $st, $en ) = $el =~ /([A-Z0-9]+)\.(\d+)\:(\d+)\.\.(\d+)/ ) {
                    push( @{ $seqs{$acc}->{'ctg'} }, { 'id'      => $id,
                                                       'ver'     => $ver,
                                                       'clst'    => $st,
                                                       'clen'    => $en,
                                                       'chrst'   => $offset + 1,
                                                       'chren'   => $offset += ($en-$st+1),
                                                       'strand'  => $strand,
                                                   } );
                }
                else {
                    warn "WARNING: don't understand CO element [$el] for this $acc\n";
                }
            }
        }
    }
    close(E);
}#each file
    return \%seqs;
}

#######################################################################################

sub help {
    print STDERR <<EOF;

make_genome_agp.pl -dataset rfam -release 10.0 -db rfam_10_0

This generates the Golden Path mapping for all of the genomes we have annotations to:
This happens differently for those that are whole genomes in Rfam and those that are CON files.

Requires the Genomes_assigned file output from assign.genomes.pl
In addition we need access to :
-need the mini confiles files to run this
-it obtains the information on each genome in Rfam using RDB
-it gets the information from contig genomes from the CON files downloaded with the EMBL release 

-it should output file of agp for all of the whole genome accessions and the CON

Usage:  make_genome_agp.pl <options>
Options:       -h          show this help
               -release    e.g 10.0
               -dataset    rfam | con -will parse datasets sepearately
	       -db         e.g. rfam_10_0
            

EOF

}

