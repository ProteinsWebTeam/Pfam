#!/software/bin/perl

#possibly add more error checks:
#(1) number of GP rows inserted = expected from GParray
#error check when no entry in rfam_eg_full

use strict;
use Getopt::Long;
use Bio::SeqIO;
use DBI;  
use Cwd;
use Rfam;

my($dbname, $help, $release, $file);

&GetOptions(  'release=s' => \$release,
              'help' => \$help,
              'db=s' => \$dbname );

if( $help) {
    &help();
    exit(1);
}

if (! $release){
    die "Need to specify the release version for location of gff files\n";
}

if (! $dbname){
    die "Need to specify the RDB to use e.g 'rfam_9_1'\n";
}

#----------------------------------------------
#RDB on dev connection stuff

my $dbName=$dbname;
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

#-----------------------------------------------

# prepare all the queries that we'll need

# select all of the taxonomy data
my $asth = $dbh->prepare( 'Select ncbi_id, species, tax_string from taxonomy ' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

# select all the distinct ncbi_ids (and total length) that have rfam hits
my $bsth = $dbh->prepare( 'select distinct(g.ncbi_id) from genome_entry as g, rfam_reg_full as rf where g.auto_genome=rf.auto_genome and rf.auto_genome > 0 group by ncbi_id order by ncbi_id ASC;' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

# select all the auto_genomes for this ncbi_id
my $csth = $dbh->prepare( 'Select auto_genome, length from genome_entry where ncbi_id=?' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

# select all the regions to this genome framgment
my $dsth = $dbh->prepare( 'select count(auto_rfamseq) from rfam_reg_full where auto_genome=?' )
   or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

# select all the distinct families annotated on this genome fragment
 my $esth = $dbh->prepare( 'select distinct (auto_rfam) from rfam_reg_full where auto_genome=?' )
   or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

# 
# my $fsth = $dbh->prepare( 'Insert into genome_summary (ncbi_id, species, kingdom, regions, families, genome_size) values (?,?,?,?,?,?)' )
#   or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;



#--------------------------------------------------

#get all the species info and taxids from the rdb
my $species;
unless( $species=get_species() ) {
    die "(WW) WARNING: couldn't get the taxonoy info";
}

#get all of the ncbi_ids 
my $allncbi;
unless( $allncbi=get_ncbi() ) {
    die "(WW) WARNING: couldn't get all the ncbi_ids";
}

#-------------------------------------------------
   

print STDERR "(ii) NUmber of taxids with annotations", scalar(@$allncbi), "\n"; 

#foreach ncbi id with annotations
foreach my $tax (@{$allncbi}){

    my $ncbi=$tax->[0];
    my $g_regions=0;
    my $famhash;
    my $g_fams=0;
    my $g_species;
    my $g_taxstring;
    my $g_kingdom;
    my $g_length=0;

    print STDERR "Collating data for ncbi_id=$ncbi\n";

    #assign the species and tax string
    $g_species=$species->{$ncbi}->{'sp'};
    $g_taxstring=$species->{$ncbi}->{'string'};
    
    if (! $g_species || ! $g_taxstring ) {
	die "(WW) WARNING: Problem with the taxonomy info for ncbi_id $ncbi";
    }
    	 
    #assign kingdom
    my @phylo=split(";", $g_taxstring);
    $g_kingdom=$phylo[0];
    
    if ($g_kingdom != /Archaea|Bacteria|Eukaryotes|Viruses/) {
	die "New kingdom $g_kingdom";
    }
   
    #get all of genome fragments from rdb
    my $genomes;
    unless( $genomes=get_genomes($ncbi) ) {
	die "(WW) WARNING: couldn't get the list of genomes fragmensts";
    }

    
    foreach my $g( @{$genomes}){
	
	my ($auto, $length)=@$g;
	
	$g_length+=$length;

	#get the hits to each of these genome fragments
	#print STDERR "(ii) Getting regions for auto_genome $auto\n";
	my $nregions;

	#some xsome fragments might not have any hits
	if ( $nregions=get_regions($auto)) {
	    $g_regions+=$nregions;
	}else {  
	    print STDERR "(ii) No region annotations for this genome fragment\n";
	    next;
	} 
              
        #print STDERR "(ii) Getting distinct rfam hits for auto_genome $auto\n";
        my $fams;
        unless( $fams=get_distinct($auto) ) {
	    die "(WW) WARNING: Problem with the taxonomy info for ncbi_id $ncbi";
	}
	
	#add the families to the fams hash
	foreach my $af (@$fams){
	    ++$famhash->{$af->[0]};
	}


    } #each genome frag

    $g_fams=keys(%{$famhash});

print STDERR  join("'\t'",$ncbi, $g_species, $g_kingdom, $g_regions, $g_fams, $g_length ), "\n\n";
#	unless( load_summary(\@data) ) {
#	    die "(WW) WARNING: couldn't load the genome_summary info";
#	}
#}
   
} #end of each taxid 
    

$dbh->disconnect   
    or warn "Error disconnecting: $DBI::errtr\n";
exit(1);

###-------------------------------
#SUBROUTINES


# sub load_summary {
#    my $v=shift;
#    my ($tax, $sp, $k, $regions, $fams, $length)=@$v;
#    
#  #get all genome hits 
#     $fsth->execute($tax, $sp, $k, $regions, $fams, $length) ;
#     die "(EE) ERROR: error whilst getting data into genome_entry : " 
# 	. $dbh->errstr . "\n" if $DBI::err;
#    
#    
#    $fsth->finish();
#   
#}


sub get_distinct {

    my $auto_g=shift;
    #get all genome hits 
    $esth->execute($auto_g) ;
    my  $rows=$esth->fetchall_arrayref();
    die "(EE) ERROR: error whilst getting data into genome_entry : " 
	. $dbh->errstr . "\n" if $DBI::err;
    
    
    $esth->finish();
    return $rows;
    
}



sub get_regions {

    my $auto_g=shift;
    #get all genome hits 
    $dsth->execute($auto_g) ;
    my  $row=$dsth->fetchrow();
    die "(EE) ERROR: error whilst getting data into genome_entry : " 
	. $dbh->errstr . "\n" if $DBI::err;
    
    
    $dsth->finish();
    return $row;
    
}


sub get_genomes {
    
    my $ncbi=shift;
    #get all genomes bit 
    $csth->execute($ncbi) ;
    my  $rows=$csth->fetchall_arrayref();
    die "(EE) ERROR: error whilst getting data into genome_entry : " 
	. $dbh->errstr . "\n" if $DBI::err;
    
    
    $csth->finish();
    return $rows;
    
}

sub get_ncbi {

    #get all genomes bit 
    $bsth->execute() ;
     my  $row=$bsth->fetchall_arrayref();
    die "(EE) ERROR: error whilst getting data into genome_entry : " 
	. $dbh->errstr . "\n" if $DBI::err;
       
    $bsth->finish();
    return $row;
    
}



sub get_species {

    my $species;
    $asth->execute();
    while( my $row = $asth->fetchrow_arrayref()) {
        my ($ncbi, $sp, $string)=@{$row};
        $species->{$ncbi}->{'sp'}=$sp;
	$species->{$ncbi}->{'string'}=$string;
    }
    die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err;

    $asth->finish();
    return $species;
  
}

