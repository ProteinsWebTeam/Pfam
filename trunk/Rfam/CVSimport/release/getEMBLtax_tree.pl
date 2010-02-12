#!/software/bin/perl -w

=head1 NAME 

getEMBLtax_tree.pl


=head1 DESCRIPTION

Didnt use this code for sequence update but I  have added it simply so there is a record of how to get the
taxonomy string from Mole for a given ncbi_id using the lft rgt method.

This is essentially the same code as getEMBLtax_mfetch.pl but uses mole to get
the tax string. Found it ...slow.... so never bothered using it plus for N reasons
we decided to use the EMBL data.

=head1 PARAMETERS


=head1 AUTHOR 

jd7@sanger.ac.uk

=cut


use strict;
use DBI;
use Rfam;

### Values we need
my $tax=shift;
my $class=shift;
my %seq;
my %idhash;  #hash for all accessions and list of ncbi taxid 
my %preAcchash;  #hash for all previous accessions for each seq 
my %taxhash; #storing the tax string for each ncbi tax id
my @singles;  #list of accs that have single taxids
my @multi;   #list of accession that need parsed to get a unique taxid

#Rfamseq_dr
my $dir='/lustre/pfam/rfam/Production/rfamseq/9/Taxonomy/';
#File to write list of problem accessions to
my $dupfile='Dup_NCBI_ID';
open ( DUP, ">>$dir/$dupfile") || die "cant open file for writing duplicates to $!";

# File to write out the taxonomy data to:
my $taxfile="Taxonomy"."_".$tax;
open ( TAX, ">>$dir/$taxfile") || die "cant open file for writing taxonomy data  to $!";

#need to get the root for the tax trees;
my %root=( 'PLN' => 276779,
	'HUM' => 276779, 
	'FUN' => 276779,
	'MAM' => 276779,
	'MUS' => 276779,
	'VRT' => 276779,
	'PHG' => 2,
	'ENV' => 71745,
	'INV' => 276779,
	'ROD' => 276779,
	'PRO' => 71745,
	'UNC' => 63144,
	'VRL' => 2,
	
	);

######Mole DB connection parameters--------

my $dbName= 'embl_92';
my $dbHost='cbi3';
my $dbUser='genero';

#Mole connection and statement handles

my $dbAttr = { RaiseError => 1,
                           PrintError => 1 };
#Mole connect
my $dbh = DBI->connect("dbi:mysql:$dbName;$dbHost", $dbUser,"", $dbAttr )  
    or die "(EE) ERROR: couldn't connect to database: $!";


#Mole prepare all the queries 
my $asth = $dbh->prepare("select e.accession_version, t.ncbi_tax_id from taxonomy as t, entry as e 
                          where t.entry_id=e.entry_id and tax_division=? and data_class=?" )
    or die '(EE) ERROR: couldn\'t prepare query to retrieve ncbi tax ids: ' . $dbh->errstr;



######-MUSHROOM-DB parameters------------


my $MRdbName= 'mushroom_200710';

#Mushroom connection and statement handles

my $MRdbAttr = { RaiseError => 1,
                           PrintError => 1 };
#Mushroom connect
my $MRdbh = DBI->connect("dbi:mysql:$MRdbName;$dbHost", $dbUser,"", $MRdbAttr )  
    or die "(EE) ERROR: couldn't connect to database: $!";

#Mushroom queries
my $bsth = $MRdbh->prepare("select n.name  from taxonomy t1, taxonomy t2, taxonomy_name n 
                            where t2.ncbi_tax_id=? and t1.lft <= t2.lft and t1.rgt >= t2.rgt and t1.lft >=? 
                            and t1.ncbi_tax_id=n.ncbi_tax_id and n.name_type='scientific name' order by t1.lft asc")
    or die '(EE) ERROR: couldn\'t prepare query to retrieve tax string from mushroom    : ' . $MRdbh->errstr;
 
##MAIN----------------------------------------------------------------------------------------

####  NCBI TAX IDS  #####---------------------------------------------------------

##get a list of ALL ncbi_tax_id entires for this tax_division and data class
print STDERR "(II) Doing query to get all the tax ids;\n";
my $data;
unless(  $data= getTax($tax, $class )) {
    print STDERR "(EE) ERROR: no data for this tax and class\n";
    next;
}

my $taxEntries=@$data;
print STDERR "'$taxEntries' tax entries are in mole for $tax and STD division\n";

##Parse the mole data for nbi_tax_ids.
##store IDS for each accession:
foreach my $a (@$data){
    my ($acc_v, $ncbiId)=@$a;
    #store the first tax id with the accession
    if (! $idhash{$acc_v}) {
	$idhash{$acc_v}=[$ncbiId];
    }else{
	push (@{$idhash{$acc_v}}, $ncbiId);
    }
}

##Identify which seqs have greater than one ncbi_id
foreach my $acc (keys(%idhash)){
    if (@{$idhash{$acc}} > 1){
	#add to array to process later
	push(@multi, $acc);
    }
    elsif ( @{$idhash{$acc}} == 1) {
	push(@singles, $acc);
    }else{
	die "no data for $acc so problems!";
    }    
}

##Use list of seqs with single ncbi_tax_ids
##put each ncbi_tax_id into array
my @allIDS; 
foreach my $a (@singles){
    push(@allIDS,  $idhash{$a}->[0]);
}

#make unique list of ncbi_tax_ids so we only have to do Mushroom query once for each tax id.
print STDERR "Full list  of $tax ids = ". @allIDS.  "\n";
my  %saw;
my @uniqIDS = grep(!$saw{$_}++, @allIDS);
my @uniqSorted= sort { $a <=> $b } @uniqIDS;

print STDERR "Uniq tax ids for $tax = ". @uniqSorted.  "\n";
print STDERR "Number of $tax seq in STD seqs with > 1 ncbiId= ". @multi.  "\n";
print STDERR "Number of $tax STD seqs with = 1 ncbiId= ". @singles , "\n"; 

print DUP $tax, "\t", join("\t", @multi), "\n";
close(DUP);


#### TAXONOMY STRING #######------------------------------------------------

#Do mushroom queries for each taxid to get the tax string only once for tax-id
print STDERR "(ii) Doing query to get tax string for each ncbi_tax_id;\n";

foreach my $taxid (@uniqSorted){
   #print STDERR "(ii) Getting tax data for $taxid\n";
   my $string;
   my $root=$root{$tax}; #different for each tax division
   unless( $string= getTaxString($taxid, $root ) ) {
       print STDERR "(EE) ERROR: no tax data in mushroom this tax id\n";
       next;
   }

   #catentate all the taxonomy data (OClines)
   my @fullString;

   foreach my $text( @$string ){
       push (@fullString, $text->[0]);
   }
   
    if (@fullString <= 1){
	$taxhash{$taxid}={'string' => 'Unclassified',
			  'species' => 'Unclassified',
		      };
    }else{
	my $species= pop@fullString; 
	$taxhash{$taxid}={'string' => join(";", @fullString),
			  'species' => $species,
		      };
    }
    #print STDERR "got taxonomy for $taxid ", $taxhash{$taxid}->{'species'}, " \n";
    print TAX join("\t", $taxid, $taxhash{$taxid}->{'species'}, $taxhash{$taxid}->{'string'}), "\n";
} #end of parsing each UNIQ tax_id

##tidy up
$dbh->disconnect;
$MRdbh->disconnect;

close (TAX);

#----------------------------------------------
#methods----------------------------------------

sub getTax{

    my $tax=shift;
    my $class=shift;

    $asth->execute($tax, $class);

    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get ncbi tax ids: "
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


sub getTaxString {

    my $ncbi_tx=shift;
    my $root=shift;

    $bsth->execute($ncbi_tx, $root);

    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get ncbi tax string: "
	    . $dbh->errstr . "\n";
        return;
    }
    
    my $row = $bsth->fetchall_arrayref();
    if( $bsth->err ) {
        print STDERR "(WW) WARNING: error whilst retrieving query bsth"
            . $MRdbh->errstr . "\n";
        return;
    }
    
    return $row;

}    





#############################################################################
