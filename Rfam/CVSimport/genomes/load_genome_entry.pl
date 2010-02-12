#!/software/bin/perl

=head1 NAME

load_genome_entry.pl 

=head1 DESCRIPTION

Load in the agp files we have generated for each genome accession. Note we dont have annotations to them all
but we put them into the genome_entry table anyway.
It loads in the AGP data to the chromosome_build table.
It then updates Rfam-reg_full with the relevant genome and genome_annotation co-ordinates,

-to do - some sort of flag to allow to either replace existing data in RDB 
       - or method to check if data already in database and prevent overwriting etc

=head1 AUTHOR

jd7@sanger.ac.uk

=cut

use strict;
use Getopt::Long;
use Bio::SeqIO;
use DBI;  
use Cwd;
use Rfam;

my($dbname, $help, $release, $file);

&GetOptions(  'release=s' => \$release,
	      'help' => \$help,
	      'agp=s' => \$file,
	      'db=s' => \$dbname );

if( $help) {
    &help();
    exit(1);
}

if ( !$dbname  || ! $release){
    print STDERR "\n You must specify release(e.g. 10.0) and database name e.g (rfam_10_0) \n";
    &help();
    exit(1);

}

#location of the agp files:
my $rfamseq=$Rfam::rfamseq_current_dir; #nfs loaction
my $genomes="$rfamseq/Genomes\_$release/"; #e.g Genomes_10.0
my $agpfile="$genomes/$file"; #catenated output from make_genome_agp.pl

if (! -d $genomes){
    print STDERR "\n No directory $genomes??\n";
    &help();
    exit(1);

}


if ( ! $file  || ! -e $agpfile){
    print STDERR "\n No full.agp file found at: $agpfile ??\n";
    &help();
    exit(1);

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

#check for content in the tables
my $asth = $dbh->prepare( 'SELECT * from genome_entry limit 2' )
  or die '(EE) ERROR: couldn\'t prepare query to select data from ' . $dbh->errstr;

#check for content in the tables
my $bsth = $dbh->prepare( 'SELECT * from chromosome_build limit 2' )
  or die '(EE) ERROR: couldn\'t prepare query to select data from ' . $dbh->errstr;

# statement for inserting data into genome_entry table
my $csth = $dbh->prepare( 'INSERT into genome_entry (genome_acc, description, ncbi_id, taxonomy, length) values (?,?,?,?, ?) ' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

# query to get the rfamseq id
my $dsth = $dbh->prepare( 'Select auto_rfamseq from rfamseq where rfamseq_acc=?  and version=?' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

# statement to insert into chromosome_build
my $fsth = $dbh->prepare( 'INSERT into chromosome_build 
                          (auto_genome, 
                           auto_rfamseq,
                           xsome_start,
                           xsome_end,
                           clone_start,
                           clone_end,
                           strand) values (?,?,?,?,?,?,?)' )
  or die '(EE) ERROR: couldn\'t prepare statement to insert data into chromosome_build' . $dbh->errstr;

#select relevant annotations from rfam_reg_full;
my $psth = $dbh->prepare( 'SELECT seq_start, seq_end from rfam_reg_full where auto_rfamseq=? and seq_start>=? and seq_end<=? order by seq_start' )
  or die '(EE) ERROR: couldn\'t prepare query to select data from rfam_reg_full' . $dbh->errstr;

#statment to insert into rfam_reg_full
my $gsth = $dbh->prepare( 'UPDATE rfam_reg_full set auto_genome=?, genome_start=?, genome_end=? where auto_rfamseq=? and seq_start=? and seq_end=?' )
  or die '(EE) ERROR: couldn\'t prepare query to insert ino rfam_reg_full' . $dbh->errstr;




#-------------------------------------------------
#maybe make this a flag option of some sort to warn but allow to insert 


#check there is no data in either table before unless flagged
#if (  &checkGenome_entry()) {
#    die "WARNING: Care there is already data in the genome_entry table$!";
#}

#if ( &checkChromosome_build()){
#    die "WARNING: Care there is already data in the chromosme_build table$!";
#}

#--------------------------------------------------
#parse each agpfile set at a time:



#read in the agp file
local $/="//\n";
open(_FILE, "$agpfile");
my @file=<_FILE>;
close (_FILE);

GENOME:foreach my $entry (@file) {
    my($ac, $de, $tx,  $oc, @rf, $ci, $le, $gff);
    my @lines;
    @lines= split("\n", $entry);
    chomp @lines;
    foreach my $line (@lines){
	if ( $line=~/^AC\s+(.*)$/ ) { $ac=$1;}
	if ( $line=~/^DE\s+(.*)$/ ) { $de=$1;}
	if ( $line=~/^TX\s+(.*)$/ ) { $tx=$1;}
	if ( $line=~/^OC\s+(.*)$/ ) { $oc=$1;}
	if ( $line=~/^LE\s+(.*)$/ ) { $le=$1;}
	if ( $ac && $de && $tx && $oc && $le){
	 last;   
	}
    }

    #$ac=~ s/\.\d+//g;
    
    print STDERR "(ii) Parsing data for $ac\n";
    #this is historical text not sure why its needed but format anyway?
    my $formattedtax=&formatTaxString($oc);
    
    #load date into genome_entry:
    my $auto_genome;
    unless( $auto_genome = loadGenomeEntry($ac, $de, $tx, $oc, $le ) ) {
	print STDERR "(WW) WARNING: couldn't insert data into genome_entr table";
	next;
        }
  #  if ($auto_genome > 4) {exit;}    
    #foreac accession in the GPmapping get the auto_rfamseq number
    my @gparray= grep{/^GP/} @lines;
    
    if (@gparray == 0 ) {
	die "No GP lines for $ac";

    }else{
        foreach my $gpline (@gparray){
   
 	    $gpline=~s/^GP\s+//;
	    my @gpdata=split("\t", $gpline);
	    my $gbacc;
	    my $gbv;
	    my $trim_acc;
	   
	    #need to remove the decimal place to fit with rfamseq...its left on in the infile.
	    if ($gpdata[2]=~/^(\S+)\.(\d+)/){
		$gbacc=$1;
		$gbv=$2;
	    }else{
		warn "Problem with the gbacc '$gbacc' not in correct format?\n";
		$dbh->disconnect ;
		
	    }

	    #xsome stat, xsome end clone name clone_start, clone_end
	    @gpdata=@gpdata[0,1,3,4,5]; 
	    
            my ($xsome_start, $xsome_end, $clone_start, $clone_end, $strand)=@gpdata;
	    
            #print STDERR "(ii)Getting the auto_rfamseq for '$gbacc' and '$gbv'\n";
	    my $auto_rfseq;
	    unless( $auto_rfseq = &getAuto_rfamseq($gbacc, $gbv) ) {
		warn "(EE) Sequences for CONs genome $ac, $gbacc are not in Rfamseq so skip";
		next;
	    }

            #add the values from the RDB to the array for this GP line
	    unshift(@gpdata, $auto_genome, $auto_rfseq,);
            #print STDERR join(",", @gpdata), " gpdata\n";;
	    #can now load all the data for this GP line;
	    foreach my $e (@gpdata){
		if (! defined $e){
		    die "(EE) gparray empty values Problem with data for $gbacc";
		}
	    }

	    #print STDERR "(ii) Loading the chromosome build data for '$auto_genome: $auto_rfseq'\n";
	    unless(&loadChromosome_build(@gpdata)) {
		die "(EE) Could not insert the chromosome build data for $gbacc";
		next;
	   }

            #get the rfam_reg_full data for this rfamseq and region
	    my $hits;
	    if( $hits=&get_reg_full_hits($auto_rfseq, $clone_start, $clone_end)) {
		#have lits of hits need to resolve them w.r.t the genome co-ordinates
                foreach my $region (@{$hits}){
		    #print STDERR $region->[0],",", $region->[1], "\n"; next;
                    
		    my @info=(@gpdata[2..6], @$region); # adds hit start and end
		    #$xsome_start, $xsome_end, $clone_start, $clone_end, $strand, $hit_start, $hit_end
		     
		    #resolve
		    my $resolved=&resolve( @info);
		    
		    #load this into rdb along with the auto-genome
		    #print STDERR "(ii) Loading the rfam_reg_full data for suto_rfamseq: '$auto_rfseq'\n";
		    unless( &loadReg_Full($auto_genome, $resolved->[0],$resolved->[1], $auto_rfseq, $region->[0], $region->[1])) {
			die "(EE) Could not insert the rfam_reg_full data for $gbacc";
			next;
		    }
		    
		}#each region on seq
	    }else{
		next; #No hits to this rfamseq in rfamregfull;
	    }

      } #single gp line
    } #end of all GP lines for $ac

    
} #all entries in file

$dbh->disconnect   
    or warn "Error disconnecting: $DBI::errtr\n";

exit(1);

###-------------------------------
#SUBROUTINES

sub checkGenome_entry {
      $asth->execute();
      my $row = $asth->fetchrow;
      die '(EE) ERROR: error whilst retrieving rfamseq info: ' . $dbh->errstr . "\n"
          if $DBI::err;

      $asth->finish;
  if ( $row){
     return 1;
  }else{
     return 0;
  }
      
}


sub checkChromosome_build {
      $bsth->execute();
      my $row = $bsth->fetchrow;
      die '(EE) ERROR: error whilst retrieving rfamseq info: ' . $dbh->errstr . "\n"
          if $DBI::err;

      $bsth->finish;
  if ( $row){
      return 1;
  }else{
      return 0;
  }
      
  }

sub formatTaxString{
    my $oc=shift;
    my(@tax) = split(/;/, $oc);
    my $last;
    my $last_count = 0;
    foreach (@tax) {
      $last = $_;
      $last_count++;
    }
    my($start) = $1 if ($last =~ /(\S+)/);

    my @new_tax;

    if ($start !~ /\./) {
      foreach (@tax) {
        my($first_tmp) =  $1 if ($_ =~ /(\S+)/);
        $first_tmp =~ s/\s+//g;
        $start  =~ s/\s+//g;

        if ($first_tmp eq $start) {
          next;
        } else {
          push @new_tax, $_;
        }
    }
      push @new_tax, $start . ".";
  } else {
      @new_tax = @tax;
  }

    my $joined_tax = join ";",  @new_tax;
    return $joined_tax;

}


sub loadGenomeEntry {
  my( $gnome, $desc, $tax, $len, $type ) = @_;

  #insert genome entry data 
  $csth->execute($gnome, $desc, $tax, $len, $type );

  die "(EE) ERROR: error whilst inserting data into genome_entry table for $gnome: "
               . $dbh->errstr . "\n"
        if $DBI::err;

  my $genome_auto = $csth->{mysql_insertid};
  $dsth->finish();
  return $genome_auto;
  
}

sub getAuto_rfamseq {
  my($rfam_acc, $ver) = @_;

  #get auto_rfamseq 
  $dsth->execute($rfam_acc, $ver );
  my $row=$dsth->fetchrow();
  die "(EE) ERROR: error whilst retreiving auto_rfamseq for $rfam_acc: "
               . $dbh->errstr . "\n"
        if $DBI::err;

  $dsth->finish();  
  return $row;;
  

}

sub loadChromosome_build {
  my(@data ) = @_;

  #insert genome entry data 
  $fsth->execute(@data) ;
  die "(EE) ERROR: error whilst inserting data into genome_entry chromosome_build table: "
               . $dbh->errstr . "\n"
        if $DBI::err;

  
  $fsth->finish();
  
  
}

sub get_reg_full_hits {
  my @rfdata = @_;

  #get auto_rfamseq 
  $psth->execute(@rfdata);
  my $rows=$psth->fetchall_arrayref();
  die "(EE) ERROR: error whilst retreiving auto_rfamseq for:"
      . $dbh->errstr . "\n"
      if $DBI::err;
  
  $psth->finish();  
  return $rows;
  

}

sub resolve{
    #$xsome_start, $xsome_end, $clone_start, $clone_end, $strand, $hit_start, $hit_end
    my ($xs_start, $xs_end, $cl_start, $cl_end, $str, $hit_start, $hit_end)= @_;
    my ($start, $end);
        #co-ordnates are not fixed for hit orientation
        #++ and +-
	if( $str eq '+' ) {                                    
	    $start = $xs_start - $cl_start + $hit_start;
	    $end   = $xs_start - $cl_start + $hit_end;
	}
        #-- and -+
	elsif( $str eq '-') {
	    $start = $xs_start + $cl_end - $hit_start;
	    $end   = $xs_start + $cl_end - $hit_end;
	    $str='+';
	}else{
	    warn "problem with strand value $!";

	}

    my @fixed=($start, $end);
    return \@fixed;

}



sub loadReg_Full {
  my (@data) = @_;

  #insert auto_genome 
  $gsth->execute(@data) ;
  die "(EE) ERROR: error whilst inserting data into rfam_reg_full: "
               . $dbh->errstr . "\n"
        if $DBI::err;

  
  $gsth->finish();

    
}

sub help{

     print STDERR <<EOF;

./load_genome_entry.pl -release 10.0 -db rfam_10_0 -agp genome_agp_embl100.rfam

This will read in the agp file and load the relevant data into the genome_entry, chromosome_build
and rfam_reg_full data. In the rfam_reg_full table it will add which genome the annotation is on 
in addition to adding in the co-ordinates on the genome fragment. Note in this case the co-ordinates
reflect the orientation of the annotation. 

Usage:  load_genome_entry.pl <options>
Options:       -h          show this help
               -release    e.g 10.0
               -agp        agp file to parse and load
               -db         e.g. rfam_10_0

EOF

}
