#!/software/bin/perl -w

=head1 NAME

makeGFF3.pl

=head1 DESCRIPTION

Generates the gff3 files for each genome accession from the RDB data. 
Loads the gzip file into genome_gff table if specified.

=head1 AUTHOR

jd7@sanger.ac.uk

=cut


use strict;
use DBI; 
use Rfam;
use Getopt::Long;
use Compress::Zlib;
use DateTime;

my ($help, $release, $db, $load);

GetOptions( "h|help"  => \$help,
	    'release=s' => \$release,
	    'db=s'      => \$db, 
	    'load'    => \$load );

if( $help) {
    &help();
    exit(1);
}

#These are needed for the gff3 header things 
my $sofa='http://song.cvs.sourceforge.net/viewvc/song/ontology/sofa.obo?revision=1.147';
my $ncbi='http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=';

#date
my $dt = DateTime->now; 
my $date = $dt->date;


if (! defined $db){
    die "(EE) The sql database needs to be sepecified\n";
     &help;
}

if (! defined $release){
    die "(EE) The relevant version should be provided e.g 9.0\n";
     &help;
}

#location for ouput files- RELEASE DIR-note all the agp files and other data are actually elsewhere.
my $outdir;
if( not -d "$Rfam::releases_dir/$release/Genomes" ) {
    mkdir( "$Rfam::releases_dir/$release/Genomes", 0755 ) or die "Cant make release directory $Rfam::releases_dir/$release/Genomes $!";
    $outdir="$Rfam::releases_dir/$release/Genomes";
}else {
    $outdir="$Rfam::releases_dir/$release/Genomes";
}
print STDERR "Writing the GFF files to $outdir \n";

#--------------------------------------------

#----------------------------------------------
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

# All the genomes with rfam annotations
my $asth = $dbh->prepare( 'SELECT rf.auto_genome, g.genome_acc, g.description, g.ncbi_id, g.length from rfam_reg_full as rf, genome_entry as g  where rf.auto_genome=g.auto_genome and rf.auto_genome > 0  group by auto_genome' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

my $bsth = $dbh->prepare( 'SELECT * from rfam_reg_full where auto_genome=? order by genome_start' )
  or die '(EE) ERROR: couldn\'t prepare query to select all the rfam_reg_full data from RDB ' . $dbh->errstr;

##family info for each annotation
my $csth = $dbh->prepare( 'SELECT r.rfam_acc, r.rfam_id, r.type, rs.rfamseq_acc, rs.version from rfam_reg_full as rf, rfamseq as rs, rfam as r  where rs.auto_rfamseq=rf.auto_rfamseq and rf.auto_rfam=r.auto_rfam and rf.auto_rfamseq=? and r.auto_rfam=?' )
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;

my $gsth = $dbh->prepare( 'INSERT into genome_gff (auto_genome, gff3) values (?,?)')
  or die '(EE) ERROR: couldn\'t prepare query to select auto_rfamseq from RDB ' . $dbh->errstr;


#--------------------------------------------------
#main
#-------------------------------------------------

#$rfam_genomes is hash of unique auto_genomes with genome_accession
print STDERR "(ii) Getting the list of rfam_genomes to annotate\n";
my $rfam_genomes;
  unless( $rfam_genomes = getRfamGenomes( ) ) {
        die  "(WW) WARNING: couldn't retrieve the rfam_genomes\n\\n";
        next;
  }

#report how many annotations we expect on this genome
my $k= scalar(keys(%$rfam_genomes)) ;
print STDERR "(ii)The number of genomes to annotate= ", $k, "\n";

#Generate GFF info for each genome sequence
foreach my $g ( sort {$a<=>$b} keys (%{$rfam_genomes})){
    
    my %naming; # used to index the multiple family hits on one genome acc

    my $auto_genome=$g;    
    my $ncbi_tax=$rfam_genomes->{$g}->{'tax'};
    my $desc=$rfam_genomes->{$g}->{'de'};
    my $genome_acc=$rfam_genomes->{$g}->{'ac'};  #genome accession from genome_entry table with version
    my $genome_length=$rfam_genomes->{$g}->{'len'};
    my @gff; #assemble the gff lines
    
    #get all the annotations for this auto_genome:all will have annotations.
    my $regions;
    unless( $regions = getRegions($auto_genome ) ) {
        die  "(WW) WARNING: couldn't retrieve the rfam_reg_full annotations\n\\n";
    } 

    print STDERR "Parsing regions for $genome_acc\n";
    #each annotation needs to be parsed
    foreach my $r (@$regions){
	#$r=auto_rfam, auto_rfamseq, auto_genome, seq_start, seq_end, bits, eval, type, genome_start, genome_end, seq
        my ($auto_rf, $auto_rfseq, $junk, $hit_start, $hit_end, $bits, $ev, $junk2, $g_start, $g_end, $seq)=@$r;
 	my $strand='+';
	#fix genome coordinates to +strand
	if ($g_start > $g_end){
	    ($g_start ,$g_end)= ($g_end, $g_start);
	    $strand='-';
	}
	#get the family info for the hit;
	my $annot;
	unless( $annot = &getRfam_Annot($auto_rfseq, $auto_rf ) ) {
	    die  "(WW) WARNING: couldn't retrieve the rfam_genomes for $auto_rf\n\\n";
	    next;
	 }
    	my ($fam_acc, $rfam_id, $type, $rfamseq_acc, $version)=@$annot;

	#get index for annotations IDs which must be unique:
	if (defined $naming{$fam_acc}){
	    ++$naming{$fam_acc};
	}else {
	    $naming{$fam_acc}=1;
	}
	#get SOFA ontology for type
	if ($type=~/Gene/) { $type='ncRNA';} #SOFA SO:
	elsif ($type=~/cis/i) { $type='regulatory_region';} #SOFA SO:
	elsif ($type=~/Intron/i) { $type='autocatalytically_spliced_intron';} #SOFA SO:
	else {die "(EE) Type problem for $genome_acc, $rfamseq_acc, $fam_acc, $rfam_id";}
	
	my $index=$naming{$fam_acc};
	my $gff_index=$fam_acc.".".$index;
	my $string="ID=$gff_index;Name=$fam_acc;Alias=$rfam_id;Note=$rfamseq_acc.$version/$hit_start-$hit_end";
	my @list;

	#construct the 9 column annotation line
	$list[0]=$genome_acc; die if (!defined $genome_acc);#seq_id
	$list[1]='Rfam';      #source
	$list[2]=$type;       die if (!defined $type);#so ontology type
	$list[3]=$g_start;    die if (!defined $g_start);   #xsome hit start
	$list[4]=$g_end;      die if (!defined $g_end); #xsome hit end
	$list[5]=$bits;       die if (!defined $bits); #hit bit score
	$list[6]=$strand;     die if (!defined $strand); #hit strand
	$list[7]='.';         #phase not used
	$list[8]=$string;     die if (!defined $string);#hit index, rfam_acc, hit seq etc 
 	push(@gff, [@list]);
    } # each region 

    &Write($genome_acc, $genome_length, $desc, $ncbi_tax, \@gff);
     if (defined $load ){
        &Load($genome_acc, $auto_genome);
     }

   
}# each genome acc
    $dbh->disconnect;
    
    exit 1;

#-----------------------------------------------------------------
# Subroutines
#------------------------------------------------------------------

 sub Load {
     my ($gacc, $auto_genome)=@_;
     my $in=$gacc.".gff3";
     my $infile="$outdir/$in";   
     open (IN, "<$infile") || die "Cant open $infile\n";
     my @lines=<IN>;
     close(IN);
     
     my $string=join('', @lines);
     my $gzip = Compress::Zlib::memGzip($string) ;
     
     $gsth->execute($auto_genome, $gzip); 
     die '(EE) ERROR: error inserting gff3 data: ' . $dbh->errstr . "\n"
	 if $DBI::err  ;
     
     $gsth->finish();
    
 }


sub Write{
    my ($genome_acc, $head1, $head2, $head3, $gffarray)=@_;
    my $outgff=$genome_acc.'.gff3';
    open (OUT, ">$outdir/$outgff") || die "cant open the outfile $outdir/$outgff";
    print OUT  "##gff-version 3\n";
    print OUT  "##Rfam 10.0 $date\n";
    print OUT  "##sequence-region $genome_acc 1 $head1\n";
    print OUT  "##description $head2\n";
    print OUT  "##species $ncbi".$head3, "\n";
    foreach my $r (@$gffarray){
	print OUT join("\t", @$r), "\n";
    }
        
    close (OUT);
}



sub getRfam_Annot {

  my ($a_rfseq,$a_rf) =@_;
  # retrieve id and and acc info-may get more than one but will always be same:
  $csth->execute($a_rfseq, $a_rf);
  my $row = $csth->fetchrow_arrayref(); 
  die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err  ;
  $csth->finish();
  return $row;
}


sub getRegions {

  my ($auto_g) =shift;
  # retrieve all annotation for a given genome:
  $bsth->execute($auto_g);
  my $rows = $bsth->fetchall_arrayref(); 
  die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err  ;
  $bsth->finish();
  return $rows;
}


sub getRfamGenomes {

  # retrieve data for each genome annotated with families
  $asth->execute;
  my %genomes;
  while( my $row = $asth->fetchrow_arrayref()) {
      my ($ag, $ac, $desc, $ncbi, $len)=@{$row};
	  $genomes{$ag}={ 'ac' => $ac,
	                  'de' => $desc,
			  'tax' => $ncbi,
			  'len' => $len
			  }
  }
  die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err;
  return \%genomes;
}


sub help {
    print STDERR <<EOF;

Replaces the old rfam2chrpl code and uses the RDB to make the rfam genome annotations
Generates gg3 files in the release/Genomes dir and if specified loads the gzipped file into genoem_gff table.

Makes a gff3 format file for all of the genomes we annotate :

-queries the rdb to 
-get a list of all distinct genomes which we have rfam hit annotations to (rfam_reg_full)
-in order makes a gff for each genome.
-they are ordered on xsome start so hits should be in order..
-each annotation:
	-clone strand and hit orientation need to be resolved as the final annotation needs to be wrt to the genome +ve 
	-I converted the strings into -1 and +1 in order to allow -- to cancel itself out easily later on
	-annotations that lie off the ends of the chromosome_builds are excluded using the same code as the old rfam2chr
	-get the family information acc id etc from the RDB
	-index the hits ie multiple RF0000X hits need to be indexed .1 .2 etc and ID must be unique
	-get the correct sofa ontology for the type obtained from the RDB-this will change when we have SO ontology  in the RDB
 	-construct the annotation line

	 
Usage:  ./makeGFF3.pl -db rfam_10_0 -rel 10.0 -load

Options:       -h                  show this help
               -load               load gff3 into genome_gff table

EOF

}


