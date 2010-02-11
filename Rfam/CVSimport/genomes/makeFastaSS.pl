#!/software/bin/perl -w


=head1 NAME

makeFastaSS.pl

=head1 DESCRIPTION

Takes the gff3 files and generates a stockholm format entry for each annotation. 
It reads in the gff3 file but then gets the sequence region and sscons line from the RDB.
It then parses these a baseat a time to remove any unused annotation from the ss line.

it is slow.

so-tend to run it in parallel- set it running on various subsets using prefix letter


./makeFastaSS.pl -db rfam_10_0 -rel 10.0 -set CM


=head1 AUTHOR

jd7@sanger.ac.uk

=cut


use strict;

use Rfam;
use RfamQC;
use Getopt::Long;
use Compress::Zlib;
use bigint;
use Math::BigFloat;
use File::Copy;

my ($help, $release, $db, $set);
&GetOptions( "rel=s" => \$release,
	     'db=s' => \$db,
	     'set=s' => \$set );

if (! $release){
    die "Need to specify the release version for location of gff files\n";
}

if (! $db){
    die "Need to specify the RDB to use e.g 'rfam_9_1'\n";
}
#location for gff files - RELEASE DIR

my $dir="$Rfam::releases_dir/$release/Genomes/";
my $in="genome_gff";
my $out="genome_sscons";
if( ! -d "$dir/$in" ) {
    die "No indirectory: $dir/$in $!";
}

my $outdir;
if( not -d "$dir/$out" ) {
    mkdir( "$dir/$out", 0755 ) or die "Cant make release directory $dir/$out $!";
    $outdir="$dir/$out";
}else {
    $outdir="$dir/$out";
}
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


# get all the sscons we might need
my $asth = $dbh->prepare( 'SELECT rfam_acc, auto_rfam, full_structure from rfam' )
  or die '(EE) ERROR: couldn\'t prepare query to select sscons from rfam ' . $dbh->errstr;

#get all of the genome tax ids 
my $bsth = $dbh->prepare( 'SELECT distinct(t.species), g.ncbi_id from taxonomy as t, genome_entry as g where g.ncbi_id=t.ncbi_id;' )
  or die '(EE) ERROR: couldn\'t prepare query to select sscons from rfam ' . $dbh->errstr;


my $csth = $dbh->prepare( 'SELECT rf.sequence from rfam_reg_full as rf, rfamseq as rs where rf.auto_rfamseq=rs.auto_rfamseq and rs.rfamseq_acc=? and rs.version=? and rf.auto_rfam=? and rf.seq_start=? and rf.seq_end=?' )
  or die '(EE) ERROR: couldn\'t prepare query to select all the rfam_reg_full data from RDB ' . $dbh->errstr;


#=--------------------------------------------

#Read in the gff files in turn:
my @allgff = glob( "$dir/$in/$set*.gff3" );

unless (@allgff){
    die "(EE) Didnt find any gff3 files?\n";
}

#get all the tax and species. key=ncbi_id value=species string
my $tax;
unless( $tax = &getTax() ) {
    die  "(WW) WARNING: couldn't retrieve the ncbi_ids from the RDB\n\\n";
} 

#et all of the sscons. key = rfam_acc value=cons
my $sscons; 
unless( $sscons = &getCons() ) {
    die  "(WW) WARNING: couldn't retrieve the sscons lines from the  RDB\n\\n";
}

 GFF:foreach my $file (@allgff){
     my %index;
     my %rfams;
     my %alias_id;
     my @problems;
     
     my $ff=$file;
     $ff=~s/^.*$in\/(\S+.*)\.gff3.*/$1/g;
     $ff=~s/^\///g;

     open (IN, "<$file") || die "cant open the infile";
     print STDERR "(ii)Reading in $file\n";
     if ( -e "$dir/$out/$ff.sscons") {
	 print STDERR "Skipping $ff\n";
	 next;
     } else{ 
	 open (OUT, ">$dir/$out/$ff.sscons") || die "Cant open the outfile $dir/$out/$ff.sscons $!";
     } 
     print STDERR "(ii)Writing out to $dir/$out/$ff.sscons\n";
     
     my ($genome, $NCBI, $DR);
     while (my $line= <IN>){
         
         if  ($line=~/gff-version/) {next;}
         if ( $line=~/^\#/ && $line=~/sequence-region\s*\b(\S+\.\d+)\b/) {
	     $genome=$1;
             next;
         }
	 
         if ($line=~/^\#/ && $line=~/species\s+\b(http.*id\=)(\d+)$/) {
             $DR=$1;
	     $NCBI=$2;
	     $DR=$1.$NCBI;
	     next;
         }

         #each annotation line:
	 if ($line!~/^\#/){
	     my @gffline=split("\t", $line);
             my ($genome_acc, $j1, $j2, $g_start, $g_end, $bits, $strand, $ph, $string)=split("\t", $line); 
             #if this is a complete genome dont need these extra coordinates
	     #regenerate the genome_mapping
	     my $genome_map;
	     if ($strand eq '+'){
		 $genome_map="$genome_acc/$g_start-$g_end";
	     }else{
		 $genome_map="$genome_acc/$g_end-$g_start";
	     }

	     my ($annot_id,$rfamacc, $alias, $hitmapping);
	     if ($string=~/ID\=(.*)\;Name\=(.*)\;Alias\=(.*)\;Note\=(.*)$/){
		 $annot_id=$1; #unique to gff 
		 $rfamacc=$2;  #group 
		 $alias=$3;  #always the same for every rfamacc
		 $hitmapping=$4; #uniqe to annot_acc
		 if ($hitmapping eq $genome_map) { 
		     $genome_map='';
		 }
	     }
             #store id with the mappings 
             $index{$hitmapping}={ 'genome'   => $genome,
				   'OS'       => $tax->{$NCBI},
				   'DR'       => $DR,
				   'annot'    => $annot_id, 
				   'alias'    => $alias,
				   'map'      => $genome_map };
       
             #array of annotations
             push( @{$rfams{$rfamacc}}, $hitmapping);
         }#end of an annotation line
     } #end of GFF
     close (IN);
     
     my $count=keys (%rfams);
     print STDERR "(ii) Have ", $count, " families to check\n";
 
     #each family:
   FAMILY:foreach my $rf (keys(%rfams)){
       
       #get sscons for this family
       my $refConString;
       my $auto_rfam;
       unless ($refConString=$sscons->{$rf}->{'sscons'}){
	   die "(EE)Problem with sscons for $rf\n";
       }#get consstring from rdb data;

       unless ($auto_rfam=$sscons->{$rf}->{'auto'}){
	   die "(EE)Problem with auto_rfam for $rf\n";
       }#get auto from rdb;
        
      
      
     ANNOT: foreach my $a (@{$rfams{$rf}}){         
	 my $ac;
	 my $v;
	 my $hs;
	 my $he;
         #get the seq string from the RDB for this annotation
         if ($a=~/^(\S+)\.(\d+)\/(\d+)\-(\d+)/){
	     $ac=$1;
	     $v=$2;
	     $hs=$3;
	     $he=$4;
	 }
	     
	 #get the seq string from reg_full
	 my $seqString;
	   unless ($seqString=&getSeq($ac, $v, $auto_rfam, $hs, $he)){
		 die "(EE)Problem getting the seq for $ac from rfam_reg_full$rf $1";
	     }#    

         #check lenghts:
         unless  (checkLength($refConString, $seqString)){
             print STDERR "'$seqString'\n$refConString\n";
             warn "(EE)Problem with original grepped length of seq and cons dont match for $rf, $a $!";
         }
         
	 #These are the starting seqs & strctures
         
         my $conString=$refConString;
         my $delcount=0;
         my (@table, @seq, @con);
         for (my $i=0; $i<length($seqString); $i++){    
             @seq=split(//,$seqString);
             @con=split(//, $conString);
             my $l=scalar(@seq)-1;
             if(! is_nucleotide($seq[$i])){
		 
                 if( is_pair($con[$i])){
                     #get pair index;
                     @table= make_pair_table($conString);
                     
                     my $p=($table[$i+1])-1;
                     #print STDERR "SS Pairs", $seq[$i],"|", $con[$i], " with ", $seq[$p], "|", $con[$p], "\n";
                     #print STDERR "Deleting pair seq at $i, $seq[$i]\n"; 
                     delete $seq[$i]; 
                     #print STDERR "Deleting pair con at $i, $con[$i]\n"; 
                     delete $con[$i];
                     ++$delcount;
                     #set the pair to '.'
                     #print STDERR "Set con pair for $i at $p to '.'\n"; 
		     $con[$p]='.';
                     $seqString=join("",@seq[0..($i-1),($i+1)..$l]);
                     $conString=join("", @con[0..($i-1),($i+1)..$l]); 
                     $i--;
                     
                 }else{ # not nucleotide and is NOT a pair
                     #print STDERR "deleting seq at  $i, $seq[$i]\n"; 
                     delete $seq[$i];
                     #print STDERR "deleting con at  $i, $con[$i]\n"; 
                     delete $con[$i];
                     $seqString=join("",@seq[0..($i-1),($i+1)..$l]);
                     $conString=join("", @con[0..($i-1),($i+1)..$l]);
                     $i--;
                }
                 
            } # end of if not nucleotide
          }#end of loop over the seq string
             
	     unless(&checkLength($seqString, $conString)){
		 die "Problems with code!! Final seq string and con string not eq for $rf, $a\n";
	     }
             #this is needed as some seqs are currently missing from the aligns
             unless (!$seqString) {
                 &writeStock($a, \%index, $seqString, $conString);
	     };
      
     }#each annotation in family
         
    }#each family in the GFF
   #exit;
    close(OUT);
 } #end of each gff3 file
$dbh->disconnect();



#--------------------------------------------------------

sub writeStock{
    my ($seqname, $hash, $seq, $con)=@_;
    
    my $l=length($seqname);
    my $pad_char=" ";
    my $padded = '#=GC SS_cons'. $pad_char x ( $l - 10 );  # ths value 10 is specific to using  #=GC SS_cons string
    
    print OUT "# STOCKHOLM 1.0\n",
              "#=GF  AC  ", $hash->{$seqname}->{'annot'},"\n",
              "#=GF  ID  ", $hash->{$seqname}->{'alias'},"\n",    
              "#=GF  DE  Rfam release $release annotation of ", $hash->{$seqname}->{'genome'}, "\n",
              "#=GF  OS  ", $hash->{$seqname}->{'OS'},"\n",
              "#=GF  DR  ", $hash->{$seqname}->{'DR'},"\n",
              "#=GS  $seqname ",  join(" ", $hash->{$seqname}->{'annot'},  
                                 $hash->{$seqname}->{'alias'},  
                                 $hash->{$seqname}->{'map'}), "\n",
              "$seqname  $seq\n",
              $padded.$con, "\n",
              "//\n"; 
}



sub getSeq{
    my @data=@_;
    $csth->execute(@data);
    my $seq = $csth->fetchrow();
    die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err;
    
    $csth->finish();
    return $seq;  
}

sub getCons{
      my $cons;
    $asth->execute();
    while( my $row = $asth->fetchrow_arrayref()) {
	my ($family_acc, $auto_rf,  $ss)=@{$row};
	$cons->{$family_acc}->{'sscons'}=$ss;
	$cons->{$family_acc}->{'auto'}=$auto_rf;
    }
    die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err;
    
    $bsth->finish();
    return $cons;  
    


 }

sub getTax{
    
    my $tax;
    $bsth->execute();
    while( my $row = $bsth->fetchrow_arrayref()) {
	my ($sp, $tx)=@{$row};
	$tax->{$tx}=$sp;
    }
    die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err;
    
    $bsth->finish();
    return $tax; 
}

sub checkLength{
    my ($c, $s)=@_;
    print STDERR length($c), "and",  length($s), "\n";
    if (length($c) == length($s)){
        return 1;
    }else{
        return 0;
    }
    
}


##############################################################
#Structure cons #
################# 


#returns true if input character is a nucleotide (IUPAC codes):
sub is_nucleotide {
    my $a = shift;
    
   if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
	return 1;
    }
    else {
	return 0;
    }
    
}

#returns true if input character is a basepair:
sub is_pair {
    my $a = shift;
    
   if (defined($a) && length($a) && ($a =~ /[\(\)\<\>\[\]\{\}AaBbCcDdEeFfGgHhIi]/) ){
	return 1;
    }
    else {
	return 0;
    }
    
}












######################################################################
#make_pair_table: takes a structure string and returns an array/pair_table.
#                 pair_table[0] contains the length of the string.
#                 pair_table[1..pair_table[0]] contains 0's if no basepair exists 
#                 for this position, otherwise it contains the index for the 
#                 corresponding pair.
#                 Eg. make_pair_table(".((...))") returns:
#                 (8,0,8,7,0,0,0,3,2)
######################################################################
sub make_pair_table {

    my $str = shift;
    
    my $unbalanced = 0;
    my %bpsymbs5p3p = (
	'(' => ')',
	'<' => '>',
	'[' => ']',
	'{' => '}',
	A => 'a',
	B => 'b',
	C => 'c',
	D => 'd',
	E => 'e',
	F => 'f',
	G => 'g',
	H => 'h',
	I => 'i'
	);

    my %bpsymbs5p3p_counts = (
	'(' => 0,
	'<' => 0,
	'[' => 0,
	'{' => 0,
	A => 0,
	B => 0,
	C => 0,
	D => 0,
	E => 0,
	F => 0,
	G => 0,
	H => 0,
	I => 0
	);

    my %bpsymbs3p5p = ();
    my %bpsymbs3p5p_counts = ();
    
    foreach my $symb5p (keys %bpsymbs5p3p){
	my $symb3p = $bpsymbs5p3p{$symb5p};
	$bpsymbs3p5p{$symb3p} = $symb5p;
	$bpsymbs3p5p_counts{$symb3p} = 0;
    }
    
    my %unpairedsymbs = (
	'.' => 1,
	',' => 2,
	'-' => 3,
	':' => 4,
	'_' => 5
	);
    
    my %bpsymbs_posns = ();
    
    my @pair_table;
    my $prime5;
    my $prime3;
    my $count = 0;
    
    my @ss = split(//,$str);
    $pair_table[0]  = length($str);
    
    my $j=0;
    foreach my $char (@ss){
	$j++;
	$pair_table[$j] = 0;
	
	if ( defined( $unpairedsymbs{$char} ) ) {
	    next; #Boring unpaired region.
	}
	elsif ( defined( $bpsymbs5p3p{$char}) ){#Record position of open bps:
	    push( @{ $bpsymbs_posns{$char} }, $j);
	    ++$count;
	    $bpsymbs5p3p_counts{$char}++;
	}
	elsif ( defined( $bpsymbs3p5p{$char}) ){#close bp, save positions of matches in pair_table:
	    my $mchar = $bpsymbs3p5p{$char};
	    $prime5 = pop( @{ $bpsymbs_posns{$mchar} } );
	    $prime3 = $j;
	    $pair_table[$prime3] = $prime5;
	    $pair_table[$prime5] = $prime3;
	    --$count;
	    $bpsymbs3p5p_counts{$char}++;
	}
       	else {
	    printf STDERR "Strange character \"$char\" in secondary structure:\n$str\n";
	}
    }
    
    #Check basepair symbols are all matched:
    foreach my $symb5p (keys %bpsymbs5p3p_counts){
	my $symb3p = $bpsymbs5p3p{$symb5p};
	my $diff = $bpsymbs5p3p_counts{$symb5p} - $bpsymbs3p5p_counts{$symb3p};
	if ($diff!=0){
	    printf STDERR "BAD EVIL AND NASTY: Unbalanced brackets in secondary structure:\n$bpsymbs5p3p_counts{$symb5p}x\'$symb5p\' and $bpsymbs3p5p_counts{$symb3p}x\'$symb3p\'\n";
	    $unbalanced = 1;
	}
    }
     
    if ($count != 0 || $unbalanced){
	printf STDERR "Unbalanced basepair symbols in secondary structure:\n$str\n";
	return ();
    }    
    else {
	return @pair_table;
    }
    
}
