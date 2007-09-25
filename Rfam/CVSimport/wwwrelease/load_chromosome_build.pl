#!/software/bin/perl

##this written to parse the genome_assemblies.rgp file.
####what this does:
## for each of the genomes in the .rgp file -> all the rfamseqs that are mapped to the genome need to  
## be associated with the the auto_genome number. so not all of the 
## PROCESS:
## (1) associates all the GP mappings to this gbank acc
## (2) gets the auto_genome for this gbank acc
## (3) looks up rfamseq for the acc of each of the GP mappins.
## (4) if they are found this mapping is added to the chromosome build table
## nb not all GP mappings get entered and not all are in rfamseq
##IMPORTANT
##the two lines (52 and 103) that empty or fill tables are hashed out while i check this is doing what
##i expect for each release. These both need to be used with care.


use strict;
use Getopt::Long;

# this should change to /software once checked in the new UpdateRDB.pm
use lib '/nfs/team71/pfam/jd7/Rfam/scripts/Modules/Rfam/';

use Rfam;
use UpdateRDB;

my($read_file, $dbname);

&GetOptions(  'read=s' => \$read_file,
	      'dbname=s' => \$dbname );


die "need db name\n" if(!$dbname);
die "need readfile\n" if(!$read_file);


my $rdb = Rfam::UpdateRDB->new('-db_name' => $dbname,
                                '-db_driver' => 'mysql',
                                '-db_host' => 'pfamdb2a',
                                '-db_user' => 'pfamadmin',
                                '-db_password' => 'mafpAdmin',
                                '-db_port' => '3302' );


use strict;
use Getopt::Long;
use Bio::SeqIO;


my @tmp = ("chromosome_build");
##care with this !!!!!!
#$rdb->empty_tables( \@tmp );

local $/="//";
open(_FILE, "$read_file");
my @file=<_FILE>;
close (_FILE);

pop @file; #get rid of final // entry

foreach my $entry (@file) {
    my($ac, @chr );
    my @lines;
    
    @lines= split("\n", $entry);
    chomp @lines;

    my @accarray= grep{/^AC/} @lines;
    my @gparray=  grep{/^GP/} @lines;

    if (@accarray >1) {print "accarray > 1\n";die} 
      
  # process all the sequneces that are  mapped to the genomes  
    if (@gparray > 0 ) {
	foreach my $gpline (@gparray){
	    my @gpdata=split("\t", $gpline);
	    my ($xsome_start, $xsome_end, $gbacc, $clone_start, $clone_end, $strand) =@gpdata;  
	    $xsome_start=~ s/^GP\s+//;
	    $gbacc=~ s/\.\d//;
	    my $gp=[$xsome_start, $xsome_end, $gbacc, $clone_start, $clone_end, $strand];
	    push (@chr, $gp); #each row of gp mappings

	}
    }

    $ac=$accarray[0];
    $ac=~ s/^AC\s+//;

    #process all the genomes that are single entries: not GP mappings;
    if (@gparray == 0) {
	my @lenarray= grep{/^LE/} @lines;
	my $len=$lenarray[0];
	$len=~ s/^LE\s+//;
	my $gp=['1', $len, $ac, '1', $len, '+'];
	push (@chr, $gp); #each row of gp mappings
	#print join ("|",@$gp), "\n"
    } ##if no rfamseq matches then skip entry;

#print reports
    #print "entry=$ac=\t@chr\n";
    
#add data to tables
    # $rdb->chromosome_data ($ac, @chr);
     
 }
 


