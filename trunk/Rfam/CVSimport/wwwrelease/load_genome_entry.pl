#!/software/bin/perl


##this written to parse the genome_assemblies.rgp file.
##it takes each genome acc that we have rfamseqs mapped to
#process:
# (1) makes and entry in the genome_entry table for this genome
# (2) get the auto_genome entry for this genome
# (3) update the rfam_reg_full table with the auto_genome number for all the seqs mapped
#     to this genome. 

##IMPOTANT
##the two lines (48 and 142) that empty or fill tables are hashed out while i check this is doing what
##i expect for each release. These both need to be used with care.

use strict;
use Getopt::Long;

#change this to be /software/ once i have checked in the UpdateRDB.pm module
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


my @tmp = ("genome_entry");
##care with this!!!!
##$rdb->empty_tables( \@tmp );

local $/="//";
open(_FILE, "$read_file");
my @file=<_FILE>;
close (_FILE);

my $count; #count the number of rfamseqs that are mapped to the genomes;

foreach my $entry (@file) {
    my($ac, $de, $oc, @rf, $ci);
    
    my @lines;
    
    @lines= split("\n", $entry);
    chomp @lines;
    my @accarray= grep{/^AC/} @lines;
    my @dearray=  grep{/^DE/} @lines;
    my @ocarray=  grep{/^OC/} @lines;
    my @gparray=  grep{/^GP/} @lines;
    my @ciarray=  grep{/^CI/} @lines;

    if (@accarray >1) {print "accarray > 1\n";die} 
    if (@dearray >1) {print "dearray > 1\n";die}
    if (@ocarray >1) {print "dearray > 1\n";die}
    if (@ciarray >1) {print "ciarray > 1\n";die}
    
    ##collate the accesions from gp array

## parse all the rfamseqs mapped to this genome
    if (@gparray > 0 ) { 
	foreach my $gpline (@gparray){
	    my @gpdata=split("\t", $gpline);
	    my $gbacc=$gpdata[2];
	    $gbacc=~ s/\.\d//;
	    push (@rf, $gbacc);
	}
    }

    $ac=$accarray[0];
    $de=$dearray[0];
    $oc=$ocarray[0];
    $ci=$ciarray[0];

    $ac=~ s/^AC\s+//;
    $de=~ s/^DE\s+//;
    $oc=~ s/^OC\s+//;
    $ci=~ s/^CI\s+//;

    ##deal with those that have no GP lines, only map self acc;
    if (@gparray == 0) {
	if (@rf==0){
	    push (@rf, $ac);
	}
    }

    ##this taken directly from previous sk...not sure why is required but use it anyway??
    
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

    ##### end of section taken from new_genome_species.pl
 
##print reports for checking processed data  
    #print "entry=|$ac|$de|$ci|$joined_tax|@rf|\n";
    #print "entry=|$ac|$de|$ci|\n";
    $count+=scalar(@rf);
##fill tables
    # care with this for loading genome_entry and rfam_reg_full tables
    # $rdb->genomic_species_data($ac, $de, $ci, $joined_tax, @rf);
}
 
print "total mapped rfamseqs=$count\n"; 


