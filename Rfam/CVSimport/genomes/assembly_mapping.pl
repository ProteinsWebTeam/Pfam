#!/usr/local/ensembl/bin/perl -w

#
# A simple script to print out the mapping from chr to contig
# Written for Jennifer Daub 25/11/2008 but thought it could be useful for other people too
#

#setenv PERL5LIB '/path/to/ensembl/modules:/path/to//ensembl-pipeline/modules:/path/to/ensembl-analysis/modules:/path/to/bioperl-1.4:/path/to/ensembl-analysis/scripts:/path/to/ensembl-analysis/scripts/buildchecks/'
#
use strict;
use warnings;

use Bio::EnsEMBL::Utils::Exception qw(throw warning);
use Bio::EnsEMBL::DBSQL::DBAdaptor;
use Getopt::Long;
use ScriptUtils;

my $dbhost;#   = 'ens-livemirror';
my $dbuser;#   = 'ensro';
my $dbname;#   = 'homo_sapiens_core_51_36m';
my $dbport;#   = 3306;
my $lowest_rank;

# output
my $outfile = 'stdout';

# assembly
my $coord_system_version;# = 'NCBI36';
my $coord_system_name;#  = 'chromosome'; # toplevel

# which chromosomes to look at
my $all = undef;
my @seq_region_names;

# buffer
$| = 1;
my $verbose;

&GetOptions(
  'dbhost:s'               => \$dbhost,
  'dbuser:s'               => \$dbuser,
  'dbname:s'               => \$dbname,
  'dbport:n'               => \$dbport,
  'outfile:s'              => \$outfile,
  'verbose'                => \$verbose,
  'coord_system_version:s' => \$coord_system_version,
  'coord_system_name:s'    => \$coord_system_name,
  'lowest_rank:s'          => \$lowest_rank,
  'chromosomes:s'          => \@seq_region_names,
  'all'                    => \$all,
);

if (!$dbhost || !$dbuser || !$dbname || !$dbport || !$lowest_rank) {
  throw("Please enter -dbname, -dbuser, -dbhost and -dbport and -rank");
}
if ($all && scalar(@seq_region_names)) {
  throw("Please enter -chromosome or -all, but not both");
} elsif (!$all && !scalar(@seq_region_names)) {
  throw("Please enter -chromosome or -all");
}

# connect to dbs and get adaptors
my $db = new Bio::EnsEMBL::DBSQL::DBAdaptor(
  -host   => $dbhost,
  -user   => $dbuser,
  -port   => $dbport,
  -dbname => $dbname
);
my $slice_adaptor = $db->get_SliceAdaptor; 

# open outfile
my $fh;
if ($outfile && $outfile ne "stdout") {
  open FH,">$outfile" or die "couldn't open file ".$outfile." $!";
  $fh = \*FH;
} else {
  $fh = \*STDOUT;
}

# # #
# get all the seq_regions we need
# # #
my $chrhash = get_chrlengths_v20($db, $coord_system_version, $coord_system_name);
if ($all) {
  # get all chromosomes
  if (scalar(@seq_region_names) == 0) {
    # we don't filter the chr hash
  } else {
    throw("You have entered -all and -chr.");
  }
} else {
  # get only the chromosomes specified on commandline
  if (scalar(@seq_region_names) == 0) {
    throw("Need either -all or chr name(s).");
  } else {
    filter_to_chr_list(\@seq_region_names,$chrhash,$db->dbc->dbname);
  }
}

# # #
# now loop thru each chr
# # #
print $fh "chromosome:chr_start-chr_end -> contig:contig_start-contig_end\n";
# eg. 21:46687344-46740190 -> AP000337.1.1.53553:1-528471
foreach my $chr (@{sort_chr_names($chrhash)}) {  
  my $chrstart = 1;
  my $chrend   = $chrhash->{$chr};

  my $slicename = "$coord_system_name:$coord_system_version:$chr:$chrstart:$chrend:1";
  my $slice = $slice_adaptor->fetch_by_name($slicename);

  print $fh "CHROMOSOME ".$chr."\tLENGTH ".$slice->length."\n";

  # # #
  # from chr name, find all contigs
  # store the contigs
  # # #
  # get the 'tiling path'
  my $contig_projection_segments = $slice->project($lowest_rank);

  # # #
  # loop thru each contig on the chr
  # # #
  foreach my $segment (@$contig_projection_segments) {
    my $contig_slice = $segment->to_Slice();
    print $fh "GP\t".$slice->seq_region_name(), ':', $segment->from_start(), '-',
          $segment->from_end(), "\t",
          $contig_slice->seq_region_name(), ':', $contig_slice->start(), '-',$contig_slice->end(), "\t",
          $contig_slice->strand(), "\n";
    #print $fh $contig_slice->start()."\t".$contig_slice->end()."\t".$contig_slice->seq_region_name()."\t".$slice->seq_region_name()."\t".($contig_slice->end()-$contig_slice->start()+1)."\t".$contig_slice->strand()."\n";
  }
  print $fh "\/\/\n";
} # chr

