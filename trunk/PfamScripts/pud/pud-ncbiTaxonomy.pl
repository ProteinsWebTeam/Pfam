#!/software/bin/perl

##### This code is designed to build up the NCBI taxonomy trees
# At the moment is uses the taxonomy.dat file which is downloaded from EBI

use strict;
use LWP::Simple;
use Getopt::Long;
use Archive::Tar;
use Log::Log4perl qw(:easy);
Log::Log4perl->easy_init($DEBUG);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{  $config->pfamlive }
);

my $logger = Log::Log4perl->get_logger();
$logger->debug("Using database:".$config->pfamlive->{database});
#-------------------------------------------------------------------------------
# Get the taxonomy file from EBI.

$logger->info('Getting taxonomy.dat');
my $taxonomy = get('ftp://ftp.ebi.ac.uk/pub/databases/taxonomy/taxonomy.dat')
  or die
"Failed to get ftp://ftp.ebi.ac.uk/pub/databases/taxonomy/taxonomy.dat:[$!]\n";

#Store the taxonomy file for future reference.
unless ( -d $config->localDbsLoc . '/taxonomy' ) {
  mkdir( $config->localDbsLoc . '/taxonomy' )
    or die "Could not make the directory "
    . $config->localDbsLoc
    . "/taxonomy because: [$!]\n";
}

if(-e $config->localDbsLoc . '/taxonomy/taxonomy.dat'){
  unlink($config->localDbsLoc . '/taxonomy/taxonomy.dat')  
}
open( T, '>' . $config->localDbsLoc . '/taxonomy/taxonomy.dat' )
  or die "Could not open "
  . $config->localDbsLoc
  . "/taxonomy/taxonomy.dat because: [$!]\n";
print T $taxonomy;
close(T);

#-------------------------------------------------------------------------------
# Get the taxonomy data from NCBI
$logger->info('Getting ncbi data');
unless ( -d $config->localDbsLoc . '/ncbi' ) {
  mkdir( $config->localDbsLoc . '/ncbi' )
    or die "Could not make the directory "
    . $config->localDbsLoc
    . "/ncbi because: [$!]\n";
}

foreach my $f (qw(taxdump.tar.gz nodes.dmp)){
  if(-e $config->localDbsLoc . "/ncbi/$f"){
    unlink($config->localDbsLoc . "/ncbi/$f");  
  }
}

my $rc = getstore(
  'ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz',
  $config->localDbsLoc . '/ncbi/taxdump.tar.gz'
);
die 'Failed to get the file ncbi taxdump' unless ( is_success($rc) );


$logger->info('Extracting ncbi data');
my $tar = Archive::Tar->new;
$tar->read( $config->localDbsLoc . '/ncbi/taxdump.tar.gz' );
$tar->extract_file( 'nodes.dmp', $config->localDbsLoc . '/ncbi/nodes.dmp' );

#-------------------------------------------------------------------------------
# Build up all the ncbi nodes. Read into a hash array
$logger->info('Parsing ncbi data (nodes.dmp)');
open( _NODE, $config->localDbsLoc . '/ncbi/nodes.dmp' )
  or die "Could not open " . $config->localDbsLoc . "/ncbi/nodes.dmp: [$!]\n";

my %no_rank;
while (<_NODE>) {
  my ( $ncbi, $parent_id, $rank, $a, $b, $c, $d, $e, $f, $suppressed, $s, $i,
    $j )
    = split( /\|/, $_ );
  $s    =~ s/\s+//g;
  $ncbi =~ s/\s+//g;
  $no_rank{$ncbi} = $ncbi if ($s);
}
close(_NODE);

#-------------------------------------------------------------------------------
#Now parse the taxonomy file
#$logger->info('Parsing taxonomy.dat');
#my(%parents, %scientific_name, $id);
#
#foreach my $l (split(/\n/, $taxonomy)) {
#  if ( $l =~ /^ID\s+\:\s+(\d+)/ ) {
#    $id = $1;
#  }
#  if ( $l =~ /^PARENT ID\s+\:\s+(\d+)/ ) {
#    $parents{$id} = $1;
#  }
#  if ( $l =~ /^SCIENTIFIC NAME\s+\:\s+(.*)/ ) {
#    $scientific_name{$id} = $1;
#  }
#}
#close(_SPECIES);
#
##-------------------------------------------------------------------------------
##
#$logger->info('Munging data together!');
#
#
#foreach my $taxId (sort keys %no_rank) {
#  #$logger->debug("Working on $taxId\n");
#  
#  my $get_spec = 1;
#  my @all_parents;
#  my $species = $scientific_name{$taxId};  
#  my ($first_part) = $1 if ($scientific_name{$taxId} =~ /^(\S+)\s+\S+/);
#
#  my $first = $parents{$taxId};
#  my $full_taxonomy;
#  my $is_the_species = 0;
#  while ($get_spec) {
#    my $parents = $parents{$first};
#    my $full_name = $scientific_name{$first};
#    unless(defined($no_rank{$first})) {
#          $full_taxonomy = $full_name . ";" . $full_taxonomy;
#    }
#    push @all_parents, $parents;
#    $first = $parents;
#    $get_spec = 0 if (!$parents);
#    if  ($full_name eq $first_part)  {
#      $is_the_species = 1;
#    }
#
#  }
#  $full_taxonomy =~ s/root;cellular organisms;//;
#  $full_taxonomy =~ s/root;//;
#
#
#  #$logger->debug("$taxId\t$species\t$full_taxonomy\n");
#  next unless($species);
#  $pfamDB->getSchema
#          ->resultset('NcbiTaxonomy')
#              ->update_or_create( { ncbi_taxid => $taxId,
#                                    species    => $species,
#                                    taxonomy   => $full_taxonomy});                    
#}

#Unclassified will be set to zero;
$no_rank{0}++;
$pfamDB->getSchema
          ->resultset('NcbiTaxonomy')
              ->update_or_create( { ncbi_taxid => '0',
                                    species    => 'unclassified',
                                    taxonomy   => 'unclassified'});
                                    
#Now go through and delete any obsolete taxIds
$logger->debug("Deleting obsolete taxId");
my @allTaxIds = $pfamDB->getSchema
          ->resultset('NcbiTaxonomy')
            ->search();

#-------------------------------------------------------------------------------


foreach my $r (@allTaxIds){
  unless($no_rank{$r->ncbi_taxid}){
    $r->delete({}, {cascade_delete => 0});  
  }  
}



