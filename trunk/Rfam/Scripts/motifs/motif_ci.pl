#!/usr/bin/env perl

# motif_ci.pl - This script sources motifs from the RMfam GitHub and populates them into RfamLive.

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;
use List::Util qw( min max );

use Bio::Rfam::Config;
use Bio::Rfam::MotifIO;
use Bio::Rfam::QC;
use Bio::Rfam::SVN::Commit;
use RfamLive::ResultSet::Motif;

use Bio::Rfam::FamilyIO;
#------------------------------------------------------------------------------
# Deal with the options

my ($help);
my ($nogit);

&GetOptions(
  "help"             => \$help,
  "nogit"            => \$nogit # This option retrieves the motifs from the local directory as opposed to the git.
) or die "Unrecognised option passed in to the script.\n";

help() if ($help);

my $pwd = "/nfs/production/xfam/rfam/MOTIFS/Motifs";
my $git_source = "https://github.com/ppgardne/RMfam";
my $git_local = "/nfs/production/xfam/rfam/MOTIFS/RMfam";
my $motif_local = "/nfs/production/xfam/rfam/MOTIFS";
my $alignments_dir = "$motif_local"."/RMfam/alignments/";

# Check if we are fetching and parsing the git
unless (defined $nogit) {
  print "Retrieving Motifs from Git\n";
  #getFromGit($pwd,$git_local,$motif_local);
  gitParse($pwd,$alignments_dir);
}
else {
  print "Loading Motifs from Local Directory\n";
}

#----------------------------------------------------------------------------
## Get the motif folder or update the existing the existing motifs from the RMfam Github
# Determine if a local version of RMfam exists as a .git clone
sub getFromGit {
  my ($pwd,$git_local,$motif_local) = @_;
  if ( -d "$git_local"."/.git" ) {
    print "Local RMfam git found. Updating the local version of RMfam to the latest GitHub version ...\n"; 
    chdir($git_local) or die "Unable to open the local git repository";
    system ("git pull origin master");
    print "Local version succesfully updated.\n";
  }
  # If a local version does not exist, clone it from GitHub
  else {
    print "No local RMfam git found. Cloning RMfam from GitHub ...\n";
    chdir($motif_local) or die "Unable to open the local MOTIF folder at $motif_local\n";
    system ("git clone $git_source");
    print "RMfam successfully cloned from Github.\n";
  }
  # Check that the motifs directory exitst and can be written to
  if (!(-w "$pwd") or !(-d "$pwd")) {
    die
    "$0: Can't find/write to directory [$pwd].  Check the folder exists and/or permissions.\n";
  }
}

#----------------------------------------------------------------------------
# Parse the data from the local GitHub repository into a CM, DESC and SEED file

sub gitParse {
  my ($pwd, $alignments_dir) = @_;  
  opendir (DIR, $alignments_dir) or die "Unable to find alignments directory $alignments_dir\n";
  
  # Determine the max RM accession in the database
  my $rfamdb = Bio::Rfam::SVN::Commit->new->{config}->rfamlive;
  my @accesions;
  my @motif_entries = $rfamdb->resultset('Motif')->all();  
  foreach my $entry(@motif_entries){
    my $entry_acc = $entry->get_column('motif_acc'); 
    $entry_acc =~ /(RM)(\d+)/;
    my $accesion_num = $2;
    push (@accesions, $accesion_num);
  }  
  my $max_accession = max @accesions;

  while (my $motif = readdir(DIR)) {
    unless ($motif =~ m/^\./) {
  
    # Create a DESC file from the SEED alignment file using the line #GF AC to get the motif accession
    my $SEED = "$alignments_dir"."$motif"."/SEED";
    my $CM = "$alignments_dir"."$motif"."/CM";  
  
    my $motif_id;
    open my $SEED_file, "<", $SEED or die "Can't open $SEED\n";
    while (my $line = <$SEED_file>) {
      if ($line =~ /(#=GF ID\s+)(\S+)/) {
        $motif_id=$2;
      }
    }

    # Look up the databases for a motif ID. If the ID is there, then use the assigned accession. If not, use the next 
    # free accession with the format RMXXXXX.

    my $motif_acc;
    my $rfamdb = Bio::Rfam::SVN::Commit->new->{config}->rfamlive;
    my $mot = $rfamdb->resultset('Motif')->find( { motif_id => $motif_id } );

    if ($mot) { 
      # We have found an entry with this id. Use the accession in the database
      $motif_acc = $mot->get_column('motif_acc');
      $motif_acc =~ /(RM)(\d+)/;
      if ($2 > $max_accession) {
        $max_accession = $2;
      }
    }
    else {
      # Find the the highest RM accession either in the database or run in this script  
      my $next_number = sprintf("%05d",($max_accession+1));
      $motif_acc = "RM".$next_number;
      $max_accession = $next_number; 
    }  
  
    my $newDESC = "$pwd"."/$motif_acc"."/DESC";
    my $newSEED = "$pwd"."/$motif_acc"."/SEED";
    my $newCM = "$pwd"."/$motif_acc"."/CM";

    chdir ($pwd);
    if (!(-d "$pwd"."/$motif_acc")) {
      system ("mkdir $motif_acc"); 
    }
    system ("perl /homes/evan/Rfam/Scripts/jiffies/seed2desc.pl -acc=$motif_acc $SEED > $newDESC");
  
    print "Creating DESC for motif $motif_id with accession $motif_acc \n";    

    # Copy the SEED and CM file over to new motif working directory
    system ("cp $SEED $newSEED");
    system ("cp $CM $newCM");
    }
  }
}

#----------------------------------------------------------------------------
# Parse the Motif into an object and then into the database
opendir (DIR, $pwd) or die "Unable to find motifs directory $pwd\n";
while (my $motif = readdir(DIR)) {
  unless ($motif =~ m/^\./) {

  # Load the motif from disk into a motif object
  my ( $upMotifObj );
  my $motifIO = Bio::Rfam::MotifIO->new;
  $upMotifObj = $motifIO->loadRfamMotifFromLocalFile( $motif, $pwd );
  print "Successfully loaded $motif from local files\n";

  # Create or Update the database with the Motif Object
  my $commit = Bio::Rfam::SVN::Commit->new;
  $commit->commitMotif($upMotifObj);

  print "Successfully commited $motif to the database\n\n";
  } 
}

#---------------------------------------------------------------------------
# Cat then compress the Motif CMs together with cmpress to create a Motif CMdb 
#my $CMs="";
#opendir (DIR, $pwd) or die "Unable to find motifs directory $pwd\n";
#while (my $motif = readdir(DIR)) {
#  unless ($motif =~ m/^\./) {
#  $CMs="$CMs"."$pwd"."/$motif"."/CM ";
#  }
#} 

#print "Concatenating the CM files ...\n";
#system("cat $CMs > $motif_local/cmdb/CM");
#print "Running cmpress to compress the CMs into a CMdb\n"; 
#system("cmpress -F $motif_local/cmdb/CM");

#--------------------------------------------------------------------------------- 




