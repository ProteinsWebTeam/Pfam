#!/usr/bin/env perl

# Generate rscape images and load those to the database. Works on a directory of
# pre-generated annotated SEED files. Product of writeAnnotatedSeed.pl

use warnings;
use strict;
use IO::Compress::Gzip qw(gzip $GzipError);
use Bio::Rfam::Config;
use RfamLive;

my $config = Bio::Rfam::Config->new;

my $family = $ARGV[0];
my $seed_dir = $ARGV[1];
my $outdir = $ARGV[2];

my $reload = '';
my $num_of_params = @ARGV;

# check number of arguments and fetch the 4th if available 
if ($num_of_params == 4){
	my $reload = $ARGV[3];
}
# Create a new connection to RfamLive

my $rfamdb = $config->rfamlive;

#some init steps from the config file
  
my $seed_file = "$seed_dir/$family";
    
#look for a family entry in the database

my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $family } );

if (!defined($famRow)) {
    croak ("Failed to find entry in the Family table for $family.");
}

my $rfam_id = $famRow->rfam_id;

#create a new family directory
my $fam_dir = $outdir.'/'.$family;

if ($reload ne "-rl"){
mkdir $fam_dir;

# construct rscape command and execute
my $rscape_exec = $config->config->{binLocation} . '/R-scape';
my $rscape_cmd = "$rscape_exec --outdir $fam_dir --cyk $seed_file";

print "Making rscape image for $family\n";

system ($rscape_cmd);

	if ($? == -1) {
    	croak ("Failed to generate rscape image for $family!\n");
	}
}
#get and compress images
my $rscape_img = "$fam_dir/$family".'_'.$rfam_id.".R2R.sto.svg";
my $rscape_cyk_img = "$fam_dir/$family".'_'.$rfam_id.".cyk.R2R.sto.svg";

my $rscapeImgGzipped;
my $rscapeCykGzipped;

#compress files and load to database
if (-e $rscape_img){
	gzip $rscape_img => \$rscapeImgGzipped;
#load to database
	my $resultset = $rfamdb->resultset('SecondaryStructureImage')->find_or_create(
                   {    rfam_acc => $family,
                        type => 'rscape'},
                   {    key => 'acc_and_type'});

	$resultset->update({    image => $rscapeImgGzipped,
                        type => 'rscape'},
                   {    key => 'acc_and_type'});
}
if (-e $rscape_cyk_img){
	gzip $rscape_cyk_img => \$rscapeCykGzipped;
	my $fam_cyk_entry = $rfamdb->resultset('SecondaryStructureImage')->find_or_create(
                   {    rfam_acc => $family,
                        type => 'rscape-cyk'},
                   {    key => 'acc_and_type'});

	$fam_cyk_entry->update({image => $rscapeCykGzipped,
                        type => 'rscape-cyk'},
                   {    key => 'acc_and_type'});
}
                   
exit;
        
