#!/usr/local/bin/perl
#
# This script allows one to check out a family from a SVN repository containing Pfam families.
# The family that you wish to checkout should be 
use strict;
use warnings;

use Bio::Pfam::SVN::Client;
use Cwd;

my $family = shift;

unless(defined $family){
  warn "No family name specified\n"; 
}

my $client = Bio::Pfam::SVN::Client->new;


#Top level locks
#Check that the database is not locked

#make sure that directory does not already exist.
my $pwd = getcwd();
my $dest = $pwd."/".$family; 
if (-d $dest ){
  print "The destination directory $dest already exist\n"; 
}



#Now start doing the checks!
$client->checkFamilyExists($family);
$client->checkAllFamilyFiles($family);

#Okay- if we have not thrown an exception we should be good to go!


my $caught_cntrl_c;
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

mkdir($dest) or die "Could not make directory $dest:[$!]\n";
$client->checkoutFamily($family, $dest);

#Fix timestamps.....


if( $caught_cntrl_c ) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n**". 
               "The script has tried to ignore this and recover\n** but this could be very bad." . 
               "You really must tell someone about this!\n";
}

