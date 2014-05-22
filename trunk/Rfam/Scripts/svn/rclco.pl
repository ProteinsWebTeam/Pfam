#!/usr/bin/env perl
#
# This script allows one to check out a clan from a SVN repository 
# The clan that you wish to checkout should be in the respository
# and you can grab them via accession and/or id.
#
 
use strict;
use warnings;
use Cwd;

use Bio::Rfam::Config;
use Bio::Rfam::SVN::Client;

my $config = Bio::Rfam::Config->new;
my $db     = $config->rfamlive;

my $entry = shift;

unless(defined $entry){
  warn "No entry identifier or accession specified\n";
  help(); 
}

if ( $entry !~ /^CL\d{5}$/ ) {
  warn "Looks like $entry is an identifier, rather than an accession.\n"; 
  my $entryOri = $entry;
  $entry = $db->resultset('Clan')->id2acc($entry);
  unless($entry){
    die "Could not find anything that looked like $entryOri, please try with an accession.\n";    
  }
}

my $client = Bio::Rfam::SVN::Client->new;

print "$entry\n";

#Top level locks
#Now start doing the checks!
$client->checkClanExists($entry, $config);

#make sure that directory does not already exist.
my $pwd = getcwd();
my $dest = $pwd."/".$entry;

if (-d $dest ){
  print "The destination directory $dest already exist, remove before checking out a family\n";
  exit(1); 
}

$client->checkClanFile($entry);

#Okay- if we have not thrown an exception we should be good to go!


my $caught_cntrl_c;
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

mkdir($dest) or die "Could not make directory $dest:[$!]\n";
$client->checkoutClan($entry, $dest);

if( $caught_cntrl_c ) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n**". 
               "The script has tried to ignore this and recover\n** but this could be very bad." . 
               "You really must tell someone about this!\n";
}

sub help {
  print<<EOF;

usage: $0 <CLAN ACCESSION or IDENTIFIER>

Checks out the latest version of the clan from the SVN repository.  
It makes a directory for the clan according to its accession and will 
fail if it is already there. 

EOF

exit;

}
