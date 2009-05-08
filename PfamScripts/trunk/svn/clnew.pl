#!/usr/local/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;

my $config = Bio::Pfam::Config->new;
my $client = Bio::Pfam::SVN::Client->new;

unless ( $ARGV[0] ) {
  help();
}

my $clan = shift;

unless(-d $clan){
  die "$clan is not a local working directory!\n";    
}

my $clanIO = Bio::Pfam::ClanIO->new;
my $clanObj = $clanIO->loadClanFromLocalFile($clan, ".", "file");
print STDERR "Successfully loaded $clan through middleware\n";

$client->checkNewClanDoesNotExists($clanObj->DESC->ID);

#Automatically write the 'new' message and add it the binding.
open(M, ">.defaultclnew") or die "Could not open .defaultclnew:[$!]\n";
print M $clanObj->DESC->ID." deposited\n";
close M;
$client->addCLNEWLog();

#-------------------------------------------------------------------------------
#If we get here, then great! We can now add the family!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->addClan($clan, $clanObj->DESC->ID);

if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}

exit(0);
