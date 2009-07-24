#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;


my $clan = shift;

unless(defined $clan){
  warn "No family name specified\n"; 
}

if ( $clan !~ /^(CL\d{4})$/ ) {
 if($config->location eq 'WTSI'){
  my $connect = $config->pfamlive;
  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
    %{ $connect }
  );
  my $clanAcc = $pfamDB->clanId2Acc($clan);
  unless($clanAcc =~ /CL\d{4}/){
    warn "You passed in something that did not look like an accession.\n"; 
    warn "Because you are at WTSI, tried to map it to a Clan accession, but failed.\n";
    help();
  }
  $clan = $clanAcc;   
 }else{
  warn "Looks like you have passed in an id rather than accession, [$clan]\n";
  help();
 }
}

my $client = Bio::Pfam::SVN::Client->new;


#Top level locks
#Check that the database is not locked

#make sure that directory does not already exist.
my $pwd = getcwd();
my $dest = $pwd."/".$clan; 
if (-d $dest ){
  print "The destination directory $dest already exist, remove before checking out a clan\n";
  exit(1); 
}

#Now start doing the checks!
$client->checkClanExists($clan);
$client->checkAllClanFiles($clan);

#Okay- if we have not thrown an exception we should be good to go!
my $caught_cntrl_c;
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

mkdir($dest) or die "Could not make directory $dest:[$!]\n";
$client->checkoutClan($clan, $dest);

if( $caught_cntrl_c ) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n**". 
               "The script has tried to ignore this and recover\n** but this could be very bad." . 
               "You really must tell someone about this!\n";
}

sub help {
  print<<EOF;

usage: $0 <PFAM CLAN ACCESSION>

Checks out the latest version of the clan from the SVN repository.  
It makes a directory for the clan according to its accession and will fail if it is already there. 
If you are WTSI, you can use clan ids.

EOF
 
exit;
  
}