#!/usr/local/bin/perl
#
# This script allows one to check out a family from a SVN repository containing Pfam families.
# The family that you wish to checkout should be 
use strict;
use warnings;
use Cwd;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;


my $family = shift;

unless(defined $family){
  warn "No family name specified\n"; 
}

if ( $family !~ /^(PF\d{5})$/ ) {
 if($config->location eq 'WTSI'){
  my $connect = $config->pfamlive;
  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
    %{ $connect }
  );
  my $pfamAcc = $pfamDB->id2acc($family);
  unless($pfamAcc =~ /PF\d{5}/){
    warn "You passed in something that did not look like an accession.\n"; 
    warn "Because you are at WTSI, tried to map it to an accession, but failed.\n";
    help();
  }
  $family = $pfamAcc;   
 }else{
  warn "Looks like you have passed in an id rather than accession, [$family]\n";
  help();
 }
}

my $client = Bio::Pfam::SVN::Client->new;


#Top level locks
#Check that the database is not locked

#make sure that directory does not already exist.
my $pwd = getcwd();
my $dest = $pwd."/".$family; 
if (-d $dest ){
  print "The destination directory $dest already exist, remove before checking out a family\n";
  exit(1); 
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
foreach my $file (qw(SEED HMM OUTPUT PFAMOUT scores ALIGN DESC)){
  #Fudge the access time and modification times
  my($atime, $mtime);
  $atime = $mtime = time;
  utime $atime, $mtime, "$dest/$file";
  sleep(1);
}

if( $caught_cntrl_c ) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n**". 
               "The script has tried to ignore this and recover\n** but this could be very bad." . 
               "You really must tell someone about this!\n";
}

sub help {
  print<<EOF;

usage: $0 <PFAM ACCESSION>

Checks out the latest version of the family from the SVN repository.  
It makes a directory for the family according to its accession and will fail if it is already there. 
If you are WTSI, you can use family ids.

EOF
 

exit;
  
}
