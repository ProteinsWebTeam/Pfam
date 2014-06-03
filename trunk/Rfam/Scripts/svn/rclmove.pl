#!/usr/bin/env perl

=head1 COPYRIGHT

File: rclmove.pl

=cut

use strict;
use warnings;
use Cwd;
use Data::Printer;
use Getopt::Long;

use Bio::Rfam::SVN::Client;
use Bio::Rfam::ClanIO;
use Bio::Rfam::QC;

my $config = Bio::Rfam::Config->new;
 my $rfamdb = $config->rfamlive;

my ( $help, $clan, $newName ); 

&GetOptions(
  "help" => \$help,
);

help() if($help);

#Get the parameters from the command line.
$clan = shift;
$newName = shift;

unless($clan and $clan =~ /\S+/){
  die "Please specifiy the accesion of the clan that you want to name change.\n";  
}

unless($newName and $newName =~ /\S+/){
  die "Please specifiy the new name of the the clan.\n";  
}

#Check that the family looks like a Rfam accession, try and map if at EBI
unless($clan =~ /^CL\d{5}$/){
  if(defined($rfamdb)){
    my $clan_acc = $rfamdb->$rfamdb->resultset('Clan')->id2acc($clan);
    unless(defined($clan_acc) and $clan_acc =~ /^CL\d{5}$/){
      warn "You passed in something that did not look like an accession.\n"; 
      warn "Because you are at EBI, tried to map it to an accession, but failed.\n";
      help();
    }
    $clan = $clan_acc;
  }else{
    print STDERR "\n ***** $clan does not look like a clan accession *****\n\n";
    help();
  }  
}

#Check that the new name is okay
if( Bio::Rfam::QC::nameFormatIsOK($newName)) {
  die "\n***** There appears to be a problem with the format of the new name. ****\n".
  "***** Remember, it should be between 1 and 15 characters and not contain symbols other than _ and/or - *****\n\n";  
}

#Now check that the new name does not already exist.  Otherwise, we will have to fail server side.
if(defined($rfamdb)){
  my $row = $rfamdb->resultset('Clan')->find({ id => $newName});
  if(defined($row)){
    die "$newName is already in use for ".$row->clan_acc."\n";
  }
}

# Now we want to check out the DESC file
my $pwd = getcwd();
my $dest = $pwd."/".$clan; 
if (-d $dest ){
  print "The destination directory $dest already exist, remove before running rclmove.\n";
  exit(1); 
}

#Now check that the family exisits in the repository
my $client = Bio::Rfam::SVN::Client->new;
$client->checkClanExists($clan);

#Now load up that  DESC file and change the name, adding the new name to the PI
my $caught_cntrl_c;
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

mkdir($dest) or die "Could not make directory $dest:[$!]\n";
$client->checkoutClan($clan, $dest);

if( $caught_cntrl_c ) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n**". 
               "The script has tried to ignore this and recover\n** but this could be very bad." . 
               "You really must tell someone about this!\n";
}

#Read in the DESC and change the name, adding the old name to the PI!
my $clanIO = Bio::Rfam::ClanIO->new;
my $descObj = $clanIO->parseDESC( $dest."/CLANDESC");
my $oldName = $descObj->ID;

if($oldName eq $newName){
  die "The old name [$oldName] and the new name [$newName] are identical\n";  
}

if($descObj->PI and $descObj->PI =~ /\S+/){
  $descObj->PI( $descObj->PI."; $oldName");
}else{
  $descObj->PI("$oldName;");
}
$descObj->ID($newName);

#Write the DESC file back out.
$clanIO->writeDESC($descObj, $dest);

#Now commit this file back to the svn repository
open(M, ">.default".$$."rclmove") or die "Could not open .default".$$."rclmove:[$!]\n";
print M "Changed clan identifier from $oldName to $newName\n";
close(M);

$client->addRCLMOVELog();
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->commitClan($clan);

#Remove any file containing the check-in message
if ( -s ".default".$$."rclmove" ) {
  unlink(".default".$$."rclmove")
    or die "Could not remove old default check-in message\n";
}

#Now update the clan so we have the latest version.
$client->update($clan);

if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}

exit(0);

sub help {
  
  print<<EOF;

usage: $0 <RFAM CLAN ACCESSION> <NEW NAME>

  Synopsis
  ========
  Checks out the clan from the SVN repository. Takes the current id and replaces 
  it with the new clan name specified on the command line. The old (current) 
  id is added to the DESC file as a PI line (or appended on).

  Clan names must conform to the following pattern [\\w_-]{1,15}. i.e. 
  between 1 and 15 alpha-numeric characters and/or the following symbols: '_' '-'
  
  Note: If you are at EBI, you can use family ids instead of accessions.

EOF

exit;
  
}
