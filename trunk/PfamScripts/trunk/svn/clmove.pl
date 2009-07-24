#!/usr/local/bin/perl

=head1 COPYRIGHT

File: clmove.pl

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my ( $help, $family, $newName ); 

&GetOptions(
  "help" => \$help,
);

help() if($help);


$clan = shift;
$newClanName = shift;

unless($clan and $clan =~ /\S+/){
  die "Please specifiy the accesion of the clan that you want to change the name\n";  
}

unless($newClanName and $newClanName =~ /\S+/){
  die "Please specifiy the new name of the the clan\n";  
}


if ( $clan !~ /^(CL\d{4})$/ ) {
 if($config->location eq 'WTSI'){
  my $connect = $config->pfamlive;
  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
    %{ $connect }
  );
  my $clanAcc = $pfamDB->clanId2Acc( $clan );
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

unless( Bio::Pfam::PfamQC::nameFormatIsOK($newClanName)) {
  die "\n***** There appears to be a problem with the format of the new name. ****\n".
  "***** Remember, it should be between 1 and 15 characters and not contain symbols other than _ and/or - *****\n\n";  
}

# Now we want to check out the DESC file
my $pwd = getcwd();
my $dest = $pwd."/".$clan; 
if (-d $dest ){
  print "The destination directory $dest already exist, remove before running pfmove out a family\n";
  exit(1); 
}

#Now check that the family exisits in the repository
my $client = Bio::Pfam::SVN::Client->new;
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
my $clanIO = Bio::Pfam::ClanIO->new;
my $descObj = $clanIO->parseCLANDESC( $dest."/CLANDESC");
my $oldClanName = $descObj->ID;

if($oldClanName eq $newClanName){
  die "The old name [$oldClanName] and the new name [$newClanName] are identical\n";  
}

if($descObj->PI){
  $descObj->PI( $descObj->PI." $oldName;");
}else{
  $descObj->PI("$oldName;");
}
$descObj->ID($newName);

#Write the DESC file back out.
$familyIO->writeCLANDESC($descObj, $dest);

#Now commit this file back to the svn repository
open(M, ">.defaultclmove") or die "Could not open .defaultpfmove:[$!]\n";
print M "Moved family ID from $oldName to $newName\n";
close(M);

$client->addCLMOVELog();

$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->commitClan($family);

#Remove any file containing the check-in message
if ( -s ".defaultclmove" ) {
  unlink(".defaultclmove")
    or die "Could not remove old default check-in message\n";
}

#Now update the DESC file so we get the new id in the local copy!
$client->update($clan);

if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}

exit(0);

sub help {
  
  print<<EOF;

usage: $0 <CLAN ACCESSION> <NEW NAME>

  Synopsis
  ========
  Checks out the clan from the SVN repository. Takes the current id and replaces it with the
  new clan name specified on the command line. The old (current) id is added to the CLANDESC file
  as a PI line (or appended on).

  Clan names must conform to the following pattern [\\w_-]{1,15}. i.e. between 1 and 15 
  alpha-numeric characters and/or the following symbols: '_' '-'
  
  Note: If you are at WTSI, you can use family ids instead of accessions.

EOF

exit;
  
}