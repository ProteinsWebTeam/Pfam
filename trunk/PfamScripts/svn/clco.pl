#!/usr/local/bin/perl
#
# clco.pl <clan_acc> - check out a clan
#
# Author:        rdf
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       Feb 18, 2010
# Last Modified: $Date$

use strict;
use warnings;
use Cwd;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

help() unless($ARGV[0]); 

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
my $dest = $pwd; 
if (-d $dest."/".$clan ){
  print "The destination directory ".$dest."/".$clan." already exist, remove before checking out a clan\n";
  exit(1); 
}

#Now start doing the checks!
$client->checkClanExists($clan);
$client->checkAllClanFiles($clan);

#Okay- if we have not thrown an exception we should be good to go!
my $caught_cntrl_c;
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

mkdir($dest."/".$clan) or die "Could not make directory $dest:[$!]\n";
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

=head1 NAME

 clco.pl - a short description of the script

=cut

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Author$

=head1 COPYRIGHT

File: clco.pl

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk)

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

