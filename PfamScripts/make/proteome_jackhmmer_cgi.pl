#!/usr/bin/env perl
#
#
# proteome_jackhmmer_cgi.pl
#
# Author:        jaina
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       Jun 18, 2020
# Last Modified: $Date$


=head1 DESCRIPTION

 A cgi version of proteome_jackhmmer to be used with Pfam Docker

$Author$

=head1 COPYRIGHT

File: proteome_jackhmmer_cgi.pl

Copyright (c) 2020: EMBL-EBI

Author: Jaina Mistry (jaina@ebi.ac.uk)

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
use LWP::UserAgent;
use File::Temp qw(tempfile);
use Cwd;
use Getopt::Long;
use HTTP::Request::Common;

use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

#-------------------------------------------------------------------------------
#Check options and expected input
my ($ncbi_taxid, $help);

&GetOptions(
  "ncbi_taxid=i" => \$ncbi_taxid,
   "help"        => \$help);

if($help or !$ncbi_taxid) {
  print STDERR "Need to supply ncbi taxid on the command line\nE.g. $0 -ncbi_taxid 9606\n";
  help();
}

#-------------------------------------------------------------------------------
# Create a user agent object
my $ua = LWP::UserAgent->new;
$ua->agent("ProteomeJackhmmer/0.1 ");

print STDERR "Querying pfam database to find out which sequences have Pfam domains\n"; 

#Set any proxy
my $config = Bio::Pfam::Config->new;
if ( $config->proxy and $config->proxy =~ /http/ ) {
  print STDERR "Setting proxy\n";
  $ua->proxy( [ 'http', 'ftp', 'https' ], $config->proxy );
}
elsif ( $ENV{http_proxy} ) {
  $ua->env_proxy;
}

# A little insurance
$ua->timeout(600);

# Pass request to the user agent and get a response back
my $res = $ua->request(
  POST 'https://xfamsvn.ebi.ac.uk/cgi-bin/proteome_jackhmmer.cgi',
  Content      => [ ncbi_taxid => $ncbi_taxid]
);

#-------------------------------------------------------------------------------
# Check the outcome of the response, if successful create directories that contain a fasta file
# for each sequence that doesn't have a Pfam domain
# Also print a summary file containing the number of proteins in the proteome that have a Pfam
# domain

my $number_seqs;
my $jackhmmer_dir = "Jackhmmer";
if ( $res->is_success ) {
  open(SUMMARY, ">$ncbi_taxid.txt") or die "Couldn't open fh to $ncbi_taxid.txt, $!";
  mkdir($jackhmmer_dir, 0755) or die "Couldn't mkdir $jackhmmer_dir, $!";
  my $fasta_flag="";
  my $fasta;
  my $acc;
  foreach my $l (split(/\n/, $res->content)){
    if($fasta_flag) {
      if($l =~ /^>(\S+)/) {
        $acc = $1;
        $number_seqs++;
        my $dir = "$jackhmmer_dir/$acc";
        mkdir("$dir", 0755) or die "Couldn't mkdir $dir/$acc, $!";
        open(F, ">$dir/seq.fasta") or die "Couldn't open fh to $dir/seq.fasta, $!";
        print F "$l\n";
      }
      else {
        print F "$l\n";
        close F;
      }
    }
    elsif($l =~ /^End of summary/) {
      $fasta_flag=1;
    }
    else {
      print SUMMARY "$l\n";
    }
  }
  close SUMMARY;
}
else {
  print $res->status_line, "\n";
}

#Loop through all the directories, and run pjbuild on each one
opendir(DIR, $jackhmmer_dir) or die "Can't open dir $jackhmmer_dir, $!";
my @dirs = grep { ! /^\./ } readdir DIR;

print STDERR "Going to run pfjbuild on $number_seqs sequences\n";
chdir($jackhmmer_dir) or die "Couldn't chdir into $jackhmmer_dir, $!";
foreach my $sequence (@dirs) {
  chdir($sequence) or die "Couldn't chdir into $sequence, $!";
  print STDERR "Running pfjbuild on $sequence\n";
  system("pfjbuild -local -N 3 -fa seq.fasta -chk chk -gzip -noOverlap") and die "Failed to run 'pfjbuild -local -N 3 -fa seq.fasta -chk chk -gzip' on $sequence, $!";
  if(-s "align") {  #Check for overlaps 
    system("pfjbuild_overlap_cgi -align align");
  }
  chdir("../") or die "Couldn't chdir up from $sequence, $!";
}

exit;

#-------------------------------------------------------------------------------

sub help {
  print STDERR << "EOF";

This script takes an ncbi taxid as a command line arguent. It looks in
current Pfam database (pfamlive) to see which sequences in the proteome
do not have a Pfam domain. The script is brokered through the pfamsvn 
and should be used by those who can not get direct connection to 
the database. The proxy is taken from PFAM_CONFIG file or environment 
settings.


Usage:

    $0 -ncbi_taxid
EOF

  exit(0);
}
