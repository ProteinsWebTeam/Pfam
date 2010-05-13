#!/usr/local/bin/perl
#
#
# pqc-overlap-cgi.pl
#
# Author:        rdf
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       May 13, 2010
# Last Modified: $Date$


=head1 DESCRIPTION

 Script to perform family overlap check via the pfamsvn server.

$Author$

=head1 COPYRIGHT

File: pqc-overlap-cgi.pl

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
my ( @ignore, $endpoints_opt, $help, $add_to_clan, $remove_from_clan );

&GetOptions(
  "i=s@" => \@ignore,
  "help" => \$help
) or exit;

my $family = shift;

if ( !$family ) {
  print STDERR "ERROR:You have not specified the family\n";
  help();
}

if ( $family =~ /^(\S+)\/$/ ) {
  chop $family;
}

help() if ($help);

#Join all of the accessions that we want to allow overlap
my $ignore;
foreach my $iFam (@ignore) {
  unless ( $iFam =~ /^PF\d{5}$/ ) {
    die "$iFam does no look like a Pfam accession";
  }
  $ignore .= $iFam;
}

#-------------------------------------------------------------------------------
#Find out where we are and configure

my $pwd    = getcwd;
my $config = Bio::Pfam::Config->new;
my ( $fh, $filename ) = tempfile();

#-------------------------------------------------------------------------------
#Check that the family directory exists and that we can write to the directory

if ( !( -d "$pwd/$family" ) ) {
  die
"$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if ( !-w "$pwd/$family" ) {
  die
    "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

#-------------------------------------------------------------------------------
#Harvest all of the information needed for performing overlaps

#Get all of the region information
my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded $family through middleware\n";
getRegions( $famObj, $fh );

#Does the family have nested domains
my $nestedDomains;
if ( $famObj->DESC->NESTS ) {
  foreach my $nesting ( @{ $famObj->DESC->NESTS } ) {
    $nestedDomains = $nesting->{dom};
  }
}

#Does the family belong to a clan?
my $clan;
if ( $famObj->DESC->CL and $famObj->DESC->CL =~ /CL\d{4}/ ) {
  $clan = $famObj->DESC->CL;
}

#-------------------------------------------------------------------------------
# Create a user agent object
my $ua = LWP::UserAgent->new;
$ua->agent("PfamOverlap/0.1 ");

#Set any proxy
if ( $config->proxy and $config->proxy =~ /http/ ) {
  print STDERR "Setting proxy\n";
  $ua->proxy( [ 'http', 'ftp', 'https' ], $config->proxy );
}
elsif ( $ENV{http_proxy} ) {
  $ua->env_proxy;
}

#Some overlaps can be very long and the connection time out is long
#on the svn server.  This is a little insurance.....
$ua->timeout(600);

# Pass request to the user agent and get a response back
my $res = $ua->request(
  POST 'https://pfamsvn.sanger.ac.uk/cgi-bin/overlap.cgi',
  Content_Type => 'form-data',
  Content      => [
    file   => [$filename],
    ignore => $ignore,
    clan   => $clan,
    nest   => $nestedDomains
  ]
);

#-------------------------------------------------------------------------------
# Check the outcome of the response
my $overlaps = 0;
if ( $res->is_success ) {
  print $res->content;
  open( O, ">$pwd/$family/overlap" ) or die "Could not open $pwd/$family/overlap for writing:[$!]\n";
 
  foreach my $l (split(/\n/, $res->content)){
    if($l !~ /^Ignoring/){
      $overlaps++; 
      print O "$l\n" 
    }  
  }
  close(O);

  #Need to count the number of overlaps!!!!
  my $sOverlaps = Bio::Pfam::PfamQC::seedIntOverlaps($famObj);
  $overlaps += $sOverlaps;

  print "Found $overlaps overlaps\n";

}
else {
  print $res->status_line, "\n";
}

if ($overlaps) {
  exit(1);
}
else {
  exit(0);
}

#-------------------------------------------------------------------------------

sub help {
  print STDERR << "EOF";

This script runs checks for overlaps between a Pfam family and the
current Pfam database (pfamlive).  However, this script is brokered 
through the pfamsvn and should be used by those who can not get
direct connection to the database.  Proxy it take from PFAM_CONFIG file
or environment settings.

Usage:

    $0 <family> -i family1 -i family2

Example:

    $0 AAA -ignore other_AAA

EOF

  exit(0);
}

sub getRegions {
  my ( $famObj, $fh ) = @_;

  foreach my $seq ( $famObj->SEED->each_seq ) {
    my $id;
    if ( $seq->id =~ /(\S+)\.\d+/ ) {
      $id = $1;
    }
    else {
      $id = $seq->id;
    }

    my $string = join(
      "\t",
      (
        "SEED",
        $id,
        $seq->start,
        $seq->end,
        ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
        ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" )
      )
    );
    print $fh $string . "\n";
  }

#Then for the full, use the scores file as this contains tha alignment co-ordinates.
#We now allow overlaps between envelopes.
  foreach my $seq ( keys %{ $famObj->scores->regions } ) {
    my $id;
    if ( $seq =~ /(\S+)\.\d+/ ) {
      $id = $1;
    }
    else {
      $id = $seq;
    }
    foreach my $fullReg ( @{ $famObj->scores->regions->{$seq} } ) {
      my $string = join(
        "\t",
        (
          "FULL",
          $id,
          $fullReg->{aliStart},
          $fullReg->{aliEnd},
          ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
          ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" )
        )
      );
      print $fh $string . "\n";
    }
  }
  close($fh);
}
