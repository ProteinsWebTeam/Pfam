#!/usr/bin/env perl
#
#
# pfjbuild_overlap_cgi.pl
#
# Author:        jaina
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       June 19, 2020
# Last Modified: $Date$


=head1 DESCRIPTION

 Script to perform family overlap check on the pfjbuild output when the -noOverlap
 option is used. It uses the pfamsvn server (so can be used in Pfam Docker).
 It is used by the proteome_jackhmmer_cgi.pl script.

$Author$

=head1 COPYRIGHT

File: pfjbuild_overlap_cgi.pl

Copyright (c) 2020: EMBL-EBI

Authors: Jaina Mistry (jaina@ebi.ac.uk)

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
use Getopt::Long;
use HTTP::Request::Common;

use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

my ($help, $align);
&GetOptions("align=s" => \$align,
           "help" => \$help);

help() if ($help);

unless($align and -s $align) {
  print STDERR "Need to supply the align file on the command line";
  help();
}

my $config = Bio::Pfam::Config->new;

#-------------------------------------------------------------------------------
# Create a user agent object
my $ua = LWP::UserAgent->new;
$ua->agent("PfjbuildOverlap/0.1 ");

#Set any proxy
if ( $config->proxy and $config->proxy =~ /http/ ) {
  print STDERR "Setting proxy\n";
  $ua->proxy( [ 'http', 'ftp', 'https' ], $config->proxy );
}
elsif ( $ENV{http_proxy} ) {
  $ua->env_proxy;
}

#Some overlaps can be very long and the connection time out is long
#This is a little insurance.....
$ua->timeout(600);

# Pass request to the user agent and get a response back
my $res = $ua->request(
  POST 'https://xfamsvn.ebi.ac.uk/cgi-bin/overlap_pfjbuild.cgi',
  Content_Type => 'form-data',
  Content      => [
    file   => [$align],
  ]
);

#-------------------------------------------------------------------------------
# Check the outcome of the response
my $overlaps = 0;
if ( $res->is_success ) {
  open( O, ">overlap" ) or die "Could not open overlap for writing:[$!]\n";
  print O $res->content; 
  close(O);
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


sub help {
  print STDERR << "EOF";

This script runs checks for overlaps between a Pfam family and the
current Pfam database (pfamlive).

Usage:

    $0 -align <alignmemnt_file>

The script is brokered through the pfamsvn and should be used by 
those who can not get direct connection to the database. The proxy 
is taken from PFAM_CONFIG file or environment settings.
EOF

  exit(0);
}


