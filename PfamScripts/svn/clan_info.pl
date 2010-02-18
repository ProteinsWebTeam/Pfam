#!/usr/local/bin/perl
#
# clan_info.pl  - fetches the latest clandesc file and revision information for the 
# clan from the svn repository 
#
# Author:        rdf
# Maintainer:    $Id$
# Version:       $Revision$
# Created:       Feb 18, 2010
# Last Modified: $Date$
#

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;

#Get the configuration file
my $config = Bio::Pfam::Config->new;

#Check that a command line parameter has been padded in.
help() unless ( $ARGV[0] );
my $clan = shift; #- Right we have something

#-------------------------------------------------------------------------------
#Validate what we have been given
if ( $clan !~ /^(CL\d{4})$/ ) {
  if ( $clan =~ /^PF\d{5}$/ ) {
    die "That looks like a Pfam accession, do you mean pfinfo.pl?\n";
  }
  elsif ( $config->location eq 'WTSI' ) {
    warn
"Looks like you have passed in an ID rather than accession, got [$clan]\n";
    print STDERR "Will try and look up based on ID\n";
    my $connect = $config->pfamlive;
    my $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );
    my $clanAcc = $pfamDB->clanId2Acc($clan);
    unless ( $clanAcc =~ /CL\d{4}/ ) {
      warn "You passed in something that did not look like an accession.\n";
      warn
"Because you are at WTSI, tried to map it to a Clan accession, but failed.\n";
      help();
    }
    $clan = $clanAcc;
  }
  else {
    die
"Looks like you have passed in an ID rather than accession, got [$clan]\n";
  }
}

#-------------------------------------------------------------------------------
#Now do the SVN stuff as we should have a clan accession by now!

#Check that family exists in svn - will die in the subroutine if it does not find it.
my $client = Bio::Pfam::SVN::Client->new;
$client->checkClanExists($clan);

#Should be good to continue!
#This prints the revision history to stdout
$client->log($clan);

#This prints the clandesc file to stdout
$client->catFile( $clan, "CLANDESC" );


#-------------------------------------------------------------------------------
# Subroutines
#-------------------------------------------------------------------------------
=head2 help 

  Title    : help
  Usage    : help() 
  Function : Prints the help message for this script
  Args     : None
  Returns  : Nothing as it exits at the end.
  
=cut
sub help {

  print <<EOF;

usage: $0 <CLAN ACCESSION>

Prints the SVN revision history for the family. 
At WTSI this will work with IDs as the database is local.

EOF

  exit;
}


=head1 clan_info.pl

 clan_info.pl  - fetches the latest clandesc file and revision information for a clan.

=head1 DESCRIPTION

  clan_info.pl  - fetches the latest clandesc file and revision information for a clan.
                  Enter the clan accession on the command line, entering nothing will 
                  print the help message. Finally, if you are lucky, it will try and work
                  out the clan accession if you pass in the identifier.
                  
$Id$

=head1 COPYRIGHT

File: clan_info.pl

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

