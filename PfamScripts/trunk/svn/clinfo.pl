#!/usr/local/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;

my $config = Bio::Pfam::Config->new;

unless ( $ARGV[0] ) {
  help();
}

my $clan = shift;

if ( $clan !~ /^(CL\d{4})$/ ) {
  warn "Looks like you have passed in an id rather than accession, [$clan]\n";
}

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
$client->checkClanExists($clan);
$client->log($clan);
$client->catFile( $clan, "CLANDESC" );

sub help {

  print<<EOF;

usage: $0 <CLAN ACCESSION>

Prints the SVN revision history for the family. 

EOF
 

exit;  
}
