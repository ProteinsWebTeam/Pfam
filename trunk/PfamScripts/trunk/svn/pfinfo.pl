#!/usr/local/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::FamilyIO;

my $config = Bio::Pfam::Config->new;

unless ( $ARGV[0] ) {
  help();
}

my $family = shift;

if ( $family !~ /^(PF\d{5})$/ ) {
  warn "Looks like you have passed in an id rather than accession, [$family]\n";
}

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
$client->checkFamilyExists($family);
$client->log($family);
$client->catFile( $family, "DESC" );

sub help {

  print<<EOF;

usage: $0 <PFAM ACCESSION>

Prints the SVN revision history for the family. 

EOF
 

exit;  
}
