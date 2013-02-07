#!/usr/bin/env perl
#
# Simple script to access the DESC file from the SVN repository, and list
# revision history if desired.
#
#
use strict;
use warnings;
use Getopt::Long;

use Bio::Rfam::Config;
use Bio::Rfam::SVN::Client;
use Bio::Rfam::FamilyIO;

my $config = Bio::Rfam::Config->new;

my ($help, $revision);
GetOptions(
  "help" => \$help,
  "rev"  => \$revision
 ) or ( warn "Bad option passed in!" and $help =1);

help() if($help);

unless ( $ARGV[0] ) {
  help();
}

my $entry = $ARGV[0];

if ( $entry !~ /^(RF\d{5})$/ ) {
  warn "Looks like $entry is an identifier, rather than an accession.\n"; 
  if($config->location eq 'EBI'){
    my $rfamdb = $config->rfamlive;
    $entry = $rfamdb->resultset('Family')->id2acc($entry);
  }else{
    $entry = undef;
  }
  if(!$entry){
    die "Please try with an accession or vaild identifier it you are at the EBI\n"; 
  }
}


#Check that family exists in svn
my $client = Bio::Rfam::SVN::Client->new;
$client->checkFamilyExists($entry);
$client->log($entry) if($revision);
$client->catFile( $entry, "DESC" );

sub help {

  print<<EOF;

usage: $0 <RFAM ACCESSION or ID>

Now just prints the DESC file.

-rev :Prints the SVN revision history for the Rfam entry. 


EOF
 
exit;  
}

=head1 COPYRIGHT

File: rfinfo.pl 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Feb 7, 2013 8:31:28 AM

=cut
